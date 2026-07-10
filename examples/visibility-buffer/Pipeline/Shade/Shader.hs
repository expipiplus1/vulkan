{-# LANGUAGE QuasiQuotes #-}

{-| The shade-pass (deferred resolve) compute shader.

Reads the visibility buffer and writes linear HDR. Every surface reconstructs the
same way (DAIS): fetch the object's transform + mesh, fetch the hit triangle from the
shared vertex SSBO, and barycentric-interpolate world position + normal at the pixel.
Emissive objects (glowstones, the orb) output their stored emissive; lit objects
shade PBR-lite (albedo + metalness + roughness) with per-light @N·L / dist²@ × an
EVSM soft-shadow lookup ("Pipeline.Shadow") plus a normalized Blinn-Phong glint under
the same shadow. A @debugMode@ push overrides the output with a raw material/geometry
channel.

The @Camera@ push carries the receiver-side knobs ('Pipeline.Shade.Tuning'); the EVSM
encoding the shadow cubes were baked with is specialized in.
-}
module Pipeline.Shade.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0, rg32ui) uniform readonly uimage2D visBuffer;
    layout(set = 0, binding = 1, rgba16f) uniform writeonly image2D outColor;

    struct Vertex { vec4 position; vec4 normal; };
    layout(set = 0, binding = 2, std430) readonly buffer Vertices { Vertex verts[]; };
    struct Light { vec4 posHalf; vec4 colInt; };
    layout(set = 0, binding = 3, std430) readonly buffer Lights { Light lights[]; };
    layout(set = 0, binding = 4) uniform samplerCubeArray shadowCube;
    struct Object { mat4 transform; vec4 emissive; uint meshId; uint materialId; uint flags; uint pad; };
    layout(set = 0, binding = 5, std430) readonly buffer Objects { Object objects[]; };
    struct Material { vec4 albedo; vec4 pbr; }; // pbr = (metalness, roughness, -, -)
    layout(set = 0, binding = 6, std430) readonly buffer Materials { Material materials[]; };
    struct MeshEntry { uint baseVertex; uint vertexCount; };
    layout(set = 0, binding = 7, std430) readonly buffer Meshes { MeshEntry meshes[]; };

    layout(push_constant, std430) uniform Camera {
      mat4 viewProj;
      vec4 camPos;
      uint debugMode;
      uint lightCount;
      float ambient;
      float indirect;
      float bleed;
      float shadowBias;
      float normalBias;
    } cam;

    // The EVSM encoding the moment cubes were baked with (Pipeline.Shadow.Params).
    layout(constant_id = 0) const float SHADOW_FAR = 3.0;
    layout(constant_id = 1) const float SHADOW_C = 30.0;

    // NDC of pixel p (centre-sampled), matching the raster's clip space.
    vec2 pixelNdc(ivec2 p, ivec2 size) { return ((vec2(p) + 0.5) / vec2(size)) * 2.0 - 1.0; }

    vec3 objColor(uint id) {
      uint h = id * 2654435761u;
      return vec3(float(h & 255u), float((h >> 8) & 255u), float((h >> 16) & 255u)) / 255.0;
    }

    float linstep(float lo, float hi, float v) { return clamp((v - lo) / (hi - lo), 0.0, 1.0); }

    // One-tailed Chebyshev bound with light-bleed clamp.
    float chebyshev(vec2 m, float t) {
      float var = max(m.y - m.x * m.x, 2e-5);
      float d = t - m.x;
      float pmax = var / (var + d * d);
      return (t <= m.x) ? 1.0 : linstep(cam.bleed, 1.0, pmax);
    }

    // Filtered EVSM visibility of @wpos@ (normal @n@) from light @li@ (1 = lit).
    float shadowVis(uint li, vec3 wpos, vec3 n) {
      vec3 dir = (wpos + n * cam.normalBias) - lights[li].posHalf.xyz;
      float dist = max(0.0, length(dir) / SHADOW_FAR - cam.shadowBias);
      vec4 mo = texture(shadowCube, vec4(normalize(dir), float(li)));
      float posR = exp(SHADOW_C * dist);
      float negR = -exp(-SHADOW_C * dist);
      return min(chebyshev(mo.xy, posR), chebyshev(mo.zw, negR));
    }

    // Shadowed direct light at wpos: diffuse irradiance and the unscaled Blinn glint,
    // sharing one shadow lookup per light. The glint is normalized by (shininess+8)/8π
    // so the lobe keeps its energy as it tightens. Lights below the horizon give
    // neither term. Bounded by cam.lightCount, not lights.length(): an unlit scene
    // still binds a one-slot placeholder buffer that must never be read.
    void directShading(vec3 wpos, vec3 n, float shininess, out vec3 irr, out vec3 spec) {
      vec3 V = normalize(cam.camPos.xyz - wpos);
      float norm = (shininess + 8.0) / (8.0 * 3.14159265);
      irr = vec3(0.0);
      spec = vec3(0.0);
      for (uint i = 0u; i < cam.lightCount; ++i) {
        vec3 dv = lights[i].posHalf.xyz - wpos;
        vec3 L = normalize(dv);
        float ndl = max(0.0, dot(n, L));
        if (ndl <= 0.0) continue;
        float d2 = dot(dv, dv);
        // Fade to zero over the last 20% of the light's reach (Scene.Lights.reach:
        // reach² = intensity / 0.0125). The shadow refresh culls occluders at that
        // same reach, so the light must be spent there or culled shadows would leak.
        float reach2 = lights[i].colInt.w * 80.0;
        float win = 1.0 - smoothstep(0.64 * reach2, reach2, d2);
        if (win <= 0.0) continue;
        float atten = win * lights[i].colInt.w / (d2 + 0.001);
        vec3 radiance = lights[i].colInt.rgb * (atten * shadowVis(i, wpos, n));
        irr += radiance * ndl;
        spec += radiance * (pow(max(0.0, dot(n, normalize(L + V))), shininess) * norm);
      }
    }

    // PBR-lite shade: diffuse (metalness kills it) + Blinn glint (tinted by metal
    // albedo) + ambient specular. Metals get no diffuse, so the ambient specular is
    // their whole response to indirect light; it scales with the environment radiance,
    // which is why an unlit scene leaves them nearly black rather than glowing.
    vec3 shadeSurface(Material mat, vec3 wpos, vec3 n) {
      vec3 alb = mat.albedo.rgb;
      float metal = mat.pbr.x, rough = mat.pbr.y;
      float shininess = exp2(1.0 + (1.0 - rough) * 10.0);
      float strength = mix(0.04, 1.0, metal) * (1.0 - rough);
      vec3 irr, glint;
      directShading(wpos, n, shininess, irr, glint);
      vec3 V = normalize(cam.camPos.xyz - wpos);
      // Schlick fresnel about the normal-incidence reflectance: dielectrics 4%, metals tint.
      vec3 F0 = mix(vec3(0.04), alb, metal);
      vec3 F = F0 + (1.0 - F0) * pow(1.0 - max(0.0, dot(n, V)), 5.0);
      // Uniform environment radiance the surface reflects: constant ambient + a bounce.
      vec3 ambient = cam.ambient + cam.indirect * irr;
      vec3 diffuse = alb * (cam.ambient + irr) * (1.0 - metal);
      vec3 spec = glint * strength * mix(vec3(1.0), alb, metal);
      vec3 env = ambient * F;
      return diffuse + spec + env;
    }

    // DAIS: reconstruct a pixel's world position + smooth normal for any mesh, from
    // its object transform + the hit triangle's shared vertices.
    void meshGeometry(uint objId, uint tri, ivec2 p, ivec2 size, out vec3 wpos, out vec3 nrm) {
      Object obj = objects[objId];
      MeshEntry m = meshes[obj.meshId];
      uint b = m.baseVertex + tri * 3u;
      mat3 nm = mat3(obj.transform);

      vec3 wpA = (obj.transform * vec4(verts[b + 0u].position.xyz, 1.0)).xyz;
      vec3 wpB = (obj.transform * vec4(verts[b + 1u].position.xyz, 1.0)).xyz;
      vec3 wpC = (obj.transform * vec4(verts[b + 2u].position.xyz, 1.0)).xyz;
      vec3 nA = nm * verts[b + 0u].normal.xyz;
      vec3 nB = nm * verts[b + 1u].normal.xyz;
      vec3 nC = nm * verts[b + 2u].normal.xyz;

      vec4 cA = cam.viewProj * vec4(wpA, 1.0);
      vec4 cB = cam.viewProj * vec4(wpB, 1.0);
      vec4 cC = cam.viewProj * vec4(wpC, 1.0);
      vec2 tA = cA.xy / cA.w, tB = cB.xy / cB.w, tC = cC.xy / cC.w;
      vec2 pndc = pixelNdc(p, size);

      vec2 e0 = tB - tA, e1 = tC - tA, e2 = pndc - tA;
      float d00 = dot(e0, e0), d01 = dot(e0, e1), d11 = dot(e1, e1);
      float d20 = dot(e2, e0), d21 = dot(e2, e1);
      float denom = d00 * d11 - d01 * d01;

      if (abs(denom) < 1e-12) {
        nrm = normalize(cross(wpB - wpA, wpC - wpA));
        wpos = wpA;
      } else {
        float l1 = (d11 * d20 - d01 * d21) / denom;
        float l2 = (d00 * d21 - d01 * d20) / denom;
        float l0 = 1.0 - l1 - l2;
        float w0 = l0 / cA.w, w1 = l1 / cB.w, w2 = l2 / cC.w;
        float ws = w0 + w1 + w2;
        nrm = normalize((w0 * nA + w1 * nB + w2 * nC) / ws);
        wpos = (w0 * wpA + w1 * wpB + w2 * wpC) / ws;
      }
    }

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(outColor);
      if (p.x >= size.x || p.y >= size.y) return;

      uvec2 ids = imageLoad(visBuffer, p).rg;
      uint c0 = ids.x;

      Material mat = Material(vec4(0.0), vec4(0.0));
      vec3 nrm = vec3(0.0);
      vec3 col = vec3(0.0);
      uint objId = 0u;

      if (c0 == 0u) {
        col = vec3(0.0); // void
      } else {
        objId = c0 - 1u;
        Object obj = objects[objId];
        if (any(greaterThan(obj.emissive.rgb, vec3(0.0)))) {
          col = obj.emissive.rgb; // emissive glowstone (HDR, drives bloom)
          mat = Material(vec4(obj.emissive.rgb, 1.0), vec4(0.0));
        } else {
          vec3 wpos;
          meshGeometry(objId, ids.y, p, size, wpos, nrm);
          mat = materials[obj.materialId];
          col = shadeSurface(mat, wpos, nrm);
        }
      }

      // Debug channels override the beauty output (see the driver's debugMode).
      vec3 outc = col;
      if (cam.debugMode == 1u) outc = mat.albedo.rgb;
      else if (cam.debugMode == 2u) outc = vec3(mat.pbr.x);           // metalness
      else if (cam.debugMode == 3u) outc = vec3(mat.pbr.y);           // roughness
      else if (cam.debugMode == 4u) outc = (c0 == 0u) ? vec3(0.0) : nrm * 0.5 + 0.5; // normal
      else if (cam.debugMode == 5u) outc = (c0 == 0u) ? vec3(0.0) : objColor(objId); // object id

      imageStore(outColor, p, vec4(outc, 1.0));
    }
  |]
