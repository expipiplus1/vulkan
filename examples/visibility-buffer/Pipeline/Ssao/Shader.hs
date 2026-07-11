{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The SSAO compute shaders.

@normalsCode@ resolves half-res world normals + view depth from the visibility
buffer — the same DAIS reconstruction the shade pass runs ("Pipeline.Dais").
@aoCode@ centres on that exact-texel depth (the min-reduced pyramid would pair
a background depth with a foreground normal at silhouettes) and gathers
Alchemy-style obscurance over a spiral of taps into the depth pyramid
("Pipeline.HiZ"), stepping up the mip ladder as the taps widen. @blurCode@ is
one axis of the separable cross-bilateral blur that eats the spiral noise —
depth- and normal-weighted from the prepass texels, so obscurance never bleeds
across silhouettes or creases; the resolve folds the blurred result into its
ambient terms.
-}
module Pipeline.Ssao.Shader
  ( normalsCode
  , aoCode
  , blurCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)

import Pipeline.Dais (dais, daisTypes)

normalsCode :: ByteString
normalsCode =
  $( compileShaderQ
       Nothing
       "comp"
       Nothing
       [glsl|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0, rg32ui) uniform readonly uimage2D visBuffer;
    layout(set = 0, binding = 1, rgba16f) uniform writeonly image2D outNormal;

    $daisTypes
    layout(set = 0, binding = 2, std430) readonly buffer Vertices { Vertex verts[]; };
    layout(set = 0, binding = 3, std430) readonly buffer Objects { Object objects[]; };
    layout(set = 0, binding = 4, std430) readonly buffer Meshes { MeshEntry meshes[]; };

    layout(push_constant, std430) uniform Prepass {
      mat4 viewProj;
    } cam;

    $dais

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(outNormal);
      if (p.x >= size.x || p.y >= size.y) return;

      // Point-sample the full-res visibility buffer at the pixel this half-res
      // texel covers; w carries the view depth at that same texel (0 = void), so
      // the AO/blur passes never mix this normal with another surface's depth.
      ivec2 fullSize = imageSize(visBuffer);
      ivec2 fp = min(p * 2, fullSize - 1);
      uvec2 ids = imageLoad(visBuffer, fp).rg;
      if (ids.x == 0u) {
        imageStore(outNormal, p, vec4(0.0));
        return;
      }

      vec3 wpos, nrm;
      meshGeometry(ids.x - 1u, ids.y, fp, fullSize, wpos, nrm);
      // Reverse-Z clip: w is the view-space depth (positive forward).
      float zView = (cam.viewProj * vec4(wpos, 1.0)).w;
      imageStore(outNormal, p, vec4(nrm, zView));
    }
  |]
   )

aoCode :: ByteString
aoCode =
  $( compileShaderQ
       Nothing
       "comp"
       Nothing
       [glsl|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0) uniform sampler2D depthPyramid;
    layout(set = 0, binding = 1, rgba16f) uniform readonly image2D normals;
    layout(set = 0, binding = 2, r16f) uniform writeonly image2D outAo;

    layout(push_constant, std430) uniform Ao {
      mat4 view;
      float sx;      // proj diagonal: ndc.xy = (sx, sy) * view.xy / view.z
      float sy;
      float zNear;   // reverse-Z: depth = zNear / view.z
      float radius;  // world units
      float intensity;
      float bias;    // world units, against self-occlusion
    } pc;

    const uint TAPS = 12u;
    const float GOLDEN = 2.3999632;

    // View-space position at @uv@ with view depth @z@.
    vec3 viewAt(vec2 uv, float z) {
      vec2 ndc = uv * 2.0 - 1.0;
      return vec3(ndc.x * z / pc.sx, ndc.y * z / pc.sy, z);
    }

    // View-space position of the pyramid texel at @uv@ holding reverse-Z depth @d@.
    vec3 viewPos(vec2 uv, float d) { return viewAt(uv, pc.zNear / d); }

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(outAo);
      if (p.x >= size.x || p.y >= size.y) return;

      vec4 nw = imageLoad(normals, p);
      if (nw.w == 0.0) {
        imageStore(outAo, p, vec4(1.0));
        return;
      }

      // Centre on the prepass depth — the exact texel the normal came from; the
      // pyramid's min-reduce would pair a background depth with this normal at
      // silhouettes, ringing them with a dark halo.
      vec2 uv = (vec2(p) + 0.5) / vec2(size);
      vec3 P = viewAt(uv, nw.w);
      vec3 n = mat3(pc.view) * nw.xyz;

      // The world radius on screen at this depth, and in pyramid pixels for the
      // mip ladder (the min-reduce biases far, understating coarse-tap horizons —
      // acceptable for obscurance).
      vec2 uvRadius = pc.radius * vec2(pc.sx, pc.sy) / (2.0 * P.z);
      float pxRadius = uvRadius.y * float(size.y);

      float rot = float((p.x * 3 + p.y * 5) & 7) * 0.7853982;
      float occ = 0.0;
      for (uint i = 0u; i < TAPS; ++i) {
        float t = (float(i) + 0.5) / float(TAPS);
        float ang = float(i) * GOLDEN + rot;
        vec2 tuv = uv + vec2(cos(ang), sin(ang)) * t * uvRadius;
        if (any(lessThan(tuv, vec2(0.0))) || any(greaterThanEqual(tuv, vec2(1.0)))) continue;
        float lod = clamp(log2(max(t * pxRadius, 1.0)) - 3.0, 0.0, 5.0);
        float td = textureLod(depthPyramid, tuv, lod).r;
        if (td <= 0.0) continue;
        // Alchemy obscurance: falloff-weighted clamped cosine to the horizon sample.
        vec3 v = viewPos(tuv, td) - P;
        occ += max(0.0, dot(v, n) - pc.bias) / (dot(v, v) + 0.01 * pc.radius * pc.radius);
      }

      float ao = clamp(1.0 - pc.intensity * (2.0 * pc.radius / float(TAPS)) * occ, 0.0, 1.0);
      imageStore(outAo, p, vec4(ao));
    }
  |]
   )

blurCode :: ByteString
blurCode =
  $( compileShaderQ
       Nothing
       "comp"
       Nothing
       [glsl|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0, rgba16f) uniform readonly image2D normals;
    layout(set = 0, binding = 1, r16f) uniform readonly image2D src;
    layout(set = 0, binding = 2, r16f) uniform writeonly image2D dst;

    layout(push_constant, std430) uniform Blur {
      float sharpness; // depth falloff: a |Δz| of z/sharpness costs one e-fold
      int axisX;       // (1,0) then (0,1) for the separable pair
      int axisY;
    } pc;

    const int RADIUS = 4;
    const float GAUSS[5] = float[](0.2026, 0.1790, 0.1240, 0.0672, 0.0285); // σ = 2

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(dst);
      if (p.x >= size.x || p.y >= size.y) return;

      vec4 nc = imageLoad(normals, p);
      if (nc.w == 0.0) { // void: pass the gather's 1.0 through
        imageStore(dst, p, imageLoad(src, p));
        return;
      }
      float zc = nc.w;

      ivec2 axis = ivec2(pc.axisX, pc.axisY);
      float sum = GAUSS[0] * imageLoad(src, p).r;
      float wsum = GAUSS[0];
      for (int i = -RADIUS; i <= RADIUS; ++i) {
        if (i == 0) continue;
        ivec2 q = p + axis * i;
        if (any(lessThan(q, ivec2(0))) || any(greaterThanEqual(q, size))) continue;
        vec4 nq = imageLoad(normals, q);
        if (nq.w == 0.0) continue;
        float zq = nq.w;
        float w = GAUSS[abs(i)]
                * exp(-abs(zq - zc) * pc.sharpness / zc)
                * pow(max(dot(nq.xyz, nc.xyz), 0.0), 8.0);
        sum += w * imageLoad(src, q).r;
        wsum += w;
      }
      imageStore(dst, p, vec4(sum / wsum));
    }
  |]
   )
