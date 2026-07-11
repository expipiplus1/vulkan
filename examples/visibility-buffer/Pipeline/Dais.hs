{-# LANGUAGE QuasiQuotes #-}

{-| The DAIS reconstruction GLSL, shared by the resolve and the SSAO prepass.

Spliced into shaders as @$daisTypes@ / @$dais@ — 'Vulkan.Utils.ShaderQQ.Interpolate'
inserts values with 'show', which 'GlslChunk' turns into raw GLSL. Compiler
errors inside a chunk report the host shader's (splice-shifted) line numbers.
-}
module Pipeline.Dais
  ( GlslChunk (..)
  , daisTypes
  , dais
  ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

-- | A GLSL fragment for @$@-splicing into 'glsl' quasiquotes; 'show' is raw text.
newtype GlslChunk = GlslChunk String

instance Show GlslChunk where
  show (GlslChunk s) = s

{- | The table struct shapes 'dais' dereferences.

Spliced (as @$daisTypes@) before the splicing shader's own @verts@ / @objects@
/ @meshes@ buffer block declarations, so the layouts can't drift between
consumers.
-}
daisTypes :: GlslChunk
daisTypes =
  GlslChunk
    [glsl|
    struct Vertex { vec4 position; vec4 normal; };
    struct Object { mat4 transform; vec4 emissive; uint meshId; uint materialId; uint flags; uint pad; };
    struct MeshEntry { uint baseVertex; uint vertexCount; };
    |]

{- | @pixelNdc@ + @meshGeometry@: reconstruct a pixel's world position and
smooth normal for any mesh, from its object transform + the hit triangle's
shared vertices.

The splicing shader must declare the @verts@ / @objects@ / @meshes@ buffers
(over the '@daisTypes@' structs) and a @cam@ push carrying @viewProj@.
-}
dais :: GlslChunk
dais =
  GlslChunk
    [glsl|
    // NDC of pixel p (centre-sampled), matching the raster's clip space.
    vec2 pixelNdc(ivec2 p, ivec2 size) { return ((vec2(p) + 0.5) / vec2(size)) * 2.0 - 1.0; }

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
    |]
