{-# LANGUAGE QuasiQuotes #-}

{-| Shared GLSL building blocks.

Spliced into shaders as @$name@ — 'Vulkan.Utils.ShaderQQ.Interpolate' inserts
values with 'show', which 'GlslChunk' turns into raw GLSL. Splicing needs the
interpolating 'comp' \/ 'vert' \/ 'frag' compilers, not the bare stage
quasiquoters. Compiler errors inside a chunk report the host shader's
(splice-shifted) line numbers.

Each chunk documents the declarations its host must provide. The structs here
are the single source of truth for the table layouts: the Haskell-side records
are reflected back out of a shader that splices them ("Pipeline.Mesh").
-}
module Pipeline.Common
  ( GlslChunk (..)
  , comp
  , vert
  , frag
  , vertexStruct
  , objectStruct
  , tables
  , pullVertex
  , dais
  , evsm
  ) where

import Language.Haskell.TH (Exp, Q)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)

-- | A GLSL fragment for @$@-splicing into 'glsl' quasiquotes; 'show' is raw text.
newtype GlslChunk = GlslChunk String

instance Show GlslChunk where
  show (GlslChunk s) = s

-- | The stage compilers, pre-applied: @$(Common.comp [glsl|…|])@ splices chunks.
comp, vert, frag :: String -> Q Exp
comp = compileShaderQ Nothing "comp" Nothing
vert = compileShaderQ Nothing "vert" Nothing
frag = compileShaderQ Nothing "frag" Nothing

-- | The shared vertex SSBO's element ("Scene.Meshes").
vertexStruct :: GlslChunk
vertexStruct = GlslChunk [glsl|struct Vertex { vec4 position; vec4 normal; };|]

-- | The mesh table's element ("Scene.Meshes"): a slice of the vertex SSBO.
meshEntryStruct :: GlslChunk
meshEntryStruct = GlslChunk [glsl|struct MeshEntry { uint baseVertex; uint vertexCount; };|]

-- | The object table's element ("Scene.Objects").
objectStruct :: GlslChunk
objectStruct = GlslChunk [glsl|struct Object { mat4 transform; vec4 emissive; uint meshId; uint materialId; uint flags; uint pad; };|]

-- | All three table structs, for the shaders that walk object → mesh → vertices.
tables :: GlslChunk
tables =
  GlslChunk
    [glsl|
    $vertexStruct
    $meshEntryStruct
    $objectStruct
    |]

{- | Fetch the world-space vertex a unified-mesh draw pulls.

@gl_InstanceIndex@ resolves through the @visible@ remap to the object, whose
mesh slices the shared vertex SSBO at @gl_VertexIndex@. The splicing shader
must declare the @verts@ / @meshes@ / @objects@ / @visible@ buffers over the
'tables' structs.
-}
pullVertex :: GlslChunk
pullVertex =
  GlslChunk
    [glsl|
    // World position of the pulled vertex; @objId@ is the remapped object.
    vec3 pullVertex(out uint objId) {
      objId = visible[gl_InstanceIndex];
      Object obj = objects[objId];
      MeshEntry m = meshes[obj.meshId];
      Vertex vtx = verts[m.baseVertex + uint(gl_VertexIndex)];
      return (obj.transform * vec4(vtx.position.xyz, 1.0)).xyz;
    }
    |]

{- | @pixelNdc@ + @meshGeometry@: reconstruct a pixel's world position and
smooth normal for any mesh, from its object transform + the hit triangle's
shared vertices (DAIS).

The splicing shader must declare the @verts@ / @objects@ / @meshes@ buffers
(over the 'tables' structs) and a @cam@ push carrying @viewProj@.
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

{- | The EVSM encoding: the 'Pipeline.Shadow.Params.Params' specialization
constants and the two-tailed exponential warp.

Shared verbatim by the moment writer ("Pipeline.Shadow.Occluder") and the
resolve that Chebyshev-tests against them ("Pipeline.Shade.Shader"), so the
encoding cannot drift between bake and lookup.
-}
evsm :: GlslChunk
evsm =
  GlslChunk
    [glsl|
    layout(constant_id = 0) const float SHADOW_FAR = 3.0;
    layout(constant_id = 1) const float SHADOW_C = 30.0;
    // Two-tailed exponential warp of the normalized light distance.
    vec2 evsmWarp(float d) { return vec2(exp(SHADOW_C * d), -exp(-SHADOW_C * d)); }
    |]
