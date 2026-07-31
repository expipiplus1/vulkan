{-# LANGUAGE QuasiQuotes #-}

{-| Depth-pyramid min-reduce.

Under reverse-Z the minimum is the /farthest/ depth, so a pyramid texel
conservatively bounds everything rasterized beneath it — exactly what the occlusion
test in "Pipeline.Cull.Shader" compares against.

'code' is one per-level step (each target texel min's its source footprint; the
first step reads the depth attachment itself). 'tailCode' finishes the chain in a
single dispatch once a level fits in shared memory: a serial per-level tail is all
pipeline-drain and no work, so one workgroup loads the level and emits every
remaining mip from it directly.
-}
module Pipeline.HiZ.Shader
  ( code
  , tailCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0) uniform sampler2D src;
    layout(set = 0, binding = 1, r32f) writeonly uniform image2D dst;

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 dstSize = imageSize(dst);
      if (any(greaterThanEqual(p, dstSize))) return;
      ivec2 srcSize = textureSize(src, 0);

      // 2x2 min, widened by a column/row where halving dropped one — skipping those
      // texels could only raise the result, i.e. wrongly cull behind them.
      ivec2 base = p * 2;
      ivec2 n = ivec2(srcSize.x > dstSize.x * 2 ? 3 : 2, srcSize.y > dstSize.y * 2 ? 3 : 2);
      float m = 1.0;
      for (int y = 0; y < n.y; ++y)
        for (int x = 0; x < n.x; ++x) {
          ivec2 q = min(base + ivec2(x, y), srcSize - 1);
          m = min(m, texelFetch(src, q, 0).r);
        }
      imageStore(dst, p, vec4(m));
    }
  |]

{- | The fused tail: one workgroup reduces a ≤32×32 level to every remaining mip.

Each output texel min's its exact texture-space footprint in the shared-memory
copy of the source level — no iterated widening, and no inter-level dependency,
so the whole tail is one dispatch with a single shared-memory barrier.
-}
tailCode :: ByteString
tailCode =
  [comp|
    #version 450
    layout(local_size_x = 64) in;

    layout(set = 0, binding = 0) uniform sampler2D src;
    // dst arity = HiZ.tailMax; tile edge = HiZ.tailFits' bound. Change together.
    layout(set = 0, binding = 1, r32f) writeonly uniform image2D dst[5];
    layout(push_constant, std430) uniform Tail {
      uint levels;  // populated entries of dst[]
    } pc;

    shared float tile[32 * 32];

    // Min over dst texel p's texture-space footprint [p, p+1)/dstSize in the
    // source level: src texels floor(p*ss/ds) .. ceil((p+1)*ss/ds)-1.
    float footprintMin(ivec2 p, ivec2 dstSize, ivec2 srcSize) {
      ivec2 lo = p * srcSize / dstSize;
      ivec2 hi = ((p + 1) * srcSize + dstSize - 1) / dstSize - 1;
      float m = 1.0;
      for (int y = lo.y; y <= hi.y; ++y)
        for (int x = lo.x; x <= hi.x; ++x)
          m = min(m, tile[y * srcSize.x + x]);
      return m;
    }

    // A macro, not a function: glslang can't type an image parameter with the
    // r32f format qualifier, and dynamic dst[] indexing would need a feature.
    #define REDUCE_LEVEL(K)                                                    \
      {                                                                        \
        ivec2 dstSize = imageSize(dst[K]);                                     \
        uint count = uint(dstSize.x * dstSize.y);                              \
        for (uint i = gl_LocalInvocationIndex; i < count; i += 64u) {          \
          ivec2 p = ivec2(int(i) % dstSize.x, int(i) / dstSize.x);             \
          imageStore(dst[K], p, vec4(footprintMin(p, dstSize, srcSize)));      \
        }                                                                      \
      }

    void main() {
      ivec2 srcSize = textureSize(src, 0);
      uint n = uint(srcSize.x * srcSize.y);
      for (uint i = gl_LocalInvocationIndex; i < n; i += 64u)
        tile[i] = texelFetch(src, ivec2(int(i) % srcSize.x, int(i) / srcSize.x), 0).r;
      memoryBarrierShared();
      barrier();

      REDUCE_LEVEL(0)
      if (pc.levels > 1u) REDUCE_LEVEL(1)
      if (pc.levels > 2u) REDUCE_LEVEL(2)
      if (pc.levels > 3u) REDUCE_LEVEL(3)
      if (pc.levels > 4u) REDUCE_LEVEL(4)
    }
  |]
