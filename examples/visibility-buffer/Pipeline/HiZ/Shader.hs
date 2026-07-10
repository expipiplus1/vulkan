{-# LANGUAGE QuasiQuotes #-}

{-| Depth-pyramid min-reduce.

One step of the Hi-Z build: each target texel takes the minimum of its source
footprint. Under reverse-Z the minimum is the /farthest/ depth, so a pyramid texel
conservatively bounds everything rasterized beneath it — exactly what the occlusion
test in "Pipeline.Cull.Shader" compares against. The first step reads the depth
attachment itself; later steps read the previous mip.
-}
module Pipeline.HiZ.Shader
  ( code
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
