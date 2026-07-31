{-# LANGUAGE QuasiQuotes #-}

{-| The bloom upsample compute shader.

The 3×3 tent filter from the Call of Duty method. Progressive upsample:
@mip[i] += tent(mip[i+1])@. The add is in place (@imageLoad@ +
@imageStore@ on the destination mip) — the frame-graph read+write of that subresource
is what forces the intra-image barrier.
-}
module Pipeline.Bloom.Upsample
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0) uniform sampler2D srcTex;         // mip[i+1], blur source
    layout(set = 0, binding = 1, rgba16f) uniform image2D dst;     // mip[i], read + add + write

    layout(push_constant) uniform PC { float radius; } pc;

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 dstSize = imageSize(dst);
      if (p.x >= dstSize.x || p.y >= dstSize.y) return;

      vec2 uv = (vec2(p) + 0.5) / vec2(dstSize);
      float x = pc.radius, y = pc.radius;

      vec3 a = texture(srcTex, uv + vec2(-x,  y)).rgb;
      vec3 b = texture(srcTex, uv + vec2( 0,  y)).rgb;
      vec3 c = texture(srcTex, uv + vec2( x,  y)).rgb;
      vec3 d = texture(srcTex, uv + vec2(-x,  0)).rgb;
      vec3 e = texture(srcTex, uv).rgb;
      vec3 f = texture(srcTex, uv + vec2( x,  0)).rgb;
      vec3 g = texture(srcTex, uv + vec2(-x, -y)).rgb;
      vec3 h = texture(srcTex, uv + vec2( 0, -y)).rgb;
      vec3 i = texture(srcTex, uv + vec2( x, -y)).rgb;

      vec3 blur = (e*4.0 + (b+d+f+h)*2.0 + (a+c+g+i)) * (1.0 / 16.0);
      vec3 base = imageLoad(dst, p).rgb;
      imageStore(dst, p, vec4(base + blur, 1.0));
    }
  |]
