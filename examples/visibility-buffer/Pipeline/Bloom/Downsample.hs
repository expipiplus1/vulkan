{-# LANGUAGE QuasiQuotes #-}

{-| The bloom downsample compute shader.

The Call of Duty (Sledgehammer, Siggraph 2014) 13-tap weighted downsample. The HDR
image is downsampled directly, no brightness threshold (the blur favours
bright pixels on its own). On the first level (@karis == 1@) a Karis average tames
fireflies, and the result is floored above 0 to avoid the propagating-black-box
artifact. Bilinear taps come from a linear sampler.
-}
module Pipeline.Bloom.Downsample
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0) uniform sampler2D srcTex;
    layout(set = 0, binding = 1, rgba16f) uniform writeonly image2D dst;

    layout(push_constant) uniform PC { int karis; } pc;

    float luma(vec3 c) { return dot(c, vec3(0.2126, 0.7152, 0.0722)); }
    vec3 toSRGB(vec3 v) { return pow(max(v, 0.0), vec3(1.0 / 2.2)); }
    // 1 / (1 + luma): weights down bright subpixels so single fireflies don't dominate.
    float karisWeight(vec3 c) { return 1.0 / (1.0 + luma(toSRGB(c)) * 0.25); }

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 dstSize = imageSize(dst);
      if (p.x >= dstSize.x || p.y >= dstSize.y) return;

      vec2 uv = (vec2(p) + 0.5) / vec2(dstSize);
      vec2 t = 1.0 / vec2(textureSize(srcTex, 0));
      float x = t.x, y = t.y;

      vec3 a = texture(srcTex, uv + vec2(-2*x,  2*y)).rgb;
      vec3 b = texture(srcTex, uv + vec2(  0,   2*y)).rgb;
      vec3 c = texture(srcTex, uv + vec2( 2*x,  2*y)).rgb;
      vec3 d = texture(srcTex, uv + vec2(-2*x,  0  )).rgb;
      vec3 e = texture(srcTex, uv).rgb;
      vec3 f = texture(srcTex, uv + vec2( 2*x,  0  )).rgb;
      vec3 g = texture(srcTex, uv + vec2(-2*x, -2*y)).rgb;
      vec3 h = texture(srcTex, uv + vec2(  0,  -2*y)).rgb;
      vec3 i = texture(srcTex, uv + vec2( 2*x, -2*y)).rgb;
      vec3 j = texture(srcTex, uv + vec2(-x,  y)).rgb;
      vec3 k = texture(srcTex, uv + vec2( x,  y)).rgb;
      vec3 l = texture(srcTex, uv + vec2(-x, -y)).rgb;
      vec3 m = texture(srcTex, uv + vec2( x, -y)).rgb;

      vec3 result;
      if (pc.karis == 1) {
        vec3 g0 = (a+b+d+e) * (0.125/4.0);
        vec3 g1 = (b+c+e+f) * (0.125/4.0);
        vec3 g2 = (d+e+g+h) * (0.125/4.0);
        vec3 g3 = (e+f+h+i) * (0.125/4.0);
        vec3 g4 = (j+k+l+m) * (0.5/4.0);
        result = g0*karisWeight(g0) + g1*karisWeight(g1) + g2*karisWeight(g2)
               + g3*karisWeight(g3) + g4*karisWeight(g4);
      } else {
        result  = e * 0.125;
        result += (a+c+g+i) * 0.03125;
        result += (b+d+f+h) * 0.0625;
        result += (j+k+l+m) * 0.125;
      }
      // Avoid exact zeros propagating into black boxes through the chain.
      imageStore(dst, p, vec4(max(result, vec3(0.0001)), 1.0));
    }
  |]
