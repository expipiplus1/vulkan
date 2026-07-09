{-# LANGUAGE QuasiQuotes #-}

{-| The luminance-reduction compute shader.

The scene's average log-luminance, for auto-exposure. A single workgroup strides over
a bloom mip ('Scene.lumMipIndex') — already downsampled and blurred, so a bright
emitter's scatter veils the frame and pulls the exposure down — sums @log(luminance)@
into shared memory, reduces, and writes the average + its geometric mean to a small
buffer the CPU reads back.
-}
module Pipeline.Luminance.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 16, local_size_y = 16) in;

    layout(set = 0, binding = 0, rgba16f) uniform readonly image2D src;
    layout(set = 0, binding = 1, std430) buffer Lum { float avgLogLum; float geoMean; } lum;

    shared float partial[256];

    void main() {
      ivec2 size = imageSize(src);
      uint total = uint(size.x * size.y);
      uint gw = gl_WorkGroupSize.x * gl_WorkGroupSize.y; // 256
      uint idx = gl_LocalInvocationIndex;

      float sum = 0.0;
      for (uint i = idx; i < total; i += gw) {
        ivec2 p = ivec2(int(i) % size.x, int(i) / size.x);
        vec3 c = imageLoad(src, p).rgb;
        float L = dot(c, vec3(0.2126, 0.7152, 0.0722));
        sum += log(max(L, 1e-4));
      }
      partial[idx] = sum;
      barrier();

      for (uint s = gw / 2u; s > 0u; s >>= 1u) {
        if (idx < s) partial[idx] += partial[idx + s];
        barrier();
      }

      if (idx == 0u) {
        float avg = partial[0] / float(total);
        lum.avgLogLum = avg;
        lum.geoMean = exp(avg);
      }
    }
  |]
