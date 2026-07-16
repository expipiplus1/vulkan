{-# LANGUAGE QuasiQuotes #-}

{-| The gamma (output-encode) compute shader.

Display-linear → sRGB. A separate pass because it is target-dependent: an sRGB
swapchain encodes in
hardware, while a plain @UNORM@ target or a PNG needs this. See
'Rendering.Passes.PassOutputs' for how the graph selects it.
-}
module Pipeline.Gamma.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0, rgba16f) uniform readonly image2D lin;
    layout(set = 0, binding = 1, rgba8) uniform writeonly image2D outSrgb;

    float encode(float c) {
      return c <= 0.0031308 ? 12.92 * c : 1.055 * pow(c, 1.0 / 2.4) - 0.055;
    }

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(outSrgb);
      if (p.x >= size.x || p.y >= size.y) return;
      vec3 c = imageLoad(lin, p).rgb;
      imageStore(outSrgb, p, vec4(encode(c.r), encode(c.g), encode(c.b), 1.0));
    }
  |]
