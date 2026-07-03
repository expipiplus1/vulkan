{-# LANGUAGE QuasiQuotes #-}

{-| The fullscreen presentation pass, compiled to SPIR-V at build time: a
@gl_VertexIndex@ triangle and a fragment stage fetching the tracer's texel
buffer. Reflected into a pipeline in "Present".
-}
module Present.Shader
  ( vertCode
  , fragCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

-- | The fullscreen triangle, from @gl_VertexIndex@ alone (no vertex input).
vertCode :: ByteString
vertCode =
  [vert|
    #version 450

    void main() {
      vec2 corner = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
      gl_Position = vec4(corner * 2.0 - 1.0, 0.0, 1.0);
    }
  |]

{- | Present the path tracer's RGBA32F texel buffer (the tracer's @Image@ SSBO,
binding 0 of its own set here): each fragment fetches its pixel. The tracer
stores gamma-2.0-encoded color, ready for a UNORM swapchain as-is; an sRGB
swapchain re-encodes on attachment write, so @SRGB_SWAPCHAIN@ decodes back to
linear first instead of double-grading.
-}
fragCode :: ByteString
fragCode =
  [frag|
    #version 450

    layout(constant_id = 0) const bool SRGB_SWAPCHAIN = false;

    layout(set = 0, binding = 0, std430) readonly buffer Image {
      vec4 texels[];
    };

    layout(push_constant) uniform Present {
      uint width;
    };

    layout(location = 0) out vec4 outColor;

    void main() {
      ivec2 p = ivec2(gl_FragCoord.xy);
      vec3 c = clamp(texels[p.y * int(width) + p.x].rgb, vec3(0.0), vec3(1.0));
      outColor = vec4(SRGB_SWAPCHAIN ? c * c : c, 1.0);
    }
  |]
