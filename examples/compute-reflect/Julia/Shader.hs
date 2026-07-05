{-# LANGUAGE QuasiQuotes #-}

{-| The Julia-set compute shader, compiled to SPIR-V at build time.

Its interface is reflected by vulkan-utils-spirv in the "Julia" module:
the @Params@ push-constant block, the output SSBO's descriptor set layout,
and the specialization constants.
-}
module Julia.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

    // Specialized at pipeline-creation time (see Vulkan.Utils.SpirV.Specialization).
    layout(constant_id = 0) const uint  maxIterations = 1000u;
    layout(constant_id = 1) const float escapeRadius = 2.0;

    // Fields are ordered by non-increasing alignment (vec2/uvec2 = 8) so gl-block's
    // Generic std430 layout matches the shader; see the guardrail in
    // Vulkan.Utils.SpirV.Block.
    layout(push_constant, std430) uniform Params {
      vec2  center;      // c in the Julia iteration
      uvec2 resolution;  // output size in pixels
    } params;

    layout(set = 0, binding = 0, std430) buffer OutputBuffer {
      vec4 pixels[];
    };

    // From https://iquilezles.org/articles/palettes/
    vec3 palette(float t) {
      const vec3 a = vec3(0.5);
      const vec3 b = vec3(0.5);
      const vec3 c = vec3(1.0);
      const vec3 d = vec3(0.0, 0.10, 0.20);
      return a + b * cos(6.28318530718 * (c * t + d));
    }

    void main() {
      uvec2 gid = gl_GlobalInvocationID.xy;
      if (gid.x >= params.resolution.x || gid.y >= params.resolution.y) {
        return;
      }

      vec2 uv =
        ( vec2(gid) / vec2(params.resolution) * 2.0 - 1.0 ) * escapeRadius;

      vec2 z = uv;
      int i = 0;
      for (; i < int(maxIterations); ++i) {
        vec2 z2 = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y);
        z = z2 + params.center;
        if (dot(z, z) > escapeRadius * escapeRadius) {
          break;
        }
      }

      uint idx = gid.y * params.resolution.x + gid.x;
      if (i == int(maxIterations)) {
        pixels[idx] = vec4(0.0, 0.0, 0.0, 1.0);
      } else {
        pixels[idx] = vec4(palette(float(i) / float(maxIterations)), 1.0);
      }
    }
  |]
