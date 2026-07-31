{-# LANGUAGE QuasiQuotes #-}

{-| The tonemap compute shader.

Exposure + Uchimura \"Gran Turismo\" curve applied per channel. Scales scene-linear
HDR by the @exposure@ read from the metering buffer and maps to display-linear
@[0,1]@. Per-channel (not luminance-only) so bright saturated emitters desaturate
toward white as they clip — the filmic look. Gamma encoding is a separate pass
("Pipeline.Gamma").
-}
module Pipeline.Tonemap.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 8, local_size_y = 8) in;

    layout(set = 0, binding = 0, rgba16f) uniform readonly image2D hdr;
    layout(set = 0, binding = 1, rgba16f) uniform writeonly image2D outTone;
    layout(set = 0, binding = 2) uniform sampler2D bloomTex;
    // Written by the host meter pass (or the caller, pre-frame).
    layout(set = 0, binding = 3, std430) readonly buffer Metering { float exposure; } metering;

    layout(push_constant, std430) uniform PC { float bloomStrength; } pc;

    // Uchimura (Gran Turismo) tone curve — piecewise toe/linear/shoulder.
    float uchimura(float x, float P, float a, float m, float l, float c, float b) {
      float l0 = ((P - m) * l) / a;
      float S0 = m + l0;
      float S1 = m + a * l0;
      float C2 = (a * P) / (P - S1);
      float CP = -C2 / P;
      float w0 = 1.0 - smoothstep(0.0, m, x);
      float w2 = step(m + l0, x);
      float w1 = 1.0 - w0 - w2;
      float T = m * pow(x / m, c) + b;
      float S = P - (P - S1) * exp(CP * (x - S0));
      float L = m + a * (x - m);
      return T * w0 + L * w1 + S * w2;
    }

    float uchimura(float x) {
      // P max brightness, a contrast, m linear start, l linear length,
      // c black tightness, b pedestal.
      return uchimura(x, 1.0, 1.0, 0.22, 0.4, 1.33, 0.0);
    }

    void main() {
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      ivec2 size = imageSize(outTone);
      if (p.x >= size.x || p.y >= size.y) return;
      vec2 uv = (vec2(p) + 0.5) / vec2(size);
      vec3 hdrC = imageLoad(hdr, p).rgb;
      // Composite: mix the whole image toward the bloom pyramid, biased to source.
      vec3 bloomC = texture(bloomTex, uv).rgb;
      vec3 c = mix(hdrC, bloomC, pc.bloomStrength) * metering.exposure;
      vec3 t = vec3(uchimura(c.r), uchimura(c.g), uchimura(c.b));
      imageStore(outTone, p, vec4(t, 1.0));
    }
  |]
