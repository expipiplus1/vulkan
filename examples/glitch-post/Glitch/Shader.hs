{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The demoscene bit: the scene image in, a corrupted copy out.

Datamoshed blocks, a sweeping scanline tear, beat-pumped chromatic aberration, a
poor-man's bloom and CRT dressing, all driven by the tune.
-}
module Glitch.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Glitch.Grid (tile)
import Glsl (comp)

code :: ByteString
code =
  $( comp
       [glsl|
    #version 450

    layout(local_size_x = $tile, local_size_y = $tile, local_size_z = 1) in;

    layout(set = 0, binding = 0, rgba8) uniform readonly image2D scene;
    layout(set = 0, binding = 1, rgba8) uniform writeonly image2D glitched;

    layout(push_constant, std430) uniform Corruption {
      float time;
      float banner;
      float beat;
      float energy;
      float build;
      float impact;
    } push;

    float hash(vec2 p) {
      return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123);
    }

    // Edge-clamped, so displaced lookups never leave the image.
    vec3 fetch(ivec2 p, ivec2 size) {
      return imageLoad(scene, clamp(p, ivec2(0), size - 1)).rgb;
    }

    // While the banner holds, the displacements settle so the text stays legible.
    float calm() {
      return 1.0 - 0.75 * push.banner;
    }

    float corruption() {
      return clamp(push.energy * (0.15 + 0.85 * push.beat) + push.build, 0.0, 1.3) * calm();
    }

    // A sparse set of cells shoves its pixels sideways, and a little down.
    ivec2 datamosh(ivec2 p, float amount) {
      const float cellSize = 24.0;
      const float rehashesPerSecond = 7.0;
      vec2 cell = floor(vec2(p) / cellSize);
      float rehash = floor(push.time * rehashesPerSecond);
      float seed = hash(cell + rehash);
      if (seed <= 1.0 - 0.25 * amount) {
        return ivec2(0);
      }
      return ivec2(
        int((seed - 0.5) * 120.0),
        int((hash(cell.yx + rehash) - 0.5) * cellSize));
    }

    // Every line wobbles a touch; the band around the sweep tears hard.
    int scanlineTear(int y, int height, float amount) {
      float line = float(y);
      float sweep = fract(push.time * 0.23) * float(height);
      float band = exp(-abs(line - sweep) / 18.0);
      return int(sin(line * 0.11 + push.time * 9.0) * (1.0 + 60.0 * band * amount));
    }

    vec3 chromaticTaps(ivec2 q, ivec2 size, vec2 centred, float amount) {
      int shift = int(length(centred) * (6.0 * calm() + 26.0 * amount));
      return vec3(
        fetch(q + ivec2(shift, 0), size).r,
        fetch(q, size).g,
        fetch(q - ivec2(shift, 0), size).b);
    }

    // Anything brighter than the threshold bleeds outward.
    vec3 bloom(ivec2 q, ivec2 size) {
      const float goldenAngle = 2.39996323;
      const float threshold = 0.6;
      vec3 glow = vec3(0.0);
      for (int g = 0; g < 8; ++g) {
        float around = float(g) * goldenAngle;
        ivec2 off = ivec2(round(vec2(cos(around), sin(around)) * (2.0 + float(g))));
        glow += max(fetch(q + off, size) - threshold, vec3(0.0));
      }
      return 0.22 * glow;
    }

    vec3 crtDressing(vec3 col, ivec2 p, vec2 centred) {
      col *= 0.88 + 0.12 * sin(float(p.y) * 3.14159);
      col *= 1.0 - 0.55 * dot(centred, centred);
      return col + (hash(vec2(p) + fract(push.time) * 100.0) - 0.5) * 0.06;
    }

    vec3 dropFlash(vec3 col) {
      return mix(col, vec3(1.1), 0.45 * push.impact);
    }

    void main() {
      ivec2 size = imageSize(scene);
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      if (p.x >= size.x || p.y >= size.y) {
        return;
      }

      float amount = corruption();
      vec2 centred = vec2(p) / vec2(size) - 0.5;

      ivec2 q = p + datamosh(p, amount);
      q.x += scanlineTear(p.y, size.y, amount);

      vec3 col = chromaticTaps(q, size, centred, amount) + bloom(q, size);
      imageStore(glitched, p, vec4(crtDressing(dropFlash(col), p, centred), 1.0));
    }
  |]
   )
