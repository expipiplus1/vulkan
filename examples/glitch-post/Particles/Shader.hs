{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The swarm's shaders: the sim that steps every particle, and the vertex stage
that draws each as a small spinning eye-space triangle (pulled by
@gl_VertexIndex@, no vertex input).
-}
module Particles.Shader
  ( simCode
  , vertCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Camera (cameraBlock)
import Glsl (comp, eyeSpace, hsv, tau, vert)
import Particles.Grid (counts, goalStruct, particleStruct, workgroup)

simCode :: ByteString
simCode =
  $( comp
       [glsl|
    #version 450

    layout(local_size_x = $workgroup, local_size_y = 1, local_size_z = 1) in;

    $particleStruct
    $goalStruct

    layout(set = 0, binding = 0, std430) readonly buffer Previous { Particle previous[]; };
    layout(set = 0, binding = 1, std430) writeonly buffer Current { Particle current[]; };
    layout(set = 0, binding = 2, std430) readonly buffer Goals { Goal goals[]; };

    layout(push_constant, std430) uniform Sim {
      float time;
      float dt;
      float banner;
      float energy;
      float spin;
    } push;

    $counts
    $eyeSpace
    $tau

    float hash(float n) {
      return fract(sin(n) * 43758.5453123);
    }

    bool spent(Particle p) {
      return p.positionAge.w <= 0.0;
    }

    Particle retire(Particle p) {
      p.positionAge.w = 0.0;
      return p;
    }

    // Drifts with time, so the swarm never repeats.
    float spawnSeed(int i) {
      return float(i) + 64.0 * floor(push.time * 3.0);
    }

    Particle respawn(int i) {
      float seed = spawnSeed(i);
      float around = TAU * hash(seed * 12.9898);
      float radius = 0.9 + 0.7 * hash(seed * 78.233);
      float depth = zNear + (zFar - zNear) * (0.2 + 0.8 * hash(seed * 3.7));
      float life = 2.0 + 3.0 * hash(seed * 9.1);
      vec2 tangent = vec2(sin(around), -cos(around));
      float towardCamera = -(1.0 + 2.0 * hash(seed * 5.3)) * (0.6 + 0.9 * push.energy);

      Particle p;
      p.positionAge = vec4(radius * cos(around), radius * sin(around), depth, life);
      p.velocityHue = vec4(tangent * 0.8 * push.spin, towardCamera, fract(0.55 + 0.3 * hash(seed * 2.1)));
      return p;
    }

    // Counter to the tube's twist, and back toward the wall it flies along.
    vec2 swirl(vec2 xy) {
      const float wallRadius = 1.6;
      float r = max(length(xy), 1e-3);
      vec2 tangent = vec2(-xy.y, xy.x) / r;
      vec2 radial = xy / r;
      return -1.4 * push.spin * tangent - 3.0 * (r - wallRadius) * radial;
    }

    vec3 towardGoal(Particle p, Goal goal) {
      float wave = 0.1 * sin(push.time * 2.0 + goal.cell.x * 1.5);
      vec3 d = vec3(goal.cell, bannerZ) - p.positionAge.xyz;
      d.y += wave;
      return d;
    }

    void main() {
      int i = int(gl_GlobalInvocationID.x);
      if (i >= particleCount) return;

      Particle p = previous[i];
      if (spent(p)) {
        p = respawn(i);
      } else {
        float phase = push.banner;
        p.velocityHue.xy += (1.0 - phase) * swirl(p.positionAge.xy) * push.dt;
        if (phase > 0.0) {
          p.velocityHue.xyz += 26.0 * phase * towardGoal(p, goals[i]) * push.dt;
          // Damped hard as the phase locks in, so the letters set instead of orbiting.
          p.velocityHue.xyz *= mix(1.0, exp(-8.0 * push.dt), phase);
          // Outlive the assembly, so the letters don't flicker as particles expire.
          p.positionAge.w = max(p.positionAge.w, 1.0 + 2.5 * hash(float(i) * 4.7));
        }
        p.positionAge.xyz += p.velocityHue.xyz * push.dt;
        p.positionAge.w -= push.dt;
        if (p.positionAge.z < zNear) {
          p = retire(p);
        }
      }
      current[i] = p;
    }
  |]
   )

vertCode :: ByteString
vertCode =
  $( vert
       [glsl|
    #version 450

    $cameraBlock

    $particleStruct
    layout(set = 0, binding = 1, std430) readonly buffer Swarm { Particle swarm[]; };

    layout(push_constant, std430) uniform Billboard {
      float mirror;
      float brightness;
      float banner;
      float beat;
    } push;

    layout(location = 0) out vec3 fragColor;

    $eyeSpace
    $tau
    $hsv

    vec3 mirrored(vec3 centre) {
      centre.xy = mix(centre.xy, -centre.xy, push.mirror);
      return centre;
    }

    vec3 billboardCorner(int index, int corner, Particle p) {
      float spin = TAU * fract(float(index) * 0.381) + 3.0 * p.positionAge.w;
      float a = spin + TAU * float(corner) / 3.0;
      float size = 0.045 * (1.0 + 0.5 * push.beat * (1.0 - push.banner));
      return size * vec3(cos(a), sin(a), 0.0);
    }

    // While the banner holds, every spark blends to one hot colour, so the
    // letters read instead of confetti.
    vec3 sparkColour(Particle p) {
      float life = clamp(p.positionAge.w, 0.0, 1.0);
      float nearFade = smoothstep(zNear, zNear + 0.8, p.positionAge.z);
      vec3 own = hsv(p.velocityHue.w, 0.65, 1.0);
      vec3 hot = vec3(1.1, 0.85, 1.0);
      return push.brightness * life * nearFade * mix(own, hot, push.banner);
    }

    void main() {
      int index = gl_VertexIndex / 3;
      Particle p = swarm[index];
      vec3 centre = mirrored(p.positionAge.xyz) + billboardCorner(index, gl_VertexIndex % 3, p);
      gl_Position = camera.clipFromEye * vec4(centre, 1.0);
      fragColor = sparkColour(p);
    }
  |]
   )
