{-# LANGUAGE QuasiQuotes #-}

{-| The cube-pass shaders: a spinning cube sampling the offscreen image,
compiled to SPIR-V at build time and reflected in "Cube".
-}
module Cube.Shader
  ( vertCode
  , fragCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

{- | Geometry comes from a vertex buffer with per-vertex position + uv
attributes — the binding\/attribute descriptions are built from reflection
("Vulkan.Utils.SpirV.VertexInput"), not hand-written. The cube spins by
@Globals.time@ (the same shared UBO at set 0, binding 0 the offscreen pass
used). A perspective projection is built inline.
-}
vertCode :: ByteString
vertCode =
  [vert|
    #version 450

    layout(set = 0, binding = 0, std140) uniform Globals {
      float time;
    } g;

    layout(location = 0) in vec3 position;
    layout(location = 1) in vec2 uv;

    layout(location = 0) out vec2 vUv;

    mat4 rotY(float a) {
      float c = cos(a), s = sin(a);
      return mat4(c, 0.0, -s, 0.0,  0.0, 1.0, 0.0, 0.0,  s, 0.0, c, 0.0,  0.0, 0.0, 0.0, 1.0);
    }

    mat4 rotX(float a) {
      float c = cos(a), s = sin(a);
      return mat4(1.0, 0.0, 0.0, 0.0,  0.0, c, s, 0.0,  0.0, -s, c, 0.0,  0.0, 0.0, 0.0, 1.0);
    }

    // Vulkan-clip perspective (z in [0,1], y flipped), column-major.
    mat4 perspective(float fovy, float aspect, float near, float far) {
      float f = 1.0 / tan(fovy * 0.5);
      return mat4(
        f / aspect, 0.0, 0.0, 0.0,
        0.0, -f, 0.0, 0.0,
        0.0, 0.0, far / (near - far), -1.0,
        0.0, 0.0, (near * far) / (near - far), 0.0);
    }

    void main() {
      // Scale the unit cube down so the framed image has padding around it.
      vec4 world = rotY(g.time) * rotX(0.5) * vec4(position * 0.62, 1.0);
      world.z -= 3.0;
      gl_Position = perspective(radians(50.0), 1.0, 0.1, 10.0) * world;
      vUv = uv;
    }
  |]

{- | Samples the offscreen RGB triangle — produced by the "Tri" pipeline and
barriered into a shader-read layout — through a combined image sampler at
set 1, binding 0. The shared Globals UBO (set 0, binding 0) is read again here
for a time-based tint; it is the SAME descriptor the offscreen pass bound,
never rebound between the two draws.
-}
fragCode :: ByteString
fragCode =
  [frag|
    #version 450

    layout(set = 0, binding = 0, std140) uniform Globals {
      float time;
    } g;

    layout(set = 1, binding = 0) uniform sampler2D offscreen;

    layout(location = 0) in vec2 vUv;

    layout(location = 0) out vec4 outColor;

    void main() {
      vec3 c = texture(offscreen, vUv).rgb;
      c *= 0.85 + 0.15 * sin(g.time);
      outColor = vec4(c, 1.0);
    }
  |]
