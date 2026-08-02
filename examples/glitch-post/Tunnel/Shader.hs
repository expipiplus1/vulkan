{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The wormhole's shaders: a compute pass that grows the whole tube into an
SSBO, and the vertex stage that pulls it back out by @gl_VertexIndex@.
-}
module Tunnel.Shader
  ( geomCode
  , vertCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Camera (cameraBlock)
import Glsl (comp, eyeSpace, hsv, tau, vert)
import Tunnel.Grid (counts, vertexStruct, workgroup)

geomCode :: ByteString
geomCode =
  $( comp
       [glsl|
    #version 450

    layout(local_size_x = $workgroup, local_size_y = 1, local_size_z = 1) in;

    $vertexStruct
    layout(set = 0, binding = 0, std430) writeonly buffer Vertices { Vertex verts[]; };

    layout(push_constant, std430) uniform Growth {
      float time;
      float twist;
      float banner;
      float beat;
      float energy;
    } push;

    $counts
    $eyeSpace
    $tau
    $hsv

    void put(int i, vec3 position, vec3 colour) {
      verts[i] = Vertex(vec4(position, 1.0), vec4(colour, 1.0));
    }

    // 0 at the camera, 1 at the far end; rings cycle toward the viewer.
    float ringCycle(int ring) {
      return fract(float(ring) / float(rings) - 0.06 * push.time);
    }

    int checkerSegment(int ring, int slot) {
      return 2 * slot + (ring % 2);
    }

    float twistAt(float cyc) {
      return 1.8 * cyc + push.twist;
    }

    float bannerDim() {
      return 1.0 - 0.55 * push.banner;
    }

    vec3 tileColour(int ring, float cyc) {
      float hue = float(ring) / float(rings) + 0.06 * push.time;
      float depthFade = pow(max(0.0, 1.0 - cyc), 1.6);
      float wrapFade = smoothstep(0.0, 0.06, cyc);
      float lit = (0.35 + 0.65 * push.energy) * bannerDim();
      return lit * hsv(hue, 0.85, depthFade * wrapFade);
    }

    void growTile(int id) {
      int ring = id / (segments / 2);
      int segment = checkerSegment(ring, id % (segments / 2));

      float cyc = ringCycle(ring);
      float nearZ = zNear + (zFar - zNear) * cyc;
      float farZ = min(zFar, nearZ + 0.5);

      float radius = 1.7 + 0.2 * sin(3.0 * push.time + TAU * cyc);
      const float gap = 0.08;
      float a0 = TAU * float(segment) / float(segments) + twistAt(cyc);
      float a1 = TAU * (float(segment) + 1.0 - gap) / float(segments) + twistAt(cyc);
      vec2 r0 = radius * vec2(cos(a0), sin(a0));
      vec2 r1 = radius * vec2(cos(a1), sin(a1));

      vec3 colour = tileColour(ring, cyc);
      int base = id * 6;
      put(base + 0, vec3(r0, nearZ), colour);
      put(base + 1, vec3(r1, nearZ), colour);
      put(base + 2, vec3(r1, farZ), colour);
      put(base + 3, vec3(r0, nearZ), colour);
      put(base + 4, vec3(r1, farZ), colour);
      put(base + 5, vec3(r0, farZ), colour);
    }

    float octahedronRadius() {
      return 0.42 + 0.05 * sin(2.3 * push.time) + 0.22 * push.beat;
    }

    vec3 tumble(vec3 p, float ax, float ay) {
      p.yz = vec2(cos(ax) * p.y - sin(ax) * p.z, sin(ax) * p.y + cos(ax) * p.z);
      p.xz = vec2(cos(ay) * p.x + sin(ay) * p.z, cos(ay) * p.z - sin(ay) * p.x);
      return p;
    }

    vec3 faceColour(int face) {
      float sheen = 0.45 + 0.55 * fract(float(face) * 0.381 + 0.15 * push.time);
      float lit = (0.75 + 0.35 * push.energy) * bannerDim();
      return lit * hsv(0.93, 0.35, sheen);
    }

    void growFace(int face) {
      float r = octahedronRadius();
      vec3 corners[6] = vec3[6](
        vec3(r, 0, 0), vec3(-r, 0, 0),
        vec3(0, r, 0), vec3(0, -r, 0),
        vec3(0, 0, r), vec3(0, 0, -r));
      ivec3 triangles[8] = ivec3[8](
        ivec3(0, 2, 4), ivec3(2, 1, 4), ivec3(1, 3, 4), ivec3(3, 0, 4),
        ivec3(2, 0, 5), ivec3(1, 2, 5), ivec3(3, 1, 5), ivec3(0, 3, 5));

      vec3 colour = faceColour(face);
      for (int v = 0; v < 3; ++v) {
        vec3 p = tumble(corners[triangles[face][v]], 0.7 * push.time, 0.9 * push.time);
        p.z += octahedronZ;
        put(tiles * 6 + face * 3 + v, p, colour);
      }
    }

    void main() {
      int id = int(gl_GlobalInvocationID.x);
      if (id < tiles) {
        growTile(id);
      } else if (id < tiles + faces) {
        growFace(id - tiles);
      }
    }
  |]
   )

vertCode :: ByteString
vertCode =
  $( vert
       [glsl|
    #version 450

    $cameraBlock

    $vertexStruct
    layout(set = 0, binding = 1, std430) readonly buffer Vertices { Vertex verts[]; };

    layout(location = 0) out vec3 fragColor;

    void main() {
      Vertex v = verts[gl_VertexIndex];
      gl_Position = camera.clipFromEye * vec4(v.position.xyz, 1.0);
      fragColor = v.colour.rgb;
    }
  |]
   )
