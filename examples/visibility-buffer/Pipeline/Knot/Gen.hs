{-# LANGUAGE QuasiQuotes #-}

{-| Torus-knot tube mesh generation.

One invocation per @(i,j)@ quad of a @segments × ring@ grid: sweep a circle of radius
@tubeR@ around a @(2,3)@ torus-knot curve of outer radius @scale@, framed by
Gram-Schmidt against the radial direction (smooth, no frame-flip seam), and emit the
quad's two triangles (6 vertices, position + outward normal) into the shared vertex
SSBO. The mesh's outer radius is @scale + tubeR@.
-}
module Pipeline.Knot.Gen
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 64) in;

    struct Vertex { vec4 position; vec4 normal; };
    layout(set = 0, binding = 0, std430) writeonly buffer Verts { Vertex verts[]; };

    layout(push_constant, std430) uniform Params {
      uint segments;
      uint ring;
      float tubeR;
      float scale;
      uint base; // first vertex in the shared buffer (Meshes.knotBase)
    } pc;

    const float TAU = 6.28318530718;
    const float KP = 2.0; // knot winds around the axis
    const float KQ = 3.0; // knot winds around the tube

    // Radii sum to 1, so the curve's outer radius is exactly pc.scale.
    const float KR = 5.0 / 7.0;
    const float Kr = 2.0 / 7.0;

    vec3 curve(float t) {
      float a = KR + Kr * cos(KQ * t);
      return vec3(a * cos(KP * t), a * sin(KP * t), Kr * sin(KQ * t)) * pc.scale;
    }

    // Smooth Frenet-ish frame: tangent by finite difference, normal by
    // Gram-Schmidt against the (smooth) radial direction.
    void frame(float t, out vec3 T, out vec3 N, out vec3 B) {
      float e = 0.001;
      T = normalize(curve(t + e) - curve(t - e));
      vec3 radial = vec3(cos(KP * t), sin(KP * t), 0.0);
      N = normalize(radial - dot(radial, T) * T);
      B = cross(T, N);
    }

    vec3 tubeVertex(uint i, uint j, out vec3 nrm) {
      float t = (float(i) / float(pc.segments)) * TAU;
      float th = (float(j) / float(pc.ring)) * TAU;
      vec3 T, N, B;
      frame(t, T, N, B);
      vec3 dir = cos(th) * N + sin(th) * B;
      nrm = dir;
      return curve(t) + pc.tubeR * dir;
    }

    void main() {
      uint idx = gl_GlobalInvocationID.x;
      uint quads = pc.segments * pc.ring;
      if (idx >= quads) return;

      uint i = idx / pc.ring;
      uint j = idx % pc.ring;
      uint i1 = (i + 1u) % pc.segments;
      uint j1 = (j + 1u) % pc.ring;

      vec3 n00, n10, n01, n11;
      vec3 p00 = tubeVertex(i,  j,  n00);
      vec3 p10 = tubeVertex(i1, j,  n10);
      vec3 p01 = tubeVertex(i,  j1, n01);
      vec3 p11 = tubeVertex(i1, j1, n11);

      uint o = pc.base + idx * 6u;
      verts[o + 0u] = Vertex(vec4(p00, 1.0), vec4(n00, 0.0));
      verts[o + 1u] = Vertex(vec4(p10, 1.0), vec4(n10, 0.0));
      verts[o + 2u] = Vertex(vec4(p11, 1.0), vec4(n11, 0.0));
      verts[o + 3u] = Vertex(vec4(p00, 1.0), vec4(n00, 0.0));
      verts[o + 4u] = Vertex(vec4(p11, 1.0), vec4(n11, 0.0));
      verts[o + 5u] = Vertex(vec4(p01, 1.0), vec4(n01, 0.0));
    }
  |]
