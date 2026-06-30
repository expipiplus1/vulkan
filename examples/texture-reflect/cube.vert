#version 450

// Cube pass, vertex stage. Geometry comes from a vertex buffer with per-vertex
// position + uv attributes — the binding/attribute descriptions are built from
// reflection (Vulkan.Utils.SpirV.VertexInput), not hand-written. The cube spins
// by Globals.time (the same shared UBO at set 0, binding 0 the offscreen pass
// used). A perspective projection is built inline.

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
