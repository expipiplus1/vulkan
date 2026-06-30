#version 450

// Vertex stage shared by both pipelines (depth-only z-prepass and depth+color).
// The geometry is pulled from the `Mesh` SSBO (set 0, binding 1) indexed by
// gl_VertexIndex — no vertex buffer. The Camera/Scene UBO (set 0, binding 0) is
// shared with the fragment stage: the vertex stage uses `transform`, the fragment
// stage uses the light fields.

struct Vertex {
  vec3 position;
  vec3 normal;
  vec3 color;
};

layout(set = 0, binding = 1, std430) readonly buffer Mesh {
  Vertex verts[];
};

layout(set = 0, binding = 0, std140) uniform Scene {
  mat4 transform;
  vec4 lightDir;
  vec4 lightColor;
} scene;

layout(location = 0) out vec3 outNormal;
layout(location = 1) out vec3 outColor;

void main() {
  Vertex v = verts[gl_VertexIndex];
  gl_Position = scene.transform * vec4(v.position, 1.0);
  outNormal = v.normal;
  outColor = v.color;
}
