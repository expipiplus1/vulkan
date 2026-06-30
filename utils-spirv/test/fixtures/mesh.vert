#version 450

// Vertex stage for the type-verified pipeline-assembly tests. Shares the `Scene`
// UBO (set 0, binding 0) with mesh.frag (vertex uses viewProj; fragment uses the
// light fields), carries a vertex-only `Model` push constant, and feeds the
// fragment stage `outNormal` (loc 0) + `outUV` (loc 1).

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;

layout(set = 0, binding = 0, std140) uniform Scene {
  mat4 viewProj;
  vec4 lightDir;
  vec4 lightColor;
} scene;

layout(push_constant, std430) uniform Model {
  mat4 model;
} model;

layout(location = 0) out vec3 outNormal;
layout(location = 1) out vec2 outUV;

void main() {
  gl_Position = scene.viewProj * model.model * vec4(inPosition, 1.0);
  outNormal = mat3(model.model) * inNormal;
  outUV = inUV;
}
