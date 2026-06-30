#version 450

// Fragment stage paired with mesh.vert. Consumes the vertex outputs (inNormal
// loc 0, inUV loc 1), shares the `Scene` UBO (set 0, binding 0 — using the light
// fields the vertex stage ignores), and reads a fragment-only `Materials` SSBO
// (set 0, binding 1).

layout(location = 0) in vec3 inNormal;
layout(location = 1) in vec2 inUV;

layout(set = 0, binding = 0, std140) uniform Scene {
  mat4 viewProj;
  vec4 lightDir;
  vec4 lightColor;
} scene;

struct Material {
  vec4 albedo;
  vec4 params;
};

layout(set = 0, binding = 1, std430) buffer Materials {
  Material materials[];
};

layout(location = 0) out vec4 outColor;

void main() {
  float ndl = max(dot(normalize(inNormal), normalize(scene.lightDir.xyz)), 0.0);
  vec4 albedo = materials[0].albedo * vec4(inUV, 1.0, 1.0);
  outColor = albedo * scene.lightColor * ndl;
}
