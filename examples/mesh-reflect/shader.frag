#version 450

// Fragment stage of the depth+color pipeline. Its inputs (normal @loc 0, color
// @loc 1) must match the vertex stage's outputs, and it shares the Scene UBO
// (using the light fields the vertex stage ignores) — both checked at compile
// time by MatchInterface / CompatibleResources. Shades the surface with a simple
// Lambert (N·L) term plus ambient.

layout(location = 0) in vec3 inNormal;
layout(location = 1) in vec3 inColor;

layout(set = 0, binding = 0, std140) uniform Scene {
  mat4 transform;
  vec4 lightDir;
  vec4 lightColor;
} scene;

layout(location = 0) out vec4 outColor;

void main() {
  float lambert = max(dot(normalize(inNormal), normalize(scene.lightDir.xyz)), 0.0);
  float shade = 0.15 + 0.85 * lambert;
  outColor = vec4(inColor * scene.lightColor.rgb * shade, 1.0);
}
