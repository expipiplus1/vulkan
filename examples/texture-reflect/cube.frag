#version 450

// Cube pass, fragment stage. Samples the offscreen RGB triangle — produced by
// the first pipeline and barriered into a shader-read layout — through a combined
// image sampler at set 1, binding 0. The shared Globals UBO (set 0, binding 0)
// is read again here for a time-based tint; it is the SAME descriptor the
// offscreen pass bound, never rebound between the two draws.

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
