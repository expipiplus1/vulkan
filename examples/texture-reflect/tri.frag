#version 450

// Offscreen pass, fragment stage. Writes the interpolated RGB gradient, pulsed
// by Globals.time — the first read of the shared UBO (set 0, binding 0).

layout(set = 0, binding = 0, std140) uniform Globals {
  float time;
} g;

layout(location = 0) in vec3 vColor;

layout(location = 0) out vec4 outColor;

void main() {
  float pulse = 0.7 + 0.3 * abs(sin(g.time));
  outColor = vec4(vColor * pulse, 1.0);
}
