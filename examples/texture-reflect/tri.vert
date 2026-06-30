#version 450

// Offscreen pass, vertex stage. A screen-filling triangle with saturated
// red / green / blue corners, so the offscreen colour image holds the full RGB
// range (each primary saturated near a vertex). That image is then sampled as a
// texture by the cube pass. The geometry is generated from gl_VertexIndex (no
// vertex buffer). The shared Globals UBO (set 0, binding 0) is read by the
// fragment stage; this pipeline and the cube pipeline keep set 0 compatible so
// the descriptor need not be rebound between them.

layout(location = 0) out vec3 vColor;

void main() {
  // Vulkan clip space is +Y down: y = +1 is the bottom, y = -1 the top.
  vec2 base[3] = vec2[](vec2(-1.0, 1.0), vec2(1.0, 1.0), vec2(0.0, -1.0));
  vec3 cols[3] = vec3[](vec3(1.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0), vec3(0.0, 0.0, 1.0));
  gl_Position = vec4(base[gl_VertexIndex], 0.0, 1.0);
  vColor = cols[gl_VertexIndex];
}
