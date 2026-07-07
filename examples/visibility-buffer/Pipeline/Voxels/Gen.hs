{-# LANGUAGE QuasiQuotes #-}

{-| Surface-shell cave generation.

One invocation per cell, occupancy (rock vs. air) evaluated directly from a
billow-fbm field carved by a central chamber.

A cell survives if it is solid and any of its 6 face-neighbours is air (out-of-bounds
counts as air), so only the visible shell is drawn. Each survivor atomically appends
a cube 'Object' (transform from cell centre/half-size, grey material by cell hash)
after the object table's cave base, and bumps both cube draw commands' @instanceCount@
(the camera pass's, and the shadow pass's cave-only one).
-}
module Pipeline.Voxels.Gen
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)

code :: ByteString
code =
  [comp|
    #version 450
    layout(local_size_x = 4, local_size_y = 4, local_size_z = 4) in;

    struct Object { mat4 transform; vec4 emissive; uint meshId; uint materialId; uint flags; uint pad; };

    layout(set = 0, binding = 0, std430) writeonly buffer Objects { Object objects[]; };
    // The indirect buffer as raw words: cmd[1] = mainCube.instanceCount (starts at
    // the glowstone count), cmd[13] = occCube.instanceCount (starts at 0).
    layout(set = 0, binding = 1, std430) buffer Indirect { uint cmd[]; };

    layout(push_constant, std430) uniform Params {
      uint gridN;
      float worldScale;
      float chamberRadius;
      float outerRadius;
      uint greyCount;
    } pc;

    uint hashu(uint n) {
      n = (n << 13) ^ n;
      return n * (n * n * 15731u + 789221u) + 1376312589u;
    }

    // Occupancy field: billow-fbm value noise above a threshold, carved by a
    // central chamber and bounded by an outer radius.
    float hash(vec3 p) {
      p = fract(p * 0.3183099 + 0.1);
      p *= 17.0;
      return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
    }
    float vnoise(vec3 x) {
      vec3 i = floor(x), f = fract(x);
      f = f * f * (3.0 - 2.0 * f);
      return mix(mix(mix(hash(i + vec3(0,0,0)), hash(i + vec3(1,0,0)), f.x),
                     mix(hash(i + vec3(0,1,0)), hash(i + vec3(1,1,0)), f.x), f.y),
                 mix(mix(hash(i + vec3(0,0,1)), hash(i + vec3(1,0,1)), f.x),
                     mix(hash(i + vec3(0,1,1)), hash(i + vec3(1,1,1)), f.x), f.y), f.z);
    }
    float billow(vec3 x) {
      float a = 0.5, s = 0.0;
      for (int i = 0; i < 4; ++i) { s += a * abs(2.0 * vnoise(x) - 1.0); x *= 2.0; a *= 0.5; }
      return s;
    }

    bool solidAt(ivec3 c) {
      if (any(lessThan(c, ivec3(0))) || any(greaterThanEqual(c, ivec3(pc.gridN)))) return false;
      vec3 pos = (vec3(c) + 0.5) / float(pc.gridN); // [0,1]
      float dist = length(pos - 0.5);
      bool rock = billow(pos * float(pc.gridN) * 0.07) > 0.34;
      bool chamber = dist < pc.chamberRadius; // clear central void for the knots
      bool bounded = dist < pc.outerRadius;   // beyond this is the outer void
      return rock && bounded && !chamber;
    }

    void main() {
      uvec3 uc = gl_GlobalInvocationID.xyz;
      if (uc.x >= pc.gridN || uc.y >= pc.gridN || uc.z >= pc.gridN) return;
      ivec3 c = ivec3(uc);
      if (!solidAt(c)) return;

      bool surface =
           !solidAt(c + ivec3(1,0,0)) || !solidAt(c - ivec3(1,0,0))
        || !solidAt(c + ivec3(0,1,0)) || !solidAt(c - ivec3(0,1,0))
        || !solidAt(c + ivec3(0,0,1)) || !solidAt(c - ivec3(0,0,1));
      if (!surface) return;

      uint slot = atomicAdd(cmd[1], 1u);  // camera cube count (starts at glowstone count) → object slot
      atomicAdd(cmd[13], 1u);             // shadow (cave-only) cube count
      vec3 centre = ((vec3(uc) + 0.5) / float(pc.gridN) - 0.5) * 2.0 * pc.worldScale;
      float hs = pc.worldScale / float(pc.gridN);
      uint cellId = uint(c.x) + pc.gridN * (uint(c.y) + pc.gridN * uint(c.z));

      Object o;
      o.transform = mat4(
        hs, 0.0, 0.0, 0.0,
        0.0, hs, 0.0, 0.0,
        0.0, 0.0, hs, 0.0,
        centre.x, centre.y, centre.z, 1.0);
      o.emissive = vec4(0.0);
      o.meshId = 0u; // cube
      o.materialId = hashu(cellId) % pc.greyCount;
      o.flags = 0u;  // reserved
      o.pad = 0u;
      objects[slot] = o;
    }
  |]
