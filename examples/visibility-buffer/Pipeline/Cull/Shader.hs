{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Per-frame cave-cube culling.

One invocation per generated cave cube. Two independent tests refill the compacted
draws the caller just reset: the camera test (bounding sphere vs. the frustum, then
vs. last frame's depth pyramid when @hizValid@) appends to the camera remap and
bumps @mainCube.instanceCount@; the orb shadow reach (sphere vs. sphere) appends to
the occluder remap and bumps @occCube.instanceCount@. The other objects
(glowstones, knot, orbs) are never culled — their remap entries are identity and
their draw commands untouched.

The pyramid lags a frame, so the occlusion test is conservative only for the
camera that rendered it: a freshly disoccluded cube pops in one frame late, and
camera motion can transiently cull a cube whose new screen rect still holds old,
closer depths. Near-plane crossers and everything the pyramid can't prove hidden
are kept.
-}
module Pipeline.Cull.Shader
  ( code
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Pipeline.Common (objectStruct)
import qualified Pipeline.Common as Common

code :: ByteString
code =
  $( Common.comp
       [glsl|
    #version 450
    layout(local_size_x = 256) in;

    $objectStruct
    layout(set = 0, binding = 0, std430) readonly buffer Objects { Object objects[]; };
    // The indirect buffer as raw words, as in the generator: cmd[1] =
    // mainCube.instanceCount, cmd[13] = occCube.instanceCount.
    layout(set = 0, binding = 1, std430) buffer Indirect { uint cmd[]; };
    layout(set = 0, binding = 2, std430) writeonly buffer VisibleMain { uint visMain[]; };
    layout(set = 0, binding = 3, std430) writeonly buffer VisibleOcc { uint visOcc[]; };
    // Last frame's depth pyramid: each texel is the farthest (reverse-Z minimum)
    // depth of its footprint ("Pipeline.HiZ.Shader"). NEAREST-sampled in GENERAL.
    layout(set = 0, binding = 4) uniform sampler2D hiz;

    layout(push_constant, std430) uniform Params {
      mat4 viewProj;
      vec4 orbSphere;   // xyz = centre, w = shadow reach (< 0 when there are no orbs)
      uint caveBase;
      uint caveCount;
      uint hizValid;    // 0 = pyramid not built yet (first frame, after resize)
    } pc;

    // Is the sphere entirely behind the (unnormalized) clip plane?
    bool outside(vec4 plane, vec3 c, float r) {
      return dot(plane.xyz, c) + plane.w < -r * length(plane.xyz);
    }

    // Is last frame's scene everywhere closer than the box's nearest point over
    // its whole screen rect? (Reverse-Z: closer = larger.) @half_@ is the
    // world-axis-aligned half-extent — exact for the gen's unrotated cubes.
    bool occluded(vec3 centre, float half_) {
      // Corner clips factor affinely: clip of centre ± half_ per axis is
      // clipC ± half_ · matrix column.
      vec4 clipC = pc.viewProj * vec4(centre, 1.0);
      vec4 dx = half_ * pc.viewProj[0];
      vec4 dy = half_ * pc.viewProj[1];
      vec4 dz = half_ * pc.viewProj[2];
      vec2 lo = vec2(1e30);
      vec3 hi = vec3(-1e30);
      for (int i = 0; i < 8; ++i) {
        vec3 s = vec3(uvec3(i, i >> 1, i >> 2) & 1u) * 2.0 - 1.0;
        vec4 clip = clipC + s.x * dx + s.y * dy + s.z * dz;
        if (clip.w <= 1e-4) return false; // crosses the near plane: keep
        vec3 ndc = clip.xyz / clip.w;
        lo = min(lo, ndc.xy);
        hi = max(hi, ndc);
      }
      vec2 uvLo = clamp(lo * 0.5 + 0.5, 0.0, 1.0);
      vec2 uvHi = clamp(hi.xy * 0.5 + 0.5, 0.0, 1.0);
      // The mip where the rect spans <= 2 texels, so its 4 corner samples cover it.
      vec2 px = (uvHi - uvLo) * vec2(textureSize(hiz, 0));
      float mip = clamp(ceil(log2(max(max(px.x, px.y), 1.0))), 0.0, float(textureQueryLevels(hiz) - 1));
      float sceneFar = min(
        min(textureLod(hiz, uvLo, mip).r, textureLod(hiz, uvHi, mip).r),
        min(textureLod(hiz, vec2(uvLo.x, uvHi.y), mip).r, textureLod(hiz, vec2(uvHi.x, uvLo.y), mip).r));
      return hi.z < sceneFar;
    }

    void main() {
      uint i = gl_GlobalInvocationID.x;
      if (i >= pc.caveCount) return;
      uint slot = pc.caveBase + i;

      mat4 m = objects[slot].transform;
      vec3 centre = m[3].xyz;
      float radius = m[0][0] * 1.7320508; // uniform cube scale × √3

      // Gribb-Hartmann planes from the view-projection rows, for the Vulkan clip
      // volume (|x|,|y| <= w, 0 <= z <= w). Reverse-Z: near is z = w, and the
      // infinite far plane z >= 0 is row 2 alone.
      vec4 r0 = vec4(pc.viewProj[0][0], pc.viewProj[1][0], pc.viewProj[2][0], pc.viewProj[3][0]);
      vec4 r1 = vec4(pc.viewProj[0][1], pc.viewProj[1][1], pc.viewProj[2][1], pc.viewProj[3][1]);
      vec4 r2 = vec4(pc.viewProj[0][2], pc.viewProj[1][2], pc.viewProj[2][2], pc.viewProj[3][2]);
      vec4 r3 = vec4(pc.viewProj[0][3], pc.viewProj[1][3], pc.viewProj[2][3], pc.viewProj[3][3]);
      bool culled =
           outside(r3 + r0, centre, radius) || outside(r3 - r0, centre, radius)
        || outside(r3 + r1, centre, radius) || outside(r3 - r1, centre, radius)
        || outside(r3 - r2, centre, radius) || outside(r2, centre, radius);
      if (!culled && pc.hizValid != 0u)
        culled = occluded(centre, m[0][0]);
      if (!culled) {
        uint n = atomicAdd(cmd[1], 1u);
        visMain[n] = slot;
      }

      if (pc.orbSphere.w >= 0.0 && distance(centre, pc.orbSphere.xyz) <= pc.orbSphere.w + radius) {
        uint n = atomicAdd(cmd[13], 1u);
        visOcc[pc.caveBase + n] = slot;
      }
    }
  |]
   )
