{-# LANGUAGE QuasiQuotes #-}

{-| The GLSL tests both cull phases splice.

Each host must declare a @pc@ push block whose first member is @mat4 viewProj@;
'hizOccluded' additionally needs a @sampler2D hiz@.
-}
module Pipeline.Cull.Blocks
  ( caveBounds
  , sphereFrustum
  , hizOccluded
  ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Pipeline.Common (GlslChunk (..))

-- | Bounding sphere + half-extent of a cave cube; the host declares @objects@.
caveBounds :: GlslChunk
caveBounds =
  GlslChunk
    [glsl|
    // Exact for the gen's unrotated, uniformly scaled cubes.
    void caveBounds(uint slot, out vec3 centre, out float half_, out float radius) {
      mat4 m = objects[slot].transform;
      centre = m[3].xyz;
      half_ = m[0][0];
      radius = half_ * 1.7320508; // uniform cube scale × √3
    }
    |]

-- | Bounding sphere vs. the @pc.viewProj@ frustum.
sphereFrustum :: GlslChunk
sphereFrustum =
  GlslChunk
    [glsl|
    // Is the sphere entirely behind the (unnormalized) clip plane?
    bool outside(vec4 plane, vec3 c, float r) {
      return dot(plane.xyz, c) + plane.w < -r * length(plane.xyz);
    }

    // Gribb-Hartmann planes from the view-projection rows, for the Vulkan clip
    // volume (|x|,|y| <= w, 0 <= z <= w). Reverse-Z: near is z = w, and the
    // infinite far plane z >= 0 is row 2 alone.
    bool frustumCulled(vec3 centre, float radius) {
      vec4 r0 = vec4(pc.viewProj[0][0], pc.viewProj[1][0], pc.viewProj[2][0], pc.viewProj[3][0]);
      vec4 r1 = vec4(pc.viewProj[0][1], pc.viewProj[1][1], pc.viewProj[2][1], pc.viewProj[3][1]);
      vec4 r2 = vec4(pc.viewProj[0][2], pc.viewProj[1][2], pc.viewProj[2][2], pc.viewProj[3][2]);
      vec4 r3 = vec4(pc.viewProj[0][3], pc.viewProj[1][3], pc.viewProj[2][3], pc.viewProj[3][3]);
      return outside(r3 + r0, centre, radius) || outside(r3 - r0, centre, radius)
          || outside(r3 + r1, centre, radius) || outside(r3 - r1, centre, radius)
          || outside(r3 - r2, centre, radius) || outside(r2, centre, radius);
    }
    |]

-- | World-AABB vs. the @hiz@ pyramid under @pc.viewProj@.
hizOccluded :: GlslChunk
hizOccluded =
  GlslChunk
    [glsl|
    // Is the pyramid's scene everywhere closer than the box's nearest point over
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
    |]
