{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Two-phase cave-cube culling.

One invocation per generated cave cube in each phase. The early phase draws
what was visible last frame: the camera test (bounding sphere vs. the frustum,
then the visibility word, then vs. last frame's depth pyramid when @hizValid@)
appends to the camera remap and bumps @mainCube.instanceCount@. The late phase
runs after the pyramid is rebuilt from the early draws and re-tests every cube
against it — the current camera vs. current depth, so the test is exact up to
the pyramid's resolution: cubes visible now but not drawn early append past the
early entries for the @lateCube@ draw, and every cube's visibility word is
rewritten for the next frame's early phase (drawn-early cubes skip the pyramid
test and keep their word — the next early phase re-derives their occlusion
against the same pyramid anyway). A freshly disoccluded cube is caught the
same frame; a cube the early phase drew on a stale word costs one frame of
overdraw, not a pop.

The early phase also refills each orb's shadow reach set (sphere vs. sphere,
the resolve's falloff window read from the lights SSBO), appending to that
orb's own occluder range and bumping its draw's count — far-apart orbs don't
inflate each other's sets. The other objects (glowstones, knot, orbs) are never
culled — their remap entries are identity and their draw commands untouched.
-}
module Pipeline.Cull.Shader
  ( earlyCode
  , lateCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Pipeline.Common (lightReach2, lightStruct, objectStruct)
import qualified Pipeline.Common as Common
import Pipeline.Cull.Blocks (caveBounds, hizOccluded, sphereFrustum)
import Scene.Objects (lateCountWord, lateFirstInstanceWord, orbOccCountWord0, orbOccCountWordStride)

earlyCode :: ByteString
earlyCode =
  $( Common.comp
       [glsl|
    #version 450
    layout(local_size_x = 256) in;

    $objectStruct
    $lightStruct
    $lightReach2
    layout(set = 0, binding = 0, std430) readonly buffer Objects { Object objects[]; };
    // The indirect buffer as raw words, as in the generator: cmd[1] =
    // mainCube.instanceCount; the per-orb counter words are spliced from
    // "Scene.Objects", the command layout's owner.
    layout(set = 0, binding = 1, std430) buffer Indirect { uint cmd[]; };
    layout(set = 0, binding = 2, std430) writeonly buffer VisibleMain { uint visMain[]; };
    layout(set = 0, binding = 3, std430) writeonly buffer VisibleOcc { uint visOcc[]; };
    // Last frame's depth pyramid: each texel is the farthest (reverse-Z minimum)
    // depth of its footprint ("Pipeline.HiZ.Shader"). NEAREST-sampled in GENERAL.
    layout(set = 0, binding = 4) uniform sampler2D hiz;
    layout(set = 0, binding = 5, std430) readonly buffer Lights { Light lights[]; };
    // Per-cube visibility words: read as "was visible last frame", overwritten
    // with this phase's draw decision for the late phase to read back.
    layout(set = 0, binding = 6, std430) buffer VisBits { uint visBits[]; };

    layout(push_constant, std430) uniform EarlyParams {
      mat4 viewProj;
      uint caveBase;
      uint caveCount;
      uint hizValid;      // 0 = pyramid not built yet (first frame, after resize)
      uint orbBase;       // first orb in the lights SSBO
      uint orbCount;
      uint orbOccBase;    // entry index of orb 0's occluder range in visOcc
      uint orbOccCap;     // entries per orb range; appends beyond it are dropped
    } pc;

    $caveBounds
    $sphereFrustum
    $hizOccluded

    void main() {
      uint i = gl_GlobalInvocationID.x;
      if (i >= pc.caveCount) return;
      uint slot = pc.caveBase + i;

      vec3 centre; float half_; float radius;
      caveBounds(slot, centre, half_, radius);

      bool drawn = !frustumCulled(centre, radius) && visBits[i] != 0u;
      if (drawn && pc.hizValid != 0u)
        drawn = !occluded(centre, half_);
      if (drawn) {
        uint n = atomicAdd(cmd[1], 1u);
        visMain[n] = slot;
      }
      visBits[i] = drawn ? 1u : 0u;

      for (uint o = 0u; o < pc.orbCount; ++o) {
        Light orb = lights[pc.orbBase + o];
        // The falloff window: where the resolve spends the light is exactly
        // where its occluders matter — an occluder of a lit receiver lies on
        // the light→receiver segment, inside the same sphere.
        float reach = sqrt(lightReach2(orb));
        if (distance(centre, orb.posHalf.xyz) <= reach + radius) {
          uint n = atomicAdd(cmd[$orbOccCountWord0 + $orbOccCountWordStride * o], 1u);
          if (n < pc.orbOccCap)
            visOcc[pc.orbOccBase + o * pc.orbOccCap + n] = slot;
        }
      }
    }
  |]
   )

lateCode :: ByteString
lateCode =
  $( Common.comp
       [glsl|
    #version 450
    layout(local_size_x = 256) in;

    $objectStruct
    layout(set = 0, binding = 0, std430) readonly buffer Objects { Object objects[]; };
    // cmd[1] (the early phase's final count) is stable during this dispatch:
    // it is both the late draw's firstInstance and the append base in visMain.
    // The late draw's own words are spliced from "Scene.Objects".
    layout(set = 0, binding = 1, std430) buffer Indirect { uint cmd[]; };
    layout(set = 0, binding = 2, std430) writeonly buffer VisibleMain { uint visMain[]; };
    // This frame's pyramid, rebuilt from the early draws' depth.
    layout(set = 0, binding = 3) uniform sampler2D hiz;
    // Read as "drawn early", overwritten with the next early phase's predicate.
    layout(set = 0, binding = 4, std430) buffer VisBits { uint visBits[]; };

    layout(push_constant, std430) uniform LateParams {
      mat4 viewProj;
      uint caveBase;
      uint caveCount;
    } pc;

    $caveBounds
    $sphereFrustum
    $hizOccluded

    void main() {
      uint i = gl_GlobalInvocationID.x;
      if (i >= pc.caveCount) return;
      if (i == 0u) cmd[$lateFirstInstanceWord] = cmd[1];
      uint slot = pc.caveBase + i;

      vec3 centre; float half_; float radius;
      caveBounds(slot, centre, half_, radius);

      // Drawn-early cubes skip the pyramid test: their append can't fire, and
      // the next early phase re-tests the same pyramid itself.
      bool visNow = !frustumCulled(centre, radius)
        && (visBits[i] != 0u || !occluded(centre, half_));
      if (visNow && visBits[i] == 0u) {
        uint n = atomicAdd(cmd[$lateCountWord], 1u);
        visMain[cmd[1] + n] = slot;
      }
      visBits[i] = visNow ? 1u : 0u;
    }
  |]
   )
