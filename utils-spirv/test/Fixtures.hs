{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The test fixture shaders, compiled to SPIR-V at build time by the
vulkan-utils GLSL quasiquoters.

The reflection splices in "Spec" and "LayoutSpec" consume these bytes; the
Template Haskell stage restriction is why they live in their own module.
-}
module Fixtures
  ( juliaComp
  , pushComp
  , ssboStructComp
  , nestedComp
  , arrayFieldComp
  , array2dComp
  , bdaComp
  , wideComp
  , specComp
  , meshVert
  , meshFrag
  , triVert
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp, compileShaderQ, frag, glsl, vert)

{- | A Julia-set compute shader whose @Params@ UBO (set 0, binding 0, std140)
becomes the generated record, next to an @OutputBuffer@ runtime-array SSBO
(binding 1, not generated). Fields are ordered by non-increasing alignment
(vec2\/uvec2 = 8, float\/uint = 4).
-}
juliaComp :: ByteString
juliaComp =
  [comp|
    #version 450

    layout(local_size_x = 16, local_size_y = 16) in;

    layout(set = 0, binding = 0, std140) uniform Params {
      vec2  center;        // align 8 @0
      uvec2 resolution;    // align 8 @8
      float escapeRadius;  // align 4 @16
      uint  maxIterations; // align 4 @20
    } params;

    layout(set = 0, binding = 1, std430) buffer OutputBuffer {
      vec4 pixels[];
    };

    void main() {
      uvec2 gid = gl_GlobalInvocationID.xy;
      if (gid.x >= params.resolution.x || gid.y >= params.resolution.y) {
        return;
      }
      vec2 z = (vec2(gid) / vec2(params.resolution) * 2.0 - 1.0) * params.escapeRadius;
      uint i = 0u;
      for (; i < params.maxIterations; ++i) {
        z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + params.center;
        if (dot(z, z) > params.escapeRadius * params.escapeRadius) {
          break;
        }
      }
      pixels[gid.y * params.resolution.x + gid.x] =
        vec4(vec3(float(i) / float(params.maxIterations)), 1.0);
    }
  |]

{- | Push-constant reflection: the @Push@ block becomes a Haskell record with a
gl-block std430 Storable, and @pushConstantRanges@ derives the
VkPushConstantRange. Fields are ordered by non-increasing alignment (mat4 =
16, vec2 = 8, float\/int = 4) to satisfy the gl-block layout guardrail.
-}
pushComp :: ByteString
pushComp =
  [comp|
    #version 450

    layout(local_size_x = 64) in;

    layout(push_constant, std430) uniform Push {
      mat4  transform;
      vec2  offset;
      float scale;
      int   count;
    } push;

    layout(set = 0, binding = 0, std430) buffer Output {
      vec4 data[];
    };

    void main() {
      uint i = gl_GlobalInvocationID.x;
      if (i >= uint(push.count)) {
        return;
      }
      data[i] = push.transform * vec4(push.offset * push.scale, 0.0, 1.0);
    }
  |]

{- | SSBO arrays of structs: the @Particle@ element type is generated as a
std430 record even though the wrapping @Particles@ block is a runtime array
(not itself representable as a flat record). Fields are ordered by
non-increasing alignment (vec3 = 16, vec2 = 8, float\/uint = 4).
-}
ssboStructComp :: ByteString
ssboStructComp =
  [comp|
    #version 450

    layout(local_size_x = 64) in;

    struct Particle {
      vec3  position; // align 16 @0
      vec2  velocity; // align 8  @16
      float mass;     // align 4  @24
      uint  flags;    // align 4  @28
    };

    layout(set = 0, binding = 0, std430) buffer Particles {
      Particle items[];
    };

    void main() {
      uint i = gl_GlobalInvocationID.x;
      items[i].position += vec3(items[i].velocity, 0.0) * items[i].mass;
    }
  |]

{- | Nested structs as fields, plus cross-layout sharing. The @Material@ struct
is all-vec4 (every member 16-byte aligned), so std140 and std430 lay it out
identically: it is used both as a field of the std140 @Scene@ UBO and as the
element of the std430 @Mats@ SSBO, and a single generated record is shared
("promoted") across both.
-}
nestedComp :: ByteString
nestedComp =
  [comp|
    #version 450

    layout(local_size_x = 1) in;

    struct Material {
      vec4 albedo;
      vec4 emission;
    };

    layout(set = 0, binding = 0, std140) uniform Scene {
      Material sun;  // nested struct field @0 (size 32)
      vec4     tint; // @32
    } scene;

    layout(set = 0, binding = 1, std430) buffer Mats {
      Material mats[];
    };

    layout(set = 0, binding = 2, std430) writeonly buffer O {
      vec4 o[];
    };

    void main() {
      o[0] = scene.sun.albedo + scene.sun.emission + scene.tint + mats[0].emission;
    }
  |]

{- | Fixed-size array fields, mapped to @Array n a@. The same fields appear in a
std140 UBO and a std430 SSBO to show the stride difference: std140 rounds
every element up to 16 bytes, std430 packs tightly.
-}
arrayFieldComp :: ByteString
arrayFieldComp =
  [comp|
    #version 450

    layout(local_size_x = 1) in;

    layout(set = 0, binding = 0, std140) uniform Kernel140 {
      vec4  taps[4];    // stride 16 in both
      float weights[4]; // stride 16 in std140
    } k140;

    layout(set = 0, binding = 1, std430) buffer Kernel430 {
      vec4  taps[4];    // stride 16
      float weights[4]; // stride 4 in std430
    } k430;

    layout(set = 0, binding = 2, std430) writeonly buffer O {
      vec4 o[];
    };

    void main() {
      o[0] = k140.taps[0] * k140.weights[0] + k430.taps[0] * k430.weights[0];
    }
  |]

{- | A multi-dimensional array field: @float grid[3][4]@ maps to
@Array 3 (Array 4 Float)@. In std430 the inner stride is 4 (outer 16); in
std140 the inner stride is 16 (outer 64).
-}
array2dComp :: ByteString
array2dComp =
  [comp|
    #version 450

    layout(local_size_x = 1) in;

    layout(set = 0, binding = 0, std430) buffer Grid430 {
      vec4  head;        // @0
      float grid[3][4];  // @16, inner stride 4, outer stride 16
    } g430;

    layout(set = 0, binding = 1, std140) uniform Grid140 {
      vec4  head;        // @0
      float grid[3][4];  // @16, inner stride 16, outer stride 64
    } g140;

    layout(set = 0, binding = 2, std430) writeonly buffer O {
      vec4 o[];
    };

    void main() {
      o[0] = g430.head + g140.head + vec4(g430.grid[0][0] + g140.grid[0][0]);
    }
  |]

{- | A self-referential buffer_reference (BDA) type: a BVH-ish node whose
children are 64-bit device addresses back to @Node@. Reflection-driven codegen
maps the pointer members to @DeviceAddress Node@ (8-byte address), not an
inlined struct, and generates @Node@ once despite the cycle.

buffer_reference needs SPIR-V 1.3+, hence the vulkan1.2 target.
-}
bdaComp :: ByteString
bdaComp =
  $( compileShaderQ
       (Just "vulkan1.2")
       "comp"
       Nothing
       [glsl|
        #version 460
        #extension GL_EXT_buffer_reference : require

        layout(local_size_x = 64) in;

        layout(buffer_reference) buffer Node;            // fwd decl enables self-reference
        layout(buffer_reference, std430) buffer Node {
          vec4 boundsMin;
          vec4 boundsMax;
          Node left;                                     // BDA pointer -> cycle
          Node right;                                    // BDA pointer -> cycle
          uint primCount;
        };

        layout(push_constant, std430) uniform Bvh {
          Node root;                                     // entry address into the graph
        } bvh;

        layout(set = 0, binding = 0, std430) writeonly buffer Out {
          uint hits[];
        };

        void main() {
          Node n = bvh.root;
          uint count = 0u;
          for (int i = 0; i < 8; ++i) {
            count += n.primCount;
            n = n.left;
          }
          hits[gl_GlobalInvocationID.x] = count;
        }
      |]
   )

{- | 64-bit integer block members. A uint64_t\/int64_t occupies an 8-byte std430
slot (alignment 8), exactly like a double — the regression fixture for
scalar-width-faithful classification, so a 64-bit int is never laid out as a
4-byte int. Fields are ordered by non-increasing alignment (8, 8, 4) to keep
gl-block's Generic layout valid.

int64 needs the Int64 capability, hence the vulkan1.1 target.
-}
wideComp :: ByteString
wideComp =
  $( compileShaderQ
       (Just "vulkan1.1")
       "comp"
       Nothing
       [glsl|
        #version 460
        #extension GL_EXT_shader_explicit_arithmetic_types_int64 : require

        layout(local_size_x = 1) in;

        layout(push_constant, std430) uniform Wide {
          uint64_t hi;  // align 8 @0,  size 8
          int64_t  lo;  // align 8 @8,  size 8
          uint     tag; // align 4 @16, size 4
        } wide;

        layout(set = 0, binding = 0, std430) writeonly buffer Out {
          uint sink[];
        };

        void main() {
          sink[0] = uint(wide.hi) + uint(wide.lo) + wide.tag;
        }
      |]
   )

{- | Specialization-constant reflection. The ids are deliberately non-contiguous
(0 and 3) to show that map entries follow the shader's actual constant_ids
while values pack tightly (offsets 0, 4).
-}
specComp :: ByteString
specComp =
  [comp|
    #version 450

    layout(local_size_x = 64) in;

    layout(constant_id = 0) const uint  count = 1u;
    layout(constant_id = 3) const float scale = 1.0;

    layout(set = 0, binding = 0, std430) buffer Output {
      float xs[];
    };

    void main() {
      uint i = gl_GlobalInvocationID.x;
      if (i < count) {
        xs[i] = scale;
      }
    }
  |]

{- | Vertex stage for the type-verified pipeline-assembly tests. Shares the
@Scene@ UBO (set 0, binding 0) with 'meshFrag' (vertex uses viewProj; fragment
uses the light fields), carries a vertex-only @Model@ push constant, and feeds
the fragment stage @outNormal@ (loc 0) + @outUV@ (loc 1).
-}
meshVert :: ByteString
meshVert =
  [vert|
    #version 450

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
  |]

{- | Fragment stage paired with 'meshVert'. Consumes the vertex outputs
(inNormal loc 0, inUV loc 1), shares the @Scene@ UBO (set 0, binding 0 — using
the light fields the vertex stage ignores), and reads a fragment-only
@Materials@ SSBO (set 0, binding 1).
-}
meshFrag :: ByteString
meshFrag =
  [frag|
    #version 450

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
  |]

{- | Vertex-input reflection: three attributes (vec3 \/ vec2 \/ vec4 at
locations 0\/1\/2) that pack tightly to offsets 0\/12\/20 and a 36-byte binding
stride.
-}
triVert :: ByteString
triVert =
  [vert|
    #version 450

    layout(location = 0) in vec3 inPosition;
    layout(location = 1) in vec2 inUV;
    layout(location = 2) in vec4 inColor;

    layout(location = 0) out vec2 outUV;
    layout(location = 1) out vec4 outColor;

    void main() {
      gl_Position = vec4(inPosition, 1.0);
      outUV = inUV;
      outColor = inColor;
    }
  |]
