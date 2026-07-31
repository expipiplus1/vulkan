{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The unified mesh visibility-pass shaders.

Draws every base mesh (cube, knot, …) by pulling
@vertex[mesh.baseVertex + gl_VertexIndex]@ from the shared vertex SSBO and placing it
with the per-object @transform@ from the object table. The object id comes through
the instance remap (@visible[gl_InstanceIndex]@), compacted per frame by
"Pipeline.Cull", so draw order never touches the ids. Each fragment writes
@(objectId + 1, triangleId)@ into the @R32G32_UINT@ visibility buffer
(@0@ = background).
-}
module Pipeline.Mesh.Shader
  ( vertCode
  , fragCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, glsl)

import Pipeline.Common (pullVertex, tables)
import qualified Pipeline.Common as Common

vertCode :: ByteString
vertCode =
  $( Common.vert
       [glsl|
    #version 450

    $tables
    layout(set = 0, binding = 0, std430) readonly buffer Vertices { Vertex verts[]; };
    layout(set = 0, binding = 1, std430) readonly buffer Meshes { MeshEntry meshes[]; };
    layout(set = 0, binding = 2, std430) readonly buffer Objects { Object objects[]; };
    layout(set = 0, binding = 3, std430) readonly buffer Visible { uint visible[]; };
    layout(push_constant, std430) uniform Camera { mat4 viewProj; } cam;

    layout(location = 0) flat out uint vObject;
    layout(location = 1) flat out uint vTriangle;

    $pullVertex

    void main() {
      uint objId;
      vec3 world = pullVertex(objId);
      gl_Position = cam.viewProj * vec4(world, 1.0);
      vObject = objId + 1u; // 0 = background
      vTriangle = uint(gl_VertexIndex) / 3u;
    }
  |]
   )

fragCode :: ByteString
fragCode =
  [frag|
    #version 450
    layout(location = 0) flat in uint vObject;
    layout(location = 1) flat in uint vTriangle;
    layout(location = 0) out uvec2 outIds;
    void main() {
      outIds = uvec2(vObject, vTriangle);
    }
  |]
