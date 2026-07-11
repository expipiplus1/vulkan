{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| The unified shadow occluder shaders.

One multiview pass draws every occluder mesh (cave cubes + knot) into a light's six
cube faces at once (@gl_ViewIndex@ picks the face), storing the four
exponential-variance moments of the light-space distance. Geometry is vertex-pulled
from the shared tables like the camera pass ("Pipeline.Mesh.Shader").

@SHADOW_FAR@ and @SHADOW_C@ are 'Pipeline.Shadow.Params.Params', specialized at
pipeline creation from the same value the resolve gets.
-}
module Pipeline.Shadow.Occluder
  ( vertCode
  , fragCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Pipeline.Common (evsm, pullVertex, tables)
import qualified Pipeline.Common as Common

vertCode :: ByteString
vertCode =
  $( Common.vert
       [glsl|
    #version 450
    #extension GL_EXT_multiview : require

    $tables
    layout(set = 0, binding = 0, std430) readonly buffer Vertices { Vertex verts[]; };
    layout(set = 0, binding = 1, std430) readonly buffer Meshes { MeshEntry meshes[]; };
    layout(set = 0, binding = 2, std430) readonly buffer Objects { Object objects[]; };
    layout(set = 0, binding = 3, std430) readonly buffer ViewProj { mat4 vp[]; };
    layout(set = 0, binding = 4, std430) readonly buffer Visible { uint visible[]; };
    layout(push_constant, std430) uniform PC { vec4 lightPos; uint lightBase; } pc;

    layout(location = 0) out vec3 vWorld;

    $pullVertex

    void main() {
      uint objId;
      vWorld = pullVertex(objId);
      gl_Position = vp[pc.lightBase + uint(gl_ViewIndex)] * vec4(vWorld, 1.0);
    }
  |]
   )

fragCode :: ByteString
fragCode =
  $( Common.frag
       [glsl|
    #version 450
    layout(location = 0) in vec3 vWorld;
    layout(push_constant, std430) uniform PC { vec4 lightPos; uint lightBase; } pc;
    layout(location = 0) out vec4 moments;

    $evsm

    void main() {
      vec2 w = evsmWarp(length(vWorld - pc.lightPos.xyz) / SHADOW_FAR);
      moments = vec4(w.x, w.x * w.x, w.y, w.y * w.y);
    }
  |]
   )
