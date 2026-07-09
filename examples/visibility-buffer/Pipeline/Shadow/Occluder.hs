{-# LANGUAGE QuasiQuotes #-}

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
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

vertCode :: ByteString
vertCode =
  [vert|
    #version 450
    #extension GL_EXT_multiview : require

    struct Vertex { vec4 position; vec4 normal; };
    struct MeshEntry { uint baseVertex; uint vertexCount; };
    struct Object { mat4 transform; vec4 emissive; uint meshId; uint materialId; uint flags; uint pad; };

    layout(set = 0, binding = 0, std430) readonly buffer Vertices { Vertex verts[]; };
    layout(set = 0, binding = 1, std430) readonly buffer Meshes { MeshEntry meshes[]; };
    layout(set = 0, binding = 2, std430) readonly buffer Objects { Object objects[]; };
    layout(set = 0, binding = 3, std430) readonly buffer ViewProj { mat4 vp[]; };
    layout(push_constant, std430) uniform PC { vec4 lightPos; uint lightBase; } pc;

    layout(location = 0) out vec3 vWorld;

    void main() {
      Object obj = objects[gl_InstanceIndex];
      MeshEntry m = meshes[obj.meshId];
      Vertex vtx = verts[m.baseVertex + uint(gl_VertexIndex)];
      vec3 world = (obj.transform * vec4(vtx.position.xyz, 1.0)).xyz;
      vWorld = world;
      gl_Position = vp[pc.lightBase + uint(gl_ViewIndex)] * vec4(world, 1.0);
    }
  |]

fragCode :: ByteString
fragCode =
  [frag|
    #version 450
    layout(location = 0) in vec3 vWorld;
    layout(push_constant, std430) uniform PC { vec4 lightPos; uint lightBase; } pc;
    layout(location = 0) out vec4 moments;

    layout(constant_id = 0) const float SHADOW_FAR = 3.0;
    layout(constant_id = 1) const float SHADOW_C = 30.0;

    void main() {
      float d = length(vWorld - pc.lightPos.xyz) / SHADOW_FAR;
      float p = exp(SHADOW_C * d);
      float n = -exp(-SHADOW_C * d);
      moments = vec4(p, p * p, n, n * n);
    }
  |]
