{-# LANGUAGE QuasiQuotes #-}

{-| The unified shadow occluder shaders.

One multiview pass draws every occluder mesh (cave cubes + knot) into a light's six
cube faces at once (@gl_ViewIndex@ picks the face), storing the four
exponential-variance moments of the light-space distance. Geometry is vertex-pulled
from the shared tables like the camera pass ("Pipeline.Mesh.Shader").
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
    layout(push_constant, std430) uniform PC { vec4 lightPosFar; uint lightBase; } pc;

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
    layout(push_constant, std430) uniform PC { vec4 lightPosFar; uint lightBase; } pc;
    layout(location = 0) out vec4 moments;

    // Exponential warp constant — fits the squared moments inside fp32.
    const float C = 30.0;

    void main() {
      float d = length(vWorld - pc.lightPosFar.xyz) / pc.lightPosFar.w;
      float p = exp(C * d);
      float n = -exp(-C * d);
      moments = vec4(p, p * p, n, n * n);
    }
  |]
