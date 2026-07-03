{-# LANGUAGE QuasiQuotes #-}

{-| The mesh vertex/fragment shaders, compiled to SPIR-V at build time.

Reflected in "Mesh": the @Scene@ UBO and @Vertex@ SSBO-element records, the
per-stage signatures the compile-time composition check runs on, and the
merged pipeline layout.
-}
module Mesh.Shader
  ( vertCode
  , fragCode
  ) where

import Data.ByteString (ByteString)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)

{- | Vertex stage shared by both pipelines (depth-only z-prepass and
depth+color). The geometry is pulled from the @Mesh@ SSBO (set 0, binding 1)
indexed by @gl_VertexIndex@ — no vertex buffer. The Camera\/Scene UBO (set 0,
binding 0) is shared with the fragment stage: the vertex stage uses
@transform@, the fragment stage uses the light fields.
-}
vertCode :: ByteString
vertCode =
  [vert|
    #version 450

    struct Vertex {
      vec3 position;
      vec3 normal;
      vec3 color;
    };

    layout(set = 0, binding = 1, std430) readonly buffer Mesh {
      Vertex verts[];
    };

    layout(set = 0, binding = 0, std140) uniform Scene {
      mat4 transform;
      vec4 lightDir;
      vec4 lightColor;
    } scene;

    layout(location = 0) out vec3 outNormal;
    layout(location = 1) out vec3 outColor;

    void main() {
      Vertex v = verts[gl_VertexIndex];
      gl_Position = scene.transform * vec4(v.position, 1.0);
      outNormal = v.normal;
      outColor = v.color;
    }
  |]

{- | Fragment stage of the depth+color pipeline. Its inputs (normal @loc 0@,
color @loc 1@) must match the vertex stage's outputs, and it shares the Scene
UBO (using the light fields the vertex stage ignores) — both checked at
compile time by 'Vulkan.Utils.SpirV.Stage.MatchInterface' \/
'Vulkan.Utils.SpirV.Stage.CompatibleResources'. Shades the surface with a
simple Lambert (N·L) term plus ambient.
-}
fragCode :: ByteString
fragCode =
  [frag|
    #version 450

    layout(location = 0) in vec3 inNormal;
    layout(location = 1) in vec3 inColor;

    layout(set = 0, binding = 0, std140) uniform Scene {
      mat4 transform;
      vec4 lightDir;
      vec4 lightColor;
    } scene;

    layout(location = 0) out vec4 outColor;

    void main() {
      float lambert = max(dot(normalize(inNormal), normalize(scene.lightDir.xyz)), 0.0);
      float shade = 0.15 + 0.85 * lambert;
      outColor = vec4(inColor * scene.lightColor.rgb * shade, 1.0);
    }
  |]
