{-# LANGUAGE QuasiQuotes #-}

-- | The tube's tessellation, shared by the geometry pass and its dispatch.
module Tunnel.Grid
  ( rings
  , segments
  , faces
  , tiles
  , vertexCount
  , workgroup
  , counts
  , vertexStruct
  ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Glsl (GlslChunk (..))

-- | Rings down the tube and segments around it; the checkerboard lights half the segments.
rings, segments :: Int
rings = 28
segments = 20

-- | The octahedron hanging in the middle of the tube.
faces :: Int
faces = 8

tiles :: Int
tiles = rings * segments `div` 2

-- | Six vertices per tile and three per face: what the geometry pass writes and the draw consumes.
vertexCount :: Int
vertexCount = tiles * 6 + faces * 3

-- | Invocations per workgroup; the pass runs one invocation per tile or face.
workgroup :: Int
workgroup = 64

counts :: GlslChunk
counts =
  GlslChunk
    [glsl|
    const int rings = $rings;
    const int segments = $segments;
    const int tiles = $tiles;
    const int faces = $faces;
    |]

vertexStruct :: GlslChunk
vertexStruct = GlslChunk [glsl|struct Vertex { vec4 position; vec4 colour; };|]
