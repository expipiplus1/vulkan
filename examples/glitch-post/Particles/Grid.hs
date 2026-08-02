{-# LANGUAGE QuasiQuotes #-}

-- | The swarm's size and state layout, shared by the sim and its dispatch.
module Particles.Grid
  ( count
  , workgroup
  , counts
  , particleStruct
  , goalStruct
  ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)

import Glsl (GlslChunk (..))

-- | Sparks in the swarm.
count :: Int
count = 2048

-- | Invocations per workgroup; the sim runs one invocation per particle.
workgroup :: Int
workgroup = 64

counts :: GlslChunk
counts = GlslChunk [glsl|const int particleCount = $count;|]

-- | What the sim carries from frame to frame, and the draw pulls.
particleStruct :: GlslChunk
particleStruct = GlslChunk [glsl|struct Particle { vec4 positionAge; vec4 velocityHue; };|]

-- | Where the banner wants a particle ("Particles.Banner").
goalStruct :: GlslChunk
goalStruct = GlslChunk [glsl|struct Goal { vec2 cell; };|]
