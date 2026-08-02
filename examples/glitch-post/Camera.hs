{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

{-| The camera both scene pipelines read: one uniform buffer per frame in
flight, holding the eye-to-clip matrix.

The frame writes the slot its 'PingPong.Slot' picks: frame @N+2@ cannot start
recording until frame @N@ retires, so overwriting @N@'s matrix is safe while
@N+1@ still draws from its own.
-}
module Camera
  ( Camera (..)
  , cameraBlock
  , allocateSlots
  , write
  , projection
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Geomancy.Mat4 (Mat4)
import qualified Geomancy.Mat4 as Mat4
import Graphics.Gl.Block (Std140 (..))
import qualified Mapped
import PingPong (PingPong)
import qualified PingPong
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (glsl)
import qualified VulkanMemoryAllocator as VMA

import Glsl (GlslChunk (..), zFar, zNear)

data Camera = Camera
  { buffer :: Vk.Buffer
  , mapped :: Ptr (Std140 Mat4)
  }

-- | Set 0, binding 0 of both scene pipelines.
cameraBlock :: GlslChunk
cameraBlock =
  GlslChunk
    [glsl|
    layout(set = 0, binding = 0) uniform Camera {
      mat4 clipFromEye;
    } camera;
    |]

allocateSlots :: VMA.Allocator -> ResourceT IO (PingPong Camera)
allocateSlots vma = PingPong.twice do
  (buffer, mapped) <- Mapped.allocateBuffer vma Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT 1
  pure Camera{buffer, mapped}

write :: (MonadIO m) => Camera -> Mat4 -> m ()
write camera = liftIO . poke camera.mapped . Std140

{- | Eye to clip for the given viewport aspect ratio (width \/ height).

Divides x and y by the (aspect-corrected) eye z and maps eye z hyperbolically,
so 'Glsl.zNear' lands on depth 0 and 'Glsl.zFar' on 1 for the @LESS@ test. Clip
@w@ is the eye z, which is what makes the vertex colours interpolate
perspective-correctly.

Rows are what each eye component contributes: geomancy multiplies row vectors
(@eye *! m@), which lands in memory as the columns GLSL reads.
-}
{- ORMOLU_DISABLE -}
projection :: Float -> Mat4
projection aspect =
  Mat4.rowMajor
    (1 / aspect) 0 0                        0
    0            1 0                        0
    0            0 (zFar / depth)           1
    0            0 (-(zFar * zNear) / depth) 0
  where
    depth = zFar - zNear
{- ORMOLU_ENABLE -}
