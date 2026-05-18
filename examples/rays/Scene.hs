{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ParallelListComp #-}

module Scene where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Linear.V3
import Linear.V4
import System.Random
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_acceleration_structure as RT
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

scene :: [Sphere]
scene =
  let n = 2000
  in [ Sphere
         ( V4
             (x * radius)
             (radius ** 2.4 * sin x)
             (radius ** 2.4 * cos x)
             (radius ** 1.3)
         )
         (V4 r g b 1)
     | radius <- (** 1.3) <$> [1, 1.1 ..]
     | x <- take n [0 ..]
     | V3 r g b <- pastels
     ]

pastels :: [V3 Float]
pastels =
  let
    (g1, (g2, g3)) = split <$> split (mkStdGen 2)
    hues = randomRs (0, 360) g1
    sats = randomRs (0.3, 0.5) g2
    vals = randomRs (0.8, 1) g3
    cs = zipWith3 hsv hues sats vals
  in
    uncurryRGB V3 <$> cs

----------------------------------------------------------------
-- Vulkan
----------------------------------------------------------------

data SceneBuffers = SceneBuffers
  { sceneAabbs :: Vk.Buffer
  , sceneSpheres :: Vk.Buffer
  , sceneSize :: Word32
  }

makeSceneBuffers
  :: (MonadResource m)
  => VMA.Allocator
  -> m SceneBuffers
makeSceneBuffers vma = do
  sceneAabbs <-
    initBuffer
      vma
      ( Vk.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
          .|. Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
      )
      (sphereAABB <$> scene)

  sceneSpheres <- initBuffer vma Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT scene

  let sceneSize = fromIntegral (length scene)

  pure SceneBuffers{..}

----------------------------------------------------------------
-- Buffer tools
----------------------------------------------------------------

initBuffer
  :: forall a m
   . (Storable a, MonadResource m)
  => VMA.Allocator
  -> Vk.BufferUsageFlags
  -> [a]
  -> m Vk.Buffer
initBuffer vma usage xs = do
  let bufferSize = sizeOf (head xs) * length xs

  (_, (buf, allocation, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.flags = zero
        , Vk.size = fromIntegral bufferSize
        , Vk.usage
        }
      zero
        { VMA.requiredFlags =
            Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
              .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
      allocate
  (unmapKey, p) <- VMA.withMappedMemory vma allocation allocate
  liftIO $ pokeArray (castPtr @() @a p) xs
  release unmapKey

  pure buf

----------------------------------------------------------------
-- Sphere
----------------------------------------------------------------

data Sphere = Sphere
  { spherePos :: V4 Float
  , sphereColor :: V4 Float
  }
  deriving (Generic)

instance Storable Sphere where
  sizeOf ~_ = 2 * sizeOf (undefined :: V4 Float)
  alignment ~_ = alignment (undefined :: V4 Float)
  peek ptr = Sphere <$> peekByteOff ptr 0 <*> peekByteOff ptr 16
  poke ptr (Sphere pos col) = do
    pokeByteOff ptr 0 pos
    pokeByteOff ptr 16 col

sphereRadius :: Sphere -> Float
sphereRadius = view _w . spherePos

sphereOrigin :: Sphere -> V3 Float
sphereOrigin = view _xyz . spherePos

sphereAABB :: Sphere -> RT.AabbPositionsKHR
sphereAABB s =
  RT.AabbPositionsKHR
    (mini ^. _x)
    (mini ^. _y)
    (mini ^. _z)
    (maxi ^. _x)
    (maxi ^. _y)
    (maxi ^. _z)
  where
    mini = sphereOrigin s - pure (sphereRadius s)
    maxi = sphereOrigin s + pure (sphereRadius s)
