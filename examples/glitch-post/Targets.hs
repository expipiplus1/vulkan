{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The offscreen images a frame renders through, one set per frame in flight.

Nothing here is presented — only the blit's destination is a swapchain image —
so these are sized by how many frames can be recording and executing at once,
not by how many images the surface handed out.
-}
module Targets
  ( Target (..)
  , Targets (..)
  , sceneFormat
  , depthFormat
  , allocateTargets
  ) where

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, register, release)
import Data.Bits ((.|.))
import Data.Foldable (for_, toList)
import RenderTarget (allocateTarget, createDepthTarget)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline (Pipeline)
import Vulkan.Utils.Swapchain (Swapchain (..))
import qualified VulkanMemoryAllocator as VMA

import qualified Glitch
import PingPong (PingPong)
import qualified PingPong

data Target = Target
  { image :: Vk.Image
  , view :: Vk.ImageView
  }

data Targets = Targets
  { scene :: Target
  , depth :: Target
  , post :: Target
  , glitchSet :: Vk.DescriptorSet
  -- ^ Binds this set's scene and post views.
  }

{- | The offscreen format, deliberately independent of the (often BGRA)
swapchain format: @R8G8B8A8_UNORM@ has mandatory storage-image support, and the
final blit converts to whatever the surface uses.
-}
sceneFormat :: Vk.Format
sceneFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

-- | Sized to the swapchain's extent; the 'ReleaseKey' frees the lot on a swap.
allocateTargets :: VMA.Allocator -> Vk.Device -> Pipeline -> Swapchain -> ResourceT IO (PingPong Targets, ReleaseKey)
allocateTargets vma dev glitch sc = do
  scene <-
    PingPong.twice $
      allocateTarget
        vma
        dev
        sceneFormat
        sc.sExtent
        (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_STORAGE_BIT)
        Vk.IMAGE_ASPECT_COLOR_BIT
        Nothing
        "glitch scene target"
  depth <- PingPong.twice $ createDepthTarget vma dev depthFormat sc.sExtent
  post <-
    PingPong.twice $
      allocateTarget
        vma
        dev
        sceneFormat
        sc.sExtent
        (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT)
        Vk.IMAGE_ASPECT_COLOR_BIT
        Nothing
        "glitch post target"
  (poolKey, sets) <- Glitch.allocateSets dev glitch (viewPair <$> scene <*> post)
  groupKey <- register do
    release poolKey
    for_ (toList scene <> toList depth <> toList post) \((imageKey, viewKey), _) ->
      release viewKey >> release imageKey
  pure (gather <$> scene <*> depth <*> post <*> sets, groupKey)
  where
    target (_, (image, view)) = Target{image, view}
    viewPair s p = ((target s).view, (target p).view)
    gather scene depth post glitchSet =
      Targets{scene = target scene, depth = target depth, post = target post, glitchSet}
