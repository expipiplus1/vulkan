{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans
  () where

import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
                                                ( ReleaseKey(..)
                                                , ReleaseMap(..)
                                                )
import           Data.Typeable                  ( Typeable )
import           Foreign.Ptr                    ( Ptr )
import           NoThunks.Class
import           SDL                            ( Window )
import           Vulkan.Core10
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
                                                ( SwapchainKHR )
import           VulkanMemoryAllocator

-- Handles
deriving via OnlyCheckWhnf (Ptr a)   instance Typeable a => NoThunks (Ptr a)
deriving via OnlyCheckWhnf AccelerationStructureKHR instance NoThunks AccelerationStructureKHR
deriving via OnlyCheckWhnf Allocation instance NoThunks Allocation
deriving via OnlyCheckWhnf Buffer instance NoThunks Buffer
deriving via OnlyCheckWhnf CommandPool instance NoThunks CommandPool
deriving via OnlyCheckWhnf DescriptorSet instance NoThunks DescriptorSet
deriving via OnlyCheckWhnf Pipeline instance NoThunks Pipeline
deriving via OnlyCheckWhnf PipelineLayout instance NoThunks PipelineLayout
deriving via OnlyCheckWhnf SDL.Window instance NoThunks SDL.Window
deriving via OnlyCheckWhnf Semaphore instance NoThunks Semaphore
deriving via OnlyCheckWhnf SurfaceKHR instance NoThunks SurfaceKHR
deriving via OnlyCheckWhnf SwapchainKHR instance NoThunks SwapchainKHR
deriving via OnlyCheckWhnf Image instance NoThunks Image
deriving via OnlyCheckWhnf ImageView instance NoThunks ImageView

-- Enums
deriving via OnlyCheckWhnf PresentModeKHR instance NoThunks PresentModeKHR

-- Simple Structs
deriving via OnlyCheckWhnf SurfaceFormatKHR instance NoThunks SurfaceFormatKHR
deriving via OnlyCheckWhnf Extent2D instance NoThunks Extent2D

-- Others

instance NoThunks ReleaseMap where
  noThunks c = \case
    (ReleaseMap n r i) -> noThunks c (n, r, i)
    ReleaseMapClosed   -> noThunks c ()
  showTypeOf _ = "ReleaseMap"
  wNoThunks c = \case
    (ReleaseMap n r i) -> wNoThunks c (n, r, i)
    ReleaseMapClosed   -> wNoThunks c ()

instance NoThunks ReleaseKey where
  noThunks c (ReleaseKey r i) = noThunks c (r, i)
  showTypeOf _ = "ReleaseKey"
  wNoThunks c (ReleaseKey r i) = wNoThunks c (r, i)
