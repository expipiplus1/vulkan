{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( MirSurfaceCreateFlagsKHR
  , withCStructMirSurfaceCreateInfoKHR
  , fromCStructMirSurfaceCreateInfoKHR
  , MirSurfaceCreateInfoKHR(..)
  , createMirSurfaceKHR
  , getPhysicalDeviceMirPresentationSupportKHR
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createMirSurfaceKHR
  , getPhysicalDeviceMirPresentationSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_mir_surface
  ( VkMirSurfaceCreateFlagsKHR(..)
  , VkMirSurfaceCreateInfoKHR(..)
  , MirConnection
  , MirSurface
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_mir_surface
  ( pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "MirSurfaceCreateFlagsKHR"
type MirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR
-- No documentation found for TopLevel "MirSurfaceCreateInfoKHR"
data MirSurfaceCreateInfoKHR = MirSurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "MirSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MirSurfaceCreateInfoKHR" "flags"
  vkFlags :: MirSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "MirSurfaceCreateInfoKHR" "connection"
  vkConnection :: Ptr MirConnection
  , -- No documentation found for Nested "MirSurfaceCreateInfoKHR" "mirSurface"
  vkMirSurface :: Ptr MirSurface
  }
  deriving (Show, Eq)
withCStructMirSurfaceCreateInfoKHR :: MirSurfaceCreateInfoKHR -> (VkMirSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructMirSurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: MirSurfaceCreateInfoKHR)) (\pPNext -> cont (VkMirSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: MirSurfaceCreateInfoKHR)) (vkConnection (from :: MirSurfaceCreateInfoKHR)) (vkMirSurface (from :: MirSurfaceCreateInfoKHR))))
fromCStructMirSurfaceCreateInfoKHR :: VkMirSurfaceCreateInfoKHR -> IO MirSurfaceCreateInfoKHR
fromCStructMirSurfaceCreateInfoKHR c = MirSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMirSurfaceCreateInfoKHR)))
                                                               <*> pure (vkFlags (c :: VkMirSurfaceCreateInfoKHR))
                                                               <*> pure (vkConnection (c :: VkMirSurfaceCreateInfoKHR))
                                                               <*> pure (vkMirSurface (c :: VkMirSurfaceCreateInfoKHR))

-- | Wrapper for vkCreateMirSurfaceKHR
createMirSurfaceKHR :: Instance ->  MirSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMirSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructMirSurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createMirSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for vkGetPhysicalDeviceMirPresentationSupportKHR
getPhysicalDeviceMirPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr MirConnection ->  IO (VkBool32)
getPhysicalDeviceMirPresentationSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> \connection -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceMirPresentationSupportKHR commandTable physicalDevice queueFamilyIndex connection >>= (\r -> pure r)
