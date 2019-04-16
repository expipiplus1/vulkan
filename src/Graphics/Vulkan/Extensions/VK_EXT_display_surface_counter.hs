{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( withCStructSurfaceCapabilities2EXT
  , fromCStructSurfaceCapabilities2EXT
  , SurfaceCapabilities2EXT(..)
  , SurfaceCounterFlagBitsEXT
  , SurfaceCounterFlagsEXT
  , getPhysicalDeviceSurfaceCapabilities2EXT
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceSurfaceCapabilities2EXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagBitsEXT(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , ImageUsageFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( CompositeAlphaFlagsKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
  )


-- No documentation found for TopLevel "SurfaceCapabilities2EXT"
data SurfaceCapabilities2EXT = SurfaceCapabilities2EXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceCapabilities2EXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageCount"
  vkMaxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentExtent"
  vkCurrentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageExtent"
  vkMinImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageExtent"
  vkMaxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedTransforms"
  vkSupportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentTransform"
  vkCurrentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedCompositeAlpha"
  vkSupportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedUsageFlags"
  vkSupportedUsageFlags :: ImageUsageFlags
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedSurfaceCounters"
  vkSupportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Show, Eq)
withCStructSurfaceCapabilities2EXT :: SurfaceCapabilities2EXT -> (VkSurfaceCapabilities2EXT -> IO a) -> IO a
withCStructSurfaceCapabilities2EXT from cont = withCStructExtent2D (vkMaxImageExtent (from :: SurfaceCapabilities2EXT)) (\maxImageExtent -> withCStructExtent2D (vkMinImageExtent (from :: SurfaceCapabilities2EXT)) (\minImageExtent -> withCStructExtent2D (vkCurrentExtent (from :: SurfaceCapabilities2EXT)) (\currentExtent -> maybeWith withSomeVkStruct (vkPNext (from :: SurfaceCapabilities2EXT)) (\pPNext -> cont (VkSurfaceCapabilities2EXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT pPNext (vkMinImageCount (from :: SurfaceCapabilities2EXT)) (vkMaxImageCount (from :: SurfaceCapabilities2EXT)) currentExtent minImageExtent maxImageExtent (vkMaxImageArrayLayers (from :: SurfaceCapabilities2EXT)) (vkSupportedTransforms (from :: SurfaceCapabilities2EXT)) (vkCurrentTransform (from :: SurfaceCapabilities2EXT)) (vkSupportedCompositeAlpha (from :: SurfaceCapabilities2EXT)) (vkSupportedUsageFlags (from :: SurfaceCapabilities2EXT)) (vkSupportedSurfaceCounters (from :: SurfaceCapabilities2EXT)))))))
fromCStructSurfaceCapabilities2EXT :: VkSurfaceCapabilities2EXT -> IO SurfaceCapabilities2EXT
fromCStructSurfaceCapabilities2EXT c = SurfaceCapabilities2EXT <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> pure (vkMinImageCount (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkMaxImageCount (c :: VkSurfaceCapabilities2EXT))
                                                               <*> (fromCStructExtent2D (vkCurrentExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> (fromCStructExtent2D (vkMinImageExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> (fromCStructExtent2D (vkMaxImageExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> pure (vkMaxImageArrayLayers (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedTransforms (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkCurrentTransform (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedCompositeAlpha (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedUsageFlags (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedSurfaceCounters (c :: VkSurfaceCapabilities2EXT))
-- No documentation found for TopLevel "SurfaceCounterFlagBitsEXT"
type SurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT
-- No documentation found for TopLevel "SurfaceCounterFlagsEXT"
type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

-- | Wrapper for vkGetPhysicalDeviceSurfaceCapabilities2EXT
getPhysicalDeviceSurfaceCapabilities2EXT :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT = \(PhysicalDevice physicalDevice commandTable) -> \surface -> alloca (\pSurfaceCapabilities -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceCapabilities2EXT commandTable physicalDevice surface pSurfaceCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructSurfaceCapabilities2EXT <=< peek) pSurfaceCapabilities)))
