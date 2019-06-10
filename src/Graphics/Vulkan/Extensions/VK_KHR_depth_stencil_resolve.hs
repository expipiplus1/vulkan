{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , 
#endif
  ResolveModeFlagBitsKHR
  , pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  , pattern RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern RESOLVE_MODE_MIN_BIT_KHR
  , pattern RESOLVE_MODE_MAX_BIT_KHR
  , pattern RESOLVE_MODE_NONE_KHR
  , ResolveModeFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , SubpassDescriptionDepthStencilResolveKHR(..)
#endif
  , pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkResolveModeFlagBitsKHR(..)
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  , pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern VK_RESOLVE_MODE_MAX_BIT_KHR
  , pattern VK_RESOLVE_MODE_MIN_BIT_KHR
  , pattern VK_RESOLVE_MODE_NONE_KHR
  , pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( AttachmentReference2KHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDepthStencilResolvePropertiesKHR"
data PhysicalDeviceDepthStencilResolvePropertiesKHR = PhysicalDeviceDepthStencilResolvePropertiesKHR
  { -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedDepthResolveModes"
  supportedDepthResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedStencilResolveModes"
  supportedStencilResolveModes :: ResolveModeFlagsKHR
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolveNone"
  independentResolveNone :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolve"
  independentResolve :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDepthStencilResolvePropertiesKHR where
  zero = PhysicalDeviceDepthStencilResolvePropertiesKHR Nothing
                                                        zero
                                                        zero
                                                        False
                                                        False

#endif

-- No documentation found for TopLevel "ResolveModeFlagBitsKHR"
type ResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR


{-# complete RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR, RESOLVE_MODE_AVERAGE_BIT_KHR, RESOLVE_MODE_MIN_BIT_KHR, RESOLVE_MODE_MAX_BIT_KHR, RESOLVE_MODE_NONE_KHR :: ResolveModeFlagBitsKHR #-}


-- No documentation found for Nested "ResolveModeFlagBitsKHR" "RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR"
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR


-- No documentation found for Nested "ResolveModeFlagBitsKHR" "RESOLVE_MODE_AVERAGE_BIT_KHR"
pattern RESOLVE_MODE_AVERAGE_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_AVERAGE_BIT_KHR = VK_RESOLVE_MODE_AVERAGE_BIT_KHR


-- No documentation found for Nested "ResolveModeFlagBitsKHR" "RESOLVE_MODE_MIN_BIT_KHR"
pattern RESOLVE_MODE_MIN_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_MIN_BIT_KHR = VK_RESOLVE_MODE_MIN_BIT_KHR


-- No documentation found for Nested "ResolveModeFlagBitsKHR" "RESOLVE_MODE_MAX_BIT_KHR"
pattern RESOLVE_MODE_MAX_BIT_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_MAX_BIT_KHR = VK_RESOLVE_MODE_MAX_BIT_KHR


-- No documentation found for Nested "ResolveModeFlagBitsKHR" "RESOLVE_MODE_NONE_KHR"
pattern RESOLVE_MODE_NONE_KHR :: (a ~ ResolveModeFlagBitsKHR) => a
pattern RESOLVE_MODE_NONE_KHR = VK_RESOLVE_MODE_NONE_KHR

-- No documentation found for TopLevel "ResolveModeFlagsKHR"
type ResolveModeFlagsKHR = ResolveModeFlagBitsKHR


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubpassDescriptionDepthStencilResolveKHR"
data SubpassDescriptionDepthStencilResolveKHR = SubpassDescriptionDepthStencilResolveKHR
  { -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "depthResolveMode"
  depthResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "stencilResolveMode"
  stencilResolveMode :: ResolveModeFlagBitsKHR
  , -- No documentation found for Nested "SubpassDescriptionDepthStencilResolveKHR" "pDepthStencilResolveAttachment"
  depthStencilResolveAttachment :: Maybe AttachmentReference2KHR
  }
  deriving (Show, Eq)

instance Zero SubpassDescriptionDepthStencilResolveKHR where
  zero = SubpassDescriptionDepthStencilResolveKHR Nothing
                                                  zero
                                                  zero
                                                  Nothing

#endif

-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME"
pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION"
pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION :: Integral a => a
pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
