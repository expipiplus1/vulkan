{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , 
  SwapchainDisplayNativeHdrCreateInfoAMD(..)
#endif
  , setLocalDimmingAMD
  , pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  , pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( vkSetLocalDimmingAMD
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayNativeHdrSurfaceCapabilitiesAMD"
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "localDimmingSupport"
  localDimmingSupport :: Bool
  }
  deriving (Show, Eq)

instance Zero DisplayNativeHdrSurfaceCapabilitiesAMD where
  zero = DisplayNativeHdrSurfaceCapabilitiesAMD Nothing
                                                False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSwapchainDisplayNativeHdrCreateInfoAMD"
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "localDimmingEnable"
  localDimmingEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero SwapchainDisplayNativeHdrCreateInfoAMD where
  zero = SwapchainDisplayNativeHdrCreateInfoAMD Nothing
                                                False

#endif


-- No documentation found for TopLevel "vkSetLocalDimmingAMD"
setLocalDimmingAMD :: Device ->  SwapchainKHR ->  Bool ->  IO ()
setLocalDimmingAMD = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME"
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION"
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION :: Integral a => a
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
