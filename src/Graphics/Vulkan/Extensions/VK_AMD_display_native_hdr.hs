{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr
  ( withCStructDisplayNativeHdrSurfaceCapabilitiesAMD
  , fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD
  , DisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , withCStructSwapchainDisplayNativeHdrCreateInfoAMD
  , fromCStructSwapchainDisplayNativeHdrCreateInfoAMD
  , SwapchainDisplayNativeHdrCreateInfoAMD(..)
  , setLocalDimmingAMD
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  , pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( setLocalDimmingAMD
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  , pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD
  )


-- No documentation found for TopLevel "DisplayNativeHdrSurfaceCapabilitiesAMD"
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "localDimmingSupport"
  vkLocalDimmingSupport :: Bool
  }
  deriving (Show, Eq)
withCStructDisplayNativeHdrSurfaceCapabilitiesAMD :: DisplayNativeHdrSurfaceCapabilitiesAMD -> (VkDisplayNativeHdrSurfaceCapabilitiesAMD -> IO a) -> IO a
withCStructDisplayNativeHdrSurfaceCapabilitiesAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: DisplayNativeHdrSurfaceCapabilitiesAMD)) (\pPNext -> cont (VkDisplayNativeHdrSurfaceCapabilitiesAMD VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD pPNext (boolToBool32 (vkLocalDimmingSupport (from :: DisplayNativeHdrSurfaceCapabilitiesAMD)))))
fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD :: VkDisplayNativeHdrSurfaceCapabilitiesAMD -> IO DisplayNativeHdrSurfaceCapabilitiesAMD
fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD c = DisplayNativeHdrSurfaceCapabilitiesAMD <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayNativeHdrSurfaceCapabilitiesAMD)))
                                                                                             <*> pure (bool32ToBool (vkLocalDimmingSupport (c :: VkDisplayNativeHdrSurfaceCapabilitiesAMD)))
-- No documentation found for TopLevel "SwapchainDisplayNativeHdrCreateInfoAMD"
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "localDimmingEnable"
  vkLocalDimmingEnable :: Bool
  }
  deriving (Show, Eq)
withCStructSwapchainDisplayNativeHdrCreateInfoAMD :: SwapchainDisplayNativeHdrCreateInfoAMD -> (VkSwapchainDisplayNativeHdrCreateInfoAMD -> IO a) -> IO a
withCStructSwapchainDisplayNativeHdrCreateInfoAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: SwapchainDisplayNativeHdrCreateInfoAMD)) (\pPNext -> cont (VkSwapchainDisplayNativeHdrCreateInfoAMD VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD pPNext (boolToBool32 (vkLocalDimmingEnable (from :: SwapchainDisplayNativeHdrCreateInfoAMD)))))
fromCStructSwapchainDisplayNativeHdrCreateInfoAMD :: VkSwapchainDisplayNativeHdrCreateInfoAMD -> IO SwapchainDisplayNativeHdrCreateInfoAMD
fromCStructSwapchainDisplayNativeHdrCreateInfoAMD c = SwapchainDisplayNativeHdrCreateInfoAMD <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSwapchainDisplayNativeHdrCreateInfoAMD)))
                                                                                             <*> pure (bool32ToBool (vkLocalDimmingEnable (c :: VkSwapchainDisplayNativeHdrCreateInfoAMD)))

-- | Wrapper for vkSetLocalDimmingAMD
setLocalDimmingAMD :: Device ->  SwapchainKHR ->  Bool ->  IO ()
setLocalDimmingAMD = \(Device device commandTable) -> \swapChain -> \localDimmingEnable -> Graphics.Vulkan.C.Dynamic.setLocalDimmingAMD commandTable device swapChain (boolToBool32 localDimmingEnable) *> (pure ())
