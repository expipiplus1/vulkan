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
  , pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  , pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  , vkSetLocalDimmingAMD
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  )



-- | VkDisplayNativeHdrSurfaceCapabilitiesAMD - Structure describing display
-- native HDR specific capabilities of a surface
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DisplayNativeHdrSurfaceCapabilitiesAMD = DisplayNativeHdrSurfaceCapabilitiesAMD
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayNativeHdrSurfaceCapabilitiesAMD" "localDimmingSupport"
  localDimmingSupport :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayNativeHdrSurfaceCapabilitiesAMD' and
-- marshal a 'DisplayNativeHdrSurfaceCapabilitiesAMD' into it. The 'VkDisplayNativeHdrSurfaceCapabilitiesAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayNativeHdrSurfaceCapabilitiesAMD :: DisplayNativeHdrSurfaceCapabilitiesAMD -> (VkDisplayNativeHdrSurfaceCapabilitiesAMD -> IO a) -> IO a
withCStructDisplayNativeHdrSurfaceCapabilitiesAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DisplayNativeHdrSurfaceCapabilitiesAMD)) (\pPNext -> cont (VkDisplayNativeHdrSurfaceCapabilitiesAMD VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD pPNext (boolToBool32 (localDimmingSupport (marshalled :: DisplayNativeHdrSurfaceCapabilitiesAMD)))))

-- | A function to read a 'VkDisplayNativeHdrSurfaceCapabilitiesAMD' and all additional
-- structures in the pointer chain into a 'DisplayNativeHdrSurfaceCapabilitiesAMD'.
fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD :: VkDisplayNativeHdrSurfaceCapabilitiesAMD -> IO DisplayNativeHdrSurfaceCapabilitiesAMD
fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD c = DisplayNativeHdrSurfaceCapabilitiesAMD <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayNativeHdrSurfaceCapabilitiesAMD)))
                                                                                             <*> pure (bool32ToBool (vkLocalDimmingSupport (c :: VkDisplayNativeHdrSurfaceCapabilitiesAMD)))

instance Zero DisplayNativeHdrSurfaceCapabilitiesAMD where
  zero = DisplayNativeHdrSurfaceCapabilitiesAMD Nothing
                                                False



-- | VkSwapchainDisplayNativeHdrCreateInfoAMD - Structure specifying display
-- native HDR parameters of a newly created swapchain object
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
-- does not contain this structure, the default value for
-- @localDimmingEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', meaning
-- local dimming is initially enabled for the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Valid Usage
--
-- -   It is only valid to set @localDimmingEnable@ to
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if
--     'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.VkDisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@
--     is supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SwapchainDisplayNativeHdrCreateInfoAMD = SwapchainDisplayNativeHdrCreateInfoAMD
  { -- Univalued member elided
  -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainDisplayNativeHdrCreateInfoAMD" "localDimmingEnable"
  localDimmingEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSwapchainDisplayNativeHdrCreateInfoAMD' and
-- marshal a 'SwapchainDisplayNativeHdrCreateInfoAMD' into it. The 'VkSwapchainDisplayNativeHdrCreateInfoAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSwapchainDisplayNativeHdrCreateInfoAMD :: SwapchainDisplayNativeHdrCreateInfoAMD -> (VkSwapchainDisplayNativeHdrCreateInfoAMD -> IO a) -> IO a
withCStructSwapchainDisplayNativeHdrCreateInfoAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SwapchainDisplayNativeHdrCreateInfoAMD)) (\pPNext -> cont (VkSwapchainDisplayNativeHdrCreateInfoAMD VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD pPNext (boolToBool32 (localDimmingEnable (marshalled :: SwapchainDisplayNativeHdrCreateInfoAMD)))))

-- | A function to read a 'VkSwapchainDisplayNativeHdrCreateInfoAMD' and all additional
-- structures in the pointer chain into a 'SwapchainDisplayNativeHdrCreateInfoAMD'.
fromCStructSwapchainDisplayNativeHdrCreateInfoAMD :: VkSwapchainDisplayNativeHdrCreateInfoAMD -> IO SwapchainDisplayNativeHdrCreateInfoAMD
fromCStructSwapchainDisplayNativeHdrCreateInfoAMD c = SwapchainDisplayNativeHdrCreateInfoAMD <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSwapchainDisplayNativeHdrCreateInfoAMD)))
                                                                                             <*> pure (bool32ToBool (vkLocalDimmingEnable (c :: VkSwapchainDisplayNativeHdrCreateInfoAMD)))

instance Zero SwapchainDisplayNativeHdrCreateInfoAMD where
  zero = SwapchainDisplayNativeHdrCreateInfoAMD Nothing
                                                False



-- | vkSetLocalDimmingAMD - Set Local Dimming
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapChain@.
--
-- -   @swapChain@ handle to enable local dimming.
--
-- -   @localDimmingEnable@ specifies whether local dimming is enabled for
--     the swapchain.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @swapChain@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle
--
-- -   Both of @device@, and @swapChain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Valid Usage
--
-- -   It is only valid to call
--     'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.vkSetLocalDimmingAMD'
--     if
--     'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.VkDisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@
--     is supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
setLocalDimmingAMD :: Device ->  SwapchainKHR ->  Bool ->  IO ()
setLocalDimmingAMD = \(Device device' commandTable) -> \swapChain' -> \localDimmingEnable' -> vkSetLocalDimmingAMD commandTable device' swapChain' (boolToBool32 localDimmingEnable') *> (pure ())

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME"
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION"
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION :: Integral a => a
pattern AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
