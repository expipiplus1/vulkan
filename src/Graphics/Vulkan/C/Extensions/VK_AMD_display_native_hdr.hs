{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  , FN_vkSetLocalDimmingAMD
  , PFN_vkSetLocalDimmingAMD
  , vkSetLocalDimmingAMD
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  , pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION
  , pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD
  , pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
data VkDisplayNativeHdrSurfaceCapabilitiesAMD = VkDisplayNativeHdrSurfaceCapabilitiesAMD
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @localDimmingSupport@ specifies whether the surface supports local
  -- dimming. If this is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
  -- 'VkSwapchainDisplayNativeHdrCreateInfoAMD' /can/ be used to explicitly
  -- enable or disable local dimming for the surface. Local dimming may also
  -- be overriden by 'vkSetLocalDimmingAMD' during the lifetime of the
  -- swapchain.
  vkLocalDimmingSupport :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDisplayNativeHdrSurfaceCapabilitiesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayNativeHdrSurfaceCapabilitiesAMD <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayNativeHdrSurfaceCapabilitiesAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayNativeHdrSurfaceCapabilitiesAMD))
                *> poke (ptr `plusPtr` 16) (vkLocalDimmingSupport (poked :: VkDisplayNativeHdrSurfaceCapabilitiesAMD))

instance Zero VkDisplayNativeHdrSurfaceCapabilitiesAMD where
  zero = VkDisplayNativeHdrSurfaceCapabilitiesAMD VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
                                                  zero
                                                  zero

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
--     'VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Valid Usage
--
-- -   It is only valid to set @localDimmingEnable@ to
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if
--     'VkDisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSwapchainDisplayNativeHdrCreateInfoAMD = VkSwapchainDisplayNativeHdrCreateInfoAMD
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @localDimmingEnable@ specifies whether local dimming is enabled for the
  -- swapchain.
  vkLocalDimmingEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSwapchainDisplayNativeHdrCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSwapchainDisplayNativeHdrCreateInfoAMD <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainDisplayNativeHdrCreateInfoAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainDisplayNativeHdrCreateInfoAMD))
                *> poke (ptr `plusPtr` 16) (vkLocalDimmingEnable (poked :: VkSwapchainDisplayNativeHdrCreateInfoAMD))

instance Zero VkSwapchainDisplayNativeHdrCreateInfoAMD where
  zero = VkSwapchainDisplayNativeHdrCreateInfoAMD VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
                                                  zero
                                                  zero

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
-- -   It is only valid to call 'vkSetLocalDimmingAMD' if
--     'VkDisplayNativeHdrSurfaceCapabilitiesAMD'::@localDimmingSupport@ is
--     supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetLocalDimmingAMD" vkSetLocalDimmingAMD :: ("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()
#else
vkSetLocalDimmingAMD :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()
vkSetLocalDimmingAMD deviceCmds = mkVkSetLocalDimmingAMD (pVkSetLocalDimmingAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLocalDimmingAMD
  :: FunPtr (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()) -> (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ())
#endif

type FN_vkSetLocalDimmingAMD = ("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()
type PFN_vkSetLocalDimmingAMD = FunPtr FN_vkSetLocalDimmingAMD

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME"
pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = "VK_AMD_display_native_hdr"

-- No documentation found for TopLevel "VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION"
pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION :: Integral a => a
pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1

-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_DISPLAY_NATIVE_AMD"
pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD = VkColorSpaceKHR 1000213000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD"
pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD = VkStructureType 1000213000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD"
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD = VkStructureType 1000213001
