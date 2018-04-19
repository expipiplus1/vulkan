{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayPlaneAlphaFlagBitsKHR(..)
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , VkDisplayModeCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , VkDisplayKHR
  , VkDisplayModeKHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , vkGetDisplayModePropertiesKHR
  , vkCreateDisplayModeKHR
  , vkGetDisplayPlaneCapabilitiesKHR
  , vkCreateDisplayPlaneSurfaceKHR
  , VkDisplayPropertiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayPlaneAlphaFlagsKHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  , CChar(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkInstance
  , VkAllocationCallbacks(..)
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkOffset2D(..)
  , VkExtent2D(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceTransformFlagsKHR
  , VkSurfaceKHR
  )


-- ** VkDisplayPlaneAlphaFlagBitsKHR

-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPlaneAlphaFlagsKHR', 'VkDisplaySurfaceCreateInfoKHR'
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDisplayPlaneAlphaFlagBitsKHR where
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
  showsPrec p (VkDisplayPlaneAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkDisplayPlaneAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR",                  pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR",                  pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR",               pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPlaneAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkDisplayPlaneAlphaFlagBitsKHR v)
                        )
                    )

-- | @VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR@ specifies that the source image
-- will be treated as opaque.
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000001

-- | @VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR@ specifies that a global alpha
-- value /must/ be specified that will be applied to all pixels in the
-- source image.
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000002

-- | @VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR@ specifies that the alpha
-- value will be determined by the alpha channel of the source image’s
-- pixels. If the source format contains no alpha values, no blending will
-- be applied. The source alpha values are not premultiplied into the
-- source image’s other color channels.
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000004

-- | @VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR@ is equivalent
-- to @VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR@, except the source alpha
-- values are assumed to be premultiplied into the source image’s other
-- color channels.
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000008
-- ** VkDisplayModeCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplayModeCreateFlagsKHR"
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDisplayModeCreateFlagsKHR where
  
  showsPrec p (VkDisplayModeCreateFlagsKHR x) = showParen (p >= 11) (showString "VkDisplayModeCreateFlagsKHR " . showsPrec 11 x)

instance Read VkDisplayModeCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayModeCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkDisplayModeCreateFlagsKHR v)
                        )
                    )


-- ** VkDisplaySurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplaySurfaceCreateFlagsKHR"
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDisplaySurfaceCreateFlagsKHR where
  
  showsPrec p (VkDisplaySurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkDisplaySurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkDisplaySurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplaySurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkDisplaySurfaceCreateFlagsKHR v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = VkStructureType 1000002000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = VkStructureType 1000002001
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern VK_KHR_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SPEC_VERSION = 21
-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern VK_KHR_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"
-- | Dummy data to tag the 'Ptr' with
data VkDisplayKHR_T
-- | VkDisplayKHR - Opaque handle to a display object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPlanePropertiesKHR', 'VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.vkAcquireXlibDisplayEXT',
-- 'vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkDisplayPowerControlEXT',
-- 'vkGetDisplayModePropertiesKHR',
-- 'vkGetDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.vkGetRandROutputDisplayEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display.vkReleaseDisplayEXT'
type VkDisplayKHR = Ptr VkDisplayKHR_T
-- | Dummy data to tag the 'Ptr' with
data VkDisplayModeKHR_T
-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModePropertiesKHR', 'VkDisplaySurfaceCreateInfoKHR',
-- 'vkCreateDisplayModeKHR', 'vkGetDisplayPlaneCapabilitiesKHR'
type VkDisplayModeKHR = Ptr VkDisplayModeKHR_T
-- | vkGetPhysicalDeviceDisplayPropertiesKHR - Query information about the
-- available displays
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display devices available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayPropertiesKHR@ structures.
--
-- = Description
-- #_description#
--
-- If @pProperties@ is @NULL@, then the number of display devices available
-- for @physicalDevice@ is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If the value of @pPropertyCount@ is less than the
-- number of display devices for @physicalDevice@, at most @pPropertyCount@
-- structures will be written. If @pPropertyCount@ is smaller than the
-- number of display devices available for @physicalDevice@,
-- @VK_INCOMPLETE@ will be returned instead of @VK_SUCCESS@ to indicate
-- that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkDisplayPropertiesKHR@ structures
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
-- | vkGetPhysicalDeviceDisplayPlanePropertiesKHR - Query the plane
-- properties
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display planes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayPlanePropertiesKHR@ structures.
--
-- = Description
-- #_description#
--
-- If @pProperties@ is @NULL@, then the number of display planes available
-- for @physicalDevice@ is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If the value of @pPropertyCount@ is less than the
-- number of display planes for @physicalDevice@, at most @pPropertyCount@
-- structures will be written.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkDisplayPlanePropertiesKHR@
--     structures
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
-- | vkGetDisplayPlaneSupportedDisplaysKHR - Query the list of displays a
-- plane supports
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @planeIndex@ is the plane which the application wishes to use, and
--     /must/ be in the range [0, physical device plane count - 1].
--
-- -   @pDisplayCount@ is a pointer to an integer related to the number of
--     displays available or queried, as described below.
--
-- -   @pDisplays@ is either @NULL@ or a pointer to an array of
--     @VkDisplayKHR@ handles.
--
-- = Description
-- #_description#
--
-- If @pDisplays@ is @NULL@, then the number of displays usable with the
-- specified @planeIndex@ for @physicalDevice@ is returned in
-- @pDisplayCount@. Otherwise, @pDisplayCount@ /must/ point to a variable
-- set by the user to the number of elements in the @pDisplays@ array, and
-- on return the variable is overwritten with the number of handles
-- actually written to @pDisplays@. If the value of @pDisplayCount@ is less
-- than the number of display planes for @physicalDevice@, at most
-- @pDisplayCount@ handles will be written. If @pDisplayCount@ is smaller
-- than the number of displays usable with the specified @planeIndex@ for
-- @physicalDevice@, @VK_INCOMPLETE@ will be returned instead of
-- @VK_SUCCESS@ to indicate that not all the available values were
-- returned.
--
-- == Valid Usage
--
-- -   @planeIndex@ /must/ be less than the number of display planes
--     supported by the device as determined by calling
--     @vkGetPhysicalDeviceDisplayPlanePropertiesKHR@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pDisplayCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pDisplayCount@ is not @0@, and
--     @pDisplays@ is not @NULL@, @pDisplays@ /must/ be a valid pointer to
--     an array of @pDisplayCount@ @VkDisplayKHR@ handles
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
-- | vkGetDisplayModePropertiesKHR - Query the set of mode properties
-- supported by the display
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device associated with @display@.
--
-- -   @display@ is the display to query.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display modes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayModePropertiesKHR@ structures.
--
-- = Description
-- #_description#
--
-- If @pProperties@ is @NULL@, then the number of display modes available
-- on the specified @display@ for @physicalDevice@ is returned in
-- @pPropertyCount@. Otherwise, @pPropertyCount@ /must/ point to a variable
-- set by the user to the number of elements in the @pProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pProperties@. If the value of @pPropertyCount@ is
-- less than the number of display modes for @physicalDevice@, at most
-- @pPropertyCount@ structures will be written. If @pPropertyCount@ is
-- smaller than the number of display modes available on the specified
-- @display@ for @physicalDevice@, @VK_INCOMPLETE@ will be returned instead
-- of @VK_SUCCESS@ to indicate that not all the available values were
-- returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @display@ /must/ be a valid @VkDisplayKHR@ handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkDisplayModePropertiesKHR@
--     structures
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayKHR', 'VkDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
-- | vkCreateDisplayModeKHR - Create a display mode
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device associated with @display@.
--
-- -   @display@ is the display to create an additional mode for.
--
-- -   @pCreateInfo@ is a 'VkDisplayModeCreateInfoKHR' structure describing
--     the new mode to create.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     display mode object when there is no more specific allocator
--     available (see
--     <{html_spec_relative}#memory-allocation Memory Allocation>).
--
-- -   @pMode@ returns the handle of the mode created.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @display@ /must/ be a valid @VkDisplayKHR@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDisplayModeCreateInfoKHR@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pMode@ /must/ be a valid pointer to a @VkDisplayModeKHR@ handle
--
-- == Host Synchronization
--
-- -   Host access to @display@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDisplayKHR', 'VkDisplayModeCreateInfoKHR', 'VkDisplayModeKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
-- | vkGetDisplayPlaneCapabilitiesKHR - Query capabilities of a mode and
-- plane combination
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device associated with @display@
--
-- -   @mode@ is the display mode the application intends to program when
--     using the specified plane. Note this parameter also implicitly
--     specifies a display.
--
-- -   @planeIndex@ is the plane which the application intends to use with
--     the display, and is less than the number of display planes supported
--     by the device.
--
-- -   @pCapabilities@ is a pointer to a 'VkDisplayPlaneCapabilitiesKHR'
--     structure in which the capabilities are returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @mode@ /must/ be a valid @VkDisplayModeKHR@ handle
--
-- -   @pCapabilities@ /must/ be a valid pointer to a
--     @VkDisplayPlaneCapabilitiesKHR@ structure
--
-- == Host Synchronization
--
-- -   Host access to @mode@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModeKHR', 'VkDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
-- | vkCreateDisplayPlaneSurfaceKHR - Create a
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR' structure
-- representing a display plane and mode
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ is the instance corresponding to the physical device the
--     targeted display is on.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDisplaySurfaceCreateInfoKHR' structure specifying which mode,
--     plane, and other parameters to use, as described below.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see <{html_spec_relative}#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a @VkSurfaceKHR@ handle in which the created
--     surface is returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDisplaySurfaceCreateInfoKHR@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSurface@ /must/ be a valid pointer to a @VkSurfaceKHR@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR'
foreign import ccall "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
-- | VkDisplayPropertiesKHR - Structure describing an available display
-- device
--
-- = Description
-- #_description#
--
-- __Note__
--
-- For devices which have no natural value to return here, implementations
-- /should/ return the maximum resolution supported.
--
-- -   @supportedTransforms@ tells which transforms are supported by this
--     display. This will contain one or more of the bits from
--     @VkSurfaceTransformFlagsKHR@.
--
-- -   @planeReorderPossible@ tells whether the planes on this display
--     /can/ have their z order changed. If this is @VK_TRUE@, the
--     application /can/ re-arrange the planes on this display in any order
--     relative to each other.
--
-- -   @persistentContent@ tells whether the display supports
--     self-refresh\/internal buffering. If this is true, the application
--     /can/ submit persistent present operations on swapchains created
--     against this display.
--
-- __Note__
--
-- Persistent presents /may/ have higher latency, and /may/ use less power
-- when the screen content is updated infrequently, or when only a portion
-- of the screen needs to be updated in most frames.
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkDisplayKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR',
-- 'vkGetPhysicalDeviceDisplayPropertiesKHR'
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR
  { -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkDisplay"
  vkDisplay :: VkDisplayKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkDisplayName"
  vkDisplayName :: Ptr CChar
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkPhysicalDimensions"
  vkPhysicalDimensions :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkPhysicalResolution"
  vkPhysicalResolution :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkSupportedTransforms"
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkPlaneReorderPossible"
  vkPlaneReorderPossible :: VkBool32
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "vkPersistentContent"
  vkPersistentContent :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkDisplayPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplay (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkDisplayName (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDimensions (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkPhysicalResolution (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkSupportedTransforms (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneReorderPossible (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkPersistentContent (poked :: VkDisplayPropertiesKHR))
-- | VkDisplayPlanePropertiesKHR - Structure describing display plane
-- properties
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayKHR', 'vkGetPhysicalDeviceDisplayPlanePropertiesKHR'
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR
  { -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "vkCurrentDisplay"
  vkCurrentDisplay :: VkDisplayKHR
  , -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "vkCurrentStackIndex"
  vkCurrentStackIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkDisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkCurrentDisplay (poked :: VkDisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentStackIndex (poked :: VkDisplayPlanePropertiesKHR))
-- | VkDisplayModeParametersKHR - Structure describing display parameters
-- associated with a display mode
--
-- = Description
-- #_description#
--
-- __Note__
--
-- For example, a 60Hz display mode would report a @refreshRate@ of 60,000.
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModeCreateInfoKHR', 'VkDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D'
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR
  { -- No documentation found for Nested "VkDisplayModeParametersKHR" "vkVisibleRegion"
  vkVisibleRegion :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayModeParametersKHR" "vkRefreshRate"
  vkRefreshRate :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVisibleRegion (poked :: VkDisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (vkRefreshRate (poked :: VkDisplayModeParametersKHR))
-- | VkDisplayModePropertiesKHR - Structure describing display mode
-- properties
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModeKHR', 'VkDisplayModeParametersKHR',
-- 'vkGetDisplayModePropertiesKHR'
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR
  { -- No documentation found for Nested "VkDisplayModePropertiesKHR" "vkDisplayMode"
  vkDisplayMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplayModePropertiesKHR" "vkParameters"
  vkParameters :: VkDisplayModeParametersKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplayMode (poked :: VkDisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkParameters (poked :: VkDisplayModePropertiesKHR))
-- | VkDisplayModeCreateInfoKHR - Structure specifying parameters of a newly
-- created display mode object
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   The @width@ and @height@ members of the @visibleRegion@ member of
--     @parameters@ /must/ be greater than @0@
--
-- -   The @refreshRate@ member of @parameters@ /must/ be greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModeCreateFlagsKHR', 'VkDisplayModeParametersKHR',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateDisplayModeKHR'
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR
  { -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "vkFlags"
  vkFlags :: VkDisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "vkParameters"
  vkParameters :: VkDisplayModeParametersKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkParameters (poked :: VkDisplayModeCreateInfoKHR))
-- | VkDisplayPlaneCapabilitiesKHR - Structure describing capabilities of a
-- mode and plane combination
--
-- = Description
-- #_description#
--
-- The minimum and maximum position and extent fields describe the
-- implementation limits, if any, as they apply to the specified display
-- mode and plane. Vendors /may/ support displaying a subset of a
-- swapchain’s presentable images on the specified display plane. This is
-- expressed by returning @minSrcPosition@, @maxSrcPosition@,
-- @minSrcExtent@, and @maxSrcExtent@ values that indicate a range of
-- possible positions and sizes /may/ be used to specify the region within
-- the presentable images that source pixels will be read from when
-- creating a swapchain on the specified display mode and plane.
--
-- Vendors /may/ also support mapping the presentable images’ content to a
-- subset or superset of the visible region in the specified display mode.
-- This is expressed by returning @minDstPosition@, @maxDstPosition@,
-- @minDstExtent@ and @maxDstExtent@ values that indicate a range of
-- possible positions and sizes /may/ be used to describe the region within
-- the display mode that the source pixels will be mapped to.
--
-- Other vendors /may/ support only a 1-1 mapping between pixels in the
-- presentable images and the display mode. This /may/ be indicated by
-- returning (0,0) for @minSrcPosition@, @maxSrcPosition@,
-- @minDstPosition@, and @maxDstPosition@, and (display mode width, display
-- mode height) for @minSrcExtent@, @maxSrcExtent@, @minDstExtent@, and
-- @maxDstExtent@.
--
-- These values indicate the limits of the implementation’s individual
-- fields. Not all combinations of values within the offset and extent
-- ranges returned in @VkDisplayPlaneCapabilitiesKHR@ are guaranteed to be
-- supported. Vendors /may/ still fail presentation requests that specify
-- unsupported combinations.
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPlaneAlphaFlagsKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.Pipeline.VkOffset2D',
-- 'vkGetDisplayPlaneCapabilitiesKHR'
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR
  { -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkSupportedAlpha"
  vkSupportedAlpha :: VkDisplayPlaneAlphaFlagsKHR
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMinSrcPosition"
  vkMinSrcPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMaxSrcPosition"
  vkMaxSrcPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMinSrcExtent"
  vkMinSrcExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMaxSrcExtent"
  vkMaxSrcExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMinDstPosition"
  vkMinDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMaxDstPosition"
  vkMaxDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMinDstExtent"
  vkMinDstExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "vkMaxDstExtent"
  vkMaxDstExtent :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkDisplayPlaneCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 12)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 28)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 52)
                                           <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSupportedAlpha (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMinSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (vkMaxSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (vkMinSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (vkMaxSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkMinDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkMaxDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (vkMinDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (vkMaxDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
-- | VkDisplaySurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created display plane surface object
--
-- = Description
-- #_description#
--
-- __Note__
--
-- Creating a display surface /must/ not modify the state of the displays,
-- planes, or other resources it names. For example, it /must/ not apply
-- the specified mode to be set on the associated display. Application of
-- display configuration occurs as a side effect of presenting to a display
-- surface.
--
-- == Valid Usage
--
-- -   @planeIndex@ /must/ be less than the number of display planes
--     supported by the device as determined by calling
--     @vkGetPhysicalDeviceDisplayPlanePropertiesKHR@
--
-- -   If the @planeReorderPossible@ member of the @VkDisplayPropertiesKHR@
--     structure returned by @vkGetPhysicalDeviceDisplayPropertiesKHR@ for
--     the display corresponding to @displayMode@ is @VK_TRUE@ then
--     @planeStackIndex@ /must/ be less than the number of display planes
--     supported by the device as determined by calling
--     @vkGetPhysicalDeviceDisplayPlanePropertiesKHR@; otherwise
--     @planeStackIndex@ /must/ equal the @currentStackIndex@ member of
--     @VkDisplayPlanePropertiesKHR@ returned by
--     @vkGetPhysicalDeviceDisplayPlanePropertiesKHR@ for the display plane
--     corresponding to @displayMode@
--
-- -   If @alphaMode@ is @VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR@ then
--     @globalAlpha@ /must/ be between @0@ and @1@, inclusive
--
-- -   @alphaMode@ /must/ be @0@ or one of the bits present in the
--     @supportedAlpha@ member of @VkDisplayPlaneCapabilitiesKHR@ returned
--     by @vkGetDisplayPlaneCapabilitiesKHR@ for the display plane
--     corresponding to @displayMode@
--
-- -   The @width@ and @height@ members of @imageExtent@ /must/ be less
--     than the @maxImageDimensions2D@ member of @VkPhysicalDeviceLimits@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @displayMode@ /must/ be a valid @VkDisplayModeKHR@ handle
--
-- -   @transform@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
--     value
--
-- -   @alphaMode@ /must/ be a valid 'VkDisplayPlaneAlphaFlagBitsKHR' value
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayModeKHR', 'VkDisplayPlaneAlphaFlagBitsKHR',
-- 'VkDisplaySurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR',
-- 'vkCreateDisplayPlaneSurfaceKHR'
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkFlags"
  vkFlags :: VkDisplaySurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkDisplayMode"
  vkDisplayMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkPlaneIndex"
  vkPlaneIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkPlaneStackIndex"
  vkPlaneStackIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkTransform"
  vkTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkGlobalAlpha"
  vkGlobalAlpha :: CFloat
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkAlphaMode"
  vkAlphaMode :: VkDisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "vkImageExtent"
  vkImageExtent :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkDisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplaySurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 48)
                                           <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplayMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPlaneIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneStackIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkTransform (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkGlobalAlpha (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkAlphaMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageExtent (poked :: VkDisplaySurfaceCreateInfoKHR))
-- | VkDisplayPlaneAlphaFlagsKHR - Bitmask of VkDisplayPlaneAlphaFlagBitsKHR
--
-- = Description
-- #_description#
--
-- @VkDisplayPlaneAlphaFlagsKHR@ is a bitmask type for setting a mask of
-- zero or more 'VkDisplayPlaneAlphaFlagBitsKHR'.
--
-- = See Also
-- #_see_also#
--
-- 'VkDisplayPlaneAlphaFlagBitsKHR', 'VkDisplayPlaneCapabilitiesKHR'
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR
