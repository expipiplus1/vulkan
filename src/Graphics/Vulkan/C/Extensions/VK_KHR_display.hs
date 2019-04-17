{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  , VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeKHR
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , VkDisplayPlaneAlphaFlagsKHR
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateDisplayModeKHR
#endif
  , FN_vkCreateDisplayModeKHR
  , PFN_vkCreateDisplayModeKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateDisplayPlaneSurfaceKHR
#endif
  , FN_vkCreateDisplayPlaneSurfaceKHR
  , PFN_vkCreateDisplayPlaneSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDisplayModePropertiesKHR
#endif
  , FN_vkGetDisplayModePropertiesKHR
  , PFN_vkGetDisplayModePropertiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDisplayPlaneCapabilitiesKHR
#endif
  , FN_vkGetDisplayPlaneCapabilitiesKHR
  , PFN_vkGetDisplayPlaneCapabilitiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDisplayPlaneSupportedDisplaysKHR
#endif
  , FN_vkGetDisplayPlaneSupportedDisplaysKHR
  , PFN_vkGetDisplayPlaneSupportedDisplaysKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
#endif
  , FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceDisplayPropertiesKHR
#endif
  , FN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
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
  ( CChar(..)
  , CFloat(..)
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
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkOffset2D(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  , VkSurfaceTransformFlagsKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDisplayKHR_T
-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- No cross-references are available
type VkDisplayKHR = Ptr VkDisplayKHR_T
-- ** VkDisplayModeCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplayModeCreateFlagsKHR"
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkDisplayModeCreateInfoKHR - Structure specifying parameters of a newly
-- created display mode object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkDisplayModeCreateFlagsKHR
  , -- | @parameters@ is a 'VkDisplayModeParametersKHR' structure describing the
  -- display parameters to use in creating the new mode. If the parameters
  -- are not compatible with the specified display, the implementation /must/
  -- return @VK_ERROR_INITIALIZATION_FAILED@.
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

instance Zero VkDisplayModeCreateInfoKHR where
  zero = VkDisplayModeCreateInfoKHR zero
                                    zero
                                    zero
                                    zero
-- | Dummy data to tag the 'Ptr' with
data VkDisplayModeKHR_T
-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- No cross-references are available
type VkDisplayModeKHR = Ptr VkDisplayModeKHR_T
-- | VkDisplayModeParametersKHR - Structure describing display parameters
-- associated with a display mode
--
-- = Description
--
-- __Note__
--
-- For example, a 60Hz display mode would report a @refreshRate@ of 60,000.
--
-- == Valid Usage
--
-- -   The @width@ member of @visibleRegion@ /must/ be greater than @0@
--
-- -   The @height@ member of @visibleRegion@ /must/ be greater than @0@
--
-- -   @refreshRate@ /must/ be greater than @0@
--
-- = See Also
--
-- No cross-references are available
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR
  { -- | @visibleRegion@ is the 2D extents of the visible region.
  vkVisibleRegion :: VkExtent2D
  , -- | @refreshRate@ is a @uint32_t@ that is the number of times the display is
  -- refreshed each second multiplied by 1000.
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

instance Zero VkDisplayModeParametersKHR where
  zero = VkDisplayModeParametersKHR zero
                                    zero
-- | VkDisplayModePropertiesKHR - Structure describing display mode
-- properties
--
-- = See Also
--
-- No cross-references are available
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR
  { -- | @displayMode@ is a handle to the display mode described in this
  -- structure. This handle will be valid for the lifetime of the Vulkan
  -- instance.
  vkDisplayMode :: VkDisplayModeKHR
  , -- | @parameters@ is a 'VkDisplayModeParametersKHR' structure describing the
  -- display parameters associated with @displayMode@.
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

instance Zero VkDisplayModePropertiesKHR where
  zero = VkDisplayModePropertiesKHR zero
                                    zero
-- ** VkDisplayPlaneAlphaFlagBitsKHR

-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
--
-- No cross-references are available
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
-- | VkDisplayPlaneAlphaFlagsKHR - Bitmask of VkDisplayPlaneAlphaFlagBitsKHR
--
-- = Description
--
-- @VkDisplayPlaneAlphaFlagsKHR@ is a bitmask type for setting a mask of
-- zero or more 'VkDisplayPlaneAlphaFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR
-- | VkDisplayPlaneCapabilitiesKHR - Structure describing capabilities of a
-- mode and plane combination
--
-- = Description
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
--
-- No cross-references are available
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR
  { -- | @supportedAlpha@ is a bitmask of 'VkDisplayPlaneAlphaFlagBitsKHR'
  -- describing the supported alpha blending modes.
  vkSupportedAlpha :: VkDisplayPlaneAlphaFlagsKHR
  , -- | @minSrcPosition@ is the minimum source rectangle offset supported by
  -- this plane using the specified mode.
  vkMinSrcPosition :: VkOffset2D
  , -- | @maxSrcPosition@ is the maximum source rectangle offset supported by
  -- this plane using the specified mode. The @x@ and @y@ components of
  -- @maxSrcPosition@ /must/ each be greater than or equal to the @x@ and @y@
  -- components of @minSrcPosition@, respectively.
  vkMaxSrcPosition :: VkOffset2D
  , -- | @minSrcExtent@ is the minimum source rectangle size supported by this
  -- plane using the specified mode.
  vkMinSrcExtent :: VkExtent2D
  , -- | @maxSrcExtent@ is the maximum source rectangle size supported by this
  -- plane using the specified mode.
  vkMaxSrcExtent :: VkExtent2D
  , -- | @minDstPosition@, @maxDstPosition@, @minDstExtent@, @maxDstExtent@ all
  -- have similar semantics to their corresponding @*Src*@ equivalents, but
  -- apply to the output region within the mode rather than the input region
  -- within the source image. Unlike the @*Src*@ offsets, @minDstPosition@
  -- and @maxDstPosition@ /may/ contain negative values.
  vkMinDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstPosition"
  vkMaxDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstExtent"
  vkMinDstExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstExtent"
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

instance Zero VkDisplayPlaneCapabilitiesKHR where
  zero = VkDisplayPlaneCapabilitiesKHR zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
-- | VkDisplayPlanePropertiesKHR - Structure describing display plane
-- properties
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR
  { -- | @currentDisplay@ is the handle of the display the plane is currently
  -- associated with. If the plane is not currently attached to any displays,
  -- this will be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'.
  vkCurrentDisplay :: VkDisplayKHR
  , -- | @currentStackIndex@ is the current z-order of the plane. This will be
  -- between 0 and the value returned by
  -- @vkGetPhysicalDeviceDisplayPlanePropertiesKHR@ in @pPropertyCount@.
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

instance Zero VkDisplayPlanePropertiesKHR where
  zero = VkDisplayPlanePropertiesKHR zero
                                     zero
-- | VkDisplayPropertiesKHR - Structure describing an available display
-- device
--
-- = Description
--
-- __Note__
--
-- For devices which have no natural value to return here, implementations
-- /should/ return the maximum resolution supported.
--
-- __Note__
--
-- Persistent presents /may/ have higher latency, and /may/ use less power
-- when the screen content is updated infrequently, or when only a portion
-- of the screen needs to be updated in most frames.
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR
  { -- | @display@ is a handle that is used to refer to the display described
  -- here. This handle will be valid for the lifetime of the Vulkan instance.
  vkDisplay :: VkDisplayKHR
  , -- | @displayName@ is a pointer to a NULL-terminated string containing the
  -- name of the display. Generally, this will be the name provided by the
  -- display’s EDID. It /can/ be @NULL@ if no suitable name is available. If
  -- not @NULL@, the memory it points to /must/ remain accessible as long as
  -- @display@ is valid.
  vkDisplayName :: Ptr CChar
  , -- | @physicalDimensions@ describes the physical width and height of the
  -- visible portion of the display, in millimeters.
  vkPhysicalDimensions :: VkExtent2D
  , -- | @physicalResolution@ describes the physical, native, or preferred
  -- resolution of the display.
  vkPhysicalResolution :: VkExtent2D
  , -- | @supportedTransforms@ is a bitmask of
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
  -- describing which transforms are supported by this display.
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- | @planeReorderPossible@ tells whether the planes on this display /can/
  -- have their z order changed. If this is @VK_TRUE@, the application /can/
  -- re-arrange the planes on this display in any order relative to each
  -- other.
  vkPlaneReorderPossible :: VkBool32
  , -- | @persistentContent@ tells whether the display supports
  -- self-refresh\/internal buffering. If this is true, the application /can/
  -- submit persistent present operations on swapchains created against this
  -- display.
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

instance Zero VkDisplayPropertiesKHR where
  zero = VkDisplayPropertiesKHR zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
-- ** VkDisplaySurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplaySurfaceCreateFlagsKHR"
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkDisplaySurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created display plane surface object
--
-- = Description
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
--     value
--
-- -   @alphaMode@ /must/ be a valid 'VkDisplayPlaneAlphaFlagBitsKHR' value
--
-- = See Also
--
-- No cross-references are available
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use, and /must/ be zero.
  vkFlags :: VkDisplaySurfaceCreateFlagsKHR
  , -- | @displayMode@ is a 'VkDisplayModeKHR' handle specifying the mode to use
  -- when displaying this surface.
  vkDisplayMode :: VkDisplayModeKHR
  , -- | @planeIndex@ is the plane on which this surface appears.
  vkPlaneIndex :: Word32
  , -- | @planeStackIndex@ is the z-order of the plane.
  vkPlaneStackIndex :: Word32
  , -- | @transform@ is a
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
  -- value specifying the transformation to apply to images as part of the
  -- scanout operation.
  vkTransform :: VkSurfaceTransformFlagBitsKHR
  , -- | @globalAlpha@ is the global alpha value. This value is ignored if
  -- @alphaMode@ is not @VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR@.
  vkGlobalAlpha :: CFloat
  , -- | @alphaMode@ is a 'VkDisplayPlaneAlphaFlagBitsKHR' value specifying the
  -- type of alpha blending to use.
  vkAlphaMode :: VkDisplayPlaneAlphaFlagBitsKHR
  , -- | @imageExtent@ The size of the presentable images to use with the
  -- surface.
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

instance Zero VkDisplaySurfaceCreateInfoKHR where
  zero = VkDisplaySurfaceCreateInfoKHR zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateDisplayModeKHR - Create a display mode
--
-- = Parameters
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
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pMode@ returns the handle of the mode created.
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult

#endif
type FN_vkCreateDisplayModeKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
type PFN_vkCreateDisplayModeKHR = FunPtr FN_vkCreateDisplayModeKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateDisplayPlaneSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' structure
-- representing a display plane and mode
--
-- = Parameters
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
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface is returned.
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult

#endif
type FN_vkCreateDisplayPlaneSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateDisplayPlaneSurfaceKHR = FunPtr FN_vkCreateDisplayPlaneSurfaceKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetDisplayModePropertiesKHR - Query the set of mode properties
-- supported by the display
--
-- = Parameters
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult

#endif
type FN_vkGetDisplayModePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
type PFN_vkGetDisplayModePropertiesKHR = FunPtr FN_vkGetDisplayModePropertiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetDisplayPlaneCapabilitiesKHR - Query capabilities of a mode and
-- plane combination
--
-- = Parameters
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult

#endif
type FN_vkGetDisplayPlaneCapabilitiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilitiesKHR = FunPtr FN_vkGetDisplayPlaneCapabilitiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetDisplayPlaneSupportedDisplaysKHR - Query the list of displays a
-- plane supports
--
-- = Parameters
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult

#endif
type FN_vkGetDisplayPlaneSupportedDisplaysKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneSupportedDisplaysKHR = FunPtr FN_vkGetDisplayPlaneSupportedDisplaysKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetPhysicalDeviceDisplayPlanePropertiesKHR - Query the plane
-- properties
--
-- = Parameters
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetPhysicalDeviceDisplayPropertiesKHR - Query information about the
-- available displays
--
-- = Parameters
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceDisplayPropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPropertiesKHR
-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern VK_KHR_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"
-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern VK_KHR_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SPEC_VERSION = 21
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = VkStructureType 1000002000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = VkStructureType 1000002001
