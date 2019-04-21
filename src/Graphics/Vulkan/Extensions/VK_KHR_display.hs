{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  , DisplayModeCreateFlagsKHR
  , withCStructDisplayModeCreateInfoKHR
  , fromCStructDisplayModeCreateInfoKHR
  , DisplayModeCreateInfoKHR(..)
  , DisplayModeKHR
  , withCStructDisplayModeParametersKHR
  , fromCStructDisplayModeParametersKHR
  , DisplayModeParametersKHR(..)
  , withCStructDisplayModePropertiesKHR
  , fromCStructDisplayModePropertiesKHR
  , DisplayModePropertiesKHR(..)
  , DisplayPlaneAlphaFlagBitsKHR
  , pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , DisplayPlaneAlphaFlagsKHR
  , withCStructDisplayPlaneCapabilitiesKHR
  , fromCStructDisplayPlaneCapabilitiesKHR
  , DisplayPlaneCapabilitiesKHR(..)
  , withCStructDisplayPlanePropertiesKHR
  , fromCStructDisplayPlanePropertiesKHR
  , DisplayPlanePropertiesKHR(..)
  , withCStructDisplayPropertiesKHR
  , fromCStructDisplayPropertiesKHR
  , DisplayPropertiesKHR(..)
  , DisplaySurfaceCreateFlagsKHR
  , withCStructDisplaySurfaceCreateInfoKHR
  , fromCStructDisplaySurfaceCreateInfoKHR
  , DisplaySurfaceCreateInfoKHR(..)
  , createDisplayModeKHR
  , createDisplayPlaneSurfaceKHR
  , getNumDisplayModePropertiesKHR
  , getDisplayModePropertiesKHR
  , getAllDisplayModePropertiesKHR
  , getDisplayPlaneCapabilitiesKHR
  , getNumDisplayPlaneSupportedDisplaysKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , getAllDisplayPlaneSupportedDisplaysKHR
  , getNumPhysicalDeviceDisplayPlanePropertiesKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , getAllPhysicalDeviceDisplayPlanePropertiesKHR
  , getNumPhysicalDeviceDisplayPropertiesKHR
  , getPhysicalDeviceDisplayPropertiesKHR
  , getAllPhysicalDeviceDisplayPropertiesKHR
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.ByteString
  ( ByteString
  , packCString
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  , vkCreateDisplayModeKHR
  , vkCreateDisplayPlaneSurfaceKHR
  , vkGetDisplayModePropertiesKHR
  , vkGetDisplayPlaneCapabilitiesKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Offset2D(..)
  , fromCStructExtent2D
  , fromCStructOffset2D
  , withCStructExtent2D
  , withCStructOffset2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  )


-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- No cross-references are available
type DisplayKHR = VkDisplayKHR

-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR


-- | VkDisplayModeCreateInfoKHR - Structure specifying parameters of a newly
-- created display mode object
--
-- = Description
--
-- Unresolved directive in VkDisplayModeCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayModeCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayModeCreateInfoKHR = DisplayModeCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayModeCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "flags"
  flags :: DisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "parameters"
  parameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayModeCreateInfoKHR' and
-- marshal a 'DisplayModeCreateInfoKHR' into it. The 'VkDisplayModeCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayModeCreateInfoKHR :: DisplayModeCreateInfoKHR -> (VkDisplayModeCreateInfoKHR -> IO a) -> IO a
withCStructDisplayModeCreateInfoKHR marshalled cont = withCStructDisplayModeParametersKHR (parameters (marshalled :: DisplayModeCreateInfoKHR)) (\parameters'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayModeCreateInfoKHR)) (\pPNext -> cont (VkDisplayModeCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR pPNext (flags (marshalled :: DisplayModeCreateInfoKHR)) parameters'')))

-- | A function to read a 'VkDisplayModeCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'DisplayModeCreateInfoKHR'.
fromCStructDisplayModeCreateInfoKHR :: VkDisplayModeCreateInfoKHR -> IO DisplayModeCreateInfoKHR
fromCStructDisplayModeCreateInfoKHR c = DisplayModeCreateInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayModeCreateInfoKHR)))
                                                                 <*> pure (vkFlags (c :: VkDisplayModeCreateInfoKHR))
                                                                 <*> (fromCStructDisplayModeParametersKHR (vkParameters (c :: VkDisplayModeCreateInfoKHR)))

instance Zero DisplayModeCreateInfoKHR where
  zero = DisplayModeCreateInfoKHR Nothing
                                  zero
                                  zero


-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- No cross-references are available
type DisplayModeKHR = VkDisplayModeKHR


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
-- Unresolved directive in VkDisplayModeParametersKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayModeParametersKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayModeParametersKHR = DisplayModeParametersKHR
  { -- No documentation found for Nested "DisplayModeParametersKHR" "visibleRegion"
  visibleRegion :: Extent2D
  , -- No documentation found for Nested "DisplayModeParametersKHR" "refreshRate"
  refreshRate :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayModeParametersKHR' and
-- marshal a 'DisplayModeParametersKHR' into it. The 'VkDisplayModeParametersKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayModeParametersKHR :: DisplayModeParametersKHR -> (VkDisplayModeParametersKHR -> IO a) -> IO a
withCStructDisplayModeParametersKHR marshalled cont = withCStructExtent2D (visibleRegion (marshalled :: DisplayModeParametersKHR)) (\visibleRegion'' -> cont (VkDisplayModeParametersKHR visibleRegion'' (refreshRate (marshalled :: DisplayModeParametersKHR))))

-- | A function to read a 'VkDisplayModeParametersKHR' and all additional
-- structures in the pointer chain into a 'DisplayModeParametersKHR'.
fromCStructDisplayModeParametersKHR :: VkDisplayModeParametersKHR -> IO DisplayModeParametersKHR
fromCStructDisplayModeParametersKHR c = DisplayModeParametersKHR <$> (fromCStructExtent2D (vkVisibleRegion (c :: VkDisplayModeParametersKHR)))
                                                                 <*> pure (vkRefreshRate (c :: VkDisplayModeParametersKHR))

instance Zero DisplayModeParametersKHR where
  zero = DisplayModeParametersKHR zero
                                  zero



-- | VkDisplayModePropertiesKHR - Structure describing display mode
-- properties
--
-- = Description
--
-- Unresolved directive in VkDisplayModePropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayModePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayModePropertiesKHR = DisplayModePropertiesKHR
  { -- No documentation found for Nested "DisplayModePropertiesKHR" "displayMode"
  displayMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayModePropertiesKHR" "parameters"
  parameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayModePropertiesKHR' and
-- marshal a 'DisplayModePropertiesKHR' into it. The 'VkDisplayModePropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayModePropertiesKHR :: DisplayModePropertiesKHR -> (VkDisplayModePropertiesKHR -> IO a) -> IO a
withCStructDisplayModePropertiesKHR marshalled cont = withCStructDisplayModeParametersKHR (parameters (marshalled :: DisplayModePropertiesKHR)) (\parameters'' -> cont (VkDisplayModePropertiesKHR (displayMode (marshalled :: DisplayModePropertiesKHR)) parameters''))

-- | A function to read a 'VkDisplayModePropertiesKHR' and all additional
-- structures in the pointer chain into a 'DisplayModePropertiesKHR'.
fromCStructDisplayModePropertiesKHR :: VkDisplayModePropertiesKHR -> IO DisplayModePropertiesKHR
fromCStructDisplayModePropertiesKHR c = DisplayModePropertiesKHR <$> pure (vkDisplayMode (c :: VkDisplayModePropertiesKHR))
                                                                 <*> (fromCStructDisplayModeParametersKHR (vkParameters (c :: VkDisplayModePropertiesKHR)))

instance Zero DisplayModePropertiesKHR where
  zero = DisplayModePropertiesKHR zero
                                  zero


-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
--
-- No cross-references are available
type DisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR'
-- specifies that the source image will be treated as opaque.
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR'
-- specifies that a global alpha value /must/ be specified that will be
-- applied to all pixels in the source image.
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR'
-- specifies that the alpha value will be determined by the alpha channel
-- of the source image’s pixels. If the source format contains no alpha
-- values, no blending will be applied. The source alpha values are not
-- premultiplied into the source image’s other color channels.
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR'
-- is equivalent to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR',
-- except the source alpha values are assumed to be premultiplied into the
-- source image’s other color channels.
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR

-- | VkDisplayPlaneAlphaFlagsKHR - Bitmask of VkDisplayPlaneAlphaFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR


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
-- ranges returned in
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
-- are guaranteed to be supported. Vendors /may/ still fail presentation
-- requests that specify unsupported combinations.
--
-- Unresolved directive in VkDisplayPlaneCapabilitiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayPlaneCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayPlaneCapabilitiesKHR = DisplayPlaneCapabilitiesKHR
  { -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "supportedAlpha"
  supportedAlpha :: DisplayPlaneAlphaFlagsKHR
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minSrcPosition"
  minSrcPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxSrcPosition"
  maxSrcPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minSrcExtent"
  minSrcExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxSrcExtent"
  maxSrcExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minDstPosition"
  minDstPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxDstPosition"
  maxDstPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minDstExtent"
  minDstExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxDstExtent"
  maxDstExtent :: Extent2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPlaneCapabilitiesKHR' and
-- marshal a 'DisplayPlaneCapabilitiesKHR' into it. The 'VkDisplayPlaneCapabilitiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPlaneCapabilitiesKHR :: DisplayPlaneCapabilitiesKHR -> (VkDisplayPlaneCapabilitiesKHR -> IO a) -> IO a
withCStructDisplayPlaneCapabilitiesKHR marshalled cont = withCStructExtent2D (maxDstExtent (marshalled :: DisplayPlaneCapabilitiesKHR)) (\maxDstExtent'' -> withCStructExtent2D (minDstExtent (marshalled :: DisplayPlaneCapabilitiesKHR)) (\minDstExtent'' -> withCStructOffset2D (maxDstPosition (marshalled :: DisplayPlaneCapabilitiesKHR)) (\maxDstPosition'' -> withCStructOffset2D (minDstPosition (marshalled :: DisplayPlaneCapabilitiesKHR)) (\minDstPosition'' -> withCStructExtent2D (maxSrcExtent (marshalled :: DisplayPlaneCapabilitiesKHR)) (\maxSrcExtent'' -> withCStructExtent2D (minSrcExtent (marshalled :: DisplayPlaneCapabilitiesKHR)) (\minSrcExtent'' -> withCStructOffset2D (maxSrcPosition (marshalled :: DisplayPlaneCapabilitiesKHR)) (\maxSrcPosition'' -> withCStructOffset2D (minSrcPosition (marshalled :: DisplayPlaneCapabilitiesKHR)) (\minSrcPosition'' -> cont (VkDisplayPlaneCapabilitiesKHR (supportedAlpha (marshalled :: DisplayPlaneCapabilitiesKHR)) minSrcPosition'' maxSrcPosition'' minSrcExtent'' maxSrcExtent'' minDstPosition'' maxDstPosition'' minDstExtent'' maxDstExtent'')))))))))

-- | A function to read a 'VkDisplayPlaneCapabilitiesKHR' and all additional
-- structures in the pointer chain into a 'DisplayPlaneCapabilitiesKHR'.
fromCStructDisplayPlaneCapabilitiesKHR :: VkDisplayPlaneCapabilitiesKHR -> IO DisplayPlaneCapabilitiesKHR
fromCStructDisplayPlaneCapabilitiesKHR c = DisplayPlaneCapabilitiesKHR <$> pure (vkSupportedAlpha (c :: VkDisplayPlaneCapabilitiesKHR))
                                                                       <*> (fromCStructOffset2D (vkMinSrcPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMaxSrcPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMinSrcExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMaxSrcExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMinDstPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMaxDstPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMinDstExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMaxDstExtent (c :: VkDisplayPlaneCapabilitiesKHR)))

instance Zero DisplayPlaneCapabilitiesKHR where
  zero = DisplayPlaneCapabilitiesKHR zero
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
-- = Description
--
-- Unresolved directive in VkDisplayPlanePropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayPlanePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayPlanePropertiesKHR = DisplayPlanePropertiesKHR
  { -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentDisplay"
  currentDisplay :: DisplayKHR
  , -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentStackIndex"
  currentStackIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPlanePropertiesKHR' and
-- marshal a 'DisplayPlanePropertiesKHR' into it. The 'VkDisplayPlanePropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPlanePropertiesKHR :: DisplayPlanePropertiesKHR -> (VkDisplayPlanePropertiesKHR -> IO a) -> IO a
withCStructDisplayPlanePropertiesKHR marshalled cont = cont (VkDisplayPlanePropertiesKHR (currentDisplay (marshalled :: DisplayPlanePropertiesKHR)) (currentStackIndex (marshalled :: DisplayPlanePropertiesKHR)))

-- | A function to read a 'VkDisplayPlanePropertiesKHR' and all additional
-- structures in the pointer chain into a 'DisplayPlanePropertiesKHR'.
fromCStructDisplayPlanePropertiesKHR :: VkDisplayPlanePropertiesKHR -> IO DisplayPlanePropertiesKHR
fromCStructDisplayPlanePropertiesKHR c = DisplayPlanePropertiesKHR <$> pure (vkCurrentDisplay (c :: VkDisplayPlanePropertiesKHR))
                                                                   <*> pure (vkCurrentStackIndex (c :: VkDisplayPlanePropertiesKHR))

instance Zero DisplayPlanePropertiesKHR where
  zero = DisplayPlanePropertiesKHR zero
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
-- Unresolved directive in VkDisplayPropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplayPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplayPropertiesKHR = DisplayPropertiesKHR
  { -- No documentation found for Nested "DisplayPropertiesKHR" "display"
  display :: DisplayKHR
  , -- No documentation found for Nested "DisplayPropertiesKHR" "displayName"
  displayName :: ByteString
  , -- No documentation found for Nested "DisplayPropertiesKHR" "physicalDimensions"
  physicalDimensions :: Extent2D
  , -- No documentation found for Nested "DisplayPropertiesKHR" "physicalResolution"
  physicalResolution :: Extent2D
  , -- No documentation found for Nested "DisplayPropertiesKHR" "supportedTransforms"
  supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "DisplayPropertiesKHR" "planeReorderPossible"
  planeReorderPossible :: Bool
  , -- No documentation found for Nested "DisplayPropertiesKHR" "persistentContent"
  persistentContent :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPropertiesKHR' and
-- marshal a 'DisplayPropertiesKHR' into it. The 'VkDisplayPropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPropertiesKHR :: DisplayPropertiesKHR -> (VkDisplayPropertiesKHR -> IO a) -> IO a
withCStructDisplayPropertiesKHR marshalled cont = withCStructExtent2D (physicalResolution (marshalled :: DisplayPropertiesKHR)) (\physicalResolution'' -> withCStructExtent2D (physicalDimensions (marshalled :: DisplayPropertiesKHR)) (\physicalDimensions'' -> useAsCString (displayName (marshalled :: DisplayPropertiesKHR)) (\pDisplayName -> cont (VkDisplayPropertiesKHR (display (marshalled :: DisplayPropertiesKHR)) pDisplayName physicalDimensions'' physicalResolution'' (supportedTransforms (marshalled :: DisplayPropertiesKHR)) (boolToBool32 (planeReorderPossible (marshalled :: DisplayPropertiesKHR))) (boolToBool32 (persistentContent (marshalled :: DisplayPropertiesKHR)))))))

-- | A function to read a 'VkDisplayPropertiesKHR' and all additional
-- structures in the pointer chain into a 'DisplayPropertiesKHR'.
fromCStructDisplayPropertiesKHR :: VkDisplayPropertiesKHR -> IO DisplayPropertiesKHR
fromCStructDisplayPropertiesKHR c = DisplayPropertiesKHR <$> pure (vkDisplay (c :: VkDisplayPropertiesKHR))
                                                         <*> packCString (vkDisplayName (c :: VkDisplayPropertiesKHR))
                                                         <*> (fromCStructExtent2D (vkPhysicalDimensions (c :: VkDisplayPropertiesKHR)))
                                                         <*> (fromCStructExtent2D (vkPhysicalResolution (c :: VkDisplayPropertiesKHR)))
                                                         <*> pure (vkSupportedTransforms (c :: VkDisplayPropertiesKHR))
                                                         <*> pure (bool32ToBool (vkPlaneReorderPossible (c :: VkDisplayPropertiesKHR)))
                                                         <*> pure (bool32ToBool (vkPersistentContent (c :: VkDisplayPropertiesKHR)))

instance Zero DisplayPropertiesKHR where
  zero = DisplayPropertiesKHR zero
                              Data.ByteString.empty
                              zero
                              zero
                              zero
                              False
                              False


-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR


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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR'
--
-- -   If the @planeReorderPossible@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR'
--     for the display corresponding to @displayMode@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE' then @planeStackIndex@
--     /must/ be less than the number of display planes supported by the
--     device as determined by calling
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR';
--     otherwise @planeStackIndex@ /must/ equal the @currentStackIndex@
--     member of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR'
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR'
--     for the display plane corresponding to @displayMode@
--
-- -   If @alphaMode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR'
--     then @globalAlpha@ /must/ be between @0@ and @1@, inclusive
--
-- -   @alphaMode@ /must/ be @0@ or one of the bits present in the
--     @supportedAlpha@ member of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR'
--     for the display plane corresponding to @displayMode@
--
-- -   The @width@ and @height@ members of @imageExtent@ /must/ be less
--     than the @maxImageDimensions2D@ member of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'
--
-- Unresolved directive in VkDisplaySurfaceCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkDisplaySurfaceCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DisplaySurfaceCreateInfoKHR = DisplaySurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "flags"
  flags :: DisplaySurfaceCreateFlagsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "displayMode"
  displayMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "planeIndex"
  planeIndex :: Word32
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "planeStackIndex"
  planeStackIndex :: Word32
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "transform"
  transform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "globalAlpha"
  globalAlpha :: CFloat
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "alphaMode"
  alphaMode :: DisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "imageExtent"
  imageExtent :: Extent2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplaySurfaceCreateInfoKHR' and
-- marshal a 'DisplaySurfaceCreateInfoKHR' into it. The 'VkDisplaySurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplaySurfaceCreateInfoKHR :: DisplaySurfaceCreateInfoKHR -> (VkDisplaySurfaceCreateInfoKHR -> IO a) -> IO a
withCStructDisplaySurfaceCreateInfoKHR marshalled cont = withCStructExtent2D (imageExtent (marshalled :: DisplaySurfaceCreateInfoKHR)) (\imageExtent'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplaySurfaceCreateInfoKHR)) (\pPNext -> cont (VkDisplaySurfaceCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: DisplaySurfaceCreateInfoKHR)) (displayMode (marshalled :: DisplaySurfaceCreateInfoKHR)) (planeIndex (marshalled :: DisplaySurfaceCreateInfoKHR)) (planeStackIndex (marshalled :: DisplaySurfaceCreateInfoKHR)) (transform (marshalled :: DisplaySurfaceCreateInfoKHR)) (globalAlpha (marshalled :: DisplaySurfaceCreateInfoKHR)) (alphaMode (marshalled :: DisplaySurfaceCreateInfoKHR)) imageExtent'')))

-- | A function to read a 'VkDisplaySurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'DisplaySurfaceCreateInfoKHR'.
fromCStructDisplaySurfaceCreateInfoKHR :: VkDisplaySurfaceCreateInfoKHR -> IO DisplaySurfaceCreateInfoKHR
fromCStructDisplaySurfaceCreateInfoKHR c = DisplaySurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplaySurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkDisplayMode (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkPlaneIndex (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkPlaneStackIndex (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkTransform (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkGlobalAlpha (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkAlphaMode (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> (fromCStructExtent2D (vkImageExtent (c :: VkDisplaySurfaceCreateInfoKHR)))

instance Zero DisplaySurfaceCreateInfoKHR where
  zero = DisplaySurfaceCreateInfoKHR Nothing
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero



-- | vkCreateDisplayModeKHR - Create a display mode
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with @display@.
--
-- -   @display@ is the display to create an additional mode for.
--
-- -   @pCreateInfo@ is a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModeCreateInfoKHR'
--     structure describing the new mode to create.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     display mode object when there is no more specific allocator
--     available (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pMode@ returns the handle of the mode created.
--
-- = Description
--
-- Unresolved directive in vkCreateDisplayModeKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateDisplayModeKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createDisplayModeKHR :: PhysicalDevice ->  DisplayKHR ->  DisplayModeCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (DisplayModeKHR)
createDisplayModeKHR = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> \createInfo' -> \allocator -> alloca (\pMode' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDisplayModeCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDisplayModeKHR commandTable physicalDevice' display' pCreateInfo' pAllocator pMode' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pMode')))))


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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR'
--     structure specifying which mode, plane, and other parameters to use,
--     as described below.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateDisplayPlaneSurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateDisplayPlaneSurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createDisplayPlaneSurfaceKHR :: Instance ->  DisplaySurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createDisplayPlaneSurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDisplaySurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDisplayPlaneSurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))


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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR'
--     structures.
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
-- @display@ for @physicalDevice@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetDisplayModePropertiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDisplayModePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModePropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> alloca (\pPropertyCount' -> vkGetDisplayModePropertiesKHR commandTable physicalDevice' display' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR'
--     structures.
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
-- @display@ for @physicalDevice@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetDisplayModePropertiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDisplayModePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModePropertiesKHR)
getDisplayModePropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetDisplayModePropertiesKHR commandTable physicalDevice' display' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayModePropertiesKHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getDisplayModePropertiesKHR'.
getAllDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModePropertiesKHR)
getAllDisplayModePropertiesKHR physicalDevice' display' =
  snd <$> getNumDisplayModePropertiesKHR physicalDevice' display'
    >>= \num -> snd <$> getDisplayModePropertiesKHR physicalDevice' display' num



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
-- -   @pCapabilities@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- Unresolved directive in vkGetDisplayPlaneCapabilitiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDisplayPlaneCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getDisplayPlaneCapabilitiesKHR :: PhysicalDevice ->  DisplayModeKHR ->  Word32 ->  IO (DisplayPlaneCapabilitiesKHR)
getDisplayPlaneCapabilitiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \mode' -> \planeIndex' -> alloca (\pCapabilities' -> vkGetDisplayPlaneCapabilitiesKHR commandTable physicalDevice' mode' planeIndex' pCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructDisplayPlaneCapabilitiesKHR <=< peek) pCapabilities')))


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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handles.
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
-- @physicalDevice@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetDisplayPlaneSupportedDisplaysKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDisplayPlaneSupportedDisplaysKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Word32)
getNumDisplayPlaneSupportedDisplaysKHR = \(PhysicalDevice physicalDevice' commandTable) -> \planeIndex' -> alloca (\pDisplayCount' -> vkGetDisplayPlaneSupportedDisplaysKHR commandTable physicalDevice' planeIndex' pDisplayCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pDisplayCount')))

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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handles.
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
-- @physicalDevice@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetDisplayPlaneSupportedDisplaysKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDisplayPlaneSupportedDisplaysKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  Word32 ->  IO (VkResult, Vector DisplayKHR)
getDisplayPlaneSupportedDisplaysKHR = \(PhysicalDevice physicalDevice' commandTable) -> \planeIndex' -> \displayCount' -> allocaArray (fromIntegral displayCount') (\pDisplays' -> with displayCount' (\pDisplayCount' -> vkGetDisplayPlaneSupportedDisplaysKHR commandTable physicalDevice' planeIndex' pDisplayCount' pDisplays' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM (peekElemOff pDisplays') =<< (fromIntegral <$> (peek pDisplayCount')))))))
-- | Returns all the values available from 'getDisplayPlaneSupportedDisplaysKHR'.
getAllDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (Vector DisplayKHR)
getAllDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex' =
  snd <$> getNumDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex'
    >>= \num -> snd <$> getDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex' num



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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR'
--     structures.
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
-- Unresolved directive in vkGetPhysicalDeviceDisplayPlanePropertiesKHR.txt
-- -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlanePropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPlanePropertiesKHR commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR'
--     structures.
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
-- Unresolved directive in vkGetPhysicalDeviceDisplayPlanePropertiesKHR.txt
-- -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlanePropertiesKHR)
getPhysicalDeviceDisplayPlanePropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPlanePropertiesKHR commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPlanePropertiesKHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceDisplayPlanePropertiesKHR'.
getAllPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPlanePropertiesKHR)
getAllPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice' num



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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
--     structures.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetPhysicalDeviceDisplayPropertiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceDisplayPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPropertiesKHR commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
--     structures.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in vkGetPhysicalDeviceDisplayPropertiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceDisplayPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPropertiesKHR)
getPhysicalDeviceDisplayPropertiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPropertiesKHR commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPropertiesKHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceDisplayPropertiesKHR'.
getAllPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPropertiesKHR)
getAllPhysicalDeviceDisplayPropertiesKHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPropertiesKHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPropertiesKHR physicalDevice' num

