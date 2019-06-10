{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  , DisplayModeCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , DisplayModeCreateInfoKHR(..)
#endif
  , DisplayModeKHR
  , DisplayModeParametersKHR(..)
  , DisplayModePropertiesKHR(..)
  , DisplayPlaneAlphaFlagBitsKHR
  , pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , DisplayPlaneAlphaFlagsKHR
  , DisplayPlaneCapabilitiesKHR(..)
  , DisplayPlanePropertiesKHR(..)
  , DisplayPropertiesKHR(..)
  , DisplaySurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , DisplaySurfaceCreateInfoKHR(..)
#endif
  , createDisplayModeKHR
  , createDisplayPlaneSurfaceKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getNumDisplayModePropertiesKHR
  , getDisplayModePropertiesKHR
  , getAllDisplayModePropertiesKHR
  , getDisplayPlaneCapabilitiesKHR
#endif
  , getNumDisplayPlaneSupportedDisplaysKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , getAllDisplayPlaneSupportedDisplaysKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getNumPhysicalDeviceDisplayPlanePropertiesKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , getAllPhysicalDeviceDisplayPlanePropertiesKHR
  , getNumPhysicalDeviceDisplayPropertiesKHR
  , getPhysicalDeviceDisplayPropertiesKHR
  , getAllPhysicalDeviceDisplayPropertiesKHR
#endif
  , pattern KHR_DISPLAY_EXTENSION_NAME
  , pattern KHR_DISPLAY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , pattern OBJECT_TYPE_DISPLAY_KHR
  , pattern OBJECT_TYPE_DISPLAY_MODE_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.ByteString
  ( ByteString
  )
import Data.String
  ( IsString
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
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  , vkCreateDisplayModeKHR
  , vkCreateDisplayPlaneSurfaceKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( vkGetDisplayPlaneCapabilitiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( vkGetDisplayModePropertiesKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Offset2D(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  , SurfaceTransformFlagsKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceTransformFlagBitsKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_DISPLAY_KHR
  , pattern OBJECT_TYPE_DISPLAY_MODE_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "DisplayKHR"
type DisplayKHR = VkDisplayKHR

-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR


-- No complete pragma for DisplayModeCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayModeCreateInfoKHR"
data DisplayModeCreateInfoKHR = DisplayModeCreateInfoKHR
  { -- No documentation found for Nested "DisplayModeCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "flags"
  flags :: DisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "parameters"
  parameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)

instance Zero DisplayModeCreateInfoKHR where
  zero = DisplayModeCreateInfoKHR Nothing
                                  zero
                                  zero

#endif

-- No documentation found for TopLevel "DisplayModeKHR"
type DisplayModeKHR = VkDisplayModeKHR


-- No documentation found for TopLevel "VkDisplayModeParametersKHR"
data DisplayModeParametersKHR = DisplayModeParametersKHR
  { -- No documentation found for Nested "DisplayModeParametersKHR" "visibleRegion"
  visibleRegion :: Extent2D
  , -- No documentation found for Nested "DisplayModeParametersKHR" "refreshRate"
  refreshRate :: Word32
  }
  deriving (Show, Eq)

instance Zero DisplayModeParametersKHR where
  zero = DisplayModeParametersKHR zero
                                  zero



-- No documentation found for TopLevel "VkDisplayModePropertiesKHR"
data DisplayModePropertiesKHR = DisplayModePropertiesKHR
  { -- No documentation found for Nested "DisplayModePropertiesKHR" "displayMode"
  displayMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayModePropertiesKHR" "parameters"
  parameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)

instance Zero DisplayModePropertiesKHR where
  zero = DisplayModePropertiesKHR zero
                                  zero


-- No documentation found for TopLevel "DisplayPlaneAlphaFlagBitsKHR"
type DisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR


{-# complete DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR, DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR, DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR, DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR :: DisplayPlaneAlphaFlagBitsKHR #-}


-- No documentation found for Nested "DisplayPlaneAlphaFlagBitsKHR" "DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR


-- No documentation found for Nested "DisplayPlaneAlphaFlagBitsKHR" "DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR


-- No documentation found for Nested "DisplayPlaneAlphaFlagBitsKHR" "DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR


-- No documentation found for Nested "DisplayPlaneAlphaFlagBitsKHR" "DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR :: (a ~ DisplayPlaneAlphaFlagBitsKHR) => a
pattern DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR

-- No documentation found for TopLevel "DisplayPlaneAlphaFlagsKHR"
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR


-- No documentation found for TopLevel "VkDisplayPlaneCapabilitiesKHR"
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



-- No documentation found for TopLevel "VkDisplayPlanePropertiesKHR"
data DisplayPlanePropertiesKHR = DisplayPlanePropertiesKHR
  { -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentDisplay"
  currentDisplay :: DisplayKHR
  , -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentStackIndex"
  currentStackIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero DisplayPlanePropertiesKHR where
  zero = DisplayPlanePropertiesKHR zero
                                   zero



-- No documentation found for TopLevel "VkDisplayPropertiesKHR"
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

instance Zero DisplayPropertiesKHR where
  zero = DisplayPropertiesKHR zero
                              mempty
                              zero
                              zero
                              zero
                              False
                              False


-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR


-- No complete pragma for DisplaySurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplaySurfaceCreateInfoKHR"
data DisplaySurfaceCreateInfoKHR = DisplaySurfaceCreateInfoKHR
  { -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "pNext"
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
  globalAlpha :: Float
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "alphaMode"
  alphaMode :: DisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "imageExtent"
  imageExtent :: Extent2D
  }
  deriving (Show, Eq)

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

#endif


-- No documentation found for TopLevel "vkCreateDisplayModeKHR"
createDisplayModeKHR :: PhysicalDevice ->  DisplayKHR ->  DisplayModeCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (DisplayModeKHR)
createDisplayModeKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateDisplayPlaneSurfaceKHR"
createDisplayPlaneSurfaceKHR :: Instance ->  DisplaySurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createDisplayPlaneSurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDisplayModePropertiesKHR"
getNumDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModePropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetDisplayModePropertiesKHR"
getDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModePropertiesKHR)
getDisplayModePropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getDisplayModePropertiesKHR'.
getAllDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModePropertiesKHR)
getAllDisplayModePropertiesKHR physicalDevice' display' =
  snd <$> getNumDisplayModePropertiesKHR physicalDevice' display'
    >>= \num -> snd <$> getDisplayModePropertiesKHR physicalDevice' display' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilitiesKHR"
getDisplayPlaneCapabilitiesKHR :: PhysicalDevice ->  DisplayModeKHR ->  Word32 ->  IO (DisplayPlaneCapabilitiesKHR)
getDisplayPlaneCapabilitiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


-- No documentation found for TopLevel "vkGetDisplayPlaneSupportedDisplaysKHR"
getNumDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Word32)
getNumDisplayPlaneSupportedDisplaysKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetDisplayPlaneSupportedDisplaysKHR"
getDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  Word32 ->  IO (VkResult, Vector DisplayKHR)
getDisplayPlaneSupportedDisplaysKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getDisplayPlaneSupportedDisplaysKHR'.
getAllDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (Vector DisplayKHR)
getAllDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex' =
  snd <$> getNumDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex'
    >>= \num -> snd <$> getDisplayPlaneSupportedDisplaysKHR physicalDevice' planeIndex' num



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
getNumPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlanePropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
getPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlanePropertiesKHR)
getPhysicalDeviceDisplayPlanePropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceDisplayPlanePropertiesKHR'.
getAllPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPlanePropertiesKHR)
getAllPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPropertiesKHR"
getNumPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPropertiesKHR"
getPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPropertiesKHR)
getPhysicalDeviceDisplayPropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceDisplayPropertiesKHR'.
getAllPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPropertiesKHR)
getAllPhysicalDeviceDisplayPropertiesKHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPropertiesKHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPropertiesKHR physicalDevice' num

#endif

-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern KHR_DISPLAY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DISPLAY_EXTENSION_NAME = VK_KHR_DISPLAY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern KHR_DISPLAY_SPEC_VERSION :: Integral a => a
pattern KHR_DISPLAY_SPEC_VERSION = VK_KHR_DISPLAY_SPEC_VERSION
