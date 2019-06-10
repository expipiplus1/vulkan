{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DisplayModeProperties2KHR(..)
  , 
  DisplayPlaneCapabilities2KHR(..)
  , DisplayPlaneInfo2KHR(..)
  , DisplayPlaneProperties2KHR(..)
  , DisplayProperties2KHR(..)
  , getNumDisplayModeProperties2KHR
  , getDisplayModeProperties2KHR
  , getAllDisplayModeProperties2KHR
  , getDisplayPlaneCapabilities2KHR
  , getNumPhysicalDeviceDisplayPlaneProperties2KHR
  , getPhysicalDeviceDisplayPlaneProperties2KHR
  , getAllPhysicalDeviceDisplayPlaneProperties2KHR
  , getNumPhysicalDeviceDisplayProperties2KHR
  , getPhysicalDeviceDisplayProperties2KHR
  , getAllPhysicalDeviceDisplayProperties2KHR
#endif
  , pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( vkGetDisplayPlaneCapabilities2KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( vkGetDisplayModeProperties2KHR
  , vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , vkGetPhysicalDeviceDisplayProperties2KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayModePropertiesKHR(..)
  , DisplayPlaneCapabilitiesKHR(..)
  , DisplayPlanePropertiesKHR(..)
  , DisplayPropertiesKHR(..)
  , DisplayModeKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayModeProperties2KHR"
data DisplayModeProperties2KHR = DisplayModeProperties2KHR
  { -- No documentation found for Nested "DisplayModeProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeProperties2KHR" "displayModeProperties"
  displayModeProperties :: DisplayModePropertiesKHR
  }
  deriving (Show, Eq)

instance Zero DisplayModeProperties2KHR where
  zero = DisplayModeProperties2KHR Nothing
                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayPlaneCapabilities2KHR"
data DisplayPlaneCapabilities2KHR = DisplayPlaneCapabilities2KHR
  { -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "capabilities"
  capabilities :: DisplayPlaneCapabilitiesKHR
  }
  deriving (Show, Eq)

instance Zero DisplayPlaneCapabilities2KHR where
  zero = DisplayPlaneCapabilities2KHR Nothing
                                      zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayPlaneInfo2KHR"
data DisplayPlaneInfo2KHR = DisplayPlaneInfo2KHR
  { -- No documentation found for Nested "DisplayPlaneInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "mode"
  mode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "planeIndex"
  planeIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero DisplayPlaneInfo2KHR where
  zero = DisplayPlaneInfo2KHR Nothing
                              zero
                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayPlaneProperties2KHR"
data DisplayPlaneProperties2KHR = DisplayPlaneProperties2KHR
  { -- No documentation found for Nested "DisplayPlaneProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneProperties2KHR" "displayPlaneProperties"
  displayPlaneProperties :: DisplayPlanePropertiesKHR
  }
  deriving (Show, Eq)

instance Zero DisplayPlaneProperties2KHR where
  zero = DisplayPlaneProperties2KHR Nothing
                                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDisplayProperties2KHR"
data DisplayProperties2KHR = DisplayProperties2KHR
  { -- No documentation found for Nested "DisplayProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayProperties2KHR" "displayProperties"
  displayProperties :: DisplayPropertiesKHR
  }
  deriving (Show, Eq)

instance Zero DisplayProperties2KHR where
  zero = DisplayProperties2KHR Nothing
                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDisplayModeProperties2KHR"
getNumDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModeProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetDisplayModeProperties2KHR"
getDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModeProperties2KHR)
getDisplayModeProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getDisplayModeProperties2KHR'.
getAllDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModeProperties2KHR)
getAllDisplayModeProperties2KHR physicalDevice' display' =
  snd <$> getNumDisplayModeProperties2KHR physicalDevice' display'
    >>= \num -> snd <$> getDisplayModeProperties2KHR physicalDevice' display' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilities2KHR"
getDisplayPlaneCapabilities2KHR :: PhysicalDevice ->  DisplayPlaneInfo2KHR ->  IO (DisplayPlaneCapabilities2KHR)
getDisplayPlaneCapabilities2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"
getNumPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlaneProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"
getPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlaneProperties2KHR)
getPhysicalDeviceDisplayPlaneProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceDisplayPlaneProperties2KHR'.
getAllPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayPlaneProperties2KHR)
getAllPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayProperties2KHR"
getNumPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayProperties2KHR"
getPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayProperties2KHR)
getPhysicalDeviceDisplayProperties2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceDisplayProperties2KHR'.
getAllPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayProperties2KHR)
getAllPhysicalDeviceDisplayProperties2KHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayProperties2KHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayProperties2KHR physicalDevice' num

#endif

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
