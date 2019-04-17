{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( withCStructDisplayModeProperties2KHR
  , fromCStructDisplayModeProperties2KHR
  , DisplayModeProperties2KHR(..)
  , withCStructDisplayPlaneCapabilities2KHR
  , fromCStructDisplayPlaneCapabilities2KHR
  , DisplayPlaneCapabilities2KHR(..)
  , withCStructDisplayPlaneInfo2KHR
  , fromCStructDisplayPlaneInfo2KHR
  , DisplayPlaneInfo2KHR(..)
  , withCStructDisplayPlaneProperties2KHR
  , fromCStructDisplayPlaneProperties2KHR
  , DisplayPlaneProperties2KHR(..)
  , withCStructDisplayProperties2KHR
  , fromCStructDisplayProperties2KHR
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
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getDisplayModeProperties2KHR
  , getDisplayPlaneCapabilities2KHR
  , getPhysicalDeviceDisplayPlaneProperties2KHR
  , getPhysicalDeviceDisplayProperties2KHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayModePropertiesKHR(..)
  , DisplayPlaneCapabilitiesKHR(..)
  , DisplayPlanePropertiesKHR(..)
  , DisplayPropertiesKHR(..)
  , DisplayKHR
  , DisplayModeKHR
  , fromCStructDisplayModePropertiesKHR
  , fromCStructDisplayPlaneCapabilitiesKHR
  , fromCStructDisplayPlanePropertiesKHR
  , fromCStructDisplayPropertiesKHR
  , withCStructDisplayModePropertiesKHR
  , withCStructDisplayPlaneCapabilitiesKHR
  , withCStructDisplayPlanePropertiesKHR
  , withCStructDisplayPropertiesKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  )


-- No documentation found for TopLevel "DisplayModeProperties2KHR"
data DisplayModeProperties2KHR = DisplayModeProperties2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayModeProperties2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeProperties2KHR" "displayModeProperties"
  vkDisplayModeProperties :: DisplayModePropertiesKHR
  }
  deriving (Show, Eq)
withCStructDisplayModeProperties2KHR :: DisplayModeProperties2KHR -> (VkDisplayModeProperties2KHR -> IO a) -> IO a
withCStructDisplayModeProperties2KHR from cont = withCStructDisplayModePropertiesKHR (vkDisplayModeProperties (from :: DisplayModeProperties2KHR)) (\displayModeProperties -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayModeProperties2KHR)) (\pPNext -> cont (VkDisplayModeProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR pPNext displayModeProperties)))
fromCStructDisplayModeProperties2KHR :: VkDisplayModeProperties2KHR -> IO DisplayModeProperties2KHR
fromCStructDisplayModeProperties2KHR c = DisplayModeProperties2KHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayModeProperties2KHR)))
                                                                   <*> (fromCStructDisplayModePropertiesKHR (vkDisplayModeProperties (c :: VkDisplayModeProperties2KHR)))
instance Zero DisplayModeProperties2KHR where
  zero = DisplayModeProperties2KHR Nothing
                                   zero
-- No documentation found for TopLevel "DisplayPlaneCapabilities2KHR"
data DisplayPlaneCapabilities2KHR = DisplayPlaneCapabilities2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "capabilities"
  vkCapabilities :: DisplayPlaneCapabilitiesKHR
  }
  deriving (Show, Eq)
withCStructDisplayPlaneCapabilities2KHR :: DisplayPlaneCapabilities2KHR -> (VkDisplayPlaneCapabilities2KHR -> IO a) -> IO a
withCStructDisplayPlaneCapabilities2KHR from cont = withCStructDisplayPlaneCapabilitiesKHR (vkCapabilities (from :: DisplayPlaneCapabilities2KHR)) (\capabilities -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayPlaneCapabilities2KHR)) (\pPNext -> cont (VkDisplayPlaneCapabilities2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR pPNext capabilities)))
fromCStructDisplayPlaneCapabilities2KHR :: VkDisplayPlaneCapabilities2KHR -> IO DisplayPlaneCapabilities2KHR
fromCStructDisplayPlaneCapabilities2KHR c = DisplayPlaneCapabilities2KHR <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneCapabilities2KHR)))
                                                                         <*> (fromCStructDisplayPlaneCapabilitiesKHR (vkCapabilities (c :: VkDisplayPlaneCapabilities2KHR)))
instance Zero DisplayPlaneCapabilities2KHR where
  zero = DisplayPlaneCapabilities2KHR Nothing
                                      zero
-- No documentation found for TopLevel "DisplayPlaneInfo2KHR"
data DisplayPlaneInfo2KHR = DisplayPlaneInfo2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayPlaneInfo2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "mode"
  vkMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "planeIndex"
  vkPlaneIndex :: Word32
  }
  deriving (Show, Eq)
withCStructDisplayPlaneInfo2KHR :: DisplayPlaneInfo2KHR -> (VkDisplayPlaneInfo2KHR -> IO a) -> IO a
withCStructDisplayPlaneInfo2KHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: DisplayPlaneInfo2KHR)) (\pPNext -> cont (VkDisplayPlaneInfo2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR pPNext (vkMode (from :: DisplayPlaneInfo2KHR)) (vkPlaneIndex (from :: DisplayPlaneInfo2KHR))))
fromCStructDisplayPlaneInfo2KHR :: VkDisplayPlaneInfo2KHR -> IO DisplayPlaneInfo2KHR
fromCStructDisplayPlaneInfo2KHR c = DisplayPlaneInfo2KHR <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneInfo2KHR)))
                                                         <*> pure (vkMode (c :: VkDisplayPlaneInfo2KHR))
                                                         <*> pure (vkPlaneIndex (c :: VkDisplayPlaneInfo2KHR))
instance Zero DisplayPlaneInfo2KHR where
  zero = DisplayPlaneInfo2KHR Nothing
                              zero
                              zero
-- No documentation found for TopLevel "DisplayPlaneProperties2KHR"
data DisplayPlaneProperties2KHR = DisplayPlaneProperties2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayPlaneProperties2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneProperties2KHR" "displayPlaneProperties"
  vkDisplayPlaneProperties :: DisplayPlanePropertiesKHR
  }
  deriving (Show, Eq)
withCStructDisplayPlaneProperties2KHR :: DisplayPlaneProperties2KHR -> (VkDisplayPlaneProperties2KHR -> IO a) -> IO a
withCStructDisplayPlaneProperties2KHR from cont = withCStructDisplayPlanePropertiesKHR (vkDisplayPlaneProperties (from :: DisplayPlaneProperties2KHR)) (\displayPlaneProperties -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayPlaneProperties2KHR)) (\pPNext -> cont (VkDisplayPlaneProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR pPNext displayPlaneProperties)))
fromCStructDisplayPlaneProperties2KHR :: VkDisplayPlaneProperties2KHR -> IO DisplayPlaneProperties2KHR
fromCStructDisplayPlaneProperties2KHR c = DisplayPlaneProperties2KHR <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneProperties2KHR)))
                                                                     <*> (fromCStructDisplayPlanePropertiesKHR (vkDisplayPlaneProperties (c :: VkDisplayPlaneProperties2KHR)))
instance Zero DisplayPlaneProperties2KHR where
  zero = DisplayPlaneProperties2KHR Nothing
                                    zero
-- No documentation found for TopLevel "DisplayProperties2KHR"
data DisplayProperties2KHR = DisplayProperties2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayProperties2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayProperties2KHR" "displayProperties"
  vkDisplayProperties :: DisplayPropertiesKHR
  }
  deriving (Show, Eq)
withCStructDisplayProperties2KHR :: DisplayProperties2KHR -> (VkDisplayProperties2KHR -> IO a) -> IO a
withCStructDisplayProperties2KHR from cont = withCStructDisplayPropertiesKHR (vkDisplayProperties (from :: DisplayProperties2KHR)) (\displayProperties -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayProperties2KHR)) (\pPNext -> cont (VkDisplayProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR pPNext displayProperties)))
fromCStructDisplayProperties2KHR :: VkDisplayProperties2KHR -> IO DisplayProperties2KHR
fromCStructDisplayProperties2KHR c = DisplayProperties2KHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayProperties2KHR)))
                                                           <*> (fromCStructDisplayPropertiesKHR (vkDisplayProperties (c :: VkDisplayProperties2KHR)))
instance Zero DisplayProperties2KHR where
  zero = DisplayProperties2KHR Nothing
                               zero

-- | Wrapper for 'vkGetDisplayModeProperties2KHR'
getNumDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModeProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> \display -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getDisplayModeProperties2KHR commandTable physicalDevice display pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetDisplayModeProperties2KHR'
getDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModeProperties2KHR)
getDisplayModeProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> \display -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getDisplayModeProperties2KHR commandTable physicalDevice display pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayModeProperties2KHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumDisplayModeProperties2KHR' to get the number of return values, then use that
-- number to call 'getDisplayModeProperties2KHR' to get all the values.
getAllDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModeProperties2KHR)
getAllDisplayModeProperties2KHR physicalDevice display =
  snd <$> getNumDisplayModeProperties2KHR physicalDevice display
    >>= \num -> snd <$> getDisplayModeProperties2KHR physicalDevice display num


-- | Wrapper for 'vkGetDisplayPlaneCapabilities2KHR'
getDisplayPlaneCapabilities2KHR :: PhysicalDevice ->  DisplayPlaneInfo2KHR ->  IO (DisplayPlaneCapabilities2KHR)
getDisplayPlaneCapabilities2KHR = \(PhysicalDevice physicalDevice commandTable) -> \displayPlaneInfo -> alloca (\pCapabilities -> (\a -> withCStructDisplayPlaneInfo2KHR a . flip with) displayPlaneInfo (\pDisplayPlaneInfo -> Graphics.Vulkan.C.Dynamic.getDisplayPlaneCapabilities2KHR commandTable physicalDevice pDisplayPlaneInfo pCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructDisplayPlaneCapabilities2KHR <=< peek) pCapabilities))))

-- | Wrapper for 'vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
getNumPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlaneProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPlaneProperties2KHR commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
getPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlaneProperties2KHR)
getPhysicalDeviceDisplayPlaneProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPlaneProperties2KHR commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPlaneProperties2KHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceDisplayPlaneProperties2KHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceDisplayPlaneProperties2KHR' to get all the values.
getAllPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayPlaneProperties2KHR)
getAllPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice =
  snd <$> getNumPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice num


-- | Wrapper for 'vkGetPhysicalDeviceDisplayProperties2KHR'
getNumPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayProperties2KHR commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceDisplayProperties2KHR'
getPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayProperties2KHR)
getPhysicalDeviceDisplayProperties2KHR = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayProperties2KHR commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayProperties2KHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceDisplayProperties2KHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceDisplayProperties2KHR' to get all the values.
getAllPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayProperties2KHR)
getAllPhysicalDeviceDisplayProperties2KHR physicalDevice =
  snd <$> getNumPhysicalDeviceDisplayProperties2KHR physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceDisplayProperties2KHR physicalDevice num

