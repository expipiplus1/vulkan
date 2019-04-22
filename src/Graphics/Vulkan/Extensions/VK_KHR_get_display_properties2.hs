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
  , pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , vkGetDisplayModeProperties2KHR
  , vkGetDisplayPlaneCapabilities2KHR
  , vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , vkGetPhysicalDeviceDisplayProperties2KHR
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  )



-- | VkDisplayModeProperties2KHR - Structure describing an available display
-- mode
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayModeProperties2KHR'
data DisplayModeProperties2KHR = DisplayModeProperties2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayModeProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeProperties2KHR" "displayModeProperties"
  displayModeProperties :: DisplayModePropertiesKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayModeProperties2KHR' and
-- marshal a 'DisplayModeProperties2KHR' into it. The 'VkDisplayModeProperties2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayModeProperties2KHR :: DisplayModeProperties2KHR -> (VkDisplayModeProperties2KHR -> IO a) -> IO a
withCStructDisplayModeProperties2KHR marshalled cont = withCStructDisplayModePropertiesKHR (displayModeProperties (marshalled :: DisplayModeProperties2KHR)) (\displayModeProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayModeProperties2KHR)) (\pPNext -> cont (VkDisplayModeProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR pPNext displayModeProperties'')))

-- | A function to read a 'VkDisplayModeProperties2KHR' and all additional
-- structures in the pointer chain into a 'DisplayModeProperties2KHR'.
fromCStructDisplayModeProperties2KHR :: VkDisplayModeProperties2KHR -> IO DisplayModeProperties2KHR
fromCStructDisplayModeProperties2KHR c = DisplayModeProperties2KHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayModeProperties2KHR)))
                                                                   <*> (fromCStructDisplayModePropertiesKHR (vkDisplayModeProperties (c :: VkDisplayModeProperties2KHR)))

instance Zero DisplayModeProperties2KHR where
  zero = DisplayModeProperties2KHR Nothing
                                   zero



-- | VkDisplayPlaneCapabilities2KHR - Structure describing the capabilities
-- of a mode and plane combination
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayPlaneCapabilities2KHR'
data DisplayPlaneCapabilities2KHR = DisplayPlaneCapabilities2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneCapabilities2KHR" "capabilities"
  capabilities :: DisplayPlaneCapabilitiesKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPlaneCapabilities2KHR' and
-- marshal a 'DisplayPlaneCapabilities2KHR' into it. The 'VkDisplayPlaneCapabilities2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPlaneCapabilities2KHR :: DisplayPlaneCapabilities2KHR -> (VkDisplayPlaneCapabilities2KHR -> IO a) -> IO a
withCStructDisplayPlaneCapabilities2KHR marshalled cont = withCStructDisplayPlaneCapabilitiesKHR (capabilities (marshalled :: DisplayPlaneCapabilities2KHR)) (\capabilities'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayPlaneCapabilities2KHR)) (\pPNext -> cont (VkDisplayPlaneCapabilities2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR pPNext capabilities'')))

-- | A function to read a 'VkDisplayPlaneCapabilities2KHR' and all additional
-- structures in the pointer chain into a 'DisplayPlaneCapabilities2KHR'.
fromCStructDisplayPlaneCapabilities2KHR :: VkDisplayPlaneCapabilities2KHR -> IO DisplayPlaneCapabilities2KHR
fromCStructDisplayPlaneCapabilities2KHR c = DisplayPlaneCapabilities2KHR <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneCapabilities2KHR)))
                                                                         <*> (fromCStructDisplayPlaneCapabilitiesKHR (vkCapabilities (c :: VkDisplayPlaneCapabilities2KHR)))

instance Zero DisplayPlaneCapabilities2KHR where
  zero = DisplayPlaneCapabilities2KHR Nothing
                                      zero



-- | VkDisplayPlaneInfo2KHR - Structure defining the intended configuration
-- of a display plane
--
-- = Description
--
-- __Note__
--
-- This parameter also implicitly specifies a display.
--
-- -   @planeIndex@ is the plane which the application intends to use with
--     the display.
--
-- The members of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneInfo2KHR'
-- correspond to the arguments to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @mode@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModeKHR'
--     handle
--
-- == Host Synchronization
--
-- -   Host access to @mode@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModeKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayPlaneCapabilities2KHR'
data DisplayPlaneInfo2KHR = DisplayPlaneInfo2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayPlaneInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "mode"
  mode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayPlaneInfo2KHR" "planeIndex"
  planeIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPlaneInfo2KHR' and
-- marshal a 'DisplayPlaneInfo2KHR' into it. The 'VkDisplayPlaneInfo2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPlaneInfo2KHR :: DisplayPlaneInfo2KHR -> (VkDisplayPlaneInfo2KHR -> IO a) -> IO a
withCStructDisplayPlaneInfo2KHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DisplayPlaneInfo2KHR)) (\pPNext -> cont (VkDisplayPlaneInfo2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR pPNext (mode (marshalled :: DisplayPlaneInfo2KHR)) (planeIndex (marshalled :: DisplayPlaneInfo2KHR))))

-- | A function to read a 'VkDisplayPlaneInfo2KHR' and all additional
-- structures in the pointer chain into a 'DisplayPlaneInfo2KHR'.
fromCStructDisplayPlaneInfo2KHR :: VkDisplayPlaneInfo2KHR -> IO DisplayPlaneInfo2KHR
fromCStructDisplayPlaneInfo2KHR c = DisplayPlaneInfo2KHR <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneInfo2KHR)))
                                                         <*> pure (vkMode (c :: VkDisplayPlaneInfo2KHR))
                                                         <*> pure (vkPlaneIndex (c :: VkDisplayPlaneInfo2KHR))

instance Zero DisplayPlaneInfo2KHR where
  zero = DisplayPlaneInfo2KHR Nothing
                              zero
                              zero



-- | VkDisplayPlaneProperties2KHR - Structure describing an available display
-- plane
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
data DisplayPlaneProperties2KHR = DisplayPlaneProperties2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayPlaneProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPlaneProperties2KHR" "displayPlaneProperties"
  displayPlaneProperties :: DisplayPlanePropertiesKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPlaneProperties2KHR' and
-- marshal a 'DisplayPlaneProperties2KHR' into it. The 'VkDisplayPlaneProperties2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPlaneProperties2KHR :: DisplayPlaneProperties2KHR -> (VkDisplayPlaneProperties2KHR -> IO a) -> IO a
withCStructDisplayPlaneProperties2KHR marshalled cont = withCStructDisplayPlanePropertiesKHR (displayPlaneProperties (marshalled :: DisplayPlaneProperties2KHR)) (\displayPlaneProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayPlaneProperties2KHR)) (\pPNext -> cont (VkDisplayPlaneProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR pPNext displayPlaneProperties'')))

-- | A function to read a 'VkDisplayPlaneProperties2KHR' and all additional
-- structures in the pointer chain into a 'DisplayPlaneProperties2KHR'.
fromCStructDisplayPlaneProperties2KHR :: VkDisplayPlaneProperties2KHR -> IO DisplayPlaneProperties2KHR
fromCStructDisplayPlaneProperties2KHR c = DisplayPlaneProperties2KHR <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPlaneProperties2KHR)))
                                                                     <*> (fromCStructDisplayPlanePropertiesKHR (vkDisplayPlaneProperties (c :: VkDisplayPlaneProperties2KHR)))

instance Zero DisplayPlaneProperties2KHR where
  zero = DisplayPlaneProperties2KHR Nothing
                                    zero



-- | VkDisplayProperties2KHR - Structure describing an available display
-- device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayProperties2KHR'
data DisplayProperties2KHR = DisplayProperties2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayProperties2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayProperties2KHR" "displayProperties"
  displayProperties :: DisplayPropertiesKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayProperties2KHR' and
-- marshal a 'DisplayProperties2KHR' into it. The 'VkDisplayProperties2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayProperties2KHR :: DisplayProperties2KHR -> (VkDisplayProperties2KHR -> IO a) -> IO a
withCStructDisplayProperties2KHR marshalled cont = withCStructDisplayPropertiesKHR (displayProperties (marshalled :: DisplayProperties2KHR)) (\displayProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayProperties2KHR)) (\pPNext -> cont (VkDisplayProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR pPNext displayProperties'')))

-- | A function to read a 'VkDisplayProperties2KHR' and all additional
-- structures in the pointer chain into a 'DisplayProperties2KHR'.
fromCStructDisplayProperties2KHR :: VkDisplayProperties2KHR -> IO DisplayProperties2KHR
fromCStructDisplayProperties2KHR c = DisplayProperties2KHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayProperties2KHR)))
                                                           <*> (fromCStructDisplayPropertiesKHR (vkDisplayProperties (c :: VkDisplayProperties2KHR)))

instance Zero DisplayProperties2KHR where
  zero = DisplayProperties2KHR Nothing
                               zero



-- | vkGetDisplayModeProperties2KHR - Query information about the available
-- display modes.
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayModeProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @display@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModeProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> alloca (\pPropertyCount' -> vkGetDisplayModeProperties2KHR commandTable physicalDevice' display' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkGetDisplayModeProperties2KHR - Query information about the available
-- display modes.
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayModeProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @display@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModeProperties2KHR)
getDisplayModeProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetDisplayModeProperties2KHR commandTable physicalDevice' display' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayModeProperties2KHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getDisplayModeProperties2KHR'.
getAllDisplayModeProperties2KHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModeProperties2KHR)
getAllDisplayModeProperties2KHR physicalDevice' display' =
  snd <$> getNumDisplayModeProperties2KHR physicalDevice' display'
    >>= \num -> snd <$> getDisplayModeProperties2KHR physicalDevice' display' num



-- | vkGetDisplayPlaneCapabilities2KHR - Query capabilities of a mode and
-- plane combination
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with
--     @pDisplayPlaneInfo@.
--
-- -   @pDisplayPlaneInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneInfo2KHR'
--     structure describing the plane and mode.
--
-- -   @pCapabilities@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneCapabilities2KHR'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayPlaneCapabilities2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with the ability to specify extended inputs via chained input
-- structures, and to return extended information via chained output
-- structures.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneCapabilities2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneInfo2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getDisplayPlaneCapabilities2KHR :: PhysicalDevice ->  DisplayPlaneInfo2KHR ->  IO (DisplayPlaneCapabilities2KHR)
getDisplayPlaneCapabilities2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \displayPlaneInfo' -> alloca (\pCapabilities' -> (\marshalled -> withCStructDisplayPlaneInfo2KHR marshalled . flip with) displayPlaneInfo' (\pDisplayPlaneInfo' -> vkGetDisplayPlaneCapabilities2KHR commandTable physicalDevice' pDisplayPlaneInfo' pCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructDisplayPlaneCapabilities2KHR <=< peek) pCapabilities'))))


-- | vkGetPhysicalDeviceDisplayPlaneProperties2KHR - Query information about
-- the available display planes.
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display planes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlaneProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPlaneProperties2KHR commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkGetPhysicalDeviceDisplayPlaneProperties2KHR - Query information about
-- the available display planes.
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display planes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayPlaneProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlaneProperties2KHR)
getPhysicalDeviceDisplayPlaneProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceDisplayPlaneProperties2KHR commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPlaneProperties2KHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceDisplayPlaneProperties2KHR'.
getAllPhysicalDeviceDisplayPlaneProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayPlaneProperties2KHR)
getAllPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice' num



-- | vkGetPhysicalDeviceDisplayProperties2KHR - Query information about the
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceDisplayProperties2KHR commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkGetPhysicalDeviceDisplayProperties2KHR - Query information about the
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayProperties2KHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayProperties2KHR)
getPhysicalDeviceDisplayProperties2KHR = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceDisplayProperties2KHR commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayProperties2KHR <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceDisplayProperties2KHR'.
getAllPhysicalDeviceDisplayProperties2KHR :: PhysicalDevice ->  IO (Vector DisplayProperties2KHR)
getAllPhysicalDeviceDisplayProperties2KHR physicalDevice' =
  snd <$> getNumPhysicalDeviceDisplayProperties2KHR physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceDisplayProperties2KHR physicalDevice' num


-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
