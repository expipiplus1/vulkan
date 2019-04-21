{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.ExtensionDiscovery
  ( withCStructExtensionProperties
  , fromCStructExtensionProperties
  , ExtensionProperties(..)
  , getNumDeviceExtensionProperties
  , enumerateDeviceExtensionProperties
  , enumerateAllDeviceExtensionProperties
  , getNumInstanceExtensionProperties
  , enumerateInstanceExtensionProperties
  , enumerateAllInstanceExtensionProperties
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
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  , vkEnumerateDeviceExtensionProperties
  , vkEnumerateInstanceExtensionProperties
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToNullTerminatedSizedVector
  )



-- | VkExtensionProperties - Structure specifying an extension properties
--
-- = Description
--
-- Unresolved directive in VkExtensionProperties.txt -
-- include::{generated}\/validity\/structs\/VkExtensionProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties'
data ExtensionProperties = ExtensionProperties
  { -- No documentation found for Nested "ExtensionProperties" "extensionName"
  extensionName :: ByteString
  , -- No documentation found for Nested "ExtensionProperties" "specVersion"
  specVersion :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExtensionProperties' and
-- marshal a 'ExtensionProperties' into it. The 'VkExtensionProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExtensionProperties :: ExtensionProperties -> (VkExtensionProperties -> IO a) -> IO a
withCStructExtensionProperties marshalled cont = cont (VkExtensionProperties (byteStringToNullTerminatedSizedVector (extensionName (marshalled :: ExtensionProperties))) (specVersion (marshalled :: ExtensionProperties)))

-- | A function to read a 'VkExtensionProperties' and all additional
-- structures in the pointer chain into a 'ExtensionProperties'.
fromCStructExtensionProperties :: VkExtensionProperties -> IO ExtensionProperties
fromCStructExtensionProperties c = ExtensionProperties <$> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkExtensionName (c :: VkExtensionProperties))) packCString
                                                       <*> pure (vkSpecVersion (c :: VkExtensionProperties))

instance Zero ExtensionProperties where
  zero = ExtensionProperties Data.ByteString.empty
                             zero



-- | vkEnumerateDeviceExtensionProperties - Returns properties of available
-- physical device extensions
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be queried.
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, and is treated in the
--     same fashion as the
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties'::@pPropertyCount@
--     parameter.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
--     structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the device extensions provided by
-- that layer are returned.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- Unresolved directive in vkEnumerateDeviceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateDeviceExtensionProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (VkResult, Word32)
getNumDeviceExtensionProperties = \(PhysicalDevice physicalDevice' commandTable) -> \layerName' -> alloca (\pPropertyCount' -> maybeWith useAsCString layerName' (\pLayerName' -> vkEnumerateDeviceExtensionProperties commandTable physicalDevice' pLayerName' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount'))))

-- | vkEnumerateDeviceExtensionProperties - Returns properties of available
-- physical device extensions
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be queried.
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, and is treated in the
--     same fashion as the
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties'::@pPropertyCount@
--     parameter.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
--     structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the device extensions provided by
-- that layer are returned.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- Unresolved directive in vkEnumerateDeviceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateDeviceExtensionProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
enumerateDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  Word32 ->  IO (VkResult, Vector ExtensionProperties)
enumerateDeviceExtensionProperties = \(PhysicalDevice physicalDevice' commandTable) -> \layerName' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> maybeWith useAsCString layerName' (\pLayerName' -> vkEnumerateDeviceExtensionProperties commandTable physicalDevice' pLayerName' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructExtensionProperties <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount'))))))))
-- | Returns all the values available from 'enumerateDeviceExtensionProperties'.
enumerateAllDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllDeviceExtensionProperties physicalDevice' pLayerName' =
  snd <$> getNumDeviceExtensionProperties physicalDevice' pLayerName'
    >>= \num -> snd <$> enumerateDeviceExtensionProperties physicalDevice' pLayerName' num



-- | vkEnumerateInstanceExtensionProperties - Returns up to requested number
-- of global extension properties
--
-- = Parameters
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
--     structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the instance extensions provided by
-- that layer are returned.
--
-- If @pProperties@ is @NULL@, then the number of extensions properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of extension properties
-- available, at most @pPropertyCount@ structures will be written. If
-- @pPropertyCount@ is smaller than the number of extensions available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available properties were returned.
--
-- Because the list of available layers may change externally between calls
-- to
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties',
-- two calls may retrieve different results if a @pLayerName@ is available
-- in one call but not in another. The extensions supported by a layer may
-- also change between two calls, e.g. if the layer implementation is
-- replaced by a different version between those calls.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- Unresolved directive in vkEnumerateInstanceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateInstanceExtensionProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
getNumInstanceExtensionProperties :: Maybe ByteString ->  IO (VkResult, Word32)
getNumInstanceExtensionProperties = \layerName' -> alloca (\pPropertyCount' -> maybeWith useAsCString layerName' (\pLayerName' -> vkEnumerateInstanceExtensionProperties pLayerName' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount'))))

-- | vkEnumerateInstanceExtensionProperties - Returns up to requested number
-- of global extension properties
--
-- = Parameters
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
--     structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the instance extensions provided by
-- that layer are returned.
--
-- If @pProperties@ is @NULL@, then the number of extensions properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of extension properties
-- available, at most @pPropertyCount@ structures will be written. If
-- @pPropertyCount@ is smaller than the number of extensions available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available properties were returned.
--
-- Because the list of available layers may change externally between calls
-- to
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties',
-- two calls may retrieve different results if a @pLayerName@ is available
-- in one call but not in another. The extensions supported by a layer may
-- also change between two calls, e.g. if the layer implementation is
-- replaced by a different version between those calls.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- Unresolved directive in vkEnumerateInstanceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateInstanceExtensionProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.VkExtensionProperties'
enumerateInstanceExtensionProperties :: Maybe ByteString ->  Word32 ->  IO (VkResult, Vector ExtensionProperties)
enumerateInstanceExtensionProperties = \layerName' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> maybeWith useAsCString layerName' (\pLayerName' -> vkEnumerateInstanceExtensionProperties pLayerName' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructExtensionProperties <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount'))))))))
-- | Returns all the values available from 'enumerateInstanceExtensionProperties'.
enumerateAllInstanceExtensionProperties :: Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllInstanceExtensionProperties pLayerName' =
  snd <$> getNumInstanceExtensionProperties pLayerName'
    >>= \num -> snd <$> enumerateInstanceExtensionProperties pLayerName' num

