{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.LayerDiscovery
  ( withCStructLayerProperties
  , fromCStructLayerProperties
  , LayerProperties(..)
  , getNumDeviceLayerProperties
  , enumerateDeviceLayerProperties
  , enumerateAllDeviceLayerProperties
  , getNumInstanceLayerProperties
  , enumerateInstanceLayerProperties
  , enumerateAllInstanceLayerProperties
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
  ( with
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
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  , vkEnumerateDeviceLayerProperties
  , vkEnumerateInstanceLayerProperties
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



-- | VkLayerProperties - Structure specifying layer properties
--
-- = Description
--
-- Unresolved directive in VkLayerProperties.txt -
-- include::{generated}\/validity\/structs\/VkLayerProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties',
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'
data LayerProperties = LayerProperties
  { -- No documentation found for Nested "LayerProperties" "layerName"
  layerName :: ByteString
  , -- No documentation found for Nested "LayerProperties" "specVersion"
  specVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "implementationVersion"
  implementationVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "description"
  description :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkLayerProperties' and
-- marshal a 'LayerProperties' into it. The 'VkLayerProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructLayerProperties :: LayerProperties -> (VkLayerProperties -> IO a) -> IO a
withCStructLayerProperties marshalled cont = cont (VkLayerProperties (byteStringToNullTerminatedSizedVector (layerName (marshalled :: LayerProperties))) (specVersion (marshalled :: LayerProperties)) (implementationVersion (marshalled :: LayerProperties)) (byteStringToNullTerminatedSizedVector (description (marshalled :: LayerProperties))))

-- | A function to read a 'VkLayerProperties' and all additional
-- structures in the pointer chain into a 'LayerProperties'.
fromCStructLayerProperties :: VkLayerProperties -> IO LayerProperties
fromCStructLayerProperties c = LayerProperties <$> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkLayerName (c :: VkLayerProperties))) packCString
                                               <*> pure (vkSpecVersion (c :: VkLayerProperties))
                                               <*> pure (vkImplementationVersion (c :: VkLayerProperties))
                                               <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDescription (c :: VkLayerProperties))) packCString

instance Zero LayerProperties where
  zero = LayerProperties Data.ByteString.empty
                         zero
                         zero
                         Data.ByteString.empty



-- | vkEnumerateDeviceLayerProperties - Returns properties of available
-- physical device layers
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of layers enumerated by
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties'
-- /must/ be exactly the sequence of layers enabled for the instance. The
-- members of 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
-- for each enumerated layer /must/ be the same as the properties when the
-- layer was enumerated by
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'.
--
-- Unresolved directive in vkEnumerateDeviceLayerProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateDeviceLayerProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumDeviceLayerProperties :: PhysicalDevice ->  IO (VkResult, Word32)
getNumDeviceLayerProperties = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkEnumerateDeviceLayerProperties commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkEnumerateDeviceLayerProperties - Returns properties of available
-- physical device layers
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of layers enumerated by
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties'
-- /must/ be exactly the sequence of layers enabled for the instance. The
-- members of 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
-- for each enumerated layer /must/ be the same as the properties when the
-- layer was enumerated by
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'.
--
-- Unresolved directive in vkEnumerateDeviceLayerProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateDeviceLayerProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
enumerateDeviceLayerProperties :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateDeviceLayerProperties = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkEnumerateDeviceLayerProperties commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructLayerProperties <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'enumerateDeviceLayerProperties'.
enumerateAllDeviceLayerProperties :: PhysicalDevice ->  IO (Vector LayerProperties)
enumerateAllDeviceLayerProperties physicalDevice' =
  snd <$> getNumDeviceLayerProperties physicalDevice'
    >>= \num -> snd <$> enumerateDeviceLayerProperties physicalDevice' num



-- | vkEnumerateInstanceLayerProperties - Returns up to requested number of
-- global layer properties
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of available layers may change at any time due to actions
-- outside of the Vulkan implementation, so two calls to
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'
-- with the same parameters /may/ return different results, or retrieve
-- different @pPropertyCount@ values or @pProperties@ contents. Once an
-- instance has been created, the layers enabled for that instance will
-- continue to be enabled and valid for the lifetime of that instance, even
-- if some of them become unavailable for future instances.
--
-- Unresolved directive in vkEnumerateInstanceLayerProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateInstanceLayerProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
getNumInstanceLayerProperties :: IO (VkResult, Word32)
getNumInstanceLayerProperties = alloca (\pPropertyCount' -> vkEnumerateInstanceLayerProperties pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkEnumerateInstanceLayerProperties - Returns up to requested number of
-- global layer properties
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of available layers may change at any time due to actions
-- outside of the Vulkan implementation, so two calls to
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'
-- with the same parameters /may/ return different results, or retrieve
-- different @pPropertyCount@ values or @pProperties@ contents. Once an
-- instance has been created, the layers enabled for that instance will
-- continue to be enabled and valid for the lifetime of that instance, even
-- if some of them become unavailable for future instances.
--
-- Unresolved directive in vkEnumerateInstanceLayerProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateInstanceLayerProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.VkLayerProperties'
enumerateInstanceLayerProperties :: Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateInstanceLayerProperties = \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkEnumerateInstanceLayerProperties pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructLayerProperties <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'enumerateInstanceLayerProperties'.
enumerateAllInstanceLayerProperties :: IO (Vector LayerProperties)
enumerateAllInstanceLayerProperties  =
  snd <$> getNumInstanceLayerProperties 
    >>= \num -> snd <$> enumerateInstanceLayerProperties  num

