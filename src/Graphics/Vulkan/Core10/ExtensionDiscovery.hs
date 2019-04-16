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
import qualified Graphics.Vulkan.C.Dynamic
  ( enumerateDeviceExtensionProperties
  , enumerateInstanceExtensionProperties
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
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


-- No documentation found for TopLevel "ExtensionProperties"
data ExtensionProperties = ExtensionProperties
  { -- No documentation found for Nested "ExtensionProperties" "extensionName"
  vkExtensionName :: ByteString
  , -- No documentation found for Nested "ExtensionProperties" "specVersion"
  vkSpecVersion :: Word32
  }
  deriving (Show, Eq)
withCStructExtensionProperties :: ExtensionProperties -> (VkExtensionProperties -> IO a) -> IO a
withCStructExtensionProperties from cont = cont (VkExtensionProperties (byteStringToNullTerminatedSizedVector (vkExtensionName (from :: ExtensionProperties))) (vkSpecVersion (from :: ExtensionProperties)))
fromCStructExtensionProperties :: VkExtensionProperties -> IO ExtensionProperties
fromCStructExtensionProperties c = ExtensionProperties <$> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkExtensionName (c :: VkExtensionProperties))) packCString
                                                       <*> pure (vkSpecVersion (c :: VkExtensionProperties))

-- | Wrapper for 'vkEnumerateDeviceExtensionProperties'
getNumDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (VkResult, Word32)
getNumDeviceExtensionProperties = \(PhysicalDevice physicalDevice commandTable) -> \layerName -> alloca (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> Graphics.Vulkan.C.Dynamic.enumerateDeviceExtensionProperties commandTable physicalDevice pLayerName pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount))))

-- | Wrapper for 'vkEnumerateDeviceExtensionProperties'
enumerateDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  Word32 ->  IO ( VkResult
, Vector ExtensionProperties )
enumerateDeviceExtensionProperties = \(PhysicalDevice physicalDevice commandTable) -> \layerName -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> Graphics.Vulkan.C.Dynamic.enumerateDeviceExtensionProperties commandTable physicalDevice pLayerName pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructExtensionProperties <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount))))))))
-- | Call 'getNumDeviceExtensionProperties' to get the number of return values, then use that
-- number to call 'enumerateDeviceExtensionProperties' to get all the values.
enumerateAllDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllDeviceExtensionProperties physicalDevice pLayerName =
  snd <$> getNumDeviceExtensionProperties physicalDevice pLayerName
    >>= \num -> snd <$> enumerateDeviceExtensionProperties physicalDevice pLayerName num


-- | Wrapper for 'vkEnumerateInstanceExtensionProperties'
getNumInstanceExtensionProperties :: Maybe ByteString ->  IO (VkResult, Word32)
getNumInstanceExtensionProperties = \layerName -> alloca (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> Graphics.Vulkan.C.Dynamic.enumerateInstanceExtensionProperties pLayerName pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount))))

-- | Wrapper for 'vkEnumerateInstanceExtensionProperties'
enumerateInstanceExtensionProperties :: Maybe ByteString ->  Word32 ->  IO (VkResult, Vector ExtensionProperties)
enumerateInstanceExtensionProperties = \layerName -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> Graphics.Vulkan.C.Dynamic.enumerateInstanceExtensionProperties pLayerName pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructExtensionProperties <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount))))))))
-- | Call 'getNumInstanceExtensionProperties' to get the number of return values, then use that
-- number to call 'enumerateInstanceExtensionProperties' to get all the values.
enumerateAllInstanceExtensionProperties :: Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllInstanceExtensionProperties pLayerName =
  snd <$> getNumInstanceExtensionProperties pLayerName
    >>= \num -> snd <$> enumerateInstanceExtensionProperties pLayerName num

