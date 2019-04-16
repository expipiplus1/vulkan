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
import qualified Graphics.Vulkan.C.Dynamic
  ( enumerateDeviceLayerProperties
  , enumerateInstanceLayerProperties
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties(..)
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


-- No documentation found for TopLevel "LayerProperties"
data LayerProperties = LayerProperties
  { -- No documentation found for Nested "LayerProperties" "layerName"
  vkLayerName :: ByteString
  , -- No documentation found for Nested "LayerProperties" "specVersion"
  vkSpecVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "implementationVersion"
  vkImplementationVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "description"
  vkDescription :: ByteString
  }
  deriving (Show, Eq)
withCStructLayerProperties :: LayerProperties -> (VkLayerProperties -> IO a) -> IO a
withCStructLayerProperties from cont = cont (VkLayerProperties (byteStringToNullTerminatedSizedVector (vkLayerName (from :: LayerProperties))) (vkSpecVersion (from :: LayerProperties)) (vkImplementationVersion (from :: LayerProperties)) (byteStringToNullTerminatedSizedVector (vkDescription (from :: LayerProperties))))
fromCStructLayerProperties :: VkLayerProperties -> IO LayerProperties
fromCStructLayerProperties c = LayerProperties <$> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkLayerName (c :: VkLayerProperties))) packCString
                                               <*> pure (vkSpecVersion (c :: VkLayerProperties))
                                               <*> pure (vkImplementationVersion (c :: VkLayerProperties))
                                               <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDescription (c :: VkLayerProperties))) packCString

-- | Wrapper for vkEnumerateDeviceLayerProperties
getNumDeviceLayerProperties :: PhysicalDevice ->  IO (VkResult, Word32)
getNumDeviceLayerProperties = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.enumerateDeviceLayerProperties commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for vkEnumerateDeviceLayerProperties
enumerateDeviceLayerProperties :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateDeviceLayerProperties = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.enumerateDeviceLayerProperties commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructLayerProperties <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumDeviceLayerProperties' to get the number of return values, then use that
-- number to call 'enumerateDeviceLayerProperties' to get all the values.
enumerateAllDeviceLayerProperties :: PhysicalDevice ->  IO (Vector LayerProperties)
enumerateAllDeviceLayerProperties physicalDevice =
  snd <$> getNumDeviceLayerProperties physicalDevice
    >>= \num -> snd <$> enumerateDeviceLayerProperties physicalDevice num


-- | Wrapper for vkEnumerateInstanceLayerProperties
getNumInstanceLayerProperties :: IO (VkResult, Word32)
getNumInstanceLayerProperties = alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.enumerateInstanceLayerProperties pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for vkEnumerateInstanceLayerProperties
enumerateInstanceLayerProperties :: Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateInstanceLayerProperties = \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.enumerateInstanceLayerProperties pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructLayerProperties <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumInstanceLayerProperties' to get the number of return values, then use that
-- number to call 'enumerateInstanceLayerProperties' to get all the values.
enumerateAllInstanceLayerProperties :: IO (Vector LayerProperties)
enumerateAllInstanceLayerProperties  =
  snd <$> getNumInstanceLayerProperties 
    >>= \num -> snd <$> enumerateInstanceLayerProperties  num

