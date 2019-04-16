{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( withCStructExternalBufferProperties
  , fromCStructExternalBufferProperties
  , ExternalBufferProperties(..)
  , withCStructExternalImageFormatProperties
  , fromCStructExternalImageFormatProperties
  , ExternalImageFormatProperties(..)
  , ExternalMemoryFeatureFlagBits
  , ExternalMemoryFeatureFlagBitsKHR
  , ExternalMemoryFeatureFlags
  , ExternalMemoryFeatureFlagsKHR
  , ExternalMemoryHandleTypeFlagBits
  , ExternalMemoryHandleTypeFlagBitsKHR
  , ExternalMemoryHandleTypeFlags
  , ExternalMemoryHandleTypeFlagsKHR
  , withCStructExternalMemoryProperties
  , fromCStructExternalMemoryProperties
  , ExternalMemoryProperties(..)
  , withCStructPhysicalDeviceExternalBufferInfo
  , fromCStructPhysicalDeviceExternalBufferInfo
  , PhysicalDeviceExternalBufferInfo(..)
  , withCStructPhysicalDeviceExternalImageFormatInfo
  , fromCStructPhysicalDeviceExternalImageFormatInfo
  , PhysicalDeviceExternalImageFormatInfo(..)
  , withCStructPhysicalDeviceIDProperties
  , fromCStructPhysicalDeviceIDProperties
  , PhysicalDeviceIDProperties(..)
  , getPhysicalDeviceExternalBufferProperties
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  , pattern VK_LUID_SIZE
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceExternalBufferProperties
  )


import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( pattern VK_UUID_SIZE
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkExternalMemoryFeatureFlagBits(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
  , pattern VK_LUID_SIZE
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlags
  , BufferUsageFlags
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToSizedVector
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ExternalBufferProperties"
data ExternalBufferProperties = ExternalBufferProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalBufferProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalBufferProperties" "externalMemoryProperties"
  vkExternalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)
withCStructExternalBufferProperties :: ExternalBufferProperties -> (VkExternalBufferProperties -> IO a) -> IO a
withCStructExternalBufferProperties from cont = withCStructExternalMemoryProperties (vkExternalMemoryProperties (from :: ExternalBufferProperties)) (\externalMemoryProperties -> maybeWith withSomeVkStruct (vkPNext (from :: ExternalBufferProperties)) (\pPNext -> cont (VkExternalBufferProperties VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES pPNext externalMemoryProperties)))
fromCStructExternalBufferProperties :: VkExternalBufferProperties -> IO ExternalBufferProperties
fromCStructExternalBufferProperties c = ExternalBufferProperties <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalBufferProperties)))
                                                                 <*> (fromCStructExternalMemoryProperties (vkExternalMemoryProperties (c :: VkExternalBufferProperties)))
-- No documentation found for TopLevel "ExternalImageFormatProperties"
data ExternalImageFormatProperties = ExternalImageFormatProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalImageFormatProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalImageFormatProperties" "externalMemoryProperties"
  vkExternalMemoryProperties :: ExternalMemoryProperties
  }
  deriving (Show, Eq)
withCStructExternalImageFormatProperties :: ExternalImageFormatProperties -> (VkExternalImageFormatProperties -> IO a) -> IO a
withCStructExternalImageFormatProperties from cont = withCStructExternalMemoryProperties (vkExternalMemoryProperties (from :: ExternalImageFormatProperties)) (\externalMemoryProperties -> maybeWith withSomeVkStruct (vkPNext (from :: ExternalImageFormatProperties)) (\pPNext -> cont (VkExternalImageFormatProperties VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES pPNext externalMemoryProperties)))
fromCStructExternalImageFormatProperties :: VkExternalImageFormatProperties -> IO ExternalImageFormatProperties
fromCStructExternalImageFormatProperties c = ExternalImageFormatProperties <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalImageFormatProperties)))
                                                                           <*> (fromCStructExternalMemoryProperties (vkExternalMemoryProperties (c :: VkExternalImageFormatProperties)))
-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBits"
type ExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits
-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsKHR"
type ExternalMemoryFeatureFlagBitsKHR = ExternalMemoryFeatureFlagBits
-- No documentation found for TopLevel "ExternalMemoryFeatureFlags"
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits
-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsKHR"
type ExternalMemoryFeatureFlagsKHR = ExternalMemoryFeatureFlags
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBits"
type ExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsKHR"
type ExternalMemoryHandleTypeFlagBitsKHR = ExternalMemoryHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlags"
type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsKHR"
type ExternalMemoryHandleTypeFlagsKHR = ExternalMemoryHandleTypeFlags
-- No documentation found for TopLevel "ExternalMemoryProperties"
data ExternalMemoryProperties = ExternalMemoryProperties
  { -- No documentation found for Nested "ExternalMemoryProperties" "externalMemoryFeatures"
  vkExternalMemoryFeatures :: ExternalMemoryFeatureFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlags
  , -- No documentation found for Nested "ExternalMemoryProperties" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExternalMemoryProperties :: ExternalMemoryProperties -> (VkExternalMemoryProperties -> IO a) -> IO a
withCStructExternalMemoryProperties from cont = cont (VkExternalMemoryProperties (vkExternalMemoryFeatures (from :: ExternalMemoryProperties)) (vkExportFromImportedHandleTypes (from :: ExternalMemoryProperties)) (vkCompatibleHandleTypes (from :: ExternalMemoryProperties)))
fromCStructExternalMemoryProperties :: VkExternalMemoryProperties -> IO ExternalMemoryProperties
fromCStructExternalMemoryProperties c = ExternalMemoryProperties <$> pure (vkExternalMemoryFeatures (c :: VkExternalMemoryProperties))
                                                                 <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalMemoryProperties))
                                                                 <*> pure (vkCompatibleHandleTypes (c :: VkExternalMemoryProperties))
-- No documentation found for TopLevel "PhysicalDeviceExternalBufferInfo"
data PhysicalDeviceExternalBufferInfo = PhysicalDeviceExternalBufferInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "flags"
  vkFlags :: BufferCreateFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "usage"
  vkUsage :: BufferUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceExternalBufferInfo" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExternalBufferInfo :: PhysicalDeviceExternalBufferInfo -> (VkPhysicalDeviceExternalBufferInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalBufferInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExternalBufferInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalBufferInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO pPNext (vkFlags (from :: PhysicalDeviceExternalBufferInfo)) (vkUsage (from :: PhysicalDeviceExternalBufferInfo)) (vkHandleType (from :: PhysicalDeviceExternalBufferInfo))))
fromCStructPhysicalDeviceExternalBufferInfo :: VkPhysicalDeviceExternalBufferInfo -> IO PhysicalDeviceExternalBufferInfo
fromCStructPhysicalDeviceExternalBufferInfo c = PhysicalDeviceExternalBufferInfo <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalBufferInfo)))
                                                                                 <*> pure (vkFlags (c :: VkPhysicalDeviceExternalBufferInfo))
                                                                                 <*> pure (vkUsage (c :: VkPhysicalDeviceExternalBufferInfo))
                                                                                 <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalBufferInfo))
-- No documentation found for TopLevel "PhysicalDeviceExternalImageFormatInfo"
data PhysicalDeviceExternalImageFormatInfo = PhysicalDeviceExternalImageFormatInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalImageFormatInfo" "handleType"
  vkHandleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExternalImageFormatInfo :: PhysicalDeviceExternalImageFormatInfo -> (VkPhysicalDeviceExternalImageFormatInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalImageFormatInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExternalImageFormatInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalImageFormatInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO pPNext (vkHandleType (from :: PhysicalDeviceExternalImageFormatInfo))))
fromCStructPhysicalDeviceExternalImageFormatInfo :: VkPhysicalDeviceExternalImageFormatInfo -> IO PhysicalDeviceExternalImageFormatInfo
fromCStructPhysicalDeviceExternalImageFormatInfo c = PhysicalDeviceExternalImageFormatInfo <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalImageFormatInfo)))
                                                                                           <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalImageFormatInfo))
-- No documentation found for TopLevel "PhysicalDeviceIDProperties"
data PhysicalDeviceIDProperties = PhysicalDeviceIDProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceIDProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceUUID"
  vkDeviceUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "driverUUID"
  vkDriverUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceLUID"
  vkDeviceLUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceNodeMask"
  vkDeviceNodeMask :: Word32
  , -- No documentation found for Nested "PhysicalDeviceIDProperties" "deviceLUIDValid"
  vkDeviceLUIDValid :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceIDProperties :: PhysicalDeviceIDProperties -> (VkPhysicalDeviceIDProperties -> IO a) -> IO a
withCStructPhysicalDeviceIDProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceIDProperties)) (\pPNext -> cont (VkPhysicalDeviceIDProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES pPNext (byteStringToSizedVector (vkDeviceUUID (from :: PhysicalDeviceIDProperties))) (byteStringToSizedVector (vkDriverUUID (from :: PhysicalDeviceIDProperties))) (byteStringToSizedVector (vkDeviceLUID (from :: PhysicalDeviceIDProperties))) (vkDeviceNodeMask (from :: PhysicalDeviceIDProperties)) (boolToBool32 (vkDeviceLUIDValid (from :: PhysicalDeviceIDProperties)))))
fromCStructPhysicalDeviceIDProperties :: VkPhysicalDeviceIDProperties -> IO PhysicalDeviceIDProperties
fromCStructPhysicalDeviceIDProperties c = PhysicalDeviceIDProperties <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceIDProperties)))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceUUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDriverUUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                     <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceLUID (c :: VkPhysicalDeviceIDProperties))) (\p -> packCStringLen (castPtr p, VK_LUID_SIZE))
                                                                     <*> pure (vkDeviceNodeMask (c :: VkPhysicalDeviceIDProperties))
                                                                     <*> pure (bool32ToBool (vkDeviceLUIDValid (c :: VkPhysicalDeviceIDProperties)))

-- | Wrapper for vkGetPhysicalDeviceExternalBufferProperties
getPhysicalDeviceExternalBufferProperties :: PhysicalDevice ->  PhysicalDeviceExternalBufferInfo ->  IO (ExternalBufferProperties)
getPhysicalDeviceExternalBufferProperties = \(PhysicalDevice physicalDevice commandTable) -> \externalBufferInfo -> alloca (\pExternalBufferProperties -> (\a -> withCStructPhysicalDeviceExternalBufferInfo a . flip with) externalBufferInfo (\pExternalBufferInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceExternalBufferProperties commandTable physicalDevice pExternalBufferInfo pExternalBufferProperties *> ((fromCStructExternalBufferProperties <=< peek) pExternalBufferProperties)))
