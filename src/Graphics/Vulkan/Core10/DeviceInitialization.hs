{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.DeviceInitialization
  ( withCStructAllocationCallbacks
  , fromCStructAllocationCallbacks
  , AllocationCallbacks(..)
  , withCStructApplicationInfo
  , fromCStructApplicationInfo
  , ApplicationInfo(..)
  , Device(..)
  , DeviceSize
  , withCStructExtent3D
  , fromCStructExtent3D
  , Extent3D(..)
  , FormatFeatureFlagBits
  , FormatFeatureFlags
  , withCStructFormatProperties
  , fromCStructFormatProperties
  , FormatProperties(..)
  , ImageCreateFlagBits
  , ImageCreateFlags
  , withCStructImageFormatProperties
  , fromCStructImageFormatProperties
  , ImageFormatProperties(..)
  , ImageTiling
  , ImageType
  , ImageUsageFlagBits
  , ImageUsageFlags
  , Instance(..)
  , InstanceCreateFlags
  , withCStructInstanceCreateInfo
  , fromCStructInstanceCreateInfo
  , InstanceCreateInfo(..)
  , withCStructMemoryHeap
  , fromCStructMemoryHeap
  , MemoryHeap(..)
  , MemoryHeapFlagBits
  , MemoryHeapFlags
  , MemoryPropertyFlagBits
  , MemoryPropertyFlags
  , withCStructMemoryType
  , fromCStructMemoryType
  , MemoryType(..)
  , PhysicalDevice(..)
  , withCStructPhysicalDeviceFeatures
  , fromCStructPhysicalDeviceFeatures
  , PhysicalDeviceFeatures(..)
  , withCStructPhysicalDeviceLimits
  , fromCStructPhysicalDeviceLimits
  , PhysicalDeviceLimits(..)
  , withCStructPhysicalDeviceMemoryProperties
  , fromCStructPhysicalDeviceMemoryProperties
  , PhysicalDeviceMemoryProperties(..)
  , withCStructPhysicalDeviceProperties
  , fromCStructPhysicalDeviceProperties
  , PhysicalDeviceProperties(..)
  , withCStructPhysicalDeviceSparseProperties
  , fromCStructPhysicalDeviceSparseProperties
  , PhysicalDeviceSparseProperties(..)
  , PhysicalDeviceType
  , withCStructQueueFamilyProperties
  , fromCStructQueueFamilyProperties
  , QueueFamilyProperties(..)
  , QueueFlagBits
  , QueueFlags
  , SampleCountFlagBits
  , SampleCountFlags
  , createInstance
  , destroyInstance
  , getNumPhysicalDevices
  , enumeratePhysicalDevices
  , enumerateAllPhysicalDevices
  , getDeviceProcAddr
  , getInstanceProcAddr
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , getPhysicalDeviceMemoryProperties
  , getPhysicalDeviceProperties
  , getNumPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getAllPhysicalDeviceQueueFamilyProperties
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Bits
  ( zeroBits
  )
import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , useAsCString
  )
import Data.Function
  ( on
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  , take
  )
import qualified Data.Vector.Generic
  ( convert
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Generic.Sized
  ( convert
  )
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  , unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  , CSize(..)
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
  ( Ptr
  , castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createInstance
  , destroyInstance
  , enumeratePhysicalDevices
  , getDeviceProcAddr
  , getInstanceProcAddr
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , getPhysicalDeviceMemoryProperties
  , getPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkExtent3D(..)
  , VkFormatFeatureFlagBits(..)
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkInstanceCreateFlags(..)
  , VkInstanceCreateInfo(..)
  , VkMemoryHeap(..)
  , VkMemoryHeapFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkMemoryType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceType(..)
  , VkQueueFamilyProperties(..)
  , VkQueueFlagBits(..)
  , VkSampleCountFlagBits(..)
  , PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VkDevice
  , VkDeviceSize
  , VkInstance
  , VkPhysicalDevice
  , pattern VK_UUID_SIZE
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  , initInstanceCmds
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToNullTerminatedSizedVector
  , byteStringToSizedVector
  , packCStringElemOff
  , padSized
  , withArray
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "AllocationCallbacks"
data AllocationCallbacks = AllocationCallbacks
  { -- No documentation found for Nested "AllocationCallbacks" "pUserData"
  vkPUserData :: Ptr ()
  , -- No documentation found for Nested "AllocationCallbacks" "pfnAllocation"
  vkPfnAllocation :: PFN_vkAllocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnReallocation"
  vkPfnReallocation :: PFN_vkReallocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnFree"
  vkPfnFree :: PFN_vkFreeFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalAllocation"
  vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalFree"
  vkPfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Show, Eq)
withCStructAllocationCallbacks :: AllocationCallbacks -> (VkAllocationCallbacks -> IO a) -> IO a
withCStructAllocationCallbacks from cont = cont (VkAllocationCallbacks (vkPUserData (from :: AllocationCallbacks)) (vkPfnAllocation (from :: AllocationCallbacks)) (vkPfnReallocation (from :: AllocationCallbacks)) (vkPfnFree (from :: AllocationCallbacks)) (vkPfnInternalAllocation (from :: AllocationCallbacks)) (vkPfnInternalFree (from :: AllocationCallbacks)))
fromCStructAllocationCallbacks :: VkAllocationCallbacks -> IO AllocationCallbacks
fromCStructAllocationCallbacks c = AllocationCallbacks <$> pure (vkPUserData (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnAllocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnReallocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnFree (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnInternalAllocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnInternalFree (c :: VkAllocationCallbacks))
-- No documentation found for TopLevel "ApplicationInfo"
data ApplicationInfo = ApplicationInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ApplicationInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ApplicationInfo" "pApplicationName"
  vkPApplicationName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "applicationVersion"
  vkApplicationVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "pEngineName"
  vkPEngineName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "engineVersion"
  vkEngineVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "apiVersion"
  vkApiVersion :: Word32
  }
  deriving (Show, Eq)
withCStructApplicationInfo :: ApplicationInfo -> (VkApplicationInfo -> IO a) -> IO a
withCStructApplicationInfo from cont = maybeWith useAsCString (vkPEngineName (from :: ApplicationInfo)) (\pEngineName -> maybeWith useAsCString (vkPApplicationName (from :: ApplicationInfo)) (\pApplicationName -> maybeWith withSomeVkStruct (vkPNext (from :: ApplicationInfo)) (\pPNext -> cont (VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO pPNext pApplicationName (vkApplicationVersion (from :: ApplicationInfo)) pEngineName (vkEngineVersion (from :: ApplicationInfo)) (vkApiVersion (from :: ApplicationInfo))))))
fromCStructApplicationInfo :: VkApplicationInfo -> IO ApplicationInfo
fromCStructApplicationInfo c = ApplicationInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkApplicationInfo)))
                                               <*> maybePeek packCString (vkPApplicationName (c :: VkApplicationInfo))
                                               <*> pure (vkApplicationVersion (c :: VkApplicationInfo))
                                               <*> maybePeek packCString (vkPEngineName (c :: VkApplicationInfo))
                                               <*> pure (vkEngineVersion (c :: VkApplicationInfo))
                                               <*> pure (vkApiVersion (c :: VkApplicationInfo))
data Device = Device
  { deviceHandle :: VkDevice
  , deviceCmds    :: DeviceCmds
  }
  deriving Show

instance Eq Device where
  (==) = (==) `on` deviceHandle

instance Ord Device where
  compare = compare `on` deviceHandle

-- No documentation found for TopLevel "DeviceSize"
type DeviceSize = VkDeviceSize
  
-- No documentation found for TopLevel "Extent3D"
data Extent3D = Extent3D
  { -- No documentation found for Nested "Extent3D" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "Extent3D" "height"
  vkHeight :: Word32
  , -- No documentation found for Nested "Extent3D" "depth"
  vkDepth :: Word32
  }
  deriving (Show, Eq)
withCStructExtent3D :: Extent3D -> (VkExtent3D -> IO a) -> IO a
withCStructExtent3D from cont = cont (VkExtent3D (vkWidth (from :: Extent3D)) (vkHeight (from :: Extent3D)) (vkDepth (from :: Extent3D)))
fromCStructExtent3D :: VkExtent3D -> IO Extent3D
fromCStructExtent3D c = Extent3D <$> pure (vkWidth (c :: VkExtent3D))
                                 <*> pure (vkHeight (c :: VkExtent3D))
                                 <*> pure (vkDepth (c :: VkExtent3D))
-- No documentation found for TopLevel "FormatFeatureFlagBits"
type FormatFeatureFlagBits = VkFormatFeatureFlagBits
-- No documentation found for TopLevel "FormatFeatureFlags"
type FormatFeatureFlags = FormatFeatureFlagBits
-- No documentation found for TopLevel "FormatProperties"
data FormatProperties = FormatProperties
  { -- No documentation found for Nested "FormatProperties" "linearTilingFeatures"
  vkLinearTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "optimalTilingFeatures"
  vkOptimalTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "bufferFeatures"
  vkBufferFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)
withCStructFormatProperties :: FormatProperties -> (VkFormatProperties -> IO a) -> IO a
withCStructFormatProperties from cont = cont (VkFormatProperties (vkLinearTilingFeatures (from :: FormatProperties)) (vkOptimalTilingFeatures (from :: FormatProperties)) (vkBufferFeatures (from :: FormatProperties)))
fromCStructFormatProperties :: VkFormatProperties -> IO FormatProperties
fromCStructFormatProperties c = FormatProperties <$> pure (vkLinearTilingFeatures (c :: VkFormatProperties))
                                                 <*> pure (vkOptimalTilingFeatures (c :: VkFormatProperties))
                                                 <*> pure (vkBufferFeatures (c :: VkFormatProperties))
-- No documentation found for TopLevel "ImageCreateFlagBits"
type ImageCreateFlagBits = VkImageCreateFlagBits
-- No documentation found for TopLevel "ImageCreateFlags"
type ImageCreateFlags = ImageCreateFlagBits
-- No documentation found for TopLevel "ImageFormatProperties"
data ImageFormatProperties = ImageFormatProperties
  { -- No documentation found for Nested "ImageFormatProperties" "maxExtent"
  vkMaxExtent :: Extent3D
  , -- No documentation found for Nested "ImageFormatProperties" "maxMipLevels"
  vkMaxMipLevels :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "maxArrayLayers"
  vkMaxArrayLayers :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "sampleCounts"
  vkSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "ImageFormatProperties" "maxResourceSize"
  vkMaxResourceSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructImageFormatProperties :: ImageFormatProperties -> (VkImageFormatProperties -> IO a) -> IO a
withCStructImageFormatProperties from cont = withCStructExtent3D (vkMaxExtent (from :: ImageFormatProperties)) (\maxExtent -> cont (VkImageFormatProperties maxExtent (vkMaxMipLevels (from :: ImageFormatProperties)) (vkMaxArrayLayers (from :: ImageFormatProperties)) (vkSampleCounts (from :: ImageFormatProperties)) (vkMaxResourceSize (from :: ImageFormatProperties))))
fromCStructImageFormatProperties :: VkImageFormatProperties -> IO ImageFormatProperties
fromCStructImageFormatProperties c = ImageFormatProperties <$> (fromCStructExtent3D (vkMaxExtent (c :: VkImageFormatProperties)))
                                                           <*> pure (vkMaxMipLevels (c :: VkImageFormatProperties))
                                                           <*> pure (vkMaxArrayLayers (c :: VkImageFormatProperties))
                                                           <*> pure (vkSampleCounts (c :: VkImageFormatProperties))
                                                           <*> pure (vkMaxResourceSize (c :: VkImageFormatProperties))
-- No documentation found for TopLevel "ImageTiling"
type ImageTiling = VkImageTiling
-- No documentation found for TopLevel "ImageType"
type ImageType = VkImageType
-- No documentation found for TopLevel "ImageUsageFlagBits"
type ImageUsageFlagBits = VkImageUsageFlagBits
-- No documentation found for TopLevel "ImageUsageFlags"
type ImageUsageFlags = ImageUsageFlagBits
data Instance = Instance
  { instanceHandle :: VkInstance
  , instanceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq Instance where
  (==) = (==) `on` instanceHandle

instance Ord Instance where
  compare = compare `on` instanceHandle

-- No documentation found for TopLevel "InstanceCreateFlags"
type InstanceCreateFlags = VkInstanceCreateFlags
-- No documentation found for TopLevel "InstanceCreateInfo"
data InstanceCreateInfo = InstanceCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "InstanceCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "InstanceCreateInfo" "flags"
  vkFlags :: InstanceCreateFlags
  , -- No documentation found for Nested "InstanceCreateInfo" "pApplicationInfo"
  vkPApplicationInfo :: Maybe ApplicationInfo
  -- Length valued member elided
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledLayerNames"
  vkPpEnabledLayerNames :: Vector ByteString
  -- Length valued member elided
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledExtensionNames"
  vkPpEnabledExtensionNames :: Vector ByteString
  }
  deriving (Show, Eq)
withCStructInstanceCreateInfo :: InstanceCreateInfo -> (VkInstanceCreateInfo -> IO a) -> IO a
withCStructInstanceCreateInfo from cont = withVec useAsCString (vkPpEnabledExtensionNames (from :: InstanceCreateInfo)) (\pPEnabledExtensionNames -> withVec useAsCString (vkPpEnabledLayerNames (from :: InstanceCreateInfo)) (\pPEnabledLayerNames -> maybeWith (\a -> withCStructApplicationInfo a . flip with) (vkPApplicationInfo (from :: InstanceCreateInfo)) (\pApplicationInfo -> maybeWith withSomeVkStruct (vkPNext (from :: InstanceCreateInfo)) (\pPNext -> cont (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO pPNext (vkFlags (from :: InstanceCreateInfo)) pApplicationInfo (fromIntegral (Data.Vector.length (vkPpEnabledLayerNames (from :: InstanceCreateInfo)))) pPEnabledLayerNames (fromIntegral (Data.Vector.length (vkPpEnabledExtensionNames (from :: InstanceCreateInfo)))) pPEnabledExtensionNames)))))
fromCStructInstanceCreateInfo :: VkInstanceCreateInfo -> IO InstanceCreateInfo
fromCStructInstanceCreateInfo c = InstanceCreateInfo <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkInstanceCreateInfo)))
                                                     <*> pure (vkFlags (c :: VkInstanceCreateInfo))
                                                     <*> maybePeek (fromCStructApplicationInfo <=< peek) (vkPApplicationInfo (c :: VkInstanceCreateInfo))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkEnabledLayerCount (c :: VkInstanceCreateInfo))) (packCStringElemOff (vkPPEnabledLayerNames (c :: VkInstanceCreateInfo))))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkEnabledExtensionCount (c :: VkInstanceCreateInfo))) (packCStringElemOff (vkPPEnabledExtensionNames (c :: VkInstanceCreateInfo))))
-- No documentation found for TopLevel "MemoryHeap"
data MemoryHeap = MemoryHeap
  { -- No documentation found for Nested "MemoryHeap" "size"
  vkSize :: DeviceSize
  , -- No documentation found for Nested "MemoryHeap" "flags"
  vkFlags :: MemoryHeapFlags
  }
  deriving (Show, Eq)
withCStructMemoryHeap :: MemoryHeap -> (VkMemoryHeap -> IO a) -> IO a
withCStructMemoryHeap from cont = cont (VkMemoryHeap (vkSize (from :: MemoryHeap)) (vkFlags (from :: MemoryHeap)))
fromCStructMemoryHeap :: VkMemoryHeap -> IO MemoryHeap
fromCStructMemoryHeap c = MemoryHeap <$> pure (vkSize (c :: VkMemoryHeap))
                                     <*> pure (vkFlags (c :: VkMemoryHeap))
-- No documentation found for TopLevel "MemoryHeapFlagBits"
type MemoryHeapFlagBits = VkMemoryHeapFlagBits
-- No documentation found for TopLevel "MemoryHeapFlags"
type MemoryHeapFlags = MemoryHeapFlagBits
-- No documentation found for TopLevel "MemoryPropertyFlagBits"
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits
-- No documentation found for TopLevel "MemoryPropertyFlags"
type MemoryPropertyFlags = MemoryPropertyFlagBits
-- No documentation found for TopLevel "MemoryType"
data MemoryType = MemoryType
  { -- No documentation found for Nested "MemoryType" "propertyFlags"
  vkPropertyFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "MemoryType" "heapIndex"
  vkHeapIndex :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryType :: MemoryType -> (VkMemoryType -> IO a) -> IO a
withCStructMemoryType from cont = cont (VkMemoryType (vkPropertyFlags (from :: MemoryType)) (vkHeapIndex (from :: MemoryType)))
fromCStructMemoryType :: VkMemoryType -> IO MemoryType
fromCStructMemoryType c = MemoryType <$> pure (vkPropertyFlags (c :: VkMemoryType))
                                     <*> pure (vkHeapIndex (c :: VkMemoryType))
data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: VkPhysicalDevice
  , physicalDeviceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq PhysicalDevice where
  (==) = (==) `on` physicalDeviceHandle

instance Ord PhysicalDevice where
  compare = compare `on` physicalDeviceHandle

-- No documentation found for TopLevel "PhysicalDeviceFeatures"
data PhysicalDeviceFeatures = PhysicalDeviceFeatures
  { -- No documentation found for Nested "PhysicalDeviceFeatures" "robustBufferAccess"
  vkRobustBufferAccess :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fullDrawIndexUint32"
  vkFullDrawIndexUint32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "imageCubeArray"
  vkImageCubeArray :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "independentBlend"
  vkIndependentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "geometryShader"
  vkGeometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "tessellationShader"
  vkTessellationShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sampleRateShading"
  vkSampleRateShading :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "dualSrcBlend"
  vkDualSrcBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "logicOp"
  vkLogicOp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiDrawIndirect"
  vkMultiDrawIndirect :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "drawIndirectFirstInstance"
  vkDrawIndirectFirstInstance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthClamp"
  vkDepthClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBiasClamp"
  vkDepthBiasClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fillModeNonSolid"
  vkFillModeNonSolid :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBounds"
  vkDepthBounds :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "wideLines"
  vkWideLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "largePoints"
  vkLargePoints :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "alphaToOne"
  vkAlphaToOne :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiViewport"
  vkMultiViewport :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "samplerAnisotropy"
  vkSamplerAnisotropy :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionETC2"
  vkTextureCompressionETC2 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionASTC_LDR"
  vkTextureCompressionASTC_LDR :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionBC"
  vkTextureCompressionBC :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "occlusionQueryPrecise"
  vkOcclusionQueryPrecise :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "pipelineStatisticsQuery"
  vkPipelineStatisticsQuery :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "vertexPipelineStoresAndAtomics"
  vkVertexPipelineStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fragmentStoresAndAtomics"
  vkFragmentStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize"
  vkShaderTessellationAndGeometryPointSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderImageGatherExtended"
  vkShaderImageGatherExtended :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageExtendedFormats"
  vkShaderStorageImageExtendedFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageMultisample"
  vkShaderStorageImageMultisample :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat"
  vkShaderStorageImageReadWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat"
  vkShaderStorageImageWriteWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing"
  vkShaderUniformBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing"
  vkShaderSampledImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing"
  vkShaderStorageBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing"
  vkShaderStorageImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderClipDistance"
  vkShaderClipDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderCullDistance"
  vkShaderCullDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderFloat64"
  vkShaderFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt64"
  vkShaderInt64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt16"
  vkShaderInt16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceResidency"
  vkShaderResourceResidency :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceMinLod"
  vkShaderResourceMinLod :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseBinding"
  vkSparseBinding :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyBuffer"
  vkSparseResidencyBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage2D"
  vkSparseResidencyImage2D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage3D"
  vkSparseResidencyImage3D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency2Samples"
  vkSparseResidency2Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency4Samples"
  vkSparseResidency4Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency8Samples"
  vkSparseResidency8Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency16Samples"
  vkSparseResidency16Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyAliased"
  vkSparseResidencyAliased :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "variableMultisampleRate"
  vkVariableMultisampleRate :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "inheritedQueries"
  vkInheritedQueries :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFeatures :: PhysicalDeviceFeatures -> (VkPhysicalDeviceFeatures -> IO a) -> IO a
withCStructPhysicalDeviceFeatures from cont = cont (VkPhysicalDeviceFeatures (boolToBool32 (vkRobustBufferAccess (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkFullDrawIndexUint32 (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkImageCubeArray (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkIndependentBlend (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkGeometryShader (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkTessellationShader (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSampleRateShading (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkDualSrcBlend (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkLogicOp (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkMultiDrawIndirect (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkDrawIndirectFirstInstance (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkDepthClamp (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkDepthBiasClamp (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkFillModeNonSolid (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkDepthBounds (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkWideLines (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkLargePoints (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkAlphaToOne (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkMultiViewport (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSamplerAnisotropy (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkTextureCompressionETC2 (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkTextureCompressionASTC_LDR (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkTextureCompressionBC (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkOcclusionQueryPrecise (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkPipelineStatisticsQuery (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkVertexPipelineStoresAndAtomics (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkFragmentStoresAndAtomics (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderTessellationAndGeometryPointSize (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderImageGatherExtended (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageImageExtendedFormats (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageImageMultisample (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageImageReadWithoutFormat (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageImageWriteWithoutFormat (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderUniformBufferArrayDynamicIndexing (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderSampledImageArrayDynamicIndexing (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageBufferArrayDynamicIndexing (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderStorageImageArrayDynamicIndexing (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderClipDistance (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderCullDistance (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderFloat64 (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderInt64 (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderInt16 (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderResourceResidency (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkShaderResourceMinLod (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseBinding (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidencyBuffer (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidencyImage2D (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidencyImage3D (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidency2Samples (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidency4Samples (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidency8Samples (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidency16Samples (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkSparseResidencyAliased (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkVariableMultisampleRate (from :: PhysicalDeviceFeatures))) (boolToBool32 (vkInheritedQueries (from :: PhysicalDeviceFeatures))))
fromCStructPhysicalDeviceFeatures :: VkPhysicalDeviceFeatures -> IO PhysicalDeviceFeatures
fromCStructPhysicalDeviceFeatures c = PhysicalDeviceFeatures <$> pure (bool32ToBool (vkRobustBufferAccess (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFullDrawIndexUint32 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkImageCubeArray (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkIndependentBlend (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkGeometryShader (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTessellationShader (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSampleRateShading (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDualSrcBlend (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkLogicOp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkMultiDrawIndirect (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDrawIndirectFirstInstance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthClamp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthBiasClamp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFillModeNonSolid (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthBounds (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkWideLines (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkLargePoints (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkAlphaToOne (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkMultiViewport (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSamplerAnisotropy (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionETC2 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionASTC_LDR (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionBC (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkOcclusionQueryPrecise (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkPipelineStatisticsQuery (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkVertexPipelineStoresAndAtomics (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFragmentStoresAndAtomics (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderTessellationAndGeometryPointSize (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderImageGatherExtended (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageExtendedFormats (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageMultisample (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageReadWithoutFormat (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageWriteWithoutFormat (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderUniformBufferArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderSampledImageArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageBufferArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderClipDistance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderCullDistance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderFloat64 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderInt64 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderInt16 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderResourceResidency (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderResourceMinLod (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseBinding (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyBuffer (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyImage2D (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyImage3D (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency2Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency4Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency8Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency16Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyAliased (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkVariableMultisampleRate (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkInheritedQueries (c :: VkPhysicalDeviceFeatures)))
-- No documentation found for TopLevel "PhysicalDeviceLimits"
data PhysicalDeviceLimits = PhysicalDeviceLimits
  { -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension1D"
  vkMaxImageDimension1D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension2D"
  vkMaxImageDimension2D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension3D"
  vkMaxImageDimension3D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimensionCube"
  vkMaxImageDimensionCube :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelBufferElements"
  vkMaxTexelBufferElements :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxUniformBufferRange"
  vkMaxUniformBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxStorageBufferRange"
  vkMaxStorageBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPushConstantsSize"
  vkMaxPushConstantsSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxMemoryAllocationCount"
  vkMaxMemoryAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAllocationCount"
  vkMaxSamplerAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "bufferImageGranularity"
  vkBufferImageGranularity :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sparseAddressSpaceSize"
  vkSparseAddressSpaceSize :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxBoundDescriptorSets"
  vkMaxBoundDescriptorSets :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSamplers"
  vkMaxPerStageDescriptorSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorUniformBuffers"
  vkMaxPerStageDescriptorUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageBuffers"
  vkMaxPerStageDescriptorStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSampledImages"
  vkMaxPerStageDescriptorSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageImages"
  vkMaxPerStageDescriptorStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorInputAttachments"
  vkMaxPerStageDescriptorInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageResources"
  vkMaxPerStageResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSamplers"
  vkMaxDescriptorSetSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffers"
  vkMaxDescriptorSetUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffersDynamic"
  vkMaxDescriptorSetUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffers"
  vkMaxDescriptorSetStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffersDynamic"
  vkMaxDescriptorSetStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSampledImages"
  vkMaxDescriptorSetSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageImages"
  vkMaxDescriptorSetStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetInputAttachments"
  vkMaxDescriptorSetInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributes"
  vkMaxVertexInputAttributes :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindings"
  vkMaxVertexInputBindings :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributeOffset"
  vkMaxVertexInputAttributeOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindingStride"
  vkMaxVertexInputBindingStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexOutputComponents"
  vkMaxVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationGenerationLevel"
  vkMaxTessellationGenerationLevel :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationPatchSize"
  vkMaxTessellationPatchSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexInputComponents"
  vkMaxTessellationControlPerVertexInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexOutputComponents"
  vkMaxTessellationControlPerVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerPatchOutputComponents"
  vkMaxTessellationControlPerPatchOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlTotalOutputComponents"
  vkMaxTessellationControlTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationInputComponents"
  vkMaxTessellationEvaluationInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationOutputComponents"
  vkMaxTessellationEvaluationOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryShaderInvocations"
  vkMaxGeometryShaderInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryInputComponents"
  vkMaxGeometryInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputComponents"
  vkMaxGeometryOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputVertices"
  vkMaxGeometryOutputVertices :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryTotalOutputComponents"
  vkMaxGeometryTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentInputComponents"
  vkMaxFragmentInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentOutputAttachments"
  vkMaxFragmentOutputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentDualSrcAttachments"
  vkMaxFragmentDualSrcAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentCombinedOutputResources"
  vkMaxFragmentCombinedOutputResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeSharedMemorySize"
  vkMaxComputeSharedMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupCount"
  vkMaxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupInvocations"
  vkMaxComputeWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupSize"
  vkMaxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelPrecisionBits"
  vkSubPixelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subTexelPrecisionBits"
  vkSubTexelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "mipmapPrecisionBits"
  vkMipmapPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndexedIndexValue"
  vkMaxDrawIndexedIndexValue :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndirectCount"
  vkMaxDrawIndirectCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerLodBias"
  vkMaxSamplerLodBias :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAnisotropy"
  vkMaxSamplerAnisotropy :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewports"
  vkMaxViewports :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewportDimensions"
  vkMaxViewportDimensions :: (Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportBoundsRange"
  vkViewportBoundsRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportSubPixelBits"
  vkViewportSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minMemoryMapAlignment"
  vkMinMemoryMapAlignment :: CSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelBufferOffsetAlignment"
  vkMinTexelBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minUniformBufferOffsetAlignment"
  vkMinUniformBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minStorageBufferOffsetAlignment"
  vkMinStorageBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelOffset"
  vkMinTexelOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelOffset"
  vkMaxTexelOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelGatherOffset"
  vkMinTexelGatherOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelGatherOffset"
  vkMaxTexelGatherOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minInterpolationOffset"
  vkMinInterpolationOffset :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxInterpolationOffset"
  vkMaxInterpolationOffset :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelInterpolationOffsetBits"
  vkSubPixelInterpolationOffsetBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferWidth"
  vkMaxFramebufferWidth :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferHeight"
  vkMaxFramebufferHeight :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferLayers"
  vkMaxFramebufferLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferColorSampleCounts"
  vkFramebufferColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferDepthSampleCounts"
  vkFramebufferDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferStencilSampleCounts"
  vkFramebufferStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferNoAttachmentsSampleCounts"
  vkFramebufferNoAttachmentsSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxColorAttachments"
  vkMaxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageColorSampleCounts"
  vkSampledImageColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageIntegerSampleCounts"
  vkSampledImageIntegerSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageDepthSampleCounts"
  vkSampledImageDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageStencilSampleCounts"
  vkSampledImageStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "storageImageSampleCounts"
  vkStorageImageSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSampleMaskWords"
  vkMaxSampleMaskWords :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampComputeAndGraphics"
  vkTimestampComputeAndGraphics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampPeriod"
  vkTimestampPeriod :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxClipDistances"
  vkMaxClipDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCullDistances"
  vkMaxCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCombinedClipAndCullDistances"
  vkMaxCombinedClipAndCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "discreteQueuePriorities"
  vkDiscreteQueuePriorities :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeRange"
  vkPointSizeRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthRange"
  vkLineWidthRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeGranularity"
  vkPointSizeGranularity :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthGranularity"
  vkLineWidthGranularity :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "strictLines"
  vkStrictLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "standardSampleLocations"
  vkStandardSampleLocations :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyOffsetAlignment"
  vkOptimalBufferCopyOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyRowPitchAlignment"
  vkOptimalBufferCopyRowPitchAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "nonCoherentAtomSize"
  vkNonCoherentAtomSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceLimits :: PhysicalDeviceLimits -> (VkPhysicalDeviceLimits -> IO a) -> IO a
withCStructPhysicalDeviceLimits from cont = cont (VkPhysicalDeviceLimits (vkMaxImageDimension1D (from :: PhysicalDeviceLimits)) (vkMaxImageDimension2D (from :: PhysicalDeviceLimits)) (vkMaxImageDimension3D (from :: PhysicalDeviceLimits)) (vkMaxImageDimensionCube (from :: PhysicalDeviceLimits)) (vkMaxImageArrayLayers (from :: PhysicalDeviceLimits)) (vkMaxTexelBufferElements (from :: PhysicalDeviceLimits)) (vkMaxUniformBufferRange (from :: PhysicalDeviceLimits)) (vkMaxStorageBufferRange (from :: PhysicalDeviceLimits)) (vkMaxPushConstantsSize (from :: PhysicalDeviceLimits)) (vkMaxMemoryAllocationCount (from :: PhysicalDeviceLimits)) (vkMaxSamplerAllocationCount (from :: PhysicalDeviceLimits)) (vkBufferImageGranularity (from :: PhysicalDeviceLimits)) (vkSparseAddressSpaceSize (from :: PhysicalDeviceLimits)) (vkMaxBoundDescriptorSets (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorSamplers (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorUniformBuffers (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorStorageBuffers (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorSampledImages (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorStorageImages (from :: PhysicalDeviceLimits)) (vkMaxPerStageDescriptorInputAttachments (from :: PhysicalDeviceLimits)) (vkMaxPerStageResources (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetSamplers (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetUniformBuffers (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetUniformBuffersDynamic (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetStorageBuffers (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetStorageBuffersDynamic (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetSampledImages (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetStorageImages (from :: PhysicalDeviceLimits)) (vkMaxDescriptorSetInputAttachments (from :: PhysicalDeviceLimits)) (vkMaxVertexInputAttributes (from :: PhysicalDeviceLimits)) (vkMaxVertexInputBindings (from :: PhysicalDeviceLimits)) (vkMaxVertexInputAttributeOffset (from :: PhysicalDeviceLimits)) (vkMaxVertexInputBindingStride (from :: PhysicalDeviceLimits)) (vkMaxVertexOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationGenerationLevel (from :: PhysicalDeviceLimits)) (vkMaxTessellationPatchSize (from :: PhysicalDeviceLimits)) (vkMaxTessellationControlPerVertexInputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationControlPerVertexOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationControlPerPatchOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationControlTotalOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationEvaluationInputComponents (from :: PhysicalDeviceLimits)) (vkMaxTessellationEvaluationOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxGeometryShaderInvocations (from :: PhysicalDeviceLimits)) (vkMaxGeometryInputComponents (from :: PhysicalDeviceLimits)) (vkMaxGeometryOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxGeometryOutputVertices (from :: PhysicalDeviceLimits)) (vkMaxGeometryTotalOutputComponents (from :: PhysicalDeviceLimits)) (vkMaxFragmentInputComponents (from :: PhysicalDeviceLimits)) (vkMaxFragmentOutputAttachments (from :: PhysicalDeviceLimits)) (vkMaxFragmentDualSrcAttachments (from :: PhysicalDeviceLimits)) (vkMaxFragmentCombinedOutputResources (from :: PhysicalDeviceLimits)) (vkMaxComputeSharedMemorySize (from :: PhysicalDeviceLimits)) (fromTuple (vkMaxComputeWorkGroupCount (from :: PhysicalDeviceLimits))) (vkMaxComputeWorkGroupInvocations (from :: PhysicalDeviceLimits)) (fromTuple (vkMaxComputeWorkGroupSize (from :: PhysicalDeviceLimits))) (vkSubPixelPrecisionBits (from :: PhysicalDeviceLimits)) (vkSubTexelPrecisionBits (from :: PhysicalDeviceLimits)) (vkMipmapPrecisionBits (from :: PhysicalDeviceLimits)) (vkMaxDrawIndexedIndexValue (from :: PhysicalDeviceLimits)) (vkMaxDrawIndirectCount (from :: PhysicalDeviceLimits)) (vkMaxSamplerLodBias (from :: PhysicalDeviceLimits)) (vkMaxSamplerAnisotropy (from :: PhysicalDeviceLimits)) (vkMaxViewports (from :: PhysicalDeviceLimits)) (fromTuple (vkMaxViewportDimensions (from :: PhysicalDeviceLimits))) (fromTuple (vkViewportBoundsRange (from :: PhysicalDeviceLimits))) (vkViewportSubPixelBits (from :: PhysicalDeviceLimits)) (vkMinMemoryMapAlignment (from :: PhysicalDeviceLimits)) (vkMinTexelBufferOffsetAlignment (from :: PhysicalDeviceLimits)) (vkMinUniformBufferOffsetAlignment (from :: PhysicalDeviceLimits)) (vkMinStorageBufferOffsetAlignment (from :: PhysicalDeviceLimits)) (vkMinTexelOffset (from :: PhysicalDeviceLimits)) (vkMaxTexelOffset (from :: PhysicalDeviceLimits)) (vkMinTexelGatherOffset (from :: PhysicalDeviceLimits)) (vkMaxTexelGatherOffset (from :: PhysicalDeviceLimits)) (vkMinInterpolationOffset (from :: PhysicalDeviceLimits)) (vkMaxInterpolationOffset (from :: PhysicalDeviceLimits)) (vkSubPixelInterpolationOffsetBits (from :: PhysicalDeviceLimits)) (vkMaxFramebufferWidth (from :: PhysicalDeviceLimits)) (vkMaxFramebufferHeight (from :: PhysicalDeviceLimits)) (vkMaxFramebufferLayers (from :: PhysicalDeviceLimits)) (vkFramebufferColorSampleCounts (from :: PhysicalDeviceLimits)) (vkFramebufferDepthSampleCounts (from :: PhysicalDeviceLimits)) (vkFramebufferStencilSampleCounts (from :: PhysicalDeviceLimits)) (vkFramebufferNoAttachmentsSampleCounts (from :: PhysicalDeviceLimits)) (vkMaxColorAttachments (from :: PhysicalDeviceLimits)) (vkSampledImageColorSampleCounts (from :: PhysicalDeviceLimits)) (vkSampledImageIntegerSampleCounts (from :: PhysicalDeviceLimits)) (vkSampledImageDepthSampleCounts (from :: PhysicalDeviceLimits)) (vkSampledImageStencilSampleCounts (from :: PhysicalDeviceLimits)) (vkStorageImageSampleCounts (from :: PhysicalDeviceLimits)) (vkMaxSampleMaskWords (from :: PhysicalDeviceLimits)) (boolToBool32 (vkTimestampComputeAndGraphics (from :: PhysicalDeviceLimits))) (vkTimestampPeriod (from :: PhysicalDeviceLimits)) (vkMaxClipDistances (from :: PhysicalDeviceLimits)) (vkMaxCullDistances (from :: PhysicalDeviceLimits)) (vkMaxCombinedClipAndCullDistances (from :: PhysicalDeviceLimits)) (vkDiscreteQueuePriorities (from :: PhysicalDeviceLimits)) (fromTuple (vkPointSizeRange (from :: PhysicalDeviceLimits))) (fromTuple (vkLineWidthRange (from :: PhysicalDeviceLimits))) (vkPointSizeGranularity (from :: PhysicalDeviceLimits)) (vkLineWidthGranularity (from :: PhysicalDeviceLimits)) (boolToBool32 (vkStrictLines (from :: PhysicalDeviceLimits))) (boolToBool32 (vkStandardSampleLocations (from :: PhysicalDeviceLimits))) (vkOptimalBufferCopyOffsetAlignment (from :: PhysicalDeviceLimits)) (vkOptimalBufferCopyRowPitchAlignment (from :: PhysicalDeviceLimits)) (vkNonCoherentAtomSize (from :: PhysicalDeviceLimits)))
fromCStructPhysicalDeviceLimits :: VkPhysicalDeviceLimits -> IO PhysicalDeviceLimits
fromCStructPhysicalDeviceLimits c = PhysicalDeviceLimits <$> pure (vkMaxImageDimension1D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimension2D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimension3D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimensionCube (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageArrayLayers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelBufferElements (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxUniformBufferRange (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxStorageBufferRange (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPushConstantsSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxMemoryAllocationCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerAllocationCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkBufferImageGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSparseAddressSpaceSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxBoundDescriptorSets (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorSamplers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorUniformBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorStorageBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorSampledImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorStorageImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorInputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageResources (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetSamplers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetUniformBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetUniformBuffersDynamic (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageBuffersDynamic (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetSampledImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetInputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputAttributes (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputBindings (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputAttributeOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputBindingStride (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationGenerationLevel (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationPatchSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerVertexInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerVertexOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerPatchOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlTotalOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationEvaluationInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationEvaluationOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryShaderInvocations (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryOutputVertices (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryTotalOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentOutputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentDualSrcAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentCombinedOutputResources (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxComputeSharedMemorySize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let x = (vkMaxComputeWorkGroupCount (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 2 ))
                                                         <*> pure (vkMaxComputeWorkGroupInvocations (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let x = (vkMaxComputeWorkGroupSize (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 2 ))
                                                         <*> pure (vkSubPixelPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSubTexelPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMipmapPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDrawIndexedIndexValue (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDrawIndirectCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerLodBias (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerAnisotropy (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxViewports (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let x = (vkMaxViewportDimensions (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1 ))
                                                         <*> pure (let x = (vkViewportBoundsRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1 ))
                                                         <*> pure (vkViewportSubPixelBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinMemoryMapAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinUniformBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinStorageBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelGatherOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelGatherOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinInterpolationOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxInterpolationOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSubPixelInterpolationOffsetBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferWidth (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferHeight (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferLayers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferColorSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferDepthSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferStencilSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferNoAttachmentsSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxColorAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageColorSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageIntegerSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageDepthSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageStencilSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkStorageImageSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSampleMaskWords (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (bool32ToBool (vkTimestampComputeAndGraphics (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (vkTimestampPeriod (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxClipDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxCullDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxCombinedClipAndCullDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkDiscreteQueuePriorities (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let x = (vkPointSizeRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1 ))
                                                         <*> pure (let x = (vkLineWidthRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1 ))
                                                         <*> pure (vkPointSizeGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkLineWidthGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (bool32ToBool (vkStrictLines (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (bool32ToBool (vkStandardSampleLocations (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (vkOptimalBufferCopyOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkOptimalBufferCopyRowPitchAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkNonCoherentAtomSize (c :: VkPhysicalDeviceLimits))
-- No documentation found for TopLevel "PhysicalDeviceMemoryProperties"
data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { -- Fixed array valid count member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryTypes"
  vkMemoryTypes :: Vector MemoryType
  -- Fixed array valid count member elided
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryHeaps"
  vkMemoryHeaps :: Vector MemoryHeap
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMemoryProperties :: PhysicalDeviceMemoryProperties -> (VkPhysicalDeviceMemoryProperties -> IO a) -> IO a
withCStructPhysicalDeviceMemoryProperties from cont = withArray withCStructMemoryHeap (vkMemoryHeaps (from :: PhysicalDeviceMemoryProperties)) (\memoryHeaps -> withArray withCStructMemoryType (vkMemoryTypes (from :: PhysicalDeviceMemoryProperties)) (\memoryTypes -> cont (VkPhysicalDeviceMemoryProperties (fromIntegral (Data.Vector.length (vkMemoryTypes (from :: PhysicalDeviceMemoryProperties)))) (Data.Vector.Generic.Sized.convert (padSized (VkMemoryType zeroBits 0) memoryTypes)) (fromIntegral (Data.Vector.length (vkMemoryHeaps (from :: PhysicalDeviceMemoryProperties)))) (Data.Vector.Generic.Sized.convert (padSized (VkMemoryHeap 0 zeroBits) memoryHeaps)))))
fromCStructPhysicalDeviceMemoryProperties :: VkPhysicalDeviceMemoryProperties -> IO PhysicalDeviceMemoryProperties
fromCStructPhysicalDeviceMemoryProperties c = PhysicalDeviceMemoryProperties <$> -- Fixed array valid count member elided
                                                                             traverse fromCStructMemoryType (Data.Vector.take (fromIntegral (vkMemoryTypeCount (c :: VkPhysicalDeviceMemoryProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkMemoryTypes (c :: VkPhysicalDeviceMemoryProperties)))))
                                                                             -- Fixed array valid count member elided
                                                                             <*> traverse fromCStructMemoryHeap (Data.Vector.take (fromIntegral (vkMemoryHeapCount (c :: VkPhysicalDeviceMemoryProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkMemoryHeaps (c :: VkPhysicalDeviceMemoryProperties)))))
-- No documentation found for TopLevel "PhysicalDeviceProperties"
data PhysicalDeviceProperties = PhysicalDeviceProperties
  { -- No documentation found for Nested "PhysicalDeviceProperties" "apiVersion"
  vkApiVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "driverVersion"
  vkDriverVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "vendorID"
  vkVendorID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceID"
  vkDeviceID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceType"
  vkDeviceType :: PhysicalDeviceType
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceName"
  vkDeviceName :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "pipelineCacheUUID"
  vkPipelineCacheUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "limits"
  vkLimits :: PhysicalDeviceLimits
  , -- No documentation found for Nested "PhysicalDeviceProperties" "sparseProperties"
  vkSparseProperties :: PhysicalDeviceSparseProperties
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceProperties :: PhysicalDeviceProperties -> (VkPhysicalDeviceProperties -> IO a) -> IO a
withCStructPhysicalDeviceProperties from cont = withCStructPhysicalDeviceSparseProperties (vkSparseProperties (from :: PhysicalDeviceProperties)) (\sparseProperties -> withCStructPhysicalDeviceLimits (vkLimits (from :: PhysicalDeviceProperties)) (\limits -> cont (VkPhysicalDeviceProperties (vkApiVersion (from :: PhysicalDeviceProperties)) (vkDriverVersion (from :: PhysicalDeviceProperties)) (vkVendorID (from :: PhysicalDeviceProperties)) (vkDeviceID (from :: PhysicalDeviceProperties)) (vkDeviceType (from :: PhysicalDeviceProperties)) (byteStringToNullTerminatedSizedVector (vkDeviceName (from :: PhysicalDeviceProperties))) (byteStringToSizedVector (vkPipelineCacheUUID (from :: PhysicalDeviceProperties))) limits sparseProperties)))
fromCStructPhysicalDeviceProperties :: VkPhysicalDeviceProperties -> IO PhysicalDeviceProperties
fromCStructPhysicalDeviceProperties c = PhysicalDeviceProperties <$> pure (vkApiVersion (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDriverVersion (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkVendorID (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDeviceID (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDeviceType (c :: VkPhysicalDeviceProperties))
                                                                 <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceName (c :: VkPhysicalDeviceProperties))) packCString
                                                                 <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkPipelineCacheUUID (c :: VkPhysicalDeviceProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                 <*> (fromCStructPhysicalDeviceLimits (vkLimits (c :: VkPhysicalDeviceProperties)))
                                                                 <*> (fromCStructPhysicalDeviceSparseProperties (vkSparseProperties (c :: VkPhysicalDeviceProperties)))
-- No documentation found for TopLevel "PhysicalDeviceSparseProperties"
data PhysicalDeviceSparseProperties = PhysicalDeviceSparseProperties
  { -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DBlockShape"
  vkResidencyStandard2DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DMultisampleBlockShape"
  vkResidencyStandard2DMultisampleBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard3DBlockShape"
  vkResidencyStandard3DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyAlignedMipSize"
  vkResidencyAlignedMipSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyNonResidentStrict"
  vkResidencyNonResidentStrict :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSparseProperties :: PhysicalDeviceSparseProperties -> (VkPhysicalDeviceSparseProperties -> IO a) -> IO a
withCStructPhysicalDeviceSparseProperties from cont = cont (VkPhysicalDeviceSparseProperties (boolToBool32 (vkResidencyStandard2DBlockShape (from :: PhysicalDeviceSparseProperties))) (boolToBool32 (vkResidencyStandard2DMultisampleBlockShape (from :: PhysicalDeviceSparseProperties))) (boolToBool32 (vkResidencyStandard3DBlockShape (from :: PhysicalDeviceSparseProperties))) (boolToBool32 (vkResidencyAlignedMipSize (from :: PhysicalDeviceSparseProperties))) (boolToBool32 (vkResidencyNonResidentStrict (from :: PhysicalDeviceSparseProperties))))
fromCStructPhysicalDeviceSparseProperties :: VkPhysicalDeviceSparseProperties -> IO PhysicalDeviceSparseProperties
fromCStructPhysicalDeviceSparseProperties c = PhysicalDeviceSparseProperties <$> pure (bool32ToBool (vkResidencyStandard2DBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyStandard2DMultisampleBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyStandard3DBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyAlignedMipSize (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyNonResidentStrict (c :: VkPhysicalDeviceSparseProperties)))
-- No documentation found for TopLevel "PhysicalDeviceType"
type PhysicalDeviceType = VkPhysicalDeviceType
-- No documentation found for TopLevel "QueueFamilyProperties"
data QueueFamilyProperties = QueueFamilyProperties
  { -- No documentation found for Nested "QueueFamilyProperties" "queueFlags"
  vkQueueFlags :: QueueFlags
  , -- No documentation found for Nested "QueueFamilyProperties" "queueCount"
  vkQueueCount :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "timestampValidBits"
  vkTimestampValidBits :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "minImageTransferGranularity"
  vkMinImageTransferGranularity :: Extent3D
  }
  deriving (Show, Eq)
withCStructQueueFamilyProperties :: QueueFamilyProperties -> (VkQueueFamilyProperties -> IO a) -> IO a
withCStructQueueFamilyProperties from cont = withCStructExtent3D (vkMinImageTransferGranularity (from :: QueueFamilyProperties)) (\minImageTransferGranularity -> cont (VkQueueFamilyProperties (vkQueueFlags (from :: QueueFamilyProperties)) (vkQueueCount (from :: QueueFamilyProperties)) (vkTimestampValidBits (from :: QueueFamilyProperties)) minImageTransferGranularity))
fromCStructQueueFamilyProperties :: VkQueueFamilyProperties -> IO QueueFamilyProperties
fromCStructQueueFamilyProperties c = QueueFamilyProperties <$> pure (vkQueueFlags (c :: VkQueueFamilyProperties))
                                                           <*> pure (vkQueueCount (c :: VkQueueFamilyProperties))
                                                           <*> pure (vkTimestampValidBits (c :: VkQueueFamilyProperties))
                                                           <*> (fromCStructExtent3D (vkMinImageTransferGranularity (c :: VkQueueFamilyProperties)))
-- No documentation found for TopLevel "QueueFlagBits"
type QueueFlagBits = VkQueueFlagBits
-- No documentation found for TopLevel "QueueFlags"
type QueueFlags = QueueFlagBits
-- No documentation found for TopLevel "SampleCountFlagBits"
type SampleCountFlagBits = VkSampleCountFlagBits
-- No documentation found for TopLevel "SampleCountFlags"
type SampleCountFlags = SampleCountFlagBits

-- | Wrapper for vkCreateInstance
createInstance :: InstanceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Instance)
createInstance = \createInfo -> \allocator -> alloca (\pInstance -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructInstanceCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createInstance pCreateInfo pAllocator pInstance >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pInstance >>= (\instanceH -> Instance instanceH <$> initInstanceCmds instanceH))))))

-- | Wrapper for vkDestroyInstance
destroyInstance :: Instance ->  Maybe AllocationCallbacks ->  IO ()
destroyInstance = \(Instance instance' commandTable) -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyInstance commandTable instance' pAllocator *> (pure ()))

-- | Wrapper for vkEnumeratePhysicalDevices
getNumPhysicalDevices :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDevices = \(Instance instance' commandTable) -> alloca (\pPhysicalDeviceCount -> Graphics.Vulkan.C.Dynamic.enumeratePhysicalDevices commandTable instance' pPhysicalDeviceCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPhysicalDeviceCount)))

-- | Wrapper for vkEnumeratePhysicalDevices
enumeratePhysicalDevices :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDevice)
enumeratePhysicalDevices = \(Instance instance' commandTable) -> \physicalDeviceCount -> allocaArray (fromIntegral physicalDeviceCount) (\pPhysicalDevices -> with physicalDeviceCount (\pPhysicalDeviceCount -> Graphics.Vulkan.C.Dynamic.enumeratePhysicalDevices commandTable instance' pPhysicalDeviceCount pPhysicalDevices >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p i -> PhysicalDevice <$> peekElemOff p i <*> pure commandTable) pPhysicalDevices) =<< (fromIntegral <$> (peek pPhysicalDeviceCount)))))))
-- | Call 'getNumPhysicalDevices' to get the number of return values, then use that
-- number to call 'enumeratePhysicalDevices' to get all the values.
enumerateAllPhysicalDevices :: Instance ->  IO (Vector PhysicalDevice)
enumerateAllPhysicalDevices instance' =
  snd <$> getNumPhysicalDevices instance'
    >>= \num -> snd <$> enumeratePhysicalDevices instance' num


-- | Wrapper for vkGetDeviceProcAddr
getDeviceProcAddr :: Device ->  ByteString ->  IO (PFN_vkVoidFunction)
getDeviceProcAddr = \(Device device commandTable) -> \name -> useAsCString name (\pName -> Graphics.Vulkan.C.Dynamic.getDeviceProcAddr commandTable device pName >>= (\r -> pure r))

-- | Wrapper for vkGetInstanceProcAddr
getInstanceProcAddr :: Instance ->  ByteString ->  IO (PFN_vkVoidFunction)
getInstanceProcAddr = \(Instance instance' commandTable) -> \name -> useAsCString name (\pName -> Graphics.Vulkan.C.Dynamic.getInstanceProcAddr commandTable instance' pName >>= (\r -> pure r))

-- | Wrapper for vkGetPhysicalDeviceFeatures
getPhysicalDeviceFeatures :: PhysicalDevice ->  IO (PhysicalDeviceFeatures)
getPhysicalDeviceFeatures = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pFeatures -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceFeatures commandTable physicalDevice pFeatures *> ((fromCStructPhysicalDeviceFeatures <=< peek) pFeatures))

-- | Wrapper for vkGetPhysicalDeviceFormatProperties
getPhysicalDeviceFormatProperties :: PhysicalDevice ->  Format ->  IO (FormatProperties)
getPhysicalDeviceFormatProperties = \(PhysicalDevice physicalDevice commandTable) -> \format -> alloca (\pFormatProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceFormatProperties commandTable physicalDevice format pFormatProperties *> ((fromCStructFormatProperties <=< peek) pFormatProperties))

-- | Wrapper for vkGetPhysicalDeviceImageFormatProperties
getPhysicalDeviceImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  IO (ImageFormatProperties)
getPhysicalDeviceImageFormatProperties = \(PhysicalDevice physicalDevice commandTable) -> \format -> \type' -> \tiling -> \usage -> \flags -> alloca (\pImageFormatProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceImageFormatProperties commandTable physicalDevice format type' tiling usage flags pImageFormatProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructImageFormatProperties <=< peek) pImageFormatProperties)))

-- | Wrapper for vkGetPhysicalDeviceMemoryProperties
getPhysicalDeviceMemoryProperties :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties)
getPhysicalDeviceMemoryProperties = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pMemoryProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceMemoryProperties commandTable physicalDevice pMemoryProperties *> ((fromCStructPhysicalDeviceMemoryProperties <=< peek) pMemoryProperties))

-- | Wrapper for vkGetPhysicalDeviceProperties
getPhysicalDeviceProperties :: PhysicalDevice ->  IO (PhysicalDeviceProperties)
getPhysicalDeviceProperties = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceProperties commandTable physicalDevice pProperties *> ((fromCStructPhysicalDeviceProperties <=< peek) pProperties))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties
getNumPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pQueueFamilyPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceQueueFamilyProperties commandTable physicalDevice pQueueFamilyPropertyCount nullPtr *> (peek pQueueFamilyPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties
getPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties)
getPhysicalDeviceQueueFamilyProperties = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyPropertyCount -> allocaArray (fromIntegral queueFamilyPropertyCount) (\pQueueFamilyProperties -> with queueFamilyPropertyCount (\pQueueFamilyPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceQueueFamilyProperties commandTable physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties *> ((flip Data.Vector.generateM ((\p -> fromCStructQueueFamilyProperties <=< peekElemOff p) pQueueFamilyProperties) =<< (fromIntegral <$> (peek pQueueFamilyPropertyCount))))))
-- | Call 'getNumPhysicalDeviceQueueFamilyProperties' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceQueueFamilyProperties' to get all the values.
getAllPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Vector QueueFamilyProperties)
getAllPhysicalDeviceQueueFamilyProperties physicalDevice =
  getNumPhysicalDeviceQueueFamilyProperties physicalDevice
    >>= \num -> getPhysicalDeviceQueueFamilyProperties physicalDevice num

