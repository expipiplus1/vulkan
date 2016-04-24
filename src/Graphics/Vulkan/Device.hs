{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Device where

import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , VkBool32(..)
                           , Result(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CChar(..)
                      )

-- ** vkCreateDevice
foreign import ccall "vkCreateDevice" vkCreateDevice ::
  PhysicalDevice ->
  Ptr DeviceCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Device -> IO Result


data PhysicalDeviceFeatures =
  PhysicalDeviceFeatures{ robustBufferAccess :: VkBool32 
                        , fullDrawIndexUint32 :: VkBool32 
                        , imageCubeArray :: VkBool32 
                        , independentBlend :: VkBool32 
                        , geometryShader :: VkBool32 
                        , tessellationShader :: VkBool32 
                        , sampleRateShading :: VkBool32 
                        , dualSrcBlend :: VkBool32 
                        , logicOp :: VkBool32 
                        , multiDrawIndirect :: VkBool32 
                        , drawIndirectFirstInstance :: VkBool32 
                        , depthClamp :: VkBool32 
                        , depthBiasClamp :: VkBool32 
                        , fillModeNonSolid :: VkBool32 
                        , depthBounds :: VkBool32 
                        , wideLines :: VkBool32 
                        , largePoints :: VkBool32 
                        , alphaToOne :: VkBool32 
                        , multiViewport :: VkBool32 
                        , samplerAnisotropy :: VkBool32 
                        , textureCompressionETC2 :: VkBool32 
                        , textureCompressionASTC_LDR :: VkBool32 
                        , textureCompressionBC :: VkBool32 
                        , occlusionQueryPrecise :: VkBool32 
                        , pipelineStatisticsQuery :: VkBool32 
                        , vertexPipelineStoresAndAtomics :: VkBool32 
                        , fragmentStoresAndAtomics :: VkBool32 
                        , shaderTessellationAndGeometryPointSize :: VkBool32 
                        , shaderImageGatherExtended :: VkBool32 
                        , shaderStorageImageExtendedFormats :: VkBool32 
                        , shaderStorageImageMultisample :: VkBool32 
                        , shaderStorageImageReadWithoutFormat :: VkBool32 
                        , shaderStorageImageWriteWithoutFormat :: VkBool32 
                        , shaderUniformBufferArrayDynamicIndexing :: VkBool32 
                        , shaderSampledImageArrayDynamicIndexing :: VkBool32 
                        , shaderStorageBufferArrayDynamicIndexing :: VkBool32 
                        , shaderStorageImageArrayDynamicIndexing :: VkBool32 
                        , shaderClipDistance :: VkBool32 
                        , shaderCullDistance :: VkBool32 
                        , shaderFloat64 :: VkBool32 
                        , shaderInt64 :: VkBool32 
                        , shaderInt16 :: VkBool32 
                        , shaderResourceResidency :: VkBool32 
                        , shaderResourceMinLod :: VkBool32 
                        , sparseBinding :: VkBool32 
                        , sparseResidencyBuffer :: VkBool32 
                        , sparseResidencyImage2D :: VkBool32 
                        , sparseResidencyImage3D :: VkBool32 
                        , sparseResidency2Samples :: VkBool32 
                        , sparseResidency4Samples :: VkBool32 
                        , sparseResidency8Samples :: VkBool32 
                        , sparseResidency16Samples :: VkBool32 
                        , sparseResidencyAliased :: VkBool32 
                        , variableMultisampleRate :: VkBool32 
                        , inheritedQueries :: VkBool32 
                        }
  deriving (Eq)

instance Storable PhysicalDeviceFeatures where
  sizeOf ~_ = 220
  alignment ~_ = 4
  peek ptr = PhysicalDeviceFeatures <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 12)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 44)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 52)
                                    <*> peek (ptr `plusPtr` 56)
                                    <*> peek (ptr `plusPtr` 60)
                                    <*> peek (ptr `plusPtr` 64)
                                    <*> peek (ptr `plusPtr` 68)
                                    <*> peek (ptr `plusPtr` 72)
                                    <*> peek (ptr `plusPtr` 76)
                                    <*> peek (ptr `plusPtr` 80)
                                    <*> peek (ptr `plusPtr` 84)
                                    <*> peek (ptr `plusPtr` 88)
                                    <*> peek (ptr `plusPtr` 92)
                                    <*> peek (ptr `plusPtr` 96)
                                    <*> peek (ptr `plusPtr` 100)
                                    <*> peek (ptr `plusPtr` 104)
                                    <*> peek (ptr `plusPtr` 108)
                                    <*> peek (ptr `plusPtr` 112)
                                    <*> peek (ptr `plusPtr` 116)
                                    <*> peek (ptr `plusPtr` 120)
                                    <*> peek (ptr `plusPtr` 124)
                                    <*> peek (ptr `plusPtr` 128)
                                    <*> peek (ptr `plusPtr` 132)
                                    <*> peek (ptr `plusPtr` 136)
                                    <*> peek (ptr `plusPtr` 140)
                                    <*> peek (ptr `plusPtr` 144)
                                    <*> peek (ptr `plusPtr` 148)
                                    <*> peek (ptr `plusPtr` 152)
                                    <*> peek (ptr `plusPtr` 156)
                                    <*> peek (ptr `plusPtr` 160)
                                    <*> peek (ptr `plusPtr` 164)
                                    <*> peek (ptr `plusPtr` 168)
                                    <*> peek (ptr `plusPtr` 172)
                                    <*> peek (ptr `plusPtr` 176)
                                    <*> peek (ptr `plusPtr` 180)
                                    <*> peek (ptr `plusPtr` 184)
                                    <*> peek (ptr `plusPtr` 188)
                                    <*> peek (ptr `plusPtr` 192)
                                    <*> peek (ptr `plusPtr` 196)
                                    <*> peek (ptr `plusPtr` 200)
                                    <*> peek (ptr `plusPtr` 204)
                                    <*> peek (ptr `plusPtr` 208)
                                    <*> peek (ptr `plusPtr` 212)
                                    <*> peek (ptr `plusPtr` 216)
  poke ptr poked = poke (ptr `plusPtr` 0) (robustBufferAccess (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 4) (fullDrawIndexUint32 (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 8) (imageCubeArray (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 12) (independentBlend (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 16) (geometryShader (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 20) (tessellationShader (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 24) (sampleRateShading (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 28) (dualSrcBlend (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 32) (logicOp (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 36) (multiDrawIndirect (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 40) (drawIndirectFirstInstance (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 44) (depthClamp (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 48) (depthBiasClamp (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 52) (fillModeNonSolid (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 56) (depthBounds (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 60) (wideLines (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 64) (largePoints (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 68) (alphaToOne (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 72) (multiViewport (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 76) (samplerAnisotropy (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 80) (textureCompressionETC2 (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 84) (textureCompressionASTC_LDR (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 88) (textureCompressionBC (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 92) (occlusionQueryPrecise (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 96) (pipelineStatisticsQuery (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 100) (vertexPipelineStoresAndAtomics (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 104) (fragmentStoresAndAtomics (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 108) (shaderTessellationAndGeometryPointSize (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 112) (shaderImageGatherExtended (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 116) (shaderStorageImageExtendedFormats (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 120) (shaderStorageImageMultisample (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 124) (shaderStorageImageReadWithoutFormat (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 128) (shaderStorageImageWriteWithoutFormat (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 132) (shaderUniformBufferArrayDynamicIndexing (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 136) (shaderSampledImageArrayDynamicIndexing (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 140) (shaderStorageBufferArrayDynamicIndexing (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 144) (shaderStorageImageArrayDynamicIndexing (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 148) (shaderClipDistance (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 152) (shaderCullDistance (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 156) (shaderFloat64 (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (shaderInt64 (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (shaderInt16 (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (shaderResourceResidency (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (shaderResourceMinLod (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (sparseBinding (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (sparseResidencyBuffer (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (sparseResidencyImage2D (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (sparseResidencyImage3D (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (sparseResidency2Samples (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (sparseResidency4Samples (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (sparseResidency8Samples (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (sparseResidency16Samples (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (sparseResidencyAliased (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (variableMultisampleRate (poked :: PhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (inheritedQueries (poked :: PhysicalDeviceFeatures))


-- ** DeviceCreateFlags
-- | Opaque flag
newtype DeviceCreateFlags = DeviceCreateFlags VkFlags
  deriving (Eq, Storable)


data DeviceQueueCreateInfo =
  DeviceQueueCreateInfo{ sType :: StructureType 
                       , pNext :: Ptr Void 
                       , flags :: DeviceQueueCreateFlags 
                       , queueFamilyIndex :: Word32 
                       , queueCount :: Word32 
                       , pQueuePriorities :: Ptr CFloat 
                       }
  deriving (Eq)

instance Storable DeviceQueueCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = DeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueFamilyIndex (poked :: DeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (queueCount (poked :: DeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (pQueuePriorities (poked :: DeviceQueueCreateInfo))


-- ** DeviceQueueCreateFlags
-- | Opaque flag
newtype DeviceQueueCreateFlags = DeviceQueueCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** vkDestroyDevice
foreign import ccall "vkDestroyDevice" vkDestroyDevice ::
  Device -> Ptr AllocationCallbacks -> IO ()

data VkPhysicalDevice_T
type PhysicalDevice = Ptr VkPhysicalDevice_T

data VkDevice_T
type Device = Ptr VkDevice_T


data DeviceCreateInfo =
  DeviceCreateInfo{ sType :: StructureType 
                  , pNext :: Ptr Void 
                  , flags :: DeviceCreateFlags 
                  , queueCreateInfoCount :: Word32 
                  , pQueueCreateInfos :: Ptr DeviceQueueCreateInfo 
                  , enabledLayerCount :: Word32 
                  , ppEnabledLayerNames :: Ptr (Ptr CChar) 
                  , enabledExtensionCount :: Word32 
                  , ppEnabledExtensionNames :: Ptr (Ptr CChar) 
                  , pEnabledFeatures :: Ptr PhysicalDeviceFeatures 
                  }
  deriving (Eq)

instance Storable DeviceCreateInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = DeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 20)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
                              <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueCreateInfoCount (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (pQueueCreateInfos (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (enabledLayerCount (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (ppEnabledLayerNames (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (enabledExtensionCount (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (ppEnabledExtensionNames (poked :: DeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (pEnabledFeatures (poked :: DeviceCreateInfo))


