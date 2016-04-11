{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Device where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )

-- ** vkCreateDevice
foreign import ccall "vkCreateDevice" vkCreateDevice ::
  PhysicalDevice ->
  Ptr VkDeviceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Device -> IO VkResult


data VkPhysicalDeviceFeatures =
  VkPhysicalDeviceFeatures{ robustBufferAccess :: VkBool32 
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

instance Storable VkPhysicalDeviceFeatures where
  sizeOf ~_ = 220
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceFeatures <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (robustBufferAccess (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 4) (fullDrawIndexUint32 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 8) (imageCubeArray (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 12) (independentBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 16) (geometryShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 20) (tessellationShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 24) (sampleRateShading (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 28) (dualSrcBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 32) (logicOp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 36) (multiDrawIndirect (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 40) (drawIndirectFirstInstance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 44) (depthClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 48) (depthBiasClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 52) (fillModeNonSolid (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 56) (depthBounds (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 60) (wideLines (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 64) (largePoints (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 68) (alphaToOne (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 72) (multiViewport (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 76) (samplerAnisotropy (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 80) (textureCompressionETC2 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 84) (textureCompressionASTC_LDR (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 88) (textureCompressionBC (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 92) (occlusionQueryPrecise (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 96) (pipelineStatisticsQuery (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 100) (vertexPipelineStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 104) (fragmentStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 108) (shaderTessellationAndGeometryPointSize (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 112) (shaderImageGatherExtended (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 116) (shaderStorageImageExtendedFormats (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 120) (shaderStorageImageMultisample (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 124) (shaderStorageImageReadWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 128) (shaderStorageImageWriteWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 132) (shaderUniformBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 136) (shaderSampledImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 140) (shaderStorageBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 144) (shaderStorageImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 148) (shaderClipDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 152) (shaderCullDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 156) (shaderFloat64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (shaderInt64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (shaderInt16 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (shaderResourceResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (shaderResourceMinLod (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (sparseBinding (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (sparseResidencyBuffer (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (sparseResidencyImage2D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (sparseResidencyImage3D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (sparseResidency2Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (sparseResidency4Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (sparseResidency8Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (sparseResidency16Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (sparseResidencyAliased (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (variableMultisampleRate (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (inheritedQueries (poked :: VkPhysicalDeviceFeatures))


-- ** VkDeviceCreateFlags
-- | Opaque flag
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Storable)


data VkDeviceQueueCreateInfo =
  VkDeviceQueueCreateInfo{ sType :: VkStructureType 
                         , pNext :: Ptr Void 
                         , flags :: VkDeviceQueueCreateFlags 
                         , queueFamilyIndex :: Word32 
                         , queueCount :: Word32 
                         , pQueuePriorities :: Ptr CFloat 
                         }
  deriving (Eq)

instance Storable VkDeviceQueueCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueFamilyIndex (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (queueCount (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (pQueuePriorities (poked :: VkDeviceQueueCreateInfo))


-- ** VkDeviceQueueCreateFlags
-- | Opaque flag
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** vkDestroyDevice
foreign import ccall "vkDestroyDevice" vkDestroyDevice ::
  Device -> Ptr VkAllocationCallbacks -> IO ()

data VkPhysicalDevice_T
type PhysicalDevice = Ptr VkPhysicalDevice_T

data VkDevice_T
type Device = Ptr VkDevice_T


data VkDeviceCreateInfo =
  VkDeviceCreateInfo{ sType :: VkStructureType 
                    , pNext :: Ptr Void 
                    , flags :: VkDeviceCreateFlags 
                    , queueCreateInfoCount :: Word32 
                    , pQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo 
                    , enabledLayerCount :: Word32 
                    , ppEnabledLayerNames :: Ptr (Ptr CChar) 
                    , enabledExtensionCount :: Word32 
                    , ppEnabledExtensionNames :: Ptr (Ptr CChar) 
                    , pEnabledFeatures :: Ptr VkPhysicalDeviceFeatures 
                    }
  deriving (Eq)

instance Storable VkDeviceCreateInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
                                <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueCreateInfoCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (pQueueCreateInfos (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (enabledLayerCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (ppEnabledLayerNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (enabledExtensionCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (ppEnabledExtensionNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (pEnabledFeatures (poked :: VkDeviceCreateInfo))


