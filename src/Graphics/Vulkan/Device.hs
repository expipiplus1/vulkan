{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Device where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
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
  VkPhysicalDevice ->
  Ptr VkDeviceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult


data VkPhysicalDeviceFeatures =
  VkPhysicalDeviceFeatures{ vkRobustBufferAccess :: VkBool32 
                          , vkFullDrawIndexUint32 :: VkBool32 
                          , vkImageCubeArray :: VkBool32 
                          , vkIndependentBlend :: VkBool32 
                          , vkGeometryShader :: VkBool32 
                          , vkTessellationShader :: VkBool32 
                          , vkSampleRateShading :: VkBool32 
                          , vkDualSrcBlend :: VkBool32 
                          , vkLogicOp :: VkBool32 
                          , vkMultiDrawIndirect :: VkBool32 
                          , vkDrawIndirectFirstInstance :: VkBool32 
                          , vkDepthClamp :: VkBool32 
                          , vkDepthBiasClamp :: VkBool32 
                          , vkFillModeNonSolid :: VkBool32 
                          , vkDepthBounds :: VkBool32 
                          , vkWideLines :: VkBool32 
                          , vkLargePoints :: VkBool32 
                          , vkAlphaToOne :: VkBool32 
                          , vkMultiViewport :: VkBool32 
                          , vkSamplerAnisotropy :: VkBool32 
                          , vkTextureCompressionETC2 :: VkBool32 
                          , vkTextureCompressionASTC_LDR :: VkBool32 
                          , vkTextureCompressionBC :: VkBool32 
                          , vkOcclusionQueryPrecise :: VkBool32 
                          , vkPipelineStatisticsQuery :: VkBool32 
                          , vkVertexPipelineStoresAndAtomics :: VkBool32 
                          , vkFragmentStoresAndAtomics :: VkBool32 
                          , vkShaderTessellationAndGeometryPointSize :: VkBool32 
                          , vkShaderImageGatherExtended :: VkBool32 
                          , vkShaderStorageImageExtendedFormats :: VkBool32 
                          , vkShaderStorageImageMultisample :: VkBool32 
                          , vkShaderStorageImageReadWithoutFormat :: VkBool32 
                          , vkShaderStorageImageWriteWithoutFormat :: VkBool32 
                          , vkShaderUniformBufferArrayDynamicIndexing :: VkBool32 
                          , vkShaderSampledImageArrayDynamicIndexing :: VkBool32 
                          , vkShaderStorageBufferArrayDynamicIndexing :: VkBool32 
                          , vkShaderStorageImageArrayDynamicIndexing :: VkBool32 
                          , vkShaderClipDistance :: VkBool32 
                          , vkShaderCullDistance :: VkBool32 
                          , vkShaderFloat64 :: VkBool32 
                          , vkShaderInt64 :: VkBool32 
                          , vkShaderInt16 :: VkBool32 
                          , vkShaderResourceResidency :: VkBool32 
                          , vkShaderResourceMinLod :: VkBool32 
                          , vkSparseBinding :: VkBool32 
                          , vkSparseResidencyBuffer :: VkBool32 
                          , vkSparseResidencyImage2D :: VkBool32 
                          , vkSparseResidencyImage3D :: VkBool32 
                          , vkSparseResidency2Samples :: VkBool32 
                          , vkSparseResidency4Samples :: VkBool32 
                          , vkSparseResidency8Samples :: VkBool32 
                          , vkSparseResidency16Samples :: VkBool32 
                          , vkSparseResidencyAliased :: VkBool32 
                          , vkVariableMultisampleRate :: VkBool32 
                          , vkInheritedQueries :: VkBool32 
                          }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRobustBufferAccess (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 4) (vkFullDrawIndexUint32 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 8) (vkImageCubeArray (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 12) (vkIndependentBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 16) (vkGeometryShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 20) (vkTessellationShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 24) (vkSampleRateShading (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 28) (vkDualSrcBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 32) (vkLogicOp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 36) (vkMultiDrawIndirect (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 40) (vkDrawIndirectFirstInstance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 44) (vkDepthClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 52) (vkFillModeNonSolid (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 56) (vkDepthBounds (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 60) (vkWideLines (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 64) (vkLargePoints (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 68) (vkAlphaToOne (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 72) (vkMultiViewport (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 76) (vkSamplerAnisotropy (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 80) (vkTextureCompressionETC2 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 84) (vkTextureCompressionASTC_LDR (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 88) (vkTextureCompressionBC (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 92) (vkOcclusionQueryPrecise (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 96) (vkPipelineStatisticsQuery (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 100) (vkVertexPipelineStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 104) (vkFragmentStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 108) (vkShaderTessellationAndGeometryPointSize (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 112) (vkShaderImageGatherExtended (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 116) (vkShaderStorageImageExtendedFormats (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 120) (vkShaderStorageImageMultisample (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 124) (vkShaderStorageImageReadWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 128) (vkShaderStorageImageWriteWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 132) (vkShaderUniformBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 136) (vkShaderSampledImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 140) (vkShaderStorageBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 144) (vkShaderStorageImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 148) (vkShaderClipDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 152) (vkShaderCullDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 156) (vkShaderFloat64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (vkShaderInt64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (vkShaderInt16 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (vkShaderResourceResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (vkShaderResourceMinLod (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (vkSparseBinding (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (vkSparseResidencyBuffer (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (vkSparseResidencyImage2D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (vkSparseResidencyImage3D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (vkSparseResidency2Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (vkSparseResidency4Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (vkSparseResidency8Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (vkSparseResidency16Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (vkSparseResidencyAliased (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (vkVariableMultisampleRate (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (vkInheritedQueries (poked :: VkPhysicalDeviceFeatures))


-- ** VkDeviceCreateFlags
-- | Opaque flag
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)


data VkDeviceQueueCreateInfo =
  VkDeviceQueueCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkDeviceQueueCreateFlags 
                         , vkQueueFamilyIndex :: Word32 
                         , vkQueueCount :: Word32 
                         , vkPQueuePriorities :: Ptr CFloat 
                         }
  deriving (Eq, Ord)

instance Storable VkDeviceQueueCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueueCount (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPQueuePriorities (poked :: VkDeviceQueueCreateInfo))


-- ** VkDeviceQueueCreateFlags
-- | Opaque flag
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- ** vkDestroyDevice
foreign import ccall "vkDestroyDevice" vkDestroyDevice ::
  VkDevice -> Ptr VkAllocationCallbacks -> IO ()

data VkPhysicalDevice_T
type VkPhysicalDevice = Ptr VkPhysicalDevice_T

data VkDevice_T
type VkDevice = Ptr VkDevice_T


data VkDeviceCreateInfo =
  VkDeviceCreateInfo{ vkSType :: VkStructureType 
                    , vkPNext :: Ptr Void 
                    , vkFlags :: VkDeviceCreateFlags 
                    , vkQueueCreateInfoCount :: Word32 
                    , vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo 
                    , vkEnabledLayerCount :: Word32 
                    , vkPpEnabledLayerNames :: Ptr (Ptr CChar) 
                    , vkEnabledExtensionCount :: Word32 
                    , vkPpEnabledExtensionNames :: Ptr (Ptr CChar) 
                    , vkPEnabledFeatures :: Ptr VkPhysicalDeviceFeatures 
                    }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueCreateInfoCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPQueueCreateInfos (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPpEnabledLayerNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPpEnabledExtensionNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPEnabledFeatures (poked :: VkDeviceCreateInfo))


