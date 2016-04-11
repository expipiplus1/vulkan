{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DeviceInitialization where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDeviceFeatures
                             , Device
                             , PhysicalDevice
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word8
                , Word32
                )
import Foreign.Ptr( Ptr
                  , FunPtr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkAllocationCallbacks
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlags
                              )
import Graphics.Vulkan.Image( VkImageType
                            , VkImageCreateFlags
                            , VkImageTiling
                            , VkImageUsageFlags
                            )
import Graphics.Vulkan.DeviceInitialization( VkQueueFlags
                                           , VkMemoryPropertyFlags
                                           , VkPhysicalDeviceProperties
                                           , VkApplicationInfo
                                           , VkPhysicalDeviceMemoryProperties
                                           , VkInstanceCreateFlags
                                           , VkPhysicalDeviceSparseProperties
                                           , VkFormatFeatureFlags
                                           , VkMemoryType
                                           , VkInstanceCreateInfo
                                           , VkMemoryHeap
                                           , VkImageFormatProperties
                                           , VkMemoryHeapFlags
                                           , VkPhysicalDeviceType
                                           , Instance
                                           , VkFormatProperties
                                           , PFN_vkVoidFunction
                                           , VkQueueFamilyProperties
                                           , VkPhysicalDeviceLimits
                                           )
import Graphics.Vulkan.Core( VkResult
                           , VkExtent3D
                           , VkDeviceSize
                           , VkBool32
                           , VkFlags
                           , VkFormat
                           , VkStructureType
                           )
import Foreign.C.Types( CSize
                      , CFloat
                      , CChar
                      )

-- ** VkPhysicalDeviceType

newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Storable)

instance Show VkPhysicalDeviceType where
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
  showsPrec p (VkPhysicalDeviceType x) = showParen (p >= 11) (showString "VkPhysicalDeviceType " . showsPrec 11 x)

instance Read VkPhysicalDeviceType where
  readPrec = parens ( choose [ ("VK_PHYSICAL_DEVICE_TYPE_OTHER", pure VK_PHYSICAL_DEVICE_TYPE_OTHER)
                             , ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU", pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU", pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_CPU", pure VK_PHYSICAL_DEVICE_TYPE_CPU)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPhysicalDeviceType")
                        v <- step readPrec
                        pure (VkPhysicalDeviceType v)
                        )
                    )


pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3

pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4


data VkInstanceCreateInfo =
  VkInstanceCreateInfo{ sType :: VkStructureType 
                      , pNext :: Ptr Void 
                      , flags :: VkInstanceCreateFlags 
                      , pApplicationInfo :: Ptr VkApplicationInfo 
                      , enabledLayerCount :: Word32 
                      , ppEnabledLayerNames :: Ptr (Ptr CChar) 
                      , enabledExtensionCount :: Word32 
                      , ppEnabledExtensionNames :: Ptr (Ptr CChar) 
                      }
  deriving (Eq)

instance Storable VkInstanceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkInstanceCreateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (pApplicationInfo (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (enabledLayerCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (ppEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (enabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (ppEnabledExtensionNames (poked :: VkInstanceCreateInfo))


-- ** vkGetPhysicalDeviceImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties ::
  PhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkImageTiling ->
        VkImageUsageFlags ->
          VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult

type PFN_vkVoidFunction = FunPtr (IO ())


data VkApplicationInfo =
  VkApplicationInfo{ sType :: VkStructureType 
                   , pNext :: Ptr Void 
                   , pApplicationName :: Ptr CChar 
                   , applicationVersion :: Word32 
                   , pEngineName :: Ptr CChar 
                   , engineVersion :: Word32 
                   , apiVersion :: Word32 
                   }
  deriving (Eq)

instance Storable VkApplicationInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkApplicationInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 16) (pApplicationName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 24) (applicationVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 32) (pEngineName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 40) (engineVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 44) (apiVersion (poked :: VkApplicationInfo))



data VkPhysicalDeviceLimits =
  VkPhysicalDeviceLimits{ maxImageDimension1D :: Word32 
                        , maxImageDimension2D :: Word32 
                        , maxImageDimension3D :: Word32 
                        , maxImageDimensionCube :: Word32 
                        , maxImageArrayLayers :: Word32 
                        , maxTexelBufferElements :: Word32 
                        , maxUniformBufferRange :: Word32 
                        , maxStorageBufferRange :: Word32 
                        , maxPushConstantsSize :: Word32 
                        , maxMemoryAllocationCount :: Word32 
                        , maxSamplerAllocationCount :: Word32 
                        , bufferImageGranularity :: VkDeviceSize 
                        , sparseAddressSpaceSize :: VkDeviceSize 
                        , maxBoundDescriptorSets :: Word32 
                        , maxPerStageDescriptorSamplers :: Word32 
                        , maxPerStageDescriptorUniformBuffers :: Word32 
                        , maxPerStageDescriptorStorageBuffers :: Word32 
                        , maxPerStageDescriptorSampledImages :: Word32 
                        , maxPerStageDescriptorStorageImages :: Word32 
                        , maxPerStageDescriptorInputAttachments :: Word32 
                        , maxPerStageResources :: Word32 
                        , maxDescriptorSetSamplers :: Word32 
                        , maxDescriptorSetUniformBuffers :: Word32 
                        , maxDescriptorSetUniformBuffersDynamic :: Word32 
                        , maxDescriptorSetStorageBuffers :: Word32 
                        , maxDescriptorSetStorageBuffersDynamic :: Word32 
                        , maxDescriptorSetSampledImages :: Word32 
                        , maxDescriptorSetStorageImages :: Word32 
                        , maxDescriptorSetInputAttachments :: Word32 
                        , maxVertexInputAttributes :: Word32 
                        , maxVertexInputBindings :: Word32 
                        , maxVertexInputAttributeOffset :: Word32 
                        , maxVertexInputBindingStride :: Word32 
                        , maxVertexOutputComponents :: Word32 
                        , maxTessellationGenerationLevel :: Word32 
                        , maxTessellationPatchSize :: Word32 
                        , maxTessellationControlPerVertexInputComponents :: Word32 
                        , maxTessellationControlPerVertexOutputComponents :: Word32 
                        , maxTessellationControlPerPatchOutputComponents :: Word32 
                        , maxTessellationControlTotalOutputComponents :: Word32 
                        , maxTessellationEvaluationInputComponents :: Word32 
                        , maxTessellationEvaluationOutputComponents :: Word32 
                        , maxGeometryShaderInvocations :: Word32 
                        , maxGeometryInputComponents :: Word32 
                        , maxGeometryOutputComponents :: Word32 
                        , maxGeometryOutputVertices :: Word32 
                        , maxGeometryTotalOutputComponents :: Word32 
                        , maxFragmentInputComponents :: Word32 
                        , maxFragmentOutputAttachments :: Word32 
                        , maxFragmentDualSrcAttachments :: Word32 
                        , maxFragmentCombinedOutputResources :: Word32 
                        , maxComputeSharedMemorySize :: Word32 
                        , maxComputeWorkGroupCount :: Vector 3 Word32 
                        , maxComputeWorkGroupInvocations :: Word32 
                        , maxComputeWorkGroupSize :: Vector 3 Word32 
                        , subPixelPrecisionBits :: Word32 
                        , subTexelPrecisionBits :: Word32 
                        , mipmapPrecisionBits :: Word32 
                        , maxDrawIndexedIndexValue :: Word32 
                        , maxDrawIndirectCount :: Word32 
                        , maxSamplerLodBias :: CFloat 
                        , maxSamplerAnisotropy :: CFloat 
                        , maxViewports :: Word32 
                        , maxViewportDimensions :: Vector 2 Word32 
                        , viewportBoundsRange :: Vector 2 CFloat 
                        , viewportSubPixelBits :: Word32 
                        , minMemoryMapAlignment :: CSize 
                        , minTexelBufferOffsetAlignment :: VkDeviceSize 
                        , minUniformBufferOffsetAlignment :: VkDeviceSize 
                        , minStorageBufferOffsetAlignment :: VkDeviceSize 
                        , minTexelOffset :: Int32 
                        , maxTexelOffset :: Word32 
                        , minTexelGatherOffset :: Int32 
                        , maxTexelGatherOffset :: Word32 
                        , minInterpolationOffset :: CFloat 
                        , maxInterpolationOffset :: CFloat 
                        , subPixelInterpolationOffsetBits :: Word32 
                        , maxFramebufferWidth :: Word32 
                        , maxFramebufferHeight :: Word32 
                        , maxFramebufferLayers :: Word32 
                        , framebufferColorSampleCounts :: VkSampleCountFlags 
                        , framebufferDepthSampleCounts :: VkSampleCountFlags 
                        , framebufferStencilSampleCounts :: VkSampleCountFlags 
                        , framebufferNoAttachmentsSampleCounts :: VkSampleCountFlags 
                        , maxColorAttachments :: Word32 
                        , sampledImageColorSampleCounts :: VkSampleCountFlags 
                        , sampledImageIntegerSampleCounts :: VkSampleCountFlags 
                        , sampledImageDepthSampleCounts :: VkSampleCountFlags 
                        , sampledImageStencilSampleCounts :: VkSampleCountFlags 
                        , storageImageSampleCounts :: VkSampleCountFlags 
                        , maxSampleMaskWords :: Word32 
                        , timestampComputeAndGraphics :: VkBool32 
                        , timestampPeriod :: CFloat 
                        , maxClipDistances :: Word32 
                        , maxCullDistances :: Word32 
                        , maxCombinedClipAndCullDistances :: Word32 
                        , discreteQueuePriorities :: Word32 
                        , pointSizeRange :: Vector 2 CFloat 
                        , lineWidthRange :: Vector 2 CFloat 
                        , pointSizeGranularity :: CFloat 
                        , lineWidthGranularity :: CFloat 
                        , strictLines :: VkBool32 
                        , standardSampleLocations :: VkBool32 
                        , optimalBufferCopyOffsetAlignment :: VkDeviceSize 
                        , optimalBufferCopyRowPitchAlignment :: VkDeviceSize 
                        , nonCoherentAtomSize :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkPhysicalDeviceLimits where
  sizeOf ~_ = 504
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceLimits <$> peek (ptr `plusPtr` 0)
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
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
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
                                    <*> peek (ptr `plusPtr` 220)
                                    <*> peek (ptr `plusPtr` 232)
                                    <*> peek (ptr `plusPtr` 236)
                                    <*> peek (ptr `plusPtr` 248)
                                    <*> peek (ptr `plusPtr` 252)
                                    <*> peek (ptr `plusPtr` 256)
                                    <*> peek (ptr `plusPtr` 260)
                                    <*> peek (ptr `plusPtr` 264)
                                    <*> peek (ptr `plusPtr` 268)
                                    <*> peek (ptr `plusPtr` 272)
                                    <*> peek (ptr `plusPtr` 276)
                                    <*> peek (ptr `plusPtr` 280)
                                    <*> peek (ptr `plusPtr` 288)
                                    <*> peek (ptr `plusPtr` 296)
                                    <*> peek (ptr `plusPtr` 304)
                                    <*> peek (ptr `plusPtr` 312)
                                    <*> peek (ptr `plusPtr` 320)
                                    <*> peek (ptr `plusPtr` 328)
                                    <*> peek (ptr `plusPtr` 336)
                                    <*> peek (ptr `plusPtr` 340)
                                    <*> peek (ptr `plusPtr` 344)
                                    <*> peek (ptr `plusPtr` 348)
                                    <*> peek (ptr `plusPtr` 352)
                                    <*> peek (ptr `plusPtr` 356)
                                    <*> peek (ptr `plusPtr` 360)
                                    <*> peek (ptr `plusPtr` 364)
                                    <*> peek (ptr `plusPtr` 368)
                                    <*> peek (ptr `plusPtr` 372)
                                    <*> peek (ptr `plusPtr` 376)
                                    <*> peek (ptr `plusPtr` 380)
                                    <*> peek (ptr `plusPtr` 384)
                                    <*> peek (ptr `plusPtr` 388)
                                    <*> peek (ptr `plusPtr` 392)
                                    <*> peek (ptr `plusPtr` 396)
                                    <*> peek (ptr `plusPtr` 400)
                                    <*> peek (ptr `plusPtr` 404)
                                    <*> peek (ptr `plusPtr` 408)
                                    <*> peek (ptr `plusPtr` 412)
                                    <*> peek (ptr `plusPtr` 416)
                                    <*> peek (ptr `plusPtr` 420)
                                    <*> peek (ptr `plusPtr` 424)
                                    <*> peek (ptr `plusPtr` 428)
                                    <*> peek (ptr `plusPtr` 432)
                                    <*> peek (ptr `plusPtr` 436)
                                    <*> peek (ptr `plusPtr` 440)
                                    <*> peek (ptr `plusPtr` 444)
                                    <*> peek (ptr `plusPtr` 452)
                                    <*> peek (ptr `plusPtr` 460)
                                    <*> peek (ptr `plusPtr` 464)
                                    <*> peek (ptr `plusPtr` 468)
                                    <*> peek (ptr `plusPtr` 472)
                                    <*> peek (ptr `plusPtr` 480)
                                    <*> peek (ptr `plusPtr` 488)
                                    <*> peek (ptr `plusPtr` 496)
  poke ptr poked = poke (ptr `plusPtr` 0) (maxImageDimension1D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (maxImageDimension2D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (maxImageDimension3D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 12) (maxImageDimensionCube (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 16) (maxImageArrayLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 20) (maxTexelBufferElements (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 24) (maxUniformBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 28) (maxStorageBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 32) (maxPushConstantsSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 36) (maxMemoryAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 40) (maxSamplerAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 48) (bufferImageGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 56) (sparseAddressSpaceSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 64) (maxBoundDescriptorSets (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 68) (maxPerStageDescriptorSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 72) (maxPerStageDescriptorUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 76) (maxPerStageDescriptorStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 80) (maxPerStageDescriptorSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 84) (maxPerStageDescriptorStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 88) (maxPerStageDescriptorInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 92) (maxPerStageResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 96) (maxDescriptorSetSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 100) (maxDescriptorSetUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 104) (maxDescriptorSetUniformBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 108) (maxDescriptorSetStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 112) (maxDescriptorSetStorageBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 116) (maxDescriptorSetSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 120) (maxDescriptorSetStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 124) (maxDescriptorSetInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 128) (maxVertexInputAttributes (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 132) (maxVertexInputBindings (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 136) (maxVertexInputAttributeOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 140) (maxVertexInputBindingStride (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 144) (maxVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 148) (maxTessellationGenerationLevel (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 152) (maxTessellationPatchSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 156) (maxTessellationControlPerVertexInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 160) (maxTessellationControlPerVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 164) (maxTessellationControlPerPatchOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 168) (maxTessellationControlTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 172) (maxTessellationEvaluationInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 176) (maxTessellationEvaluationOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 180) (maxGeometryShaderInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 184) (maxGeometryInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 188) (maxGeometryOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 192) (maxGeometryOutputVertices (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 196) (maxGeometryTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 200) (maxFragmentInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 204) (maxFragmentOutputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 208) (maxFragmentDualSrcAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 212) (maxFragmentCombinedOutputResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 216) (maxComputeSharedMemorySize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 220) (maxComputeWorkGroupCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 232) (maxComputeWorkGroupInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 236) (maxComputeWorkGroupSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 248) (subPixelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 252) (subTexelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 256) (mipmapPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 260) (maxDrawIndexedIndexValue (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 264) (maxDrawIndirectCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 268) (maxSamplerLodBias (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 272) (maxSamplerAnisotropy (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 276) (maxViewports (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 280) (maxViewportDimensions (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 288) (viewportBoundsRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 296) (viewportSubPixelBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 304) (minMemoryMapAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 312) (minTexelBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 320) (minUniformBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 328) (minStorageBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 336) (minTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 340) (maxTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 344) (minTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 348) (maxTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 352) (minInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 356) (maxInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 360) (subPixelInterpolationOffsetBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 364) (maxFramebufferWidth (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 368) (maxFramebufferHeight (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 372) (maxFramebufferLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 376) (framebufferColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 380) (framebufferDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 384) (framebufferStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 388) (framebufferNoAttachmentsSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 392) (maxColorAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 396) (sampledImageColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 400) (sampledImageIntegerSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 404) (sampledImageDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 408) (sampledImageStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 412) (storageImageSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 416) (maxSampleMaskWords (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 420) (timestampComputeAndGraphics (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 424) (timestampPeriod (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 428) (maxClipDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 432) (maxCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 436) (maxCombinedClipAndCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 440) (discreteQueuePriorities (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 444) (pointSizeRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 452) (lineWidthRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 460) (pointSizeGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 464) (lineWidthGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 468) (strictLines (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 472) (standardSampleLocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 480) (optimalBufferCopyOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 488) (optimalBufferCopyRowPitchAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 496) (nonCoherentAtomSize (poked :: VkPhysicalDeviceLimits))



data VkMemoryHeap =
  VkMemoryHeap{ size :: VkDeviceSize 
              , flags :: VkMemoryHeapFlags 
              }
  deriving (Eq)

instance Storable VkMemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (flags (poked :: VkMemoryHeap))


-- ** vkEnumeratePhysicalDevices
foreign import ccall "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices ::
  Instance -> Ptr Word32 -> Ptr PhysicalDevice -> IO VkResult

-- ** vkGetDeviceProcAddr
foreign import ccall "vkGetDeviceProcAddr" vkGetDeviceProcAddr ::
  Device -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** vkCreateInstance
foreign import ccall "vkCreateInstance" vkCreateInstance ::
  Ptr VkInstanceCreateInfo ->
  Ptr VkAllocationCallbacks -> Ptr Instance -> IO VkResult

-- ** VkFormatFeatureFlags

newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkFormatFeatureFlagBits
type VkFormatFeatureFlags = VkFormatFeatureFlagBits

instance Show VkFormatFeatureFlagBits where
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = showString "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
  showsPrec _ VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_SRC_BIT = showString "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_DST_BIT = showString "VK_FORMAT_FEATURE_BLIT_DST_BIT"
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
  
  showsPrec p (VkFormatFeatureFlagBits x) = showParen (p >= 11) (showString "VkFormatFeatureFlagBits " . showsPrec 11 x)

instance Read VkFormatFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT", pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT", pure VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT", pure VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT", pure VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT", pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT", pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT", pure VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT", pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT", pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT)
                             , ("VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT", pure VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_SRC_BIT", pure VK_FORMAT_FEATURE_BLIT_SRC_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_DST_BIT", pure VK_FORMAT_FEATURE_BLIT_DST_BIT)
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT", pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormatFeatureFlagBits")
                        v <- step readPrec
                        pure (VkFormatFeatureFlagBits v)
                        )
                    )

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 0x1
-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 0x2
-- | Format supports atomic operations in case it's used for storage images
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 0x4
-- | Format can be used for uniform texel buffers (TBOs)
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x8
-- | Format can be used for storage texel buffers (IBOs)
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x10
-- | Format supports atomic operations in case it's used for storage texel buffers
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 0x20
-- | Format can be used for vertex buffers (VBOs)
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 0x40
-- | Format can be used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x80
-- | Format supports blending in case it's used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 0x100
-- | Format can be used for depth/stencil attachment images
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x200
-- | Format can be used as the source image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 0x400
-- | Format can be used as the destination image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 0x800
-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 0x1000



data VkPhysicalDeviceMemoryProperties =
  VkPhysicalDeviceMemoryProperties{ memoryTypeCount :: Word32 
                                  , memoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType 
                                  , memoryHeapCount :: Word32 
                                  , memoryHeaps :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap 
                                  }
  deriving (Eq)

instance Storable VkPhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 260)
                                              <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (memoryTypeCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (memoryTypes (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (memoryHeapCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (memoryHeaps (poked :: VkPhysicalDeviceMemoryProperties))


data VkInstance_T
type Instance = Ptr VkInstance_T

-- ** VkMemoryHeapFlags

newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkMemoryHeapFlagBits
type VkMemoryHeapFlags = VkMemoryHeapFlagBits

instance Show VkMemoryHeapFlagBits where
  showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
  
  showsPrec p (VkMemoryHeapFlagBits x) = showParen (p >= 11) (showString "VkMemoryHeapFlagBits " . showsPrec 11 x)

instance Read VkMemoryHeapFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT", pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryHeapFlagBits")
                        v <- step readPrec
                        pure (VkMemoryHeapFlagBits v)
                        )
                    )

-- | If set, heap represents device memory
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x1



data VkQueueFamilyProperties =
  VkQueueFamilyProperties{ queueFlags :: VkQueueFlags 
                         , queueCount :: Word32 
                         , timestampValidBits :: Word32 
                         , minImageTransferGranularity :: VkExtent3D 
                         }
  deriving (Eq)

instance Storable VkQueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkQueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (queueFlags (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (queueCount (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (timestampValidBits (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (minImageTransferGranularity (poked :: VkQueueFamilyProperties))



data VkImageFormatProperties =
  VkImageFormatProperties{ maxExtent :: VkExtent3D 
                         , maxMipLevels :: Word32 
                         , maxArrayLayers :: Word32 
                         , sampleCounts :: VkSampleCountFlags 
                         , maxResourceSize :: VkDeviceSize 
                         }
  deriving (Eq)

instance Storable VkImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (maxExtent (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 12) (maxMipLevels (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (maxArrayLayers (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 20) (sampleCounts (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 24) (maxResourceSize (poked :: VkImageFormatProperties))



data VkPhysicalDeviceSparseProperties =
  VkPhysicalDeviceSparseProperties{ residencyStandard2DBlockShape :: VkBool32 
                                  , residencyStandard2DMultisampleBlockShape :: VkBool32 
                                  , residencyStandard3DBlockShape :: VkBool32 
                                  , residencyAlignedMipSize :: VkBool32 
                                  , residencyNonResidentStrict :: VkBool32 
                                  }
  deriving (Eq)

instance Storable VkPhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (residencyStandard2DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (residencyStandard2DMultisampleBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (residencyStandard3DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (residencyAlignedMipSize (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (residencyNonResidentStrict (poked :: VkPhysicalDeviceSparseProperties))


-- ** vkGetPhysicalDeviceFeatures
foreign import ccall "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures ::
  PhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()

-- ** vkGetPhysicalDeviceMemoryProperties
foreign import ccall "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties ::
  PhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ()


data VkPhysicalDeviceProperties =
  VkPhysicalDeviceProperties{ apiVersion :: Word32 
                            , driverVersion :: Word32 
                            , vendorID :: Word32 
                            , deviceID :: Word32 
                            , deviceType :: VkPhysicalDeviceType 
                            , deviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar 
                            , pipelineCacheUUID :: Vector VK_UUID_SIZE Word8 
                            , limits :: VkPhysicalDeviceLimits 
                            , sparseProperties :: VkPhysicalDeviceSparseProperties 
                            }
  deriving (Eq)

instance Storable VkPhysicalDeviceProperties where
  sizeOf ~_ = 824
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 12)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 276)
                                        <*> peek (ptr `plusPtr` 296)
                                        <*> peek (ptr `plusPtr` 800)
  poke ptr poked = poke (ptr `plusPtr` 0) (apiVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 4) (driverVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 8) (vendorID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 12) (deviceID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 16) (deviceType (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 20) (deviceName (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 276) (pipelineCacheUUID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 296) (limits (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 800) (sparseProperties (poked :: VkPhysicalDeviceProperties))


-- ** vkGetPhysicalDeviceQueueFamilyProperties
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ()


data VkMemoryType =
  VkMemoryType{ propertyFlags :: VkMemoryPropertyFlags 
              , heapIndex :: Word32 
              }
  deriving (Eq)

instance Storable VkMemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (propertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (heapIndex (poked :: VkMemoryType))


-- ** vkGetInstanceProcAddr
foreign import ccall "vkGetInstanceProcAddr" vkGetInstanceProcAddr ::
  Instance -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** VkMemoryPropertyFlags

newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkMemoryPropertyFlagBits
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits

instance Show VkMemoryPropertyFlagBits where
  showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
  
  showsPrec p (VkMemoryPropertyFlagBits x) = showParen (p >= 11) (showString "VkMemoryPropertyFlagBits " . showsPrec 11 x)

instance Read VkMemoryPropertyFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT", pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT", pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT", pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT", pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT)
                             , ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT", pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryPropertyFlagBits")
                        v <- step readPrec
                        pure (VkMemoryPropertyFlagBits v)
                        )
                    )

-- | If otherwise stated, then allocate memory on device
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x1
-- | Memory is mappable by host
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x2
-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x4
-- | Memory will be cached by the host
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x8
-- | Memory may be allocated by the driver when it is required
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x10


-- ** vkDestroyInstance
foreign import ccall "vkDestroyInstance" vkDestroyInstance ::
  Instance -> Ptr VkAllocationCallbacks -> IO ()

-- ** VkQueueFlags

newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkQueueFlagBits
type VkQueueFlags = VkQueueFlagBits

instance Show VkQueueFlagBits where
  showsPrec _ VK_QUEUE_GRAPHICS_BIT = showString "VK_QUEUE_GRAPHICS_BIT"
  showsPrec _ VK_QUEUE_COMPUTE_BIT = showString "VK_QUEUE_COMPUTE_BIT"
  showsPrec _ VK_QUEUE_TRANSFER_BIT = showString "VK_QUEUE_TRANSFER_BIT"
  showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT = showString "VK_QUEUE_SPARSE_BINDING_BIT"
  
  showsPrec p (VkQueueFlagBits x) = showParen (p >= 11) (showString "VkQueueFlagBits " . showsPrec 11 x)

instance Read VkQueueFlagBits where
  readPrec = parens ( choose [ ("VK_QUEUE_GRAPHICS_BIT", pure VK_QUEUE_GRAPHICS_BIT)
                             , ("VK_QUEUE_COMPUTE_BIT", pure VK_QUEUE_COMPUTE_BIT)
                             , ("VK_QUEUE_TRANSFER_BIT", pure VK_QUEUE_TRANSFER_BIT)
                             , ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueueFlagBits")
                        v <- step readPrec
                        pure (VkQueueFlagBits v)
                        )
                    )

-- | Queue supports graphics operations
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 0x1
-- | Queue supports compute operations
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 0x2
-- | Queue supports transfer operations
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 0x4
-- | Queue supports sparse resource memory management operations
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 0x8


-- ** vkGetPhysicalDeviceProperties
foreign import ccall "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties ::
  PhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ()

-- ** VkInstanceCreateFlags
-- | Opaque flag
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** vkGetPhysicalDeviceFormatProperties
foreign import ccall "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties ::
  PhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ()


data VkFormatProperties =
  VkFormatProperties{ linearTilingFeatures :: VkFormatFeatureFlags 
                    , optimalTilingFeatures :: VkFormatFeatureFlags 
                    , bufferFeatures :: VkFormatFeatureFlags 
                    }
  deriving (Eq)

instance Storable VkFormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkFormatProperties <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (linearTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 4) (optimalTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 8) (bufferFeatures (poked :: VkFormatProperties))


