{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DeviceInitialization where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDeviceFeatures(..)
                             , VkPhysicalDevice(..)
                             , VkDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word8
                , Word64
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
import Graphics.Vulkan.Constants( VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                                , VK_MAX_MEMORY_HEAPS
                                , VK_UUID_SIZE
                                , VK_MAX_MEMORY_TYPES
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
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              , VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( VkImageUsageFlags(..)
                            , VkImageType(..)
                            , VkImageUsageFlagBits(..)
                            , VkImageCreateFlags(..)
                            , VkImageTiling(..)
                            , VkImageCreateFlagBits(..)
                            )
import Graphics.Vulkan.Core( VkExtent3D(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )

-- ** VkPhysicalDeviceType

newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Ord, Storable)

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
  VkInstanceCreateInfo{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkFlags :: VkInstanceCreateFlags 
                      , vkPApplicationInfo :: Ptr VkApplicationInfo 
                      , vkEnabledLayerCount :: Word32 
                      , vkPpEnabledLayerNames :: Ptr (Ptr CChar) 
                      , vkEnabledExtensionCount :: Word32 
                      , vkPpEnabledExtensionNames :: Ptr (Ptr CChar) 
                      }
  deriving (Eq, Ord, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPApplicationInfo (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPpEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPpEnabledExtensionNames (poked :: VkInstanceCreateInfo))


-- ** vkGetPhysicalDeviceImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties ::
  VkPhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkImageTiling ->
        VkImageUsageFlags ->
          VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult

type PFN_vkVoidFunction = FunPtr (IO ())


data VkApplicationInfo =
  VkApplicationInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkPApplicationName :: Ptr CChar 
                   , vkApplicationVersion :: Word32 
                   , vkPEngineName :: Ptr CChar 
                   , vkEngineVersion :: Word32 
                   , vkApiVersion :: Word32 
                   }
  deriving (Eq, Ord, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 16) (vkPApplicationName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 24) (vkApplicationVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 32) (vkPEngineName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 40) (vkEngineVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 44) (vkApiVersion (poked :: VkApplicationInfo))



data VkPhysicalDeviceLimits =
  VkPhysicalDeviceLimits{ vkMaxImageDimension1D :: Word32 
                        , vkMaxImageDimension2D :: Word32 
                        , vkMaxImageDimension3D :: Word32 
                        , vkMaxImageDimensionCube :: Word32 
                        , vkMaxImageArrayLayers :: Word32 
                        , vkMaxTexelBufferElements :: Word32 
                        , vkMaxUniformBufferRange :: Word32 
                        , vkMaxStorageBufferRange :: Word32 
                        , vkMaxPushConstantsSize :: Word32 
                        , vkMaxMemoryAllocationCount :: Word32 
                        , vkMaxSamplerAllocationCount :: Word32 
                        , vkBufferImageGranularity :: VkDeviceSize 
                        , vkSparseAddressSpaceSize :: VkDeviceSize 
                        , vkMaxBoundDescriptorSets :: Word32 
                        , vkMaxPerStageDescriptorSamplers :: Word32 
                        , vkMaxPerStageDescriptorUniformBuffers :: Word32 
                        , vkMaxPerStageDescriptorStorageBuffers :: Word32 
                        , vkMaxPerStageDescriptorSampledImages :: Word32 
                        , vkMaxPerStageDescriptorStorageImages :: Word32 
                        , vkMaxPerStageDescriptorInputAttachments :: Word32 
                        , vkMaxPerStageResources :: Word32 
                        , vkMaxDescriptorSetSamplers :: Word32 
                        , vkMaxDescriptorSetUniformBuffers :: Word32 
                        , vkMaxDescriptorSetUniformBuffersDynamic :: Word32 
                        , vkMaxDescriptorSetStorageBuffers :: Word32 
                        , vkMaxDescriptorSetStorageBuffersDynamic :: Word32 
                        , vkMaxDescriptorSetSampledImages :: Word32 
                        , vkMaxDescriptorSetStorageImages :: Word32 
                        , vkMaxDescriptorSetInputAttachments :: Word32 
                        , vkMaxVertexInputAttributes :: Word32 
                        , vkMaxVertexInputBindings :: Word32 
                        , vkMaxVertexInputAttributeOffset :: Word32 
                        , vkMaxVertexInputBindingStride :: Word32 
                        , vkMaxVertexOutputComponents :: Word32 
                        , vkMaxTessellationGenerationLevel :: Word32 
                        , vkMaxTessellationPatchSize :: Word32 
                        , vkMaxTessellationControlPerVertexInputComponents :: Word32 
                        , vkMaxTessellationControlPerVertexOutputComponents :: Word32 
                        , vkMaxTessellationControlPerPatchOutputComponents :: Word32 
                        , vkMaxTessellationControlTotalOutputComponents :: Word32 
                        , vkMaxTessellationEvaluationInputComponents :: Word32 
                        , vkMaxTessellationEvaluationOutputComponents :: Word32 
                        , vkMaxGeometryShaderInvocations :: Word32 
                        , vkMaxGeometryInputComponents :: Word32 
                        , vkMaxGeometryOutputComponents :: Word32 
                        , vkMaxGeometryOutputVertices :: Word32 
                        , vkMaxGeometryTotalOutputComponents :: Word32 
                        , vkMaxFragmentInputComponents :: Word32 
                        , vkMaxFragmentOutputAttachments :: Word32 
                        , vkMaxFragmentDualSrcAttachments :: Word32 
                        , vkMaxFragmentCombinedOutputResources :: Word32 
                        , vkMaxComputeSharedMemorySize :: Word32 
                        , vkMaxComputeWorkGroupCount :: Vector 3 Word32 
                        , vkMaxComputeWorkGroupInvocations :: Word32 
                        , vkMaxComputeWorkGroupSize :: Vector 3 Word32 
                        , vkSubPixelPrecisionBits :: Word32 
                        , vkSubTexelPrecisionBits :: Word32 
                        , vkMipmapPrecisionBits :: Word32 
                        , vkMaxDrawIndexedIndexValue :: Word32 
                        , vkMaxDrawIndirectCount :: Word32 
                        , vkMaxSamplerLodBias :: CFloat 
                        , vkMaxSamplerAnisotropy :: CFloat 
                        , vkMaxViewports :: Word32 
                        , vkMaxViewportDimensions :: Vector 2 Word32 
                        , vkViewportBoundsRange :: Vector 2 CFloat 
                        , vkViewportSubPixelBits :: Word32 
                        , vkMinMemoryMapAlignment :: CSize 
                        , vkMinTexelBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinUniformBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinStorageBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinTexelOffset :: Int32 
                        , vkMaxTexelOffset :: Word32 
                        , vkMinTexelGatherOffset :: Int32 
                        , vkMaxTexelGatherOffset :: Word32 
                        , vkMinInterpolationOffset :: CFloat 
                        , vkMaxInterpolationOffset :: CFloat 
                        , vkSubPixelInterpolationOffsetBits :: Word32 
                        , vkMaxFramebufferWidth :: Word32 
                        , vkMaxFramebufferHeight :: Word32 
                        , vkMaxFramebufferLayers :: Word32 
                        , vkFramebufferColorSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferDepthSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferStencilSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferNoAttachmentsSampleCounts :: VkSampleCountFlags 
                        , vkMaxColorAttachments :: Word32 
                        , vkSampledImageColorSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageIntegerSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageDepthSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageStencilSampleCounts :: VkSampleCountFlags 
                        , vkStorageImageSampleCounts :: VkSampleCountFlags 
                        , vkMaxSampleMaskWords :: Word32 
                        , vkTimestampComputeAndGraphics :: VkBool32 
                        , vkTimestampPeriod :: CFloat 
                        , vkMaxClipDistances :: Word32 
                        , vkMaxCullDistances :: Word32 
                        , vkMaxCombinedClipAndCullDistances :: Word32 
                        , vkDiscreteQueuePriorities :: Word32 
                        , vkPointSizeRange :: Vector 2 CFloat 
                        , vkLineWidthRange :: Vector 2 CFloat 
                        , vkPointSizeGranularity :: CFloat 
                        , vkLineWidthGranularity :: CFloat 
                        , vkStrictLines :: VkBool32 
                        , vkStandardSampleLocations :: VkBool32 
                        , vkOptimalBufferCopyOffsetAlignment :: VkDeviceSize 
                        , vkOptimalBufferCopyRowPitchAlignment :: VkDeviceSize 
                        , vkNonCoherentAtomSize :: VkDeviceSize 
                        }
  deriving (Eq, Ord, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxImageDimension1D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (vkMaxImageDimension2D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (vkMaxImageDimension3D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 12) (vkMaxImageDimensionCube (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 16) (vkMaxImageArrayLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 20) (vkMaxTexelBufferElements (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 24) (vkMaxUniformBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 28) (vkMaxStorageBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 32) (vkMaxPushConstantsSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 36) (vkMaxMemoryAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 40) (vkMaxSamplerAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 48) (vkBufferImageGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 56) (vkSparseAddressSpaceSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 64) (vkMaxBoundDescriptorSets (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageDescriptorUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 76) (vkMaxPerStageDescriptorStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 80) (vkMaxPerStageDescriptorSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 84) (vkMaxPerStageDescriptorStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 88) (vkMaxPerStageDescriptorInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 92) (vkMaxPerStageResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUniformBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 108) (vkMaxDescriptorSetStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 112) (vkMaxDescriptorSetStorageBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 116) (vkMaxDescriptorSetSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 120) (vkMaxDescriptorSetStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 124) (vkMaxDescriptorSetInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 128) (vkMaxVertexInputAttributes (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 132) (vkMaxVertexInputBindings (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 136) (vkMaxVertexInputAttributeOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 140) (vkMaxVertexInputBindingStride (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 144) (vkMaxVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 148) (vkMaxTessellationGenerationLevel (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 152) (vkMaxTessellationPatchSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 156) (vkMaxTessellationControlPerVertexInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 160) (vkMaxTessellationControlPerVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 164) (vkMaxTessellationControlPerPatchOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 168) (vkMaxTessellationControlTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 172) (vkMaxTessellationEvaluationInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 176) (vkMaxTessellationEvaluationOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 180) (vkMaxGeometryShaderInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 184) (vkMaxGeometryInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 188) (vkMaxGeometryOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 192) (vkMaxGeometryOutputVertices (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 196) (vkMaxGeometryTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 200) (vkMaxFragmentInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 204) (vkMaxFragmentOutputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 208) (vkMaxFragmentDualSrcAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 212) (vkMaxFragmentCombinedOutputResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 216) (vkMaxComputeSharedMemorySize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 220) (vkMaxComputeWorkGroupCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 232) (vkMaxComputeWorkGroupInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 236) (vkMaxComputeWorkGroupSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 248) (vkSubPixelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 252) (vkSubTexelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 256) (vkMipmapPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 260) (vkMaxDrawIndexedIndexValue (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 264) (vkMaxDrawIndirectCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 268) (vkMaxSamplerLodBias (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 272) (vkMaxSamplerAnisotropy (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 276) (vkMaxViewports (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 280) (vkMaxViewportDimensions (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 288) (vkViewportBoundsRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 296) (vkViewportSubPixelBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 304) (vkMinMemoryMapAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 312) (vkMinTexelBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 320) (vkMinUniformBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 328) (vkMinStorageBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 336) (vkMinTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 340) (vkMaxTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 344) (vkMinTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 348) (vkMaxTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 352) (vkMinInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 356) (vkMaxInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 360) (vkSubPixelInterpolationOffsetBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 364) (vkMaxFramebufferWidth (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 368) (vkMaxFramebufferHeight (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 372) (vkMaxFramebufferLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 376) (vkFramebufferColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 380) (vkFramebufferDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 384) (vkFramebufferStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 388) (vkFramebufferNoAttachmentsSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 392) (vkMaxColorAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 396) (vkSampledImageColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 400) (vkSampledImageIntegerSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 404) (vkSampledImageDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 408) (vkSampledImageStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 412) (vkStorageImageSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 416) (vkMaxSampleMaskWords (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 420) (vkTimestampComputeAndGraphics (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 424) (vkTimestampPeriod (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 428) (vkMaxClipDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 432) (vkMaxCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 436) (vkMaxCombinedClipAndCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 440) (vkDiscreteQueuePriorities (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 444) (vkPointSizeRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 452) (vkLineWidthRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 460) (vkPointSizeGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 464) (vkLineWidthGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 468) (vkStrictLines (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 472) (vkStandardSampleLocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 480) (vkOptimalBufferCopyOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 488) (vkOptimalBufferCopyRowPitchAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 496) (vkNonCoherentAtomSize (poked :: VkPhysicalDeviceLimits))



data VkMemoryHeap =
  VkMemoryHeap{ vkSize :: VkDeviceSize 
              , vkFlags :: VkMemoryHeapFlags 
              }
  deriving (Eq, Ord, Show)

instance Storable VkMemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (vkFlags (poked :: VkMemoryHeap))


-- ** vkEnumeratePhysicalDevices
foreign import ccall "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices ::
  VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult

-- ** vkGetDeviceProcAddr
foreign import ccall "vkGetDeviceProcAddr" vkGetDeviceProcAddr ::
  VkDevice -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** vkCreateInstance
foreign import ccall "vkCreateInstance" vkCreateInstance ::
  Ptr VkInstanceCreateInfo ->
  Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult

-- ** VkFormatFeatureFlags

newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  VkPhysicalDeviceMemoryProperties{ vkMemoryTypeCount :: Word32 
                                  , vkMemoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType 
                                  , vkMemoryHeapCount :: Word32 
                                  , vkMemoryHeaps :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap 
                                  }
  deriving (Eq, Ord, Show)

instance Storable VkPhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 260)
                                              <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMemoryTypeCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkMemoryTypes (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (vkMemoryHeapCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (vkMemoryHeaps (poked :: VkPhysicalDeviceMemoryProperties))


data VkInstance_T
type VkInstance = Ptr VkInstance_T

-- ** VkMemoryHeapFlags

newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  VkQueueFamilyProperties{ vkQueueFlags :: VkQueueFlags 
                         , vkQueueCount :: Word32 
                         , vkTimestampValidBits :: Word32 
                         , vkMinImageTransferGranularity :: VkExtent3D 
                         }
  deriving (Eq, Ord, Show)

instance Storable VkQueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkQueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkQueueFlags (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (vkQueueCount (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (vkTimestampValidBits (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (vkMinImageTransferGranularity (poked :: VkQueueFamilyProperties))



data VkImageFormatProperties =
  VkImageFormatProperties{ vkMaxExtent :: VkExtent3D 
                         , vkMaxMipLevels :: Word32 
                         , vkMaxArrayLayers :: Word32 
                         , vkSampleCounts :: VkSampleCountFlags 
                         , vkMaxResourceSize :: VkDeviceSize 
                         }
  deriving (Eq, Ord, Show)

instance Storable VkImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxExtent (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 12) (vkMaxMipLevels (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxArrayLayers (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 20) (vkSampleCounts (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 24) (vkMaxResourceSize (poked :: VkImageFormatProperties))



data VkPhysicalDeviceSparseProperties =
  VkPhysicalDeviceSparseProperties{ vkResidencyStandard2DBlockShape :: VkBool32 
                                  , vkResidencyStandard2DMultisampleBlockShape :: VkBool32 
                                  , vkResidencyStandard3DBlockShape :: VkBool32 
                                  , vkResidencyAlignedMipSize :: VkBool32 
                                  , vkResidencyNonResidentStrict :: VkBool32 
                                  }
  deriving (Eq, Ord, Show)

instance Storable VkPhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResidencyStandard2DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (vkResidencyStandard2DMultisampleBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (vkResidencyStandard3DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (vkResidencyAlignedMipSize (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (vkResidencyNonResidentStrict (poked :: VkPhysicalDeviceSparseProperties))


-- ** vkGetPhysicalDeviceFeatures
foreign import ccall "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures ::
  VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()

-- ** vkGetPhysicalDeviceMemoryProperties
foreign import ccall "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties ::
  VkPhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ()


data VkPhysicalDeviceProperties =
  VkPhysicalDeviceProperties{ vkApiVersion :: Word32 
                            , vkDriverVersion :: Word32 
                            , vkVendorID :: Word32 
                            , vkDeviceID :: Word32 
                            , vkDeviceType :: VkPhysicalDeviceType 
                            , vkDeviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar 
                            , vkPipelineCacheUUID :: Vector VK_UUID_SIZE Word8 
                            , vkLimits :: VkPhysicalDeviceLimits 
                            , vkSparseProperties :: VkPhysicalDeviceSparseProperties 
                            }
  deriving (Eq, Ord, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkApiVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 4) (vkDriverVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 8) (vkVendorID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 12) (vkDeviceID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceType (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 20) (vkDeviceName (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 276) (vkPipelineCacheUUID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 296) (vkLimits (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 800) (vkSparseProperties (poked :: VkPhysicalDeviceProperties))


-- ** vkGetPhysicalDeviceQueueFamilyProperties
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties ::
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ()


data VkMemoryType =
  VkMemoryType{ vkPropertyFlags :: VkMemoryPropertyFlags 
              , vkHeapIndex :: Word32 
              }
  deriving (Eq, Ord, Show)

instance Storable VkMemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPropertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (vkHeapIndex (poked :: VkMemoryType))


-- ** vkGetInstanceProcAddr
foreign import ccall "vkGetInstanceProcAddr" vkGetInstanceProcAddr ::
  VkInstance -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** VkMemoryPropertyFlags

newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  VkInstance -> Ptr VkAllocationCallbacks -> IO ()

-- ** VkQueueFlags

newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ()

-- ** VkInstanceCreateFlags
-- | Opaque flag
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)

-- ** vkGetPhysicalDeviceFormatProperties
foreign import ccall "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties ::
  VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ()


data VkFormatProperties =
  VkFormatProperties{ vkLinearTilingFeatures :: VkFormatFeatureFlags 
                    , vkOptimalTilingFeatures :: VkFormatFeatureFlags 
                    , vkBufferFeatures :: VkFormatFeatureFlags 
                    }
  deriving (Eq, Ord, Show)

instance Storable VkFormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkFormatProperties <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLinearTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkOptimalTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkBufferFeatures (poked :: VkFormatProperties))


