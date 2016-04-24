{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DeviceInitialization where

import Data.Vector.Storable.Sized( Vector(..)
                                 )
import Graphics.Vulkan.Device( Device(..)
                             , PhysicalDevice(..)
                             , PhysicalDeviceFeatures(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word8(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , FunPtr(..)
                  , Ptr
                  , plusPtr
                  )
import Data.Int( Int32(..)
               , Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                                , VK_UUID_SIZE
                                , VK_MAX_MEMORY_TYPES
                                , VK_MAX_MEMORY_HEAPS
                                )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( SampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( ImageUsageFlags(..)
                            , ImageCreateFlags(..)
                            , ImageTiling(..)
                            , ImageType(..)
                            )
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Format(..)
                           , Extent3D(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CChar(..)
                      , CSize(..)
                      )

-- ** PhysicalDeviceType

newtype PhysicalDeviceType = PhysicalDeviceType Int32
  deriving (Eq, Storable)

instance Show PhysicalDeviceType where
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
  showsPrec p (PhysicalDeviceType x) = showParen (p >= 11) (showString "PhysicalDeviceType " . showsPrec 11 x)

instance Read PhysicalDeviceType where
  readPrec = parens ( choose [ ("VK_PHYSICAL_DEVICE_TYPE_OTHER", pure VK_PHYSICAL_DEVICE_TYPE_OTHER)
                             , ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU", pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU", pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_CPU", pure VK_PHYSICAL_DEVICE_TYPE_CPU)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PhysicalDeviceType")
                        v <- step readPrec
                        pure (PhysicalDeviceType v)
                        )
                    )


pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = PhysicalDeviceType 0

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = PhysicalDeviceType 1

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = PhysicalDeviceType 2

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = PhysicalDeviceType 3

pattern VK_PHYSICAL_DEVICE_TYPE_CPU = PhysicalDeviceType 4


data InstanceCreateInfo =
  InstanceCreateInfo{ sType :: StructureType 
                    , pNext :: Ptr Void 
                    , flags :: InstanceCreateFlags 
                    , pApplicationInfo :: Ptr ApplicationInfo 
                    , enabledLayerCount :: Word32 
                    , ppEnabledLayerNames :: Ptr (Ptr CChar) 
                    , enabledExtensionCount :: Word32 
                    , ppEnabledExtensionNames :: Ptr (Ptr CChar) 
                    }
  deriving (Eq)

instance Storable InstanceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = InstanceCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (pApplicationInfo (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (enabledLayerCount (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (ppEnabledLayerNames (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (enabledExtensionCount (poked :: InstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (ppEnabledExtensionNames (poked :: InstanceCreateInfo))


-- ** getPhysicalDeviceImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties" getPhysicalDeviceImageFormatProperties ::
  PhysicalDevice ->
  Format ->
    ImageType ->
      ImageTiling ->
        ImageUsageFlags ->
          ImageCreateFlags -> Ptr ImageFormatProperties -> IO Result

type PFN_vkVoidFunction = FunPtr (IO ())


data ApplicationInfo =
  ApplicationInfo{ sType :: StructureType 
                 , pNext :: Ptr Void 
                 , pApplicationName :: Ptr CChar 
                 , applicationVersion :: Word32 
                 , pEngineName :: Ptr CChar 
                 , engineVersion :: Word32 
                 , apiVersion :: Word32 
                 }
  deriving (Eq)

instance Storable ApplicationInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = ApplicationInfo <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 24)
                             <*> peek (ptr `plusPtr` 32)
                             <*> peek (ptr `plusPtr` 40)
                             <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 16) (pApplicationName (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 24) (applicationVersion (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 32) (pEngineName (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 40) (engineVersion (poked :: ApplicationInfo))
                *> poke (ptr `plusPtr` 44) (apiVersion (poked :: ApplicationInfo))



data PhysicalDeviceLimits =
  PhysicalDeviceLimits{ maxImageDimension1D :: Word32 
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
                      , bufferImageGranularity :: DeviceSize 
                      , sparseAddressSpaceSize :: DeviceSize 
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
                      , minTexelBufferOffsetAlignment :: DeviceSize 
                      , minUniformBufferOffsetAlignment :: DeviceSize 
                      , minStorageBufferOffsetAlignment :: DeviceSize 
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
                      , framebufferColorSampleCounts :: SampleCountFlags 
                      , framebufferDepthSampleCounts :: SampleCountFlags 
                      , framebufferStencilSampleCounts :: SampleCountFlags 
                      , framebufferNoAttachmentsSampleCounts :: SampleCountFlags 
                      , maxColorAttachments :: Word32 
                      , sampledImageColorSampleCounts :: SampleCountFlags 
                      , sampledImageIntegerSampleCounts :: SampleCountFlags 
                      , sampledImageDepthSampleCounts :: SampleCountFlags 
                      , sampledImageStencilSampleCounts :: SampleCountFlags 
                      , storageImageSampleCounts :: SampleCountFlags 
                      , maxSampleMaskWords :: Word32 
                      , timestampComputeAndGraphics :: Bool32 
                      , timestampPeriod :: CFloat 
                      , maxClipDistances :: Word32 
                      , maxCullDistances :: Word32 
                      , maxCombinedClipAndCullDistances :: Word32 
                      , discreteQueuePriorities :: Word32 
                      , pointSizeRange :: Vector 2 CFloat 
                      , lineWidthRange :: Vector 2 CFloat 
                      , pointSizeGranularity :: CFloat 
                      , lineWidthGranularity :: CFloat 
                      , strictLines :: Bool32 
                      , standardSampleLocations :: Bool32 
                      , optimalBufferCopyOffsetAlignment :: DeviceSize 
                      , optimalBufferCopyRowPitchAlignment :: DeviceSize 
                      , nonCoherentAtomSize :: DeviceSize 
                      }
  deriving (Eq)

instance Storable PhysicalDeviceLimits where
  sizeOf ~_ = 504
  alignment ~_ = 8
  peek ptr = PhysicalDeviceLimits <$> peek (ptr `plusPtr` 0)
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
  poke ptr poked = poke (ptr `plusPtr` 0) (maxImageDimension1D (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (maxImageDimension2D (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (maxImageDimension3D (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 12) (maxImageDimensionCube (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 16) (maxImageArrayLayers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 20) (maxTexelBufferElements (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 24) (maxUniformBufferRange (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 28) (maxStorageBufferRange (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 32) (maxPushConstantsSize (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 36) (maxMemoryAllocationCount (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 40) (maxSamplerAllocationCount (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 48) (bufferImageGranularity (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 56) (sparseAddressSpaceSize (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 64) (maxBoundDescriptorSets (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 68) (maxPerStageDescriptorSamplers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 72) (maxPerStageDescriptorUniformBuffers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 76) (maxPerStageDescriptorStorageBuffers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 80) (maxPerStageDescriptorSampledImages (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 84) (maxPerStageDescriptorStorageImages (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 88) (maxPerStageDescriptorInputAttachments (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 92) (maxPerStageResources (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 96) (maxDescriptorSetSamplers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 100) (maxDescriptorSetUniformBuffers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 104) (maxDescriptorSetUniformBuffersDynamic (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 108) (maxDescriptorSetStorageBuffers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 112) (maxDescriptorSetStorageBuffersDynamic (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 116) (maxDescriptorSetSampledImages (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 120) (maxDescriptorSetStorageImages (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 124) (maxDescriptorSetInputAttachments (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 128) (maxVertexInputAttributes (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 132) (maxVertexInputBindings (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 136) (maxVertexInputAttributeOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 140) (maxVertexInputBindingStride (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 144) (maxVertexOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 148) (maxTessellationGenerationLevel (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 152) (maxTessellationPatchSize (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 156) (maxTessellationControlPerVertexInputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 160) (maxTessellationControlPerVertexOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 164) (maxTessellationControlPerPatchOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 168) (maxTessellationControlTotalOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 172) (maxTessellationEvaluationInputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 176) (maxTessellationEvaluationOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 180) (maxGeometryShaderInvocations (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 184) (maxGeometryInputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 188) (maxGeometryOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 192) (maxGeometryOutputVertices (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 196) (maxGeometryTotalOutputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 200) (maxFragmentInputComponents (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 204) (maxFragmentOutputAttachments (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 208) (maxFragmentDualSrcAttachments (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 212) (maxFragmentCombinedOutputResources (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 216) (maxComputeSharedMemorySize (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 220) (maxComputeWorkGroupCount (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 232) (maxComputeWorkGroupInvocations (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 236) (maxComputeWorkGroupSize (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 248) (subPixelPrecisionBits (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 252) (subTexelPrecisionBits (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 256) (mipmapPrecisionBits (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 260) (maxDrawIndexedIndexValue (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 264) (maxDrawIndirectCount (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 268) (maxSamplerLodBias (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 272) (maxSamplerAnisotropy (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 276) (maxViewports (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 280) (maxViewportDimensions (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 288) (viewportBoundsRange (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 296) (viewportSubPixelBits (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 304) (minMemoryMapAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 312) (minTexelBufferOffsetAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 320) (minUniformBufferOffsetAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 328) (minStorageBufferOffsetAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 336) (minTexelOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 340) (maxTexelOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 344) (minTexelGatherOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 348) (maxTexelGatherOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 352) (minInterpolationOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 356) (maxInterpolationOffset (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 360) (subPixelInterpolationOffsetBits (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 364) (maxFramebufferWidth (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 368) (maxFramebufferHeight (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 372) (maxFramebufferLayers (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 376) (framebufferColorSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 380) (framebufferDepthSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 384) (framebufferStencilSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 388) (framebufferNoAttachmentsSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 392) (maxColorAttachments (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 396) (sampledImageColorSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 400) (sampledImageIntegerSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 404) (sampledImageDepthSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 408) (sampledImageStencilSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 412) (storageImageSampleCounts (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 416) (maxSampleMaskWords (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 420) (timestampComputeAndGraphics (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 424) (timestampPeriod (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 428) (maxClipDistances (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 432) (maxCullDistances (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 436) (maxCombinedClipAndCullDistances (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 440) (discreteQueuePriorities (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 444) (pointSizeRange (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 452) (lineWidthRange (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 460) (pointSizeGranularity (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 464) (lineWidthGranularity (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 468) (strictLines (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 472) (standardSampleLocations (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 480) (optimalBufferCopyOffsetAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 488) (optimalBufferCopyRowPitchAlignment (poked :: PhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 496) (nonCoherentAtomSize (poked :: PhysicalDeviceLimits))



data MemoryHeap =
  MemoryHeap{ size :: DeviceSize 
            , flags :: MemoryHeapFlags 
            }
  deriving (Eq)

instance Storable MemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = MemoryHeap <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: MemoryHeap))
                *> poke (ptr `plusPtr` 8) (flags (poked :: MemoryHeap))


-- ** enumeratePhysicalDevices
foreign import ccall "vkEnumeratePhysicalDevices" enumeratePhysicalDevices ::
  Instance -> Ptr Word32 -> Ptr PhysicalDevice -> IO Result

-- ** getDeviceProcAddr
foreign import ccall "vkGetDeviceProcAddr" getDeviceProcAddr ::
  Device -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** createInstance
foreign import ccall "vkCreateInstance" createInstance ::
  Ptr InstanceCreateInfo ->
  Ptr AllocationCallbacks -> Ptr Instance -> IO Result

-- ** VkFormatFeatureFlags

newtype FormatFeatureFlags = FormatFeatureFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show FormatFeatureFlags where
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
  
  showsPrec p (FormatFeatureFlags x) = showParen (p >= 11) (showString "FormatFeatureFlags " . showsPrec 11 x)

instance Read FormatFeatureFlags where
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
                        expectP (Ident "FormatFeatureFlags")
                        v <- step readPrec
                        pure (FormatFeatureFlags v)
                        )
                    )

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = FormatFeatureFlags 0x1
-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = FormatFeatureFlags 0x2
-- | Format supports atomic operations in case it's used for storage images
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = FormatFeatureFlags 0x4
-- | Format can be used for uniform texel buffers (TBOs)
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = FormatFeatureFlags 0x8
-- | Format can be used for storage texel buffers (IBOs)
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = FormatFeatureFlags 0x10
-- | Format supports atomic operations in case it's used for storage texel buffers
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = FormatFeatureFlags 0x20
-- | Format can be used for vertex buffers (VBOs)
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = FormatFeatureFlags 0x40
-- | Format can be used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = FormatFeatureFlags 0x80
-- | Format supports blending in case it's used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = FormatFeatureFlags 0x100
-- | Format can be used for depth/stencil attachment images
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = FormatFeatureFlags 0x200
-- | Format can be used as the source image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = FormatFeatureFlags 0x400
-- | Format can be used as the destination image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = FormatFeatureFlags 0x800
-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = FormatFeatureFlags 0x1000



data PhysicalDeviceMemoryProperties =
  PhysicalDeviceMemoryProperties{ memoryTypeCount :: Word32 
                                , memoryTypes :: Vector VK_MAX_MEMORY_TYPES MemoryType 
                                , memoryHeapCount :: Word32 
                                , memoryHeaps :: Vector VK_MAX_MEMORY_HEAPS MemoryHeap 
                                }
  deriving (Eq)

instance Storable PhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek ptr = PhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 4)
                                            <*> peek (ptr `plusPtr` 260)
                                            <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (memoryTypeCount (poked :: PhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (memoryTypes (poked :: PhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (memoryHeapCount (poked :: PhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (memoryHeaps (poked :: PhysicalDeviceMemoryProperties))


data VkInstance_T
type Instance = Ptr VkInstance_T

-- ** VkMemoryHeapFlags

newtype MemoryHeapFlags = MemoryHeapFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show MemoryHeapFlags where
  showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
  
  showsPrec p (MemoryHeapFlags x) = showParen (p >= 11) (showString "MemoryHeapFlags " . showsPrec 11 x)

instance Read MemoryHeapFlags where
  readPrec = parens ( choose [ ("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT", pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "MemoryHeapFlags")
                        v <- step readPrec
                        pure (MemoryHeapFlags v)
                        )
                    )

-- | If set, heap represents device memory
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = MemoryHeapFlags 0x1



data QueueFamilyProperties =
  QueueFamilyProperties{ queueFlags :: QueueFlags 
                       , queueCount :: Word32 
                       , timestampValidBits :: Word32 
                       , minImageTransferGranularity :: Extent3D 
                       }
  deriving (Eq)

instance Storable QueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = QueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (queueFlags (poked :: QueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (queueCount (poked :: QueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (timestampValidBits (poked :: QueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (minImageTransferGranularity (poked :: QueueFamilyProperties))



data ImageFormatProperties =
  ImageFormatProperties{ maxExtent :: Extent3D 
                       , maxMipLevels :: Word32 
                       , maxArrayLayers :: Word32 
                       , sampleCounts :: SampleCountFlags 
                       , maxResourceSize :: DeviceSize 
                       }
  deriving (Eq)

instance Storable ImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = ImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 12)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (maxExtent (poked :: ImageFormatProperties))
                *> poke (ptr `plusPtr` 12) (maxMipLevels (poked :: ImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (maxArrayLayers (poked :: ImageFormatProperties))
                *> poke (ptr `plusPtr` 20) (sampleCounts (poked :: ImageFormatProperties))
                *> poke (ptr `plusPtr` 24) (maxResourceSize (poked :: ImageFormatProperties))



data PhysicalDeviceSparseProperties =
  PhysicalDeviceSparseProperties{ residencyStandard2DBlockShape :: Bool32 
                                , residencyStandard2DMultisampleBlockShape :: Bool32 
                                , residencyStandard3DBlockShape :: Bool32 
                                , residencyAlignedMipSize :: Bool32 
                                , residencyNonResidentStrict :: Bool32 
                                }
  deriving (Eq)

instance Storable PhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = PhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 4)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 12)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (residencyStandard2DBlockShape (poked :: PhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (residencyStandard2DMultisampleBlockShape (poked :: PhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (residencyStandard3DBlockShape (poked :: PhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (residencyAlignedMipSize (poked :: PhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (residencyNonResidentStrict (poked :: PhysicalDeviceSparseProperties))


-- ** getPhysicalDeviceFeatures
foreign import ccall "vkGetPhysicalDeviceFeatures" getPhysicalDeviceFeatures ::
  PhysicalDevice -> Ptr PhysicalDeviceFeatures -> IO ()

-- ** getPhysicalDeviceMemoryProperties
foreign import ccall "vkGetPhysicalDeviceMemoryProperties" getPhysicalDeviceMemoryProperties ::
  PhysicalDevice -> Ptr PhysicalDeviceMemoryProperties -> IO ()


data PhysicalDeviceProperties =
  PhysicalDeviceProperties{ apiVersion :: Word32 
                          , driverVersion :: Word32 
                          , vendorID :: Word32 
                          , deviceID :: Word32 
                          , deviceType :: PhysicalDeviceType 
                          , deviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar 
                          , pipelineCacheUUID :: Vector VK_UUID_SIZE Word8 
                          , limits :: PhysicalDeviceLimits 
                          , sparseProperties :: PhysicalDeviceSparseProperties 
                          }
  deriving (Eq)

instance Storable PhysicalDeviceProperties where
  sizeOf ~_ = 824
  alignment ~_ = 8
  peek ptr = PhysicalDeviceProperties <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 276)
                                      <*> peek (ptr `plusPtr` 296)
                                      <*> peek (ptr `plusPtr` 800)
  poke ptr poked = poke (ptr `plusPtr` 0) (apiVersion (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 4) (driverVersion (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 8) (vendorID (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 12) (deviceID (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 16) (deviceType (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 20) (deviceName (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 276) (pipelineCacheUUID (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 296) (limits (poked :: PhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 800) (sparseProperties (poked :: PhysicalDeviceProperties))


-- ** getPhysicalDeviceQueueFamilyProperties
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties" getPhysicalDeviceQueueFamilyProperties ::
  PhysicalDevice -> Ptr Word32 -> Ptr QueueFamilyProperties -> IO ()


data MemoryType =
  MemoryType{ propertyFlags :: MemoryPropertyFlags 
            , heapIndex :: Word32 
            }
  deriving (Eq)

instance Storable MemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = MemoryType <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (propertyFlags (poked :: MemoryType))
                *> poke (ptr `plusPtr` 4) (heapIndex (poked :: MemoryType))


-- ** getInstanceProcAddr
foreign import ccall "vkGetInstanceProcAddr" getInstanceProcAddr ::
  Instance -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** VkMemoryPropertyFlags

newtype MemoryPropertyFlags = MemoryPropertyFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show MemoryPropertyFlags where
  showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
  
  showsPrec p (MemoryPropertyFlags x) = showParen (p >= 11) (showString "MemoryPropertyFlags " . showsPrec 11 x)

instance Read MemoryPropertyFlags where
  readPrec = parens ( choose [ ("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT", pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT", pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT", pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT", pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT)
                             , ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT", pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "MemoryPropertyFlags")
                        v <- step readPrec
                        pure (MemoryPropertyFlags v)
                        )
                    )

-- | If otherwise stated, then allocate memory on device
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = MemoryPropertyFlags 0x1
-- | Memory is mappable by host
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = MemoryPropertyFlags 0x2
-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = MemoryPropertyFlags 0x4
-- | Memory will be cached by the host
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = MemoryPropertyFlags 0x8
-- | Memory may be allocated by the driver when it is required
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = MemoryPropertyFlags 0x10


-- ** destroyInstance
foreign import ccall "vkDestroyInstance" destroyInstance ::
  Instance -> Ptr AllocationCallbacks -> IO ()

-- ** VkQueueFlags

newtype QueueFlags = QueueFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show QueueFlags where
  showsPrec _ VK_QUEUE_GRAPHICS_BIT = showString "VK_QUEUE_GRAPHICS_BIT"
  showsPrec _ VK_QUEUE_COMPUTE_BIT = showString "VK_QUEUE_COMPUTE_BIT"
  showsPrec _ VK_QUEUE_TRANSFER_BIT = showString "VK_QUEUE_TRANSFER_BIT"
  showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT = showString "VK_QUEUE_SPARSE_BINDING_BIT"
  
  showsPrec p (QueueFlags x) = showParen (p >= 11) (showString "QueueFlags " . showsPrec 11 x)

instance Read QueueFlags where
  readPrec = parens ( choose [ ("VK_QUEUE_GRAPHICS_BIT", pure VK_QUEUE_GRAPHICS_BIT)
                             , ("VK_QUEUE_COMPUTE_BIT", pure VK_QUEUE_COMPUTE_BIT)
                             , ("VK_QUEUE_TRANSFER_BIT", pure VK_QUEUE_TRANSFER_BIT)
                             , ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "QueueFlags")
                        v <- step readPrec
                        pure (QueueFlags v)
                        )
                    )

-- | Queue supports graphics operations
pattern VK_QUEUE_GRAPHICS_BIT = QueueFlags 0x1
-- | Queue supports compute operations
pattern VK_QUEUE_COMPUTE_BIT = QueueFlags 0x2
-- | Queue supports transfer operations
pattern VK_QUEUE_TRANSFER_BIT = QueueFlags 0x4
-- | Queue supports sparse resource memory management operations
pattern VK_QUEUE_SPARSE_BINDING_BIT = QueueFlags 0x8


-- ** getPhysicalDeviceProperties
foreign import ccall "vkGetPhysicalDeviceProperties" getPhysicalDeviceProperties ::
  PhysicalDevice -> Ptr PhysicalDeviceProperties -> IO ()

-- ** InstanceCreateFlags
-- | Opaque flag
newtype InstanceCreateFlags = InstanceCreateFlags Flags
  deriving (Eq, Storable)

-- ** getPhysicalDeviceFormatProperties
foreign import ccall "vkGetPhysicalDeviceFormatProperties" getPhysicalDeviceFormatProperties ::
  PhysicalDevice -> Format -> Ptr FormatProperties -> IO ()


data FormatProperties =
  FormatProperties{ linearTilingFeatures :: FormatFeatureFlags 
                  , optimalTilingFeatures :: FormatFeatureFlags 
                  , bufferFeatures :: FormatFeatureFlags 
                  }
  deriving (Eq)

instance Storable FormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = FormatProperties <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (linearTilingFeatures (poked :: FormatProperties))
                *> poke (ptr `plusPtr` 4) (optimalTilingFeatures (poked :: FormatProperties))
                *> poke (ptr `plusPtr` 8) (bufferFeatures (poked :: FormatProperties))


