{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo(..)
  , VkChromaLocation(..)
  , pattern VK_CHROMA_LOCATION_COSITED_EVEN
  , pattern VK_CHROMA_LOCATION_MIDPOINT
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversion
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  , VkSamplerYcbcrConversionInfo(..)
  , VkSamplerYcbcrModelConversion(..)
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
  , pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
  , VkSamplerYcbcrRange(..)
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL
  , pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW
  , FN_vkCreateSamplerYcbcrConversion
  , PFN_vkCreateSamplerYcbcrConversion
  , vkCreateSamplerYcbcrConversion
  , FN_vkDestroySamplerYcbcrConversion
  , PFN_vkDestroySamplerYcbcrConversion
  , vkDestroySamplerYcbcrConversion
  , pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_B16G16R16G16_422_UNORM
  , pattern VK_FORMAT_B8G8R8G8_422_UNORM
  , pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
  , pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
  , pattern VK_FORMAT_G16B16G16R16_422_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM
  , pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM
  , pattern VK_FORMAT_G8B8G8R8_422_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM
  , pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM
  , pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
  , pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16
  , pattern VK_FORMAT_R10X6_UNORM_PACK16
  , pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
  , pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16
  , pattern VK_FORMAT_R12X4_UNORM_PACK16
  , pattern VK_IMAGE_ASPECT_PLANE_0_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_1_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_2_BIT
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  , pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkBindImagePlaneMemoryInfo - Structure specifying how to bind an image
-- plane to memory
--
-- == Valid Usage
--
-- -   If the image’s tiling is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image. (That is, @planeAspect@ /must/ be
--     'VK_IMAGE_ASPECT_PLANE_0_BIT' or 'VK_IMAGE_ASPECT_PLANE_1_BIT' for
--     “@_2PLANE@” formats and @planeAspect@ /must/ be
--     'VK_IMAGE_ASPECT_PLANE_0_BIT', 'VK_IMAGE_ASPECT_PLANE_1_BIT', or
--     'VK_IMAGE_ASPECT_PLANE_2_BIT' for “@_3PLANE@” formats.)
--
-- -   A single call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     /must/ bind all or none of the planes of an image (i.e. bindings to
--     all planes of an image /must/ be made in a single
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
--     call), as separate bindings
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkBindImagePlaneMemoryInfo = VkBindImagePlaneMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @planeAspect@ is the aspect of the disjoint image plane to bind.
  vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkBindImagePlaneMemoryInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBindImagePlaneMemoryInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImagePlaneMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkBindImagePlaneMemoryInfo))

instance Zero VkBindImagePlaneMemoryInfo where
  zero = VkBindImagePlaneMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
                                    zero
                                    zero

-- ** VkChromaLocation

-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkChromaLocation = VkChromaLocation Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkChromaLocation where
  showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN = showString "VK_CHROMA_LOCATION_COSITED_EVEN"
  showsPrec _ VK_CHROMA_LOCATION_MIDPOINT = showString "VK_CHROMA_LOCATION_MIDPOINT"
  showsPrec p (VkChromaLocation x) = showParen (p >= 11) (showString "VkChromaLocation " . showsPrec 11 x)

instance Read VkChromaLocation where
  readPrec = parens ( choose [ ("VK_CHROMA_LOCATION_COSITED_EVEN", pure VK_CHROMA_LOCATION_COSITED_EVEN)
                             , ("VK_CHROMA_LOCATION_MIDPOINT",     pure VK_CHROMA_LOCATION_MIDPOINT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkChromaLocation")
                        v <- step readPrec
                        pure (VkChromaLocation v)
                        )
                    )

-- | 'VK_CHROMA_LOCATION_COSITED_EVEN' specifies that downsampled chroma
-- samples are aligned with luma samples with even coordinates.
pattern VK_CHROMA_LOCATION_COSITED_EVEN :: VkChromaLocation
pattern VK_CHROMA_LOCATION_COSITED_EVEN = VkChromaLocation 0

-- | 'VK_CHROMA_LOCATION_MIDPOINT' specifies that downsampled chroma samples
-- are located half way between each even luma sample and the nearest
-- higher odd luma sample.
pattern VK_CHROMA_LOCATION_MIDPOINT :: VkChromaLocation
pattern VK_CHROMA_LOCATION_MIDPOINT = VkChromaLocation 1

-- | VkImagePlaneMemoryRequirementsInfo - Structure specifying image plane
-- for memory requirements
--
-- == Valid Usage
--
-- -   If the image’s tiling is
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL',
--     then @planeAspect@ /must/ be a single valid /format plane/ for the
--     image. (That is, for a two-plane image @planeAspect@ /must/ be
--     'VK_IMAGE_ASPECT_PLANE_0_BIT' or 'VK_IMAGE_ASPECT_PLANE_1_BIT', and
--     for a three-plane image @planeAspect@ /must/ be
--     'VK_IMAGE_ASPECT_PLANE_0_BIT', 'VK_IMAGE_ASPECT_PLANE_1_BIT' or
--     'VK_IMAGE_ASPECT_PLANE_2_BIT').
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO'
--
-- -   @planeAspect@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkImagePlaneMemoryRequirementsInfo = VkImagePlaneMemoryRequirementsInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @planeAspect@ is the aspect corresponding to the image plane to query.
  vkPlaneAspect :: VkImageAspectFlagBits
  }
  deriving (Eq, Show)

instance Storable VkImagePlaneMemoryRequirementsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImagePlaneMemoryRequirementsInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImagePlaneMemoryRequirementsInfo))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkImagePlaneMemoryRequirementsInfo))

instance Zero VkImagePlaneMemoryRequirementsInfo where
  zero = VkImagePlaneMemoryRequirementsInfo VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
                                            zero
                                            zero

-- | VkPhysicalDeviceSamplerYcbcrConversionFeatures - Structure describing
-- Y’CbCr conversion features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceSamplerYcbcrConversionFeatures'
-- structure describe the following feature:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceSamplerYcbcrConversionFeatures = VkPhysicalDeviceSamplerYcbcrConversionFeatures
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSamplerYcbcrConversionFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- | @samplerYcbcrConversion@ specifies whether the implementation supports
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>.
  -- If @samplerYcbcrConversion@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE',
  -- sampler Y’CBCR conversion is not supported, and samplers using sampler
  -- Y’CBCR conversion /must/ not be used.
  vkSamplerYcbcrConversion :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerYcbcrConversionFeatures <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))
                *> poke (ptr `plusPtr` 16) (vkSamplerYcbcrConversion (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeatures))

instance Zero VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  zero = VkPhysicalDeviceSamplerYcbcrConversionFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
                                                        zero
                                                        zero

-- | Dummy data to tag the 'Ptr' with
data VkSamplerYcbcrConversion_T
-- | VkSamplerYcbcrConversion - Opaque handle to a device-specific sampler
-- Y’CBCR conversion description
--
-- = See Also
--
-- 'VkSamplerYcbcrConversionInfo', 'vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR'
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T

-- | VkSamplerYcbcrConversionCreateInfo - Structure specifying the parameters
-- of the newly created conversion
--
-- = Description
--
-- __Note__
--
-- Setting @forceExplicitReconstruction@ to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' /may/ have a performance penalty
-- on implementations where explicit reconstruction is not the default mode
-- of operation.
--
-- Sampler Y’CBCR conversion objects do not support /external format
-- conversion/ without additional extensions defining /external formats/.
--
-- == Valid Usage
--
-- -   @format@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED'
--
-- -   @format@ /must/ support
--     'VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT' or
--     'VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--
-- -   If the format does not support
--     'VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT', @xChromaOffset@ and
--     @yChromaOffset@ /must/ not be 'VK_CHROMA_LOCATION_COSITED_EVEN'
--
-- -   If the format does not support
--     'VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT', @xChromaOffset@ and
--     @yChromaOffset@ /must/ not be 'VK_CHROMA_LOCATION_MIDPOINT'
--
-- -   @format@ /must/ represent unsigned normalized values (i.e. the
--     format must be a @UNORM@ format)
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.g@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.a@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ONE', or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ZERO'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.r@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--     or 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_B'
--
-- -   If the format has a @_422@ or @_420@ suffix, then @components.b@
--     /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--     or 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_R'
--
-- -   If the format has a @_422@ or @_420@ suffix, and if either
--     @components.r@ or @components.b@ is
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY',
--     both values /must/ be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   If @ycbcrModel@ is not
--     'VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY', then
--     @components.r@, @components.g@, and @components.b@ /must/ correspond
--     to channels of the @format@; that is, @components.r@,
--     @components.g@, and @components.b@ /must/ not be
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ZERO' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_ONE', and
--     /must/ not correspond to a channel which contains zero or one as a
--     consequence of
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-conversion-to-rgba conversion to RGBA>
--
-- -   If the format does not support
--     'VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT',
--     @forceExplicitReconstruction@ /must/ be FALSE
--
-- -   If the format does not support
--     'VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT',
--     @chromaFilter@ /must/ be
--     'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID'
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @ycbcrModel@ /must/ be a valid 'VkSamplerYcbcrModelConversion' value
--
-- -   @ycbcrRange@ /must/ be a valid 'VkSamplerYcbcrRange' value
--
-- -   @components@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping' structure
--
-- -   @xChromaOffset@ /must/ be a valid 'VkChromaLocation' value
--
-- -   @yChromaOffset@ /must/ be a valid 'VkChromaLocation' value
--
-- -   @chromaFilter@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkFilter' value
--
-- If @chromaFilter@ is
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST', chroma samples are
-- reconstructed to luma channel resolution using nearest-neighbour
-- sampling. Otherwise, chroma samples are reconstructed using
-- interpolation. More details can be found in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y’CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkChromaLocation',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'VkSamplerYcbcrModelConversion', 'VkSamplerYcbcrRange',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR'
data VkSamplerYcbcrConversionCreateInfo = VkSamplerYcbcrConversionCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @format@ is the format of the image from which color information will be
  -- retrieved.
  vkFormat :: VkFormat
  , -- | @ycbcrModel@ describes the color matrix for conversion between color
  -- models.
  vkYcbcrModel :: VkSamplerYcbcrModelConversion
  , -- | @ycbcrRange@ describes whether the encoded values have headroom and foot
  -- room, or whether the encoding uses the full numerical range.
  vkYcbcrRange :: VkSamplerYcbcrRange
  , -- | @components@ applies a /swizzle/ based on
  -- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentSwizzle' enums prior to
  -- range expansion and color model conversion.
  vkComponents :: VkComponentMapping
  , -- | @xChromaOffset@ describes the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-chroma-reconstruction sample location>
  -- associated with downsampled chroma channels in the x dimension.
  -- @xChromaOffset@ has no effect for formats in which chroma channels are
  -- the same resolution as the luma channel.
  vkXChromaOffset :: VkChromaLocation
  , -- | @yChromaOffset@ describes the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-chroma-reconstruction sample location>
  -- associated with downsampled chroma channels in the y dimension.
  -- @yChromaOffset@ has no effect for formats in which the chroma channels
  -- are not downsampled vertically.
  vkYChromaOffset :: VkChromaLocation
  , -- | @chromaFilter@ is the filter for chroma reconstruction.
  vkChromaFilter :: VkFilter
  , -- | @forceExplicitReconstruction@ /can/ be used to ensure that
  -- reconstruction is done explicitly, if supported.
  vkForceExplicitReconstruction :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionCreateInfo <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 44)
                                                <*> peek (ptr `plusPtr` 48)
                                                <*> peek (ptr `plusPtr` 52)
                                                <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkYcbcrModel (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkYcbcrRange (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkComponents (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkXChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkYChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkChromaFilter (poked :: VkSamplerYcbcrConversionCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkForceExplicitReconstruction (poked :: VkSamplerYcbcrConversionCreateInfo))

instance Zero VkSamplerYcbcrConversionCreateInfo where
  zero = VkSamplerYcbcrConversionCreateInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero

-- | VkSamplerYcbcrConversionImageFormatProperties - Structure specifying
-- combined image sampler descriptor count for multi-planar images
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSamplerYcbcrConversionImageFormatProperties = VkSamplerYcbcrConversionImageFormatProperties
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @combinedImageSamplerDescriptorCount@ is the number of combined image
  -- sampler descriptors that the implementation uses to access the format.
  vkCombinedImageSamplerDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionImageFormatProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkCombinedImageSamplerDescriptorCount (poked :: VkSamplerYcbcrConversionImageFormatProperties))

instance Zero VkSamplerYcbcrConversionImageFormatProperties where
  zero = VkSamplerYcbcrConversionImageFormatProperties VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
                                                       zero
                                                       zero

-- | VkSamplerYcbcrConversionInfo - Structure specifying Y’CbCr conversion to
-- a sampler or image view
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @conversion@ /must/ be a valid 'VkSamplerYcbcrConversion' handle
  vkConversion :: VkSamplerYcbcrConversion
  }
  deriving (Eq, Show)

instance Storable VkSamplerYcbcrConversionInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionInfo))
                *> poke (ptr `plusPtr` 16) (vkConversion (poked :: VkSamplerYcbcrConversionInfo))

instance Zero VkSamplerYcbcrConversionInfo where
  zero = VkSamplerYcbcrConversionInfo VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
                                      zero
                                      zero

-- ** VkSamplerYcbcrModelConversion

-- | VkSamplerYcbcrModelConversion - Color model component of a color space
--
-- = Description
--
-- -   'VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY' specifies that the
--     input values to the conversion are unmodified.
--
-- -   'VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY' specifies no
--     model conversion but the inputs are range expanded as for Y’CBCR.
--
-- -   'VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709' specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.709 and
--     described in the “BT.709 Y’CBCR conversion” section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601' specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.601 and
--     described in the “BT.601 Y’CBCR conversion” section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- -   'VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020' specifies the color
--     model conversion from Y’CBCR to R’G’B\' defined in BT.2020 and
--     described in the “BT.2020 Y’CBCR conversion” section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- In the @VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_*@ color models, for the
-- input to the sampler Y’CBCR range expansion and model conversion:
--
-- -   the Y (Y\' luma) channel corresponds to the G channel of an RGB
--     image.
--
-- -   the CB (CB or “U” blue color difference) channel corresponds to the
--     B channel of an RGB image.
--
-- -   the CR (CR or “V” red color difference) channel corresponds to the R
--     channel of an RGB image.
--
-- -   the alpha channel, if present, is not modified by color model
--     conversion.
--
-- These rules reflect the mapping of channels after the channel swizzle
-- operation (controlled by
-- 'VkSamplerYcbcrConversionCreateInfo'::@components@).
--
-- __Note__
--
-- For example, an “YUVA” 32-bit format comprising four 8-bit channels can
-- be implemented as
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM' with a
-- component mapping:
--
-- -   @components.a@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- -   @components.r@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_B'
--
-- -   @components.g@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_R'
--
-- -   @components.b@ =
--     'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_G'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkSamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkSamplerYcbcrModelConversion where
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
  showsPrec p (VkSamplerYcbcrModelConversion x) = showParen (p >= 11) (showString "VkSamplerYcbcrModelConversion " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversion where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY",   pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709",      pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601",      pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020",     pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrModelConversion")
                        v <- step readPrec
                        pure (VkSamplerYcbcrModelConversion v)
                        )
                    )

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY = VkSamplerYcbcrModelConversion 0

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = VkSamplerYcbcrModelConversion 1

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709 = VkSamplerYcbcrModelConversion 2

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601 = VkSamplerYcbcrModelConversion 3

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: VkSamplerYcbcrModelConversion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 = VkSamplerYcbcrModelConversion 4

-- ** VkSamplerYcbcrRange

-- | VkSamplerYcbcrRange - Range of encoded values in a color space
--
-- = Description
--
-- The formulae for these conversions is described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion-rangeexpand Sampler Y’CBCR Range Expansion>
-- section of the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- No range modification takes place if @ycbcrModel@ is
-- 'VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY'; the @ycbcrRange@ field
-- of 'VkSamplerYcbcrConversionCreateInfo' is ignored in this case.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkSamplerYcbcrConversionCreateInfo'
newtype VkSamplerYcbcrRange = VkSamplerYcbcrRange Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkSamplerYcbcrRange where
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
  showsPrec p (VkSamplerYcbcrRange x) = showParen (p >= 11) (showString "VkSamplerYcbcrRange " . showsPrec 11 x)

instance Read VkSamplerYcbcrRange where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_RANGE_ITU_FULL",   pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL)
                             , ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW", pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrRange")
                        v <- step readPrec
                        pure (VkSamplerYcbcrRange v)
                        )
                    )

-- | 'VK_SAMPLER_YCBCR_RANGE_ITU_FULL' specifies that the full range of the
-- encoded values are valid and interpreted according to the ITU “full
-- range” quantization rules.
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL = VkSamplerYcbcrRange 0

-- | 'VK_SAMPLER_YCBCR_RANGE_ITU_NARROW' specifies that headroom and foot
-- room are reserved in the numerical range of encoded values, and the
-- remaining values are expanded according to the ITU “narrow range”
-- quantization rules.
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW :: VkSamplerYcbcrRange
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW = VkSamplerYcbcrRange 1

-- | vkCreateSamplerYcbcrConversion - Create a new Ycbcr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the sampler Y’CBCR
--     conversion.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkSamplerYcbcrConversionCreateInfo' specifying the requested
--     sampler Y’CBCR conversion.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pYcbcrConversion@ points to a 'VkSamplerYcbcrConversion' handle in
--     which the resulting sampler Y’CBCR conversion is returned.
--
-- = Description
--
-- The interpretation of the configured sampler Y’CBCR conversion is
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion the description of sampler Y’CBCR conversion>
-- in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sampler-YCbCr-conversion sampler Y’CBCR conversion feature>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkSamplerYcbcrConversionCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pYcbcrConversion@ /must/ be a valid pointer to a
--     'VkSamplerYcbcrConversion' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkSamplerYcbcrConversion', 'VkSamplerYcbcrConversionCreateInfo'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSamplerYcbcrConversion" vkCreateSamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
#else
vkCreateSamplerYcbcrConversion :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
vkCreateSamplerYcbcrConversion deviceCmds = mkVkCreateSamplerYcbcrConversion (pVkCreateSamplerYcbcrConversion deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSamplerYcbcrConversion
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult)
#endif

type FN_vkCreateSamplerYcbcrConversion = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
type PFN_vkCreateSamplerYcbcrConversion = FunPtr FN_vkCreateSamplerYcbcrConversion

-- | vkDestroySamplerYcbcrConversion - Destroy a created Y’CbCr conversion
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the Y’CBCR conversion.
--
-- -   @ycbcrConversion@ is the conversion to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @ycbcrConversion@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @ycbcrConversion@ /must/ be a valid 'VkSamplerYcbcrConversion'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @ycbcrConversion@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @ycbcrConversion@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkSamplerYcbcrConversion'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySamplerYcbcrConversion" vkDestroySamplerYcbcrConversion :: ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroySamplerYcbcrConversion :: DeviceCmds -> ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroySamplerYcbcrConversion deviceCmds = mkVkDestroySamplerYcbcrConversion (pVkDestroySamplerYcbcrConversion deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySamplerYcbcrConversion
  :: FunPtr (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroySamplerYcbcrConversion = ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySamplerYcbcrConversion = FunPtr FN_vkDestroySamplerYcbcrConversion

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = VkFormat 1000156011

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = VkFormat 1000156021

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B16G16R16G16_422_UNORM"
pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat
pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028

-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8G8_422_UNORM"
pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00800000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DISJOINT_BIT"
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DISJOINT_BIT = VkFormatFeatureFlagBits 0x00400000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VkFormatFeatureFlagBits 0x00020000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VkFormatFeatureFlagBits 0x00100000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VkFormatFeatureFlagBits 0x00200000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VkFormatFeatureFlagBits 0x00040000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VkFormatFeatureFlagBits 0x00080000

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = VkFormat 1000156010

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156013

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156015

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156012

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156014

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156016

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = VkFormat 1000156020

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = VkFormat 1000156023

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = VkFormat 1000156025

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = VkFormat 1000156022

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = VkFormat 1000156024

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 :: VkFormat
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = VkFormat 1000156026

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16B16G16R16_422_UNORM"
pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat
pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = VkFormat 1000156029

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = VkFormat 1000156031

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM = VkFormat 1000156033

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8B8G8R8_422_UNORM"
pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat
pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004

-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = VkFormat 1000156009

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6_UNORM_PACK16"
pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = VkFormat 1000156019

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018

-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4_UNORM_PACK16"
pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_0_BIT"
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectFlagBits 0x00000010

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_1_BIT"
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectFlagBits 0x00000020

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_2_BIT"
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectFlagBits 0x00000040

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DISJOINT_BIT"
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateFlagBits 0x00000200

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION = VkObjectType 1000156000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO = VkStructureType 1000156002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO = VkStructureType 1000156003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES = VkStructureType 1000156004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO = VkStructureType 1000156000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES = VkStructureType 1000156005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO = VkStructureType 1000156001
