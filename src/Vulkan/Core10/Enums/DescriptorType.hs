{-# language CPP #-}
-- No documentation found for Chapter "DescriptorType"
module Vulkan.Core10.Enums.DescriptorType  (DescriptorType( DESCRIPTOR_TYPE_SAMPLER
                                                          , DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                          , DESCRIPTOR_TYPE_SAMPLED_IMAGE
                                                          , DESCRIPTOR_TYPE_STORAGE_IMAGE
                                                          , DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
                                                          , DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
                                                          , DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                          , DESCRIPTOR_TYPE_STORAGE_BUFFER
                                                          , DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
                                                          , DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
                                                          , DESCRIPTOR_TYPE_INPUT_ATTACHMENT
                                                          , DESCRIPTOR_TYPE_MUTABLE_EXT
                                                          , DESCRIPTOR_TYPE_TENSOR_ARM
                                                          , DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM
                                                          , DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                                                          , DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK
                                                          , ..
                                                          )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'DESCRIPTOR_TYPE_SAMPLER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-sampler sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-combinedimagesampler combined image sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_SAMPLED_IMAGE' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-sampledimage sampled image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_IMAGE' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-storageimage storage image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-uniformbuffer uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-storagebuffer storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INPUT_ATTACHMENT' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-inputattachment input attachment descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-inlineuniformblock inline uniform block>.
--
-- -   'DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-accelerationstructure acceleration structure descriptor>.
--
-- -   'DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-accelerationstructure acceleration structure descriptor>.
--
-- -   'DESCRIPTOR_TYPE_MUTABLE_EXT' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-mutable descriptor of mutable type>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>
--     specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-partitionedaccelerationstructure partitioned acceleration structure descriptor>.
--
-- -   'DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-weightimage sampled weight image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-blockmatch block matching image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_TENSOR_ARM' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptors-storagetensor storage tensor descriptor>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorGetInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.DescriptorPoolSize',
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Vulkan.Extensions.VK_EXT_mutable_descriptor_type.MutableDescriptorTypeListEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.ResourceDescriptorInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.getPhysicalDeviceDescriptorSizeEXT'
newtype DescriptorType = DescriptorType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLER"
pattern DESCRIPTOR_TYPE_SAMPLER = DescriptorType 0

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = DescriptorType 1

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE = DescriptorType 2

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE = DescriptorType 3

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = DescriptorType 4

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = DescriptorType 5

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER = DescriptorType 6

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER = DescriptorType 7

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = DescriptorType 8

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = DescriptorType 9

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT = DescriptorType 10

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_MUTABLE_EXT"
pattern DESCRIPTOR_TYPE_MUTABLE_EXT = DescriptorType 1000351000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_TENSOR_ARM"
pattern DESCRIPTOR_TYPE_TENSOR_ARM = DescriptorType 1000460000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM"
pattern DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM = DescriptorType 1000440001

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM"
pattern DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM = DescriptorType 1000440000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = DescriptorType 1000165000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR = DescriptorType 1000150000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK = DescriptorType 1000138000

{-# COMPLETE
  DESCRIPTOR_TYPE_SAMPLER
  , DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , DESCRIPTOR_TYPE_STORAGE_IMAGE
  , DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , DESCRIPTOR_TYPE_STORAGE_BUFFER
  , DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , DESCRIPTOR_TYPE_MUTABLE_EXT
  , DESCRIPTOR_TYPE_TENSOR_ARM
  , DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM
  , DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM
  , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
  , DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK ::
    DescriptorType
  #-}

conNameDescriptorType :: String
conNameDescriptorType = "DescriptorType"

enumPrefixDescriptorType :: String
enumPrefixDescriptorType = "DESCRIPTOR_TYPE_"

showTableDescriptorType :: [(DescriptorType, String)]
showTableDescriptorType =
  [ (DESCRIPTOR_TYPE_SAMPLER, "SAMPLER")
  ,
    ( DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    , "COMBINED_IMAGE_SAMPLER"
    )
  , (DESCRIPTOR_TYPE_SAMPLED_IMAGE, "SAMPLED_IMAGE")
  , (DESCRIPTOR_TYPE_STORAGE_IMAGE, "STORAGE_IMAGE")
  ,
    ( DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
    , "UNIFORM_TEXEL_BUFFER"
    )
  ,
    ( DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
    , "STORAGE_TEXEL_BUFFER"
    )
  , (DESCRIPTOR_TYPE_UNIFORM_BUFFER, "UNIFORM_BUFFER")
  , (DESCRIPTOR_TYPE_STORAGE_BUFFER, "STORAGE_BUFFER")
  ,
    ( DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
    , "UNIFORM_BUFFER_DYNAMIC"
    )
  ,
    ( DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
    , "STORAGE_BUFFER_DYNAMIC"
    )
  ,
    ( DESCRIPTOR_TYPE_INPUT_ATTACHMENT
    , "INPUT_ATTACHMENT"
    )
  , (DESCRIPTOR_TYPE_MUTABLE_EXT, "MUTABLE_EXT")
  , (DESCRIPTOR_TYPE_TENSOR_ARM, "TENSOR_ARM")
  ,
    ( DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM
    , "BLOCK_MATCH_IMAGE_QCOM"
    )
  ,
    ( DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM
    , "SAMPLE_WEIGHT_IMAGE_QCOM"
    )
  ,
    ( DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
    , "ACCELERATION_STRUCTURE_NV"
    )
  ,
    ( DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
    , "ACCELERATION_STRUCTURE_KHR"
    )
  ,
    ( DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK
    , "INLINE_UNIFORM_BLOCK"
    )
  ]

instance Show DescriptorType where
  showsPrec =
    enumShowsPrec
      enumPrefixDescriptorType
      showTableDescriptorType
      conNameDescriptorType
      (\(DescriptorType x) -> x)
      (showsPrec 11)

instance Read DescriptorType where
  readPrec =
    enumReadPrec
      enumPrefixDescriptorType
      showTableDescriptorType
      conNameDescriptorType
      DescriptorType
