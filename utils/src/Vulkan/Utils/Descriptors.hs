{-# LANGUAGE OverloadedLists #-}

{-| Single-resource descriptor writes for 'Vk.updateDescriptorSets'.
'bufferWrite' covers the whole-buffer case (a uniform or storage buffer
bound in its entirety); 'imageWrite' is its samplerless image sibling (a
storage image in @GENERAL@ layout, a sampled image in
@SHADER_READ_ONLY_OPTIMAL@, …); 'combinedImageSamplerWrite' binds an image
together with a sampler. Bindings needing partial buffer ranges or
descriptor arrays are out of scope — assemble those 'Vk.WriteDescriptorSet's
directly.
-}
module Vulkan.Utils.Descriptors
  ( bufferWrite
  , imageWrite
  , combinedImageSamplerWrite
  ) where

import Data.Word (Word32)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

-- | A whole-buffer descriptor write.
bufferWrite
  :: Vk.DescriptorSet
  -> Word32
  -- ^ Binding.
  -> Vk.DescriptorType
  -- ^ 'Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER', 'Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER', …
  -> Vk.Buffer
  -> SomeStruct Vk.WriteDescriptorSet
bufferWrite set binding descriptorType buffer =
  SomeStruct
    zero
      { Vk.dstSet = set
      , Vk.dstBinding = binding
      , Vk.descriptorType = descriptorType
      , Vk.descriptorCount = 1
      , Vk.bufferInfo = [Vk.DescriptorBufferInfo buffer 0 Vk.WHOLE_SIZE]
      }

-- | A samplerless image descriptor write.
imageWrite
  :: Vk.DescriptorSet
  -> Word32
  -- ^ Binding.
  -> Vk.DescriptorType
  -- ^ 'Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE', 'Vk.DESCRIPTOR_TYPE_SAMPLED_IMAGE', …
  -> Vk.ImageLayout
  -- ^ The layout the image will be in when the set is bound.
  -> Vk.ImageView
  -> SomeStruct Vk.WriteDescriptorSet
imageWrite set binding descriptorType layout view =
  SomeStruct
    zero
      { Vk.dstSet = set
      , Vk.dstBinding = binding
      , Vk.descriptorType = descriptorType
      , Vk.descriptorCount = 1
      , Vk.imageInfo = [Vk.DescriptorImageInfo Vk.NULL_HANDLE view layout]
      }

-- | A combined image+sampler descriptor write ('Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER').
combinedImageSamplerWrite
  :: Vk.DescriptorSet
  -> Word32
  -- ^ Binding.
  -> Vk.Sampler
  -> Vk.ImageView
  -> Vk.ImageLayout
  -- ^ The layout the image will be in when the set is bound.
  -> SomeStruct Vk.WriteDescriptorSet
combinedImageSamplerWrite set binding sampler view layout =
  SomeStruct
    zero
      { Vk.dstSet = set
      , Vk.dstBinding = binding
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vk.descriptorCount = 1
      , Vk.imageInfo = [Vk.DescriptorImageInfo sampler view layout]
      }
