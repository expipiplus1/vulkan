{-# language CPP #-}
module Vulkan.Core11.Handles  ( DescriptorUpdateTemplate(..)
                              , SamplerYcbcrConversion(..)
                              , Instance(..)
                              , PhysicalDevice(..)
                              , Device(..)
                              , Queue(..)
                              , CommandBuffer(..)
                              , DeviceMemory(..)
                              , CommandPool(..)
                              , Buffer(..)
                              , Image(..)
                              , PipelineLayout(..)
                              , Sampler(..)
                              , DescriptorSet(..)
                              , DescriptorSetLayout(..)
                              ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Storable (Storable)
import Data.Word (Word64)
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Zero (Zero)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Sampler(..))
-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR'
newtype DescriptorUpdateTemplate = DescriptorUpdateTemplate Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DescriptorUpdateTemplate where
  showsPrec p (DescriptorUpdateTemplate x) = showParen (p >= 11) (showString "DescriptorUpdateTemplate 0x" . showHex x)


-- | VkSamplerYcbcrConversion - Opaque handle to a device-specific sampler
-- Y′CBCR conversion description
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR'
newtype SamplerYcbcrConversion = SamplerYcbcrConversion Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show SamplerYcbcrConversion where
  showsPrec p (SamplerYcbcrConversion x) = showParen (p >= 11) (showString "SamplerYcbcrConversion 0x" . showHex x)

