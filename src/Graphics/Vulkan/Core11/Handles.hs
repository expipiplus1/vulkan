{-# language CPP #-}
module Graphics.Vulkan.Core11.Handles  ( DescriptorUpdateTemplate(..)
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
import Graphics.Vulkan.Core10.APIConstants (IsHandle)
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandPool(..))
import Graphics.Vulkan.Core10.Handles (DescriptorSet(..))
import Graphics.Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Core10.Handles (DeviceMemory(..))
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PipelineLayout(..))
import Graphics.Vulkan.Core10.Handles (Queue(..))
import Graphics.Vulkan.Core10.Handles (Sampler(..))
-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR'
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
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR'
newtype SamplerYcbcrConversion = SamplerYcbcrConversion Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show SamplerYcbcrConversion where
  showsPrec p (SamplerYcbcrConversion x) = showParen (p >= 11) (showString "SamplerYcbcrConversion 0x" . showHex x)

