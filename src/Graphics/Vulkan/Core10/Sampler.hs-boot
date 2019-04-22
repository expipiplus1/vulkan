{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Sampler
  ( BorderColor
  , Filter
  , Sampler
  , SamplerAddressMode
  , SamplerCreateFlagBits
  , SamplerCreateFlags
  , SamplerMipmapMode
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor
  , VkFilter
  , VkSamplerAddressMode
  , VkSamplerCreateFlagBits
  , VkSamplerMipmapMode
  , VkSampler
  )


-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- These colors are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type BorderColor = VkBorderColor

-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- These filters are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
type Filter = VkFilter

-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.VkImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkDestroySampler'
type Sampler = VkSampler

-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerAddressMode = VkSamplerAddressMode

-- | VkSamplerCreateFlagBits - Bitmask specifying additional parameters of
-- sampler
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlags'
type SamplerCreateFlagBits = VkSamplerCreateFlagBits

-- | VkSamplerCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerCreateFlags = SamplerCreateFlagBits

-- | VkSamplerMipmapMode - Specify mipmap mode used for texture lookups
--
-- = Description
--
-- These modes are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo'
type SamplerMipmapMode = VkSamplerMipmapMode
