{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( ChromaLocation
  , ChromaLocationKHR
  , SamplerYcbcrConversion
  , SamplerYcbcrModelConversion
  , SamplerYcbcrModelConversionKHR
  , SamplerYcbcrRange
  , SamplerYcbcrRangeKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkChromaLocation
  , VkSamplerYcbcrConversion
  , VkSamplerYcbcrModelConversion
  , VkSamplerYcbcrRange
  )


-- No documentation found for TopLevel "ChromaLocation"
type ChromaLocation = VkChromaLocation

-- No documentation found for TopLevel "ChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation

-- No documentation found for TopLevel "SamplerYcbcrConversion"
type SamplerYcbcrConversion = VkSamplerYcbcrConversion

-- No documentation found for TopLevel "SamplerYcbcrModelConversion"
type SamplerYcbcrModelConversion = VkSamplerYcbcrModelConversion

-- No documentation found for TopLevel "SamplerYcbcrModelConversionKHR"
type SamplerYcbcrModelConversionKHR = SamplerYcbcrModelConversion

-- No documentation found for TopLevel "SamplerYcbcrRange"
type SamplerYcbcrRange = VkSamplerYcbcrRange

-- No documentation found for TopLevel "SamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange
