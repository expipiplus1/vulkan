{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( CoarseSampleOrderTypeNV
  , ShadingRatePaletteEntryNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleOrderTypeNV
  , VkShadingRatePaletteEntryNV
  )


-- | VkCoarseSampleOrderTypeNV - Shading rate image sample ordering types
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkPipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV'
type CoarseSampleOrderTypeNV = VkCoarseSampleOrderTypeNV

-- | VkShadingRatePaletteEntryNV - Shading rate image palette entry types
--
-- = Description
--
-- The following table indicates the width and height (in pixels) of each
-- fragment generated using the indicated shading rate, as well as the
-- maximum number of fragment shader invocations launched for each
-- fragment. When processing regions of a primitive that have a shading
-- rate of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV',
-- no fragments will be generated in that region.
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | Shading Rate    | Width           | Height          | Invocations     |
-- > +=================+=================+=================+=================+
-- > | 'Graphics.Vulka | 0               | 0               | 0               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_NO_IN |                 |                 |                 |
-- > | VOCATIONS_NV'   |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 16              |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_16_IN |                 |                 |                 |
-- > | VOCATIONS_PER_P |                 |                 |                 |
-- > | IXEL_NV'        |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 8               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_8_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 4               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_4_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 2               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_2_INV |                 |                 |                 |
-- > | OCATIONS_PER_PI |                 |                 |                 |
-- > | XEL_NV'         |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 1               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_PIX |                 |                 |                 |
-- > | EL_NV'          |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 1               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X1 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 1               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_1X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 4               | 2               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_4X2 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 2               | 4               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_2X4 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | 'Graphics.Vulka | 4               | 4               | 1               |
-- > | n.C.Extensions. |                 |                 |                 |
-- > | VK_NV_shading_r |                 |                 |                 |
-- > | ate_image.VK_SH |                 |                 |                 |
-- > | ADING_RATE_PALE |                 |                 |                 |
-- > | TTE_ENTRY_1_INV |                 |                 |                 |
-- > | OCATION_PER_4X4 |                 |                 |                 |
-- > | _PIXELS_NV'     |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkCoarseSampleOrderCustomNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VkShadingRatePaletteNV'
type ShadingRatePaletteEntryNV = VkShadingRatePaletteEntryNV
