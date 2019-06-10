{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  , StencilFaceFlagBits
  , StencilFaceFlags
  , SubpassContents
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkIndexType
  , VkStencilFaceFlagBits
  , VkSubpassContents
  )


-- No documentation found for TopLevel "IndexType"
type IndexType = VkIndexType

-- No documentation found for TopLevel "StencilFaceFlagBits"
type StencilFaceFlagBits = VkStencilFaceFlagBits

-- No documentation found for TopLevel "StencilFaceFlags"
type StencilFaceFlags = StencilFaceFlagBits

-- No documentation found for TopLevel "SubpassContents"
type SubpassContents = VkSubpassContents
