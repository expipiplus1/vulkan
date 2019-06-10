{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Core
  ( Format
  , ObjectType
  , Result
  , StructureType
  , VendorId
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkFormat
  , VkObjectType
  , VkResult
  , VkStructureType
  , VkVendorId
  )


-- No documentation found for TopLevel "Format"
type Format = VkFormat

-- No documentation found for TopLevel "ObjectType"
type ObjectType = VkObjectType

-- No documentation found for TopLevel "Result"
type Result = VkResult

-- No documentation found for TopLevel "StructureType"
type StructureType = VkStructureType

-- No documentation found for TopLevel "VendorId"
type VendorId = VkVendorId
