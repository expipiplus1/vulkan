{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence
  ( ExportFenceCreateInfoKHR
  , pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , FenceImportFlagsKHR
  , FenceImportFlagBitsKHR
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( ExportFenceCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence
  ( pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlagBitsKHR
  , FenceImportFlagsKHR
  )


type ExportFenceCreateInfoKHR = ExportFenceCreateInfo
-- TODO: Pattern constructor alias)
