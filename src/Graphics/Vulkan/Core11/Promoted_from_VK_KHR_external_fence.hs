{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( withCStructExportFenceCreateInfo
  , fromCStructExportFenceCreateInfo
  , ExportFenceCreateInfo(..)
  , FenceImportFlagBits
  , FenceImportFlagBitsKHR
  , FenceImportFlags
  , FenceImportFlagsKHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , VkFenceImportFlagBits(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ExportFenceCreateInfo"
data ExportFenceCreateInfo = ExportFenceCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportFenceCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceCreateInfo" "handleTypes"
  vkHandleTypes :: ExternalFenceHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExportFenceCreateInfo :: ExportFenceCreateInfo -> (VkExportFenceCreateInfo -> IO a) -> IO a
withCStructExportFenceCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportFenceCreateInfo)) (\pPNext -> cont (VkExportFenceCreateInfo VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO pPNext (vkHandleTypes (from :: ExportFenceCreateInfo))))
fromCStructExportFenceCreateInfo :: VkExportFenceCreateInfo -> IO ExportFenceCreateInfo
fromCStructExportFenceCreateInfo c = ExportFenceCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportFenceCreateInfo)))
                                                           <*> pure (vkHandleTypes (c :: VkExportFenceCreateInfo))
-- No documentation found for TopLevel "FenceImportFlagBits"
type FenceImportFlagBits = VkFenceImportFlagBits
-- No documentation found for TopLevel "FenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits
-- No documentation found for TopLevel "FenceImportFlags"
type FenceImportFlags = FenceImportFlagBits
-- No documentation found for TopLevel "FenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags
