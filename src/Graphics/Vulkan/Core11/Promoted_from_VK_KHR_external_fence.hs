{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( withCStructExportFenceCreateInfo
  , fromCStructExportFenceCreateInfo
  , ExportFenceCreateInfo(..)
  , FenceImportFlagBits
  , pattern FENCE_IMPORT_TEMPORARY_BIT
  , FenceImportFlagBitsKHR
  , FenceImportFlags
  , FenceImportFlagsKHR
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , VkFenceImportFlagBits(..)
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )



-- | VkExportFenceCreateInfo - Structure specifying handle types that can be
-- exported from a fence
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ must be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO'
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportFenceCreateInfo = ExportFenceCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ExportFenceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceCreateInfo" "handleTypes"
  handleTypes :: ExternalFenceHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportFenceCreateInfo' and
-- marshal a 'ExportFenceCreateInfo' into it. The 'VkExportFenceCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportFenceCreateInfo :: ExportFenceCreateInfo -> (VkExportFenceCreateInfo -> IO a) -> IO a
withCStructExportFenceCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportFenceCreateInfo)) (\pPNext -> cont (VkExportFenceCreateInfo VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO pPNext (handleTypes (marshalled :: ExportFenceCreateInfo))))

-- | A function to read a 'VkExportFenceCreateInfo' and all additional
-- structures in the pointer chain into a 'ExportFenceCreateInfo'.
fromCStructExportFenceCreateInfo :: VkExportFenceCreateInfo -> IO ExportFenceCreateInfo
fromCStructExportFenceCreateInfo c = ExportFenceCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportFenceCreateInfo)))
                                                           <*> pure (vkHandleTypes (c :: VkExportFenceCreateInfo))

instance Zero ExportFenceCreateInfo where
  zero = ExportFenceCreateInfo Nothing
                               zero


-- | VkFenceImportFlagBits - Bitmask specifying additional parameters of
-- fence payload import
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags'
type FenceImportFlagBits = VkFenceImportFlagBits


{-# complete FENCE_IMPORT_TEMPORARY_BIT :: FenceImportFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VK_FENCE_IMPORT_TEMPORARY_BIT'
-- specifies that the fence payload will be imported only temporarily, as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>,
-- regardless of the permanence of @handleType@.
pattern FENCE_IMPORT_TEMPORARY_BIT :: (a ~ FenceImportFlagBits) => a
pattern FENCE_IMPORT_TEMPORARY_BIT = VK_FENCE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "FenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits

-- | VkFenceImportFlags - Bitmask of VkFenceImportFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR'
type FenceImportFlags = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags
