{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportFenceCreateInfo(..)
  , 
#endif
  FenceImportFlagBits
  , pattern FENCE_IMPORT_TEMPORARY_BIT
  , FenceImportFlagBitsKHR
  , FenceImportFlags
  , FenceImportFlagsKHR
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  ) where





#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlagBits(..)
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportFenceCreateInfo"
data ExportFenceCreateInfo = ExportFenceCreateInfo
  { -- No documentation found for Nested "ExportFenceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceCreateInfo" "handleTypes"
  handleTypes :: ExternalFenceHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExportFenceCreateInfo where
  zero = ExportFenceCreateInfo Nothing
                               zero

#endif

-- No documentation found for TopLevel "FenceImportFlagBits"
type FenceImportFlagBits = VkFenceImportFlagBits


{-# complete FENCE_IMPORT_TEMPORARY_BIT :: FenceImportFlagBits #-}


-- No documentation found for Nested "FenceImportFlagBits" "FENCE_IMPORT_TEMPORARY_BIT"
pattern FENCE_IMPORT_TEMPORARY_BIT :: (a ~ FenceImportFlagBits) => a
pattern FENCE_IMPORT_TEMPORARY_BIT = VK_FENCE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "FenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlags"
type FenceImportFlags = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags
