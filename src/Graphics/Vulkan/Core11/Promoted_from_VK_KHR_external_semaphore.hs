{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ExportSemaphoreCreateInfo(..)
  , 
#endif
  SemaphoreImportFlagBits
  , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT
  , SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlags
  , SemaphoreImportFlagsKHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  ) where





#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlagBits(..)
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExportSemaphoreCreateInfo"
data ExportSemaphoreCreateInfo = ExportSemaphoreCreateInfo
  { -- No documentation found for Nested "ExportSemaphoreCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreCreateInfo" "handleTypes"
  handleTypes :: ExternalSemaphoreHandleTypeFlags
  }
  deriving (Show, Eq)

instance Zero ExportSemaphoreCreateInfo where
  zero = ExportSemaphoreCreateInfo Nothing
                                   zero

#endif

-- No documentation found for TopLevel "SemaphoreImportFlagBits"
type SemaphoreImportFlagBits = VkSemaphoreImportFlagBits


{-# complete SEMAPHORE_IMPORT_TEMPORARY_BIT :: SemaphoreImportFlagBits #-}


-- No documentation found for Nested "SemaphoreImportFlagBits" "SEMAPHORE_IMPORT_TEMPORARY_BIT"
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT :: (a ~ SemaphoreImportFlagBits) => a
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "SemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlags"
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags
