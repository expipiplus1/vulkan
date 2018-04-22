{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlagBits(..)
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  , VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlags
  )


-- ** VkSemaphoreImportFlagBits

-- | VkSemaphoreImportFlagBits - Bitmask specifying additional parameters of
-- semaphore payload import
--
-- = Description
--
-- These bits have the following meanings:
--
-- -   @VK_SEMAPHORE_IMPORT_TEMPORARY_BIT@ specifies that the semaphore
--     payload will be imported only temporarily, as described in
--     [Importing Semaphore
--     Payloads](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-importing),
--     regardless of the permanence of @handleType@.
--
-- = See Also
--
-- 'VkSemaphoreImportFlags'
newtype VkSemaphoreImportFlagBits = VkSemaphoreImportFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSemaphoreImportFlagBits where
  showsPrec _ VK_SEMAPHORE_IMPORT_TEMPORARY_BIT = showString "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT"
  showsPrec p (VkSemaphoreImportFlagBits x) = showParen (p >= 11) (showString "VkSemaphoreImportFlagBits " . showsPrec 11 x)

instance Read VkSemaphoreImportFlagBits where
  readPrec = parens ( choose [ ("VK_SEMAPHORE_IMPORT_TEMPORARY_BIT", pure VK_SEMAPHORE_IMPORT_TEMPORARY_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSemaphoreImportFlagBits")
                        v <- step readPrec
                        pure (VkSemaphoreImportFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkSemaphoreImportFlagBits" "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT"
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT :: VkSemaphoreImportFlagBits
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT = VkSemaphoreImportFlagBits 0x00000001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO = VkStructureType 1000077000
-- | VkExportSemaphoreCreateInfo - Structure specifying handle types that can
-- be exported from a semaphore
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ /must/ be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO@
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExportSemaphoreCreateInfo = VkExportSemaphoreCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleTypes@ is a bitmask of
  -- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
  -- specifying one or more semaphore handle types the application /can/
  -- export from the resulting semaphore. The application /can/ request
  -- multiple handle types for the same semaphore.
  vkHandleTypes :: VkExternalSemaphoreHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportSemaphoreCreateInfo))
-- | VkSemaphoreImportFlags - Bitmask of VkSemaphoreImportFlagBits
--
-- = Description
--
-- @VkSemaphoreImportFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkSemaphoreImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'VkSemaphoreImportFlagBits'
type VkSemaphoreImportFlags = VkSemaphoreImportFlagBits
