{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlagBits(..)
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , VkSemaphoreImportFlags
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
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


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlags
  )


-- No documentation found for TopLevel "VkExportSemaphoreCreateInfo"
data VkExportSemaphoreCreateInfo = VkExportSemaphoreCreateInfo
  { -- No documentation found for Nested "VkExportSemaphoreCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportSemaphoreCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportSemaphoreCreateInfo" "handleTypes"
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

instance Zero VkExportSemaphoreCreateInfo where
  zero = VkExportSemaphoreCreateInfo zero
                                     zero
                                     zero
-- ** VkSemaphoreImportFlagBits

-- No documentation found for TopLevel "VkSemaphoreImportFlagBits"
newtype VkSemaphoreImportFlagBits = VkSemaphoreImportFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
-- No documentation found for TopLevel "VkSemaphoreImportFlags"
type VkSemaphoreImportFlags = VkSemaphoreImportFlagBits
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO = VkStructureType 1000077000
