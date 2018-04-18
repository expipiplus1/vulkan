{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_semaphore
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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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


import Graphics.Vulkan.Version10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlags
  )


-- ** VkSemaphoreImportFlagBits

-- | 
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

-- | 
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT :: VkSemaphoreImportFlagBits
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT = VkSemaphoreImportFlagBits 0x00000001
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO = VkStructureType 1000077000
-- | TODO: Struct comments
data VkExportSemaphoreCreateInfo = VkExportSemaphoreCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleTypes :: VkExternalSemaphoreHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExportSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportSemaphoreCreateInfo))
type VkSemaphoreImportFlags = VkSemaphoreImportFlagBits
