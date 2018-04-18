{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlagBits(..)
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , VkExportFenceCreateInfo(..)
  , VkFenceImportFlags
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
import Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlags
  )


-- ** VkFenceImportFlagBits

-- | 
newtype VkFenceImportFlagBits = VkFenceImportFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkFenceImportFlagBits where
  showsPrec _ VK_FENCE_IMPORT_TEMPORARY_BIT = showString "VK_FENCE_IMPORT_TEMPORARY_BIT"
  showsPrec p (VkFenceImportFlagBits x) = showParen (p >= 11) (showString "VkFenceImportFlagBits " . showsPrec 11 x)

instance Read VkFenceImportFlagBits where
  readPrec = parens ( choose [ ("VK_FENCE_IMPORT_TEMPORARY_BIT", pure VK_FENCE_IMPORT_TEMPORARY_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceImportFlagBits")
                        v <- step readPrec
                        pure (VkFenceImportFlagBits v)
                        )
                    )

-- | 
pattern VK_FENCE_IMPORT_TEMPORARY_BIT :: VkFenceImportFlagBits
pattern VK_FENCE_IMPORT_TEMPORARY_BIT = VkFenceImportFlagBits 0x00000001
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO = VkStructureType 1000113000
-- | TODO: Struct comments
data VkExportFenceCreateInfo = VkExportFenceCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleTypes :: VkExternalFenceHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExportFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportFenceCreateInfo))
type VkFenceImportFlags = VkFenceImportFlagBits
