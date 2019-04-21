{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , VkFenceImportFlagBits(..)
  , pattern VK_FENCE_IMPORT_TEMPORARY_BIT
  , VkFenceImportFlags
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlags
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
-- Unresolved directive in VkExportFenceCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkExportFenceCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkExportFenceCreateInfo = VkExportFenceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleTypes@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
  -- specifying one or more fence handle types the application /can/ export
  -- from the resulting fence. The application /can/ request multiple handle
  -- types for the same fence.
  vkHandleTypes :: VkExternalFenceHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportFenceCreateInfo))

instance Zero VkExportFenceCreateInfo where
  zero = VkExportFenceCreateInfo VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
                                 zero
                                 zero

-- ** VkFenceImportFlagBits

-- | VkFenceImportFlagBits - Bitmask specifying additional parameters of
-- fence payload import
--
-- = See Also
--
-- 'VkFenceImportFlags'
newtype VkFenceImportFlagBits = VkFenceImportFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_FENCE_IMPORT_TEMPORARY_BIT' specifies that the fence payload will be
-- imported only temporarily, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>,
-- regardless of the permanence of @handleType@.
pattern VK_FENCE_IMPORT_TEMPORARY_BIT :: VkFenceImportFlagBits
pattern VK_FENCE_IMPORT_TEMPORARY_BIT = VkFenceImportFlagBits 0x00000001

-- | VkFenceImportFlags - Bitmask of VkFenceImportFlagBits
--
-- = Description
--
-- 'VkFenceImportFlags' is a bitmask type for setting a mask of zero or
-- more 'VkFenceImportFlagBits'.
--
-- = See Also
--
-- 'VkFenceImportFlagBits'
type VkFenceImportFlags = VkFenceImportFlagBits

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO = VkStructureType 1000113000
