{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateType(..)
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , VkDescriptorUpdateTemplateCreateFlags(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , VkDescriptorUpdateTemplate
  , vkCreateDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  , vkUpdateDescriptorSetWithTemplate
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
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
import Graphics.Vulkan.NamedType
  ( (:::)
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
  ( VkResult(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorType(..)
  , VkDescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pass
  ( VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipelineLayout
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  )


-- ** VkDescriptorUpdateTemplateType

-- | VkDescriptorUpdateTemplateType - Indicates the valid usage of the
-- descriptor update template
newtype VkDescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType Int32
  deriving (Eq, Ord, Storable)

instance Show VkDescriptorUpdateTemplateType where
  showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = showString "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorUpdateTemplateType 1) = showString "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
  showsPrec p (VkDescriptorUpdateTemplateType x) = showParen (p >= 11) (showString "VkDescriptorUpdateTemplateType " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateType where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET", pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR", pure (VkDescriptorUpdateTemplateType 1))
                             , ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR", pure (VkDescriptorUpdateTemplateType 1))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorUpdateTemplateType")
                        v <- step readPrec
                        pure (VkDescriptorUpdateTemplateType v)
                        )
                    )

-- No documentation found for Nested "VkDescriptorUpdateTemplateType" "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = VkDescriptorUpdateTemplateType 0
-- ** VkDescriptorUpdateTemplateCreateFlags

-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
newtype VkDescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorUpdateTemplateCreateFlags where
  
  showsPrec p (VkDescriptorUpdateTemplateCreateFlags x) = showParen (p >= 11) (showString "VkDescriptorUpdateTemplateCreateFlags " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorUpdateTemplateCreateFlags")
                        v <- step readPrec
                        pure (VkDescriptorUpdateTemplateCreateFlags v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO = VkStructureType 1000085000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE = VkObjectType 1000085000
-- | Dummy data to tag the 'Ptr' with
data VkDescriptorUpdateTemplate_T
-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
type VkDescriptorUpdateTemplate = Ptr VkDescriptorUpdateTemplate_T
-- | vkCreateDescriptorUpdateTemplate - Create a new descriptor update
-- template
foreign import ccall "vkCreateDescriptorUpdateTemplate" vkCreateDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
-- | vkDestroyDescriptorUpdateTemplate - Destroy a descriptor update template
-- object
foreign import ccall "vkDestroyDescriptorUpdateTemplate" vkDestroyDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkUpdateDescriptorSetWithTemplate - Update the contents of a descriptor
-- set object using an update template
foreign import ccall "vkUpdateDescriptorSetWithTemplate" vkUpdateDescriptorSetWithTemplate :: ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
-- | VkDescriptorUpdateTemplateEntry - Describes a single descriptor update
-- of the descriptor update template
data VkDescriptorUpdateTemplateEntry = VkDescriptorUpdateTemplateEntry
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkDstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkDstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkDescriptorCount"
  vkDescriptorCount :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkDescriptorType"
  vkDescriptorType :: VkDescriptorType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkOffset"
  vkOffset :: CSize
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "vkStride"
  vkStride :: CSize
  }
  deriving (Eq, Show)

instance Storable VkDescriptorUpdateTemplateEntry where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorUpdateTemplateEntry <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 4)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 12)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDstBinding (poked :: VkDescriptorUpdateTemplateEntry))
                *> poke (ptr `plusPtr` 4) (vkDstArrayElement (poked :: VkDescriptorUpdateTemplateEntry))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorUpdateTemplateEntry))
                *> poke (ptr `plusPtr` 12) (vkDescriptorType (poked :: VkDescriptorUpdateTemplateEntry))
                *> poke (ptr `plusPtr` 16) (vkOffset (poked :: VkDescriptorUpdateTemplateEntry))
                *> poke (ptr `plusPtr` 24) (vkStride (poked :: VkDescriptorUpdateTemplateEntry))
-- | VkDescriptorUpdateTemplateCreateInfo - Structure specifying parameters
-- of a newly created descriptor update template
data VkDescriptorUpdateTemplateCreateInfo = VkDescriptorUpdateTemplateCreateInfo
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkFlags"
  vkFlags :: VkDescriptorUpdateTemplateCreateFlags
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkDescriptorUpdateEntryCount"
  vkDescriptorUpdateEntryCount :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkPDescriptorUpdateEntries"
  vkPDescriptorUpdateEntries :: Ptr VkDescriptorUpdateTemplateEntry
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkTemplateType"
  vkTemplateType :: VkDescriptorUpdateTemplateType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkDescriptorSetLayout"
  vkDescriptorSetLayout :: VkDescriptorSetLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkPipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkPipelineLayout"
  vkPipelineLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "vkSet"
  vkSet :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorUpdateTemplateCreateInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkDescriptorUpdateTemplateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 48)
                                                  <*> peek (ptr `plusPtr` 56)
                                                  <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDescriptorUpdateEntryCount (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPDescriptorUpdateEntries (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkTemplateType (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkDescriptorSetLayout (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPipelineBindPoint (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPipelineLayout (poked :: VkDescriptorUpdateTemplateCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkSet (poked :: VkDescriptorUpdateTemplateCreateInfo))
