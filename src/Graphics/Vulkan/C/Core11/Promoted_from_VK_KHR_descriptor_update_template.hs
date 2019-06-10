{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplate
  , VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateType(..)
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , FN_vkCreateDescriptorUpdateTemplate
  , PFN_vkCreateDescriptorUpdateTemplate
  , vkCreateDescriptorUpdateTemplate
  , FN_vkDestroyDescriptorUpdateTemplate
  , PFN_vkDestroyDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  , FN_vkUpdateDescriptorSetWithTemplate
  , PFN_vkUpdateDescriptorSetWithTemplate
  , vkUpdateDescriptorSetWithTemplate
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
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
  ( FunPtr
  , Ptr
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
  ( VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorType(..)
  , VkDescriptorSet
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineLayout
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorUpdateTemplate_T
-- No documentation found for TopLevel "VkDescriptorUpdateTemplate"
type VkDescriptorUpdateTemplate = Ptr VkDescriptorUpdateTemplate_T

-- ** VkDescriptorUpdateTemplateCreateFlags

-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateFlags"
newtype VkDescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfo"
data VkDescriptorUpdateTemplateCreateInfo = VkDescriptorUpdateTemplateCreateInfo
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "flags"
  vkFlags :: VkDescriptorUpdateTemplateCreateFlags
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "descriptorUpdateEntryCount"
  vkDescriptorUpdateEntryCount :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pDescriptorUpdateEntries"
  vkPDescriptorUpdateEntries :: Ptr VkDescriptorUpdateTemplateEntry
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "templateType"
  vkTemplateType :: VkDescriptorUpdateTemplateType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "descriptorSetLayout"
  vkDescriptorSetLayout :: VkDescriptorSetLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pipelineLayout"
  vkPipelineLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "set"
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

instance Zero VkDescriptorUpdateTemplateCreateInfo where
  zero = VkDescriptorUpdateTemplateCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero

-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntry"
data VkDescriptorUpdateTemplateEntry = VkDescriptorUpdateTemplateEntry
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "descriptorCount"
  vkDescriptorCount :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "descriptorType"
  vkDescriptorType :: VkDescriptorType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "offset"
  vkOffset :: CSize
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "stride"
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

instance Zero VkDescriptorUpdateTemplateEntry where
  zero = VkDescriptorUpdateTemplateEntry zero
                                         zero
                                         zero
                                         zero
                                         zero
                                         zero

-- ** VkDescriptorUpdateTemplateType

-- No documentation found for TopLevel "VkDescriptorUpdateTemplateType"
newtype VkDescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplate"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorUpdateTemplate" vkCreateDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
#else
vkCreateDescriptorUpdateTemplate :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
vkCreateDescriptorUpdateTemplate deviceCmds = mkVkCreateDescriptorUpdateTemplate (pVkCreateDescriptorUpdateTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorUpdateTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult)
#endif

type FN_vkCreateDescriptorUpdateTemplate = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
type PFN_vkCreateDescriptorUpdateTemplate = FunPtr FN_vkCreateDescriptorUpdateTemplate

-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplate"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorUpdateTemplate" vkDestroyDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDescriptorUpdateTemplate :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorUpdateTemplate deviceCmds = mkVkDestroyDescriptorUpdateTemplate (pVkDestroyDescriptorUpdateTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorUpdateTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDescriptorUpdateTemplate = ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorUpdateTemplate = FunPtr FN_vkDestroyDescriptorUpdateTemplate

-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplate"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUpdateDescriptorSetWithTemplate" vkUpdateDescriptorSetWithTemplate :: ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
#else
vkUpdateDescriptorSetWithTemplate :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
vkUpdateDescriptorSetWithTemplate deviceCmds = mkVkUpdateDescriptorSetWithTemplate (pVkUpdateDescriptorSetWithTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSetWithTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ())
#endif

type FN_vkUpdateDescriptorSetWithTemplate = ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkUpdateDescriptorSetWithTemplate = FunPtr FN_vkUpdateDescriptorSetWithTemplate

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE = VkObjectType 1000085000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO = VkStructureType 1000085000
