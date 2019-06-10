{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplate
  , DescriptorUpdateTemplateCreateFlags
  , DescriptorUpdateTemplateCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , DescriptorUpdateTemplateCreateInfo(..)
#endif
  , DescriptorUpdateTemplateEntry(..)
  , DescriptorUpdateTemplateType
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , DescriptorUpdateTemplateTypeKHR
  , createDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  , withDescriptorUpdateTemplate
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateType(..)
  , VkDescriptorUpdateTemplate
  , vkCreateDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  , vkUpdateDescriptorSetWithTemplate
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  , DescriptorType
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )


-- No documentation found for TopLevel "DescriptorUpdateTemplate"
type DescriptorUpdateTemplate = VkDescriptorUpdateTemplate

-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlags"
type DescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags


-- No complete pragma for DescriptorUpdateTemplateCreateFlags as it has no patterns

-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfo"
data DescriptorUpdateTemplateCreateInfo = DescriptorUpdateTemplateCreateInfo
  { -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "flags"
  flags :: DescriptorUpdateTemplateCreateFlags
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pDescriptorUpdateEntries"
  descriptorUpdateEntries :: Vector DescriptorUpdateTemplateEntry
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "templateType"
  templateType :: DescriptorUpdateTemplateType
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "descriptorSetLayout"
  descriptorSetLayout :: DescriptorSetLayout
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pipelineLayout"
  pipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "set"
  set :: Word32
  }
  deriving (Show, Eq)

instance Zero DescriptorUpdateTemplateCreateInfo where
  zero = DescriptorUpdateTemplateCreateInfo Nothing
                                            zero
                                            mempty
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero

#endif


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntry"
data DescriptorUpdateTemplateEntry = DescriptorUpdateTemplateEntry
  { -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "dstBinding"
  dstBinding :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "dstArrayElement"
  dstArrayElement :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "descriptorCount"
  descriptorCount :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "offset"
  offset :: CSize
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "stride"
  stride :: CSize
  }
  deriving (Show, Eq)

instance Zero DescriptorUpdateTemplateEntry where
  zero = DescriptorUpdateTemplateEntry zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero


-- No documentation found for TopLevel "DescriptorUpdateTemplateType"
type DescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType


{-# complete DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET, DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR, DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: DescriptorUpdateTemplateType #-}


-- No documentation found for Nested "DescriptorUpdateTemplateType" "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET :: (a ~ DescriptorUpdateTemplateType) => a
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET


-- No documentation found for Nested "DescriptorUpdateTemplateType" "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: (a ~ DescriptorUpdateTemplateType) => a
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR

-- No documentation found for TopLevel "DescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType


-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplate"
createDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplateCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorUpdateTemplate)
createDescriptorUpdateTemplate = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplate"
destroyDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplate ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorUpdateTemplate = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplate"
updateDescriptorSetWithTemplate :: Device ->  DescriptorSet ->  DescriptorUpdateTemplate ->  Ptr () ->  IO ()
updateDescriptorSetWithTemplate = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createDescriptorUpdateTemplate' and 'destroyDescriptorUpdateTemplate' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorUpdateTemplate
  :: Device -> DescriptorUpdateTemplateCreateInfo -> Maybe AllocationCallbacks -> (DescriptorUpdateTemplate -> IO a) -> IO a
withDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks = bracket
  (createDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorUpdateTemplate device o allocationCallbacks)
