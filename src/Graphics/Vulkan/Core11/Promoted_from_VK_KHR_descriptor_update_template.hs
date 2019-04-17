{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplate
  , DescriptorUpdateTemplateCreateFlags
  , DescriptorUpdateTemplateCreateFlagsKHR
  , withCStructDescriptorUpdateTemplateCreateInfo
  , fromCStructDescriptorUpdateTemplateCreateInfo
  , DescriptorUpdateTemplateCreateInfo(..)
  , withCStructDescriptorUpdateTemplateEntry
  , fromCStructDescriptorUpdateTemplateEntry
  , DescriptorUpdateTemplateEntry(..)
  , DescriptorUpdateTemplateType
  , DescriptorUpdateTemplateTypeKHR
  , createDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  , withDescriptorUpdateTemplate
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
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
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateType(..)
  , VkDescriptorUpdateTemplate
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  , DescriptorType
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  )


-- No documentation found for TopLevel "DescriptorUpdateTemplate"
type DescriptorUpdateTemplate = VkDescriptorUpdateTemplate
-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlags"
type DescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags
-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags
-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateInfo"
data DescriptorUpdateTemplateCreateInfo = DescriptorUpdateTemplateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "flags"
  vkFlags :: DescriptorUpdateTemplateCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pDescriptorUpdateEntries"
  vkPDescriptorUpdateEntries :: Vector DescriptorUpdateTemplateEntry
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "templateType"
  vkTemplateType :: DescriptorUpdateTemplateType
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "descriptorSetLayout"
  vkDescriptorSetLayout :: DescriptorSetLayout
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pipelineBindPoint"
  vkPipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pipelineLayout"
  vkPipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "set"
  vkSet :: Word32
  }
  deriving (Show, Eq)
withCStructDescriptorUpdateTemplateCreateInfo :: DescriptorUpdateTemplateCreateInfo -> (VkDescriptorUpdateTemplateCreateInfo -> IO a) -> IO a
withCStructDescriptorUpdateTemplateCreateInfo from cont = withVec withCStructDescriptorUpdateTemplateEntry (vkPDescriptorUpdateEntries (from :: DescriptorUpdateTemplateCreateInfo)) (\pDescriptorUpdateEntries -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorUpdateTemplateCreateInfo)) (\pPNext -> cont (VkDescriptorUpdateTemplateCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO pPNext (vkFlags (from :: DescriptorUpdateTemplateCreateInfo)) (fromIntegral (Data.Vector.length (vkPDescriptorUpdateEntries (from :: DescriptorUpdateTemplateCreateInfo)))) pDescriptorUpdateEntries (vkTemplateType (from :: DescriptorUpdateTemplateCreateInfo)) (vkDescriptorSetLayout (from :: DescriptorUpdateTemplateCreateInfo)) (vkPipelineBindPoint (from :: DescriptorUpdateTemplateCreateInfo)) (vkPipelineLayout (from :: DescriptorUpdateTemplateCreateInfo)) (vkSet (from :: DescriptorUpdateTemplateCreateInfo)))))
fromCStructDescriptorUpdateTemplateCreateInfo :: VkDescriptorUpdateTemplateCreateInfo -> IO DescriptorUpdateTemplateCreateInfo
fromCStructDescriptorUpdateTemplateCreateInfo c = DescriptorUpdateTemplateCreateInfo <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorUpdateTemplateCreateInfo)))
                                                                                     <*> pure (vkFlags (c :: VkDescriptorUpdateTemplateCreateInfo))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorUpdateEntryCount (c :: VkDescriptorUpdateTemplateCreateInfo))) (((fromCStructDescriptorUpdateTemplateEntry <=<) . peekElemOff) (vkPDescriptorUpdateEntries (c :: VkDescriptorUpdateTemplateCreateInfo))))
                                                                                     <*> pure (vkTemplateType (c :: VkDescriptorUpdateTemplateCreateInfo))
                                                                                     <*> pure (vkDescriptorSetLayout (c :: VkDescriptorUpdateTemplateCreateInfo))
                                                                                     <*> pure (vkPipelineBindPoint (c :: VkDescriptorUpdateTemplateCreateInfo))
                                                                                     <*> pure (vkPipelineLayout (c :: VkDescriptorUpdateTemplateCreateInfo))
                                                                                     <*> pure (vkSet (c :: VkDescriptorUpdateTemplateCreateInfo))
instance Zero DescriptorUpdateTemplateCreateInfo where
  zero = DescriptorUpdateTemplateCreateInfo Nothing
                                            zero
                                            Data.Vector.empty
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
-- No documentation found for TopLevel "DescriptorUpdateTemplateEntry"
data DescriptorUpdateTemplateEntry = DescriptorUpdateTemplateEntry
  { -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "descriptorCount"
  vkDescriptorCount :: Word32
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "descriptorType"
  vkDescriptorType :: DescriptorType
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "offset"
  vkOffset :: CSize
  , -- No documentation found for Nested "DescriptorUpdateTemplateEntry" "stride"
  vkStride :: CSize
  }
  deriving (Show, Eq)
withCStructDescriptorUpdateTemplateEntry :: DescriptorUpdateTemplateEntry -> (VkDescriptorUpdateTemplateEntry -> IO a) -> IO a
withCStructDescriptorUpdateTemplateEntry from cont = cont (VkDescriptorUpdateTemplateEntry (vkDstBinding (from :: DescriptorUpdateTemplateEntry)) (vkDstArrayElement (from :: DescriptorUpdateTemplateEntry)) (vkDescriptorCount (from :: DescriptorUpdateTemplateEntry)) (vkDescriptorType (from :: DescriptorUpdateTemplateEntry)) (vkOffset (from :: DescriptorUpdateTemplateEntry)) (vkStride (from :: DescriptorUpdateTemplateEntry)))
fromCStructDescriptorUpdateTemplateEntry :: VkDescriptorUpdateTemplateEntry -> IO DescriptorUpdateTemplateEntry
fromCStructDescriptorUpdateTemplateEntry c = DescriptorUpdateTemplateEntry <$> pure (vkDstBinding (c :: VkDescriptorUpdateTemplateEntry))
                                                                           <*> pure (vkDstArrayElement (c :: VkDescriptorUpdateTemplateEntry))
                                                                           <*> pure (vkDescriptorCount (c :: VkDescriptorUpdateTemplateEntry))
                                                                           <*> pure (vkDescriptorType (c :: VkDescriptorUpdateTemplateEntry))
                                                                           <*> pure (vkOffset (c :: VkDescriptorUpdateTemplateEntry))
                                                                           <*> pure (vkStride (c :: VkDescriptorUpdateTemplateEntry))
instance Zero DescriptorUpdateTemplateEntry where
  zero = DescriptorUpdateTemplateEntry zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
-- No documentation found for TopLevel "DescriptorUpdateTemplateType"
type DescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType
-- No documentation found for TopLevel "DescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType

-- | Wrapper for 'vkCreateDescriptorUpdateTemplate'
createDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplateCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorUpdateTemplate)
createDescriptorUpdateTemplate = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pDescriptorUpdateTemplate -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDescriptorUpdateTemplateCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDescriptorUpdateTemplate commandTable device pCreateInfo pAllocator pDescriptorUpdateTemplate >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pDescriptorUpdateTemplate)))))

-- | Wrapper for 'vkDestroyDescriptorUpdateTemplate'
destroyDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplate ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorUpdateTemplate = \(Device device commandTable) -> \descriptorUpdateTemplate -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDescriptorUpdateTemplate commandTable device descriptorUpdateTemplate pAllocator *> (pure ()))

-- | Wrapper for 'vkUpdateDescriptorSetWithTemplate'
updateDescriptorSetWithTemplate :: Device ->  DescriptorSet ->  DescriptorUpdateTemplate ->  Ptr () ->  IO ()
updateDescriptorSetWithTemplate = \(Device device commandTable) -> \descriptorSet -> \descriptorUpdateTemplate -> \pData -> Graphics.Vulkan.C.Dynamic.updateDescriptorSetWithTemplate commandTable device descriptorSet descriptorUpdateTemplate pData *> (pure ())
-- | Wrapper for 'createDescriptorUpdateTemplate' and 'destroyDescriptorUpdateTemplate' using 'bracket'
withDescriptorUpdateTemplate
  :: Device -> DescriptorUpdateTemplateCreateInfo -> Maybe (AllocationCallbacks) -> (DescriptorUpdateTemplate -> IO a) -> IO a
withDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks = bracket
  (createDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorUpdateTemplate device o allocationCallbacks)
