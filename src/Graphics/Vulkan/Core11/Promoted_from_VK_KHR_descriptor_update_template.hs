{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

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
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
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
  , vkCreateDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  , vkUpdateDescriptorSetWithTemplate
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
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


-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate'
type DescriptorUpdateTemplate = VkDescriptorUpdateTemplate

-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
type DescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags

-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags


-- | VkDescriptorUpdateTemplateCreateInfo - Structure specifying parameters
-- of a newly created descriptor update template
--
-- == Valid Usage
--
-- -   If @templateType@ is
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET',
--     @descriptorSetLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handle
--
-- -   If @templateType@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value
--
-- -   If @templateType@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @pipelineLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   If @templateType@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @set@ /must/ be the unique set number in the pipeline layout that
--     uses a descriptor set layout that was created with
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- Unresolved directive in VkDescriptorUpdateTemplateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorUpdateTemplateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateEntry',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateType',
-- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate'
data DescriptorUpdateTemplateCreateInfo = DescriptorUpdateTemplateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorUpdateTemplateCreateInfo" "flags"
  flags :: DescriptorUpdateTemplateCreateFlags
  -- Length valued member elided
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

-- | A function to temporarily allocate memory for a 'VkDescriptorUpdateTemplateCreateInfo' and
-- marshal a 'DescriptorUpdateTemplateCreateInfo' into it. The 'VkDescriptorUpdateTemplateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorUpdateTemplateCreateInfo :: DescriptorUpdateTemplateCreateInfo -> (VkDescriptorUpdateTemplateCreateInfo -> IO a) -> IO a
withCStructDescriptorUpdateTemplateCreateInfo marshalled cont = withVec withCStructDescriptorUpdateTemplateEntry (descriptorUpdateEntries (marshalled :: DescriptorUpdateTemplateCreateInfo)) (\pPDescriptorUpdateEntries -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorUpdateTemplateCreateInfo)) (\pPNext -> cont (VkDescriptorUpdateTemplateCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO pPNext (flags (marshalled :: DescriptorUpdateTemplateCreateInfo)) (fromIntegral (Data.Vector.length (descriptorUpdateEntries (marshalled :: DescriptorUpdateTemplateCreateInfo)))) pPDescriptorUpdateEntries (templateType (marshalled :: DescriptorUpdateTemplateCreateInfo)) (descriptorSetLayout (marshalled :: DescriptorUpdateTemplateCreateInfo)) (pipelineBindPoint (marshalled :: DescriptorUpdateTemplateCreateInfo)) (pipelineLayout (marshalled :: DescriptorUpdateTemplateCreateInfo)) (set (marshalled :: DescriptorUpdateTemplateCreateInfo)))))

-- | A function to read a 'VkDescriptorUpdateTemplateCreateInfo' and all additional
-- structures in the pointer chain into a 'DescriptorUpdateTemplateCreateInfo'.
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



-- | VkDescriptorUpdateTemplateEntry - Describes a single descriptor update
-- of the descriptor update template
--
-- == Valid Usage
--
-- -   @dstBinding@ /must/ be a valid binding in the descriptor set layout
--     implicitly specified when using a descriptor update template to
--     update descriptors.
--
-- -   @dstArrayElement@ and @descriptorCount@ /must/ be less than or equal
--     to the number of array elements in the descriptor set binding
--     implicitly specified when using a descriptor update template to
--     update descriptors, and all applicable consecutive bindings, as
--     described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   If @descriptor@ type is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   If @descriptor@ type is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- Unresolved directive in VkDescriptorUpdateTemplateEntry.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorUpdateTemplateEntry.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
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

-- | A function to temporarily allocate memory for a 'VkDescriptorUpdateTemplateEntry' and
-- marshal a 'DescriptorUpdateTemplateEntry' into it. The 'VkDescriptorUpdateTemplateEntry' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorUpdateTemplateEntry :: DescriptorUpdateTemplateEntry -> (VkDescriptorUpdateTemplateEntry -> IO a) -> IO a
withCStructDescriptorUpdateTemplateEntry marshalled cont = cont (VkDescriptorUpdateTemplateEntry (dstBinding (marshalled :: DescriptorUpdateTemplateEntry)) (dstArrayElement (marshalled :: DescriptorUpdateTemplateEntry)) (descriptorCount (marshalled :: DescriptorUpdateTemplateEntry)) (descriptorType (marshalled :: DescriptorUpdateTemplateEntry)) (offset (marshalled :: DescriptorUpdateTemplateEntry)) (stride (marshalled :: DescriptorUpdateTemplateEntry)))

-- | A function to read a 'VkDescriptorUpdateTemplateEntry' and all additional
-- structures in the pointer chain into a 'DescriptorUpdateTemplateEntry'.
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


-- | VkDescriptorUpdateTemplateType - Indicates the valid usage of the
-- descriptor update template
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
type DescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET'
-- specifies that the descriptor update template will be used for
-- descriptor set updates only.
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET :: (a ~ DescriptorUpdateTemplateType) => a
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET

-- No documentation found for TopLevel "DescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType


-- | vkCreateDescriptorUpdateTemplate - Create a new descriptor update
-- template
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor update
--     template.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
--     structure specifying the set of descriptors to update with a single
--     call to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate'.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDescriptorUpdateTemplate@ points to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate'
--     handle in which the resulting descriptor update template object is
--     returned.
--
-- = Description
--
-- Unresolved directive in vkCreateDescriptorUpdateTemplate.txt -
-- include::{generated}\/validity\/protos\/vkCreateDescriptorUpdateTemplate.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplateCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorUpdateTemplate)
createDescriptorUpdateTemplate = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pDescriptorUpdateTemplate' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDescriptorUpdateTemplateCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDescriptorUpdateTemplate commandTable device' pCreateInfo' pAllocator pDescriptorUpdateTemplate' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pDescriptorUpdateTemplate')))))


-- | vkDestroyDescriptorUpdateTemplate - Destroy a descriptor update template
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that has been used to create the
--     descriptor update template
--
-- -   @descriptorUpdateTemplate@ is the descriptor update template to
--     destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorSetLayout@ was created, a compatible
--     set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorSetLayout@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- Unresolved directive in vkDestroyDescriptorUpdateTemplate.txt -
-- include::{generated}\/validity\/protos\/vkDestroyDescriptorUpdateTemplate.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyDescriptorUpdateTemplate :: Device ->  DescriptorUpdateTemplate ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorUpdateTemplate = \(Device device' commandTable) -> \descriptorUpdateTemplate' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyDescriptorUpdateTemplate commandTable device' descriptorUpdateTemplate' pAllocator *> (pure ()))


-- | vkUpdateDescriptorSetWithTemplate - Update the contents of a descriptor
-- set object using an update template
--
-- = Parameters
--
-- -   @device@ is the logical device that updates the descriptor sets.
--
-- -   @descriptorSet@ is the descriptor set to update
--
-- -   @descriptorUpdateTemplate@ is the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate'
--     which specifies the update mapping between @pData@ and the
--     descriptor set to update.
--
-- -   @pData@ is a pointer to memory which contains one or more structures
--     of 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo', or
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' used to write the
--     descriptors.
--
-- == Valid Usage
--
-- Unresolved directive in vkUpdateDescriptorSetWithTemplate.txt -
-- include::{generated}\/validity\/protos\/vkUpdateDescriptorSetWithTemplate.txt[]
--
-- __API example.__
--
-- > struct AppBufferView {
-- >     VkBufferView bufferView;
-- >     uint32_t     applicationRelatedInformation;
-- > };
-- >
-- > struct AppDataStructure
-- > {
-- >     VkDescriptorImageInfo  imageInfo;          // a single image info
-- >     VkDescriptorBufferInfo bufferInfoArray[3]; // 3 buffer infos in an array
-- >     AppBufferView          bufferView[2];      // An application defined structure containing a bufferView
-- >     // ... some more application related data
-- > };
-- >
-- > const VkDescriptorUpdateTemplateEntry descriptorUpdateTemplateEntries[] =
-- > {
-- >     // binding to a single image descriptor
-- >     {
-- >         0,                                           // binding
-- >         0,                                           // dstArrayElement
-- >         1,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,   // descriptorType
-- >         offsetof(AppDataStructure, imageInfo),       // offset
-- >         0                                            // stride is not required if descriptorCount is 1
-- >     },
-- >
-- >     // binding to an array of buffer descriptors
-- >     {
-- >         1,                                           // binding
-- >         0,                                           // dstArrayElement
-- >         3,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,           // descriptorType
-- >         offsetof(AppDataStructure, bufferInfoArray), // offset
-- >         sizeof(VkDescriptorBufferInfo)               // stride, descriptor buffer infos are compact
-- >     },
-- >
-- >     // binding to an array of buffer views
-- >     {
-- >         2,                                           // binding
-- >         0,                                           // dstArrayElement
-- >         2,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,     // descriptorType
-- >         offsetof(AppDataStructure, bufferView) +
-- >           offsetof(AppBufferView, bufferView),       // offset
-- >         sizeof(AppBufferView)                        // stride, bufferViews do not have to be compact
-- >     },
-- > };
-- >
-- > // create a descriptor update template for descriptor set updates
-- > const VkDescriptorUpdateTemplateCreateInfo createInfo =
-- > {
-- >     VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,  // sType
-- >     NULL,                                                      // pNext
-- >     0,                                                         // flags
-- >     3,                                                         // descriptorUpdateEntryCount
-- >     descriptorUpdateTemplateEntries,                           // pDescriptorUpdateEntries
-- >     VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET,         // templateType
-- >     myLayout,                                                  // descriptorSetLayout
-- >     0,                                                         // pipelineBindPoint, ignored by given templateType
-- >     0,                                                         // pipelineLayout, ignored by given templateType
-- >     0,                                                         // set, ignored by given templateType
-- > };
-- >
-- > VkDescriptorUpdateTemplate myDescriptorUpdateTemplate;
-- > myResult = vkCreateDescriptorUpdateTemplate(
-- >     myDevice,
-- >     &createInfo,
-- >     NULL,
-- >     &myDescriptorUpdateTemplate);
-- > }
-- >
-- >
-- > AppDataStructure appData;
-- >
-- > // fill appData here or cache it in your engine
-- > vkUpdateDescriptorSetWithTemplate(myDevice, myDescriptorSet, myDescriptorUpdateTemplate, &appData);
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
updateDescriptorSetWithTemplate :: Device ->  DescriptorSet ->  DescriptorUpdateTemplate ->  Ptr () ->  IO ()
updateDescriptorSetWithTemplate = \(Device device' commandTable) -> \descriptorSet' -> \descriptorUpdateTemplate' -> \pData' -> vkUpdateDescriptorSetWithTemplate commandTable device' descriptorSet' descriptorUpdateTemplate' pData' *> (pure ())

-- | A safe wrapper for 'createDescriptorUpdateTemplate' and 'destroyDescriptorUpdateTemplate' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorUpdateTemplate
  :: Device -> DescriptorUpdateTemplateCreateInfo -> Maybe (AllocationCallbacks) -> (DescriptorUpdateTemplate -> IO a) -> IO a
withDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks = bracket
  (createDescriptorUpdateTemplate device descriptorUpdateTemplateCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorUpdateTemplate device o allocationCallbacks)
