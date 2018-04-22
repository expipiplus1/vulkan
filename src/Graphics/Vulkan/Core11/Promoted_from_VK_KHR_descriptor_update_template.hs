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
  ( VkObjectType(..)
  , VkResult(..)
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
--
-- = See Also
--
-- 'VkDescriptorUpdateTemplateCreateInfo'
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

-- | @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET@ specifies that the
-- descriptor update template will be used for descriptor set updates only.
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = VkDescriptorUpdateTemplateType 0
-- ** VkDescriptorUpdateTemplateCreateFlags

-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkDescriptorUpdateTemplateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkDescriptorUpdateTemplateCreateInfo'
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
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplateKHR',
-- 'vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
type VkDescriptorUpdateTemplate = Ptr VkDescriptorUpdateTemplate_T
-- | vkCreateDescriptorUpdateTemplate - Create a new descriptor update
-- template
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor update
--     template.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDescriptorUpdateTemplateCreateInfo' structure specifying the set
--     of descriptors to update with a single call to
--     'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR'
--     or 'vkUpdateDescriptorSetWithTemplate'.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pDescriptorUpdateTemplate@ points to a @VkDescriptorUpdateTemplate@
--     handle in which the resulting descriptor update template object is
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDescriptorUpdateTemplateCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pDescriptorUpdateTemplate@ /must/ be a valid pointer to a
--     @VkDescriptorUpdateTemplate@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorUpdateTemplate', 'VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorUpdateTemplate" vkCreateDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
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
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @descriptorSetLayout@
--     was created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when
--     @descriptorSetLayout@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @descriptorUpdateTemplate@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @descriptorUpdateTemplate@ /must/ be a valid
--     @VkDescriptorUpdateTemplate@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @descriptorUpdateTemplate@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorUpdateTemplate@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorUpdateTemplate" vkDestroyDescriptorUpdateTemplate :: ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkUpdateDescriptorSetWithTemplate - Update the contents of a descriptor
-- set object using an update template
--
-- = Parameters
--
-- -   @device@ is the logical device that updates the descriptor sets.
--
-- -   @descriptorSet@ is the descriptor set to update
--
-- -   @descriptorUpdateTemplate@ is the @VkDescriptorUpdateTemplate@ which
--     specifies the update mapping between @pData@ and the descriptor set
--     to update.
--
-- -   @pData@ is a pointer to memory which contains one or more structures
--     of 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo', or
--     'Graphics.Vulkan.Core10.BufferView.VkBufferView' used to write the
--     descriptors.
--
-- == Valid Usage
--
-- -   @pData@ /must/ be a valid pointer to a memory that contains one or
--     more valid instances of
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo', or
--     'Graphics.Vulkan.Core10.BufferView.VkBufferView' in a layout defined
--     by @descriptorUpdateTemplate@ when it was created with
--     'vkCreateDescriptorUpdateTemplate'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @descriptorSet@ /must/ be a valid @VkDescriptorSet@ handle
--
-- -   @descriptorUpdateTemplate@ /must/ be a valid
--     @VkDescriptorUpdateTemplate@ handle
--
-- -   @descriptorUpdateTemplate@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorSet@ /must/ be externally synchronized
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
-- >         0                                            // stride is not required if descriptorCount is 1.
-- >     },
-- >
-- >     // binding to an array of buffer descriptors
-- >     {
-- >         0,                                           // binding
-- >         0,                                           // dstArrayElement
-- >         3,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,           // descriptorType
-- >         offsetof(AppDataStructure, bufferInfoArray), // offset
-- >         sizeof(VkDescriptorBufferInfo)               // stride, descriptor buffer infos are compact
-- >     },
-- >
-- >     // binding to an array of buffer views
-- >     {
-- >         0,                                           // binding
-- >         3,                                           // dstArrayElement
-- >         1,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,     // descriptorType
-- >         offsetof(AppDataStructure, bufferView),      // offset
-- >         sizeof(AppBufferView)                        // stride, bufferViews do not have to be compact
-- >     },
-- > };
-- >
-- > // create an descriptor update template for descriptor set updates
-- > const VkDescriptorUpdateTemplateCreateInfo createInfo =
-- > {
-- >     VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,  // sType
-- >     NULL,                                                          // pNext
-- >     0,                                                             // flags
-- >     3,                                                             // descriptorUpdateEntryCount
-- >     descriptorUpdateTemplateEntries,                               // pDescriptorUpdateEntries
-- >     VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET,         // templateType
-- >     myLayout,                                                      // descriptorSetLayout
-- >     0,                                                             // pipelineBindPoint, ignored by given templateType
-- >     0,                                                             // pipelineLayout, ignored by given templateType
-- >     0,                                                             // set, ignored by given templateType
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
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet',
-- 'VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUpdateDescriptorSetWithTemplate" vkUpdateDescriptorSetWithTemplate :: ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
-- | VkDescriptorUpdateTemplateEntry - Describes a single descriptor update
-- of the descriptor update template
--
-- = Members
--
-- -   @dstBinding@ is the descriptor binding to update when using this
--     descriptor update template.
--
-- -   @dstArrayElement@ is the starting element in the array belonging to
--     @dstBinding@.
--
-- -   @descriptorCount@ is the number of descriptors to update. If
--     @descriptorCount@ is greater than the number of remaining array
--     elements in the destination binding, those affect consecutive
--     bindings in a manner similar to
--     'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet' above.
--
-- -   @descriptorType@ is a
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorType' specifying
--     the type of the descriptor.
--
-- -   @offset@ is the offset in bytes of the first binding in the raw data
--     structure.
--
-- -   @stride@ is the stride in bytes between two consecutive array
--     elements of the descriptor update informations in the raw data
--     structure. The actual pointer ptr for each array element j of update
--     entry i is computed using the following formula:
--
--     >     const char *ptr = (const char *)pData + pDescriptorUpdateEntries[i].offset + j * pDescriptorUpdateEntries[i].stride
--
--     The stride is useful in case the bindings are stored in structs
--     along with other data.
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
--     [{html_spec_relative}#descriptorsets-updates-consecutive](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates-consecutive)
--
-- == Valid Usage (Implicit)
--
-- -   @descriptorType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorType' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorType',
-- 'VkDescriptorUpdateTemplateCreateInfo'
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
-- | VkDescriptorUpdateTemplateCreateInfo - Structure specifying parameters
-- of a newly created descriptor update template
--
-- == Valid Usage
--
-- -   If @templateType@ is
--     @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET@,
--     @descriptorSetLayout@ /must/ be a valid @VkDescriptorSetLayout@
--     handle
--
-- -   If @templateType@ is
--     @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@,
--     @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value
--
-- -   If @templateType@ is
--     @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@,
--     @pipelineLayout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   If @templateType@ is
--     @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@, @set@
--     /must/ be the unique set number in the pipeline layout that uses a
--     descriptor set layout that was created with
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @pDescriptorUpdateEntries@ /must/ be a valid pointer to an array of
--     @descriptorUpdateEntryCount@ valid @VkDescriptorUpdateTemplateEntry@
--     structures
--
-- -   @templateType@ /must/ be a valid 'VkDescriptorUpdateTemplateType'
--     value
--
-- -   If @descriptorSetLayout@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @descriptorSetLayout@ /must/ be a valid @VkDescriptorSetLayout@
--     handle
--
-- -   @descriptorUpdateEntryCount@ /must/ be greater than @0@
--
-- -   Both of @descriptorSetLayout@, and @pipelineLayout@ that are valid
--     handles /must/ have been created, allocated, or retrieved from the
--     same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'VkDescriptorUpdateTemplateCreateFlags',
-- 'VkDescriptorUpdateTemplateEntry', 'VkDescriptorUpdateTemplateType',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR'
data VkDescriptorUpdateTemplateCreateInfo = VkDescriptorUpdateTemplateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkDescriptorUpdateTemplateCreateFlags
  , -- | @descriptorUpdateEntryCount@ is the number of elements in the
  -- @pDescriptorUpdateEntries@ array.
  vkDescriptorUpdateEntryCount :: Word32
  , -- | @pDescriptorUpdateEntries@ is a pointer to an array of
  -- 'VkDescriptorUpdateTemplateEntry' structures describing the descriptors
  -- to be updated by the descriptor update template.
  vkPDescriptorUpdateEntries :: Ptr VkDescriptorUpdateTemplateEntry
  , -- | @templateType@ Specifies the type of the descriptor update template. If
  -- set to @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET@ it /can/ only
  -- be used to update descriptor sets with a fixed @descriptorSetLayout@. If
  -- set to @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@ it
  -- /can/ only be used to push descriptor sets using the provided
  -- @pipelineBindPoint@, @pipelineLayout@, and @set@ number.
  vkTemplateType :: VkDescriptorUpdateTemplateType
  , -- | @descriptorSetLayout@ is the descriptor set layout the parameter update
  -- template will be used with. All descriptor sets which are going to be
  -- updated through the newly created descriptor update template /must/ be
  -- created with this layout. @descriptorSetLayout@ is the descriptor set
  -- layout used to build the descriptor update template. All descriptor sets
  -- which are going to be updated through the newly created descriptor
  -- update template /must/ be created with a layout that matches (is the
  -- same as, or defined identically to) this layout. This parameter is
  -- ignored if @templateType@ is not
  -- @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET@.
  vkDescriptorSetLayout :: VkDescriptorSetLayout
  , -- | @pipelineBindPoint@ is a
  -- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' indicating whether the
  -- descriptors will be used by graphics pipelines or compute pipelines.
  -- This parameter is ignored if @templateType@ is not
  -- @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- | @pipelineLayout@ is a @VkPipelineLayout@ object used to program the
  -- bindings. This parameter is ignored if @templateType@ is not
  -- @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@
  vkPipelineLayout :: VkPipelineLayout
  , -- | @set@ is the set number of the descriptor set in the pipeline layout
  -- that will be updated. This parameter is ignored if @templateType@ is not
  -- @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@
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
