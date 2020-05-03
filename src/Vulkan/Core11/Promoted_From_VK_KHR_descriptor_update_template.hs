{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template  ( createDescriptorUpdateTemplate
                                                                      , withDescriptorUpdateTemplate
                                                                      , destroyDescriptorUpdateTemplate
                                                                      , updateDescriptorSetWithTemplate
                                                                      , DescriptorUpdateTemplateEntry(..)
                                                                      , DescriptorUpdateTemplateCreateInfo(..)
                                                                      , DescriptorUpdateTemplate(..)
                                                                      , DescriptorUpdateTemplateCreateFlags(..)
                                                                      , StructureType(..)
                                                                      , DescriptorUpdateTemplateType(..)
                                                                      , ObjectType(..)
                                                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorUpdateTemplate))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorUpdateTemplate))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateDescriptorSetWithTemplate))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorUpdateTemplate
  :: FunPtr (Ptr Device_T -> Ptr DescriptorUpdateTemplateCreateInfo -> Ptr AllocationCallbacks -> Ptr DescriptorUpdateTemplate -> IO Result) -> Ptr Device_T -> Ptr DescriptorUpdateTemplateCreateInfo -> Ptr AllocationCallbacks -> Ptr DescriptorUpdateTemplate -> IO Result

-- | vkCreateDescriptorUpdateTemplate - Create a new descriptor update
-- template
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor update
--     template.
--
-- -   @pCreateInfo@ is a pointer to a 'DescriptorUpdateTemplateCreateInfo'
--     structure specifying the set of descriptors to update with a single
--     call to
--     'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR'
--     or 'updateDescriptorSetWithTemplate'.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDescriptorUpdateTemplate@ is a pointer to a
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle in which the
--     resulting descriptor update template object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DescriptorUpdateTemplateCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pDescriptorUpdateTemplate@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'DescriptorUpdateTemplateCreateInfo', 'Vulkan.Core10.Handles.Device'
createDescriptorUpdateTemplate :: forall io . MonadIO io => Device -> DescriptorUpdateTemplateCreateInfo -> ("allocator" ::: Maybe AllocationCallbacks) -> io (DescriptorUpdateTemplate)
createDescriptorUpdateTemplate device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorUpdateTemplate' = mkVkCreateDescriptorUpdateTemplate (pVkCreateDescriptorUpdateTemplate (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDescriptorUpdateTemplate <- ContT $ bracket (callocBytes @DescriptorUpdateTemplate 8) free
  r <- lift $ vkCreateDescriptorUpdateTemplate' (deviceHandle (device)) pCreateInfo pAllocator (pPDescriptorUpdateTemplate)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorUpdateTemplate <- lift $ peek @DescriptorUpdateTemplate pPDescriptorUpdateTemplate
  pure $ (pDescriptorUpdateTemplate)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorUpdateTemplate' and 'destroyDescriptorUpdateTemplate'
--
-- To ensure that 'destroyDescriptorUpdateTemplate' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withDescriptorUpdateTemplate :: forall io r . MonadIO io => (io (DescriptorUpdateTemplate) -> ((DescriptorUpdateTemplate) -> io ()) -> r) -> Device -> DescriptorUpdateTemplateCreateInfo -> Maybe AllocationCallbacks -> r
withDescriptorUpdateTemplate b device pCreateInfo pAllocator =
  b (createDescriptorUpdateTemplate device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorUpdateTemplate device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorUpdateTemplate
  :: FunPtr (Ptr Device_T -> DescriptorUpdateTemplate -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorUpdateTemplate -> Ptr AllocationCallbacks -> IO ()

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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorSetLayout@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorSetLayout@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @descriptorUpdateTemplate@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @descriptorUpdateTemplate@
--     /must/ be a valid 'Vulkan.Core11.Handles.DescriptorUpdateTemplate'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.Device'
destroyDescriptorUpdateTemplate :: forall io . MonadIO io => Device -> DescriptorUpdateTemplate -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyDescriptorUpdateTemplate device descriptorUpdateTemplate allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorUpdateTemplate' = mkVkDestroyDescriptorUpdateTemplate (pVkDestroyDescriptorUpdateTemplate (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDescriptorUpdateTemplate' (deviceHandle (device)) (descriptorUpdateTemplate) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSetWithTemplate
  :: FunPtr (Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> Ptr () -> IO ()) -> Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> Ptr () -> IO ()

-- | vkUpdateDescriptorSetWithTemplate - Update the contents of a descriptor
-- set object using an update template
--
-- = Parameters
--
-- -   @device@ is the logical device that updates the descriptor sets.
--
-- -   @descriptorSet@ is the descriptor set to update
--
-- -   @descriptorUpdateTemplate@ is a
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' object specifying
--     the update mapping between @pData@ and the descriptor set to update.
--
-- -   @pData@ is a pointer to memory containing one or more
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' structures used to write the
--     descriptors.
--
-- == Valid Usage
--
-- -   @pData@ /must/ be a valid pointer to a memory containing one or more
--     valid instances of
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' in a layout defined by
--     @descriptorUpdateTemplate@ when it was created with
--     'createDescriptorUpdateTemplate'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @descriptorSet@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   @descriptorUpdateTemplate@ /must/ be a valid
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
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
-- 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.Device'
updateDescriptorSetWithTemplate :: forall io . MonadIO io => Device -> DescriptorSet -> DescriptorUpdateTemplate -> ("data" ::: Ptr ()) -> io ()
updateDescriptorSetWithTemplate device descriptorSet descriptorUpdateTemplate data' = liftIO $ do
  let vkUpdateDescriptorSetWithTemplate' = mkVkUpdateDescriptorSetWithTemplate (pVkUpdateDescriptorSetWithTemplate (deviceCmds (device :: Device)))
  vkUpdateDescriptorSetWithTemplate' (deviceHandle (device)) (descriptorSet) (descriptorUpdateTemplate) (data')
  pure $ ()


-- | VkDescriptorUpdateTemplateEntry - Describes a single descriptor update
-- of the descriptor update template
--
-- == Valid Usage
--
-- -   @dstBinding@ /must/ be a valid binding in the descriptor set layout
--     implicitly specified when using a descriptor update template to
--     update descriptors
--
-- -   @dstArrayElement@ and @descriptorCount@ /must/ be less than or equal
--     to the number of array elements in the descriptor set binding
--     implicitly specified when using a descriptor update template to
--     update descriptors, and all applicable consecutive bindings, as
--     described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   If @descriptor@ type is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   If @descriptor@ type is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @descriptorType@ /must/ be a valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'DescriptorUpdateTemplateCreateInfo'
data DescriptorUpdateTemplateEntry = DescriptorUpdateTemplateEntry
  { -- | @dstBinding@ is the descriptor binding to update when using this
    -- descriptor update template.
    dstBinding :: Word32
  , -- | @dstArrayElement@ is the starting element in the array belonging to
    -- @dstBinding@. If the descriptor binding identified by @srcBinding@ has a
    -- descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @dstArrayElement@ specifies the starting byte offset to update.
    dstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to update. If
    -- @descriptorCount@ is greater than the number of remaining array elements
    -- in the destination binding, those affect consecutive bindings in a
    -- manner similar to 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'
    -- above. If the descriptor binding identified by @dstBinding@ has a
    -- descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @descriptorCount@ specifies the number of bytes to update and the
    -- remaining array elements in the destination binding refer to the
    -- remaining number of bytes in it.
    descriptorCount :: Word32
  , -- | @descriptorType@ is a
    -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' specifying the type
    -- of the descriptor.
    descriptorType :: DescriptorType
  , -- | @offset@ is the offset in bytes of the first binding in the raw data
    -- structure.
    offset :: Word64
  , -- | @stride@ is the stride in bytes between two consecutive array elements
    -- of the descriptor update informations in the raw data structure. The
    -- actual pointer ptr for each array element j of update entry i is
    -- computed using the following formula:
    --
    -- >     const char *ptr = (const char *)pData + pDescriptorUpdateEntries[i].offset + j * pDescriptorUpdateEntries[i].stride
    --
    -- The stride is useful in case the bindings are stored in structs along
    -- with other data. If @descriptorType@ is
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then the value of @stride@ is ignored and the stride is assumed to be
    -- @1@, i.e. the descriptor update information for them is always specified
    -- as a contiguous range.
    stride :: Word64
  }
  deriving (Typeable)
deriving instance Show DescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateEntry where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorUpdateTemplateEntry{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (dstBinding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (dstArrayElement)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (descriptorCount)
    poke ((p `plusPtr` 12 :: Ptr DescriptorType)) (descriptorType)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (offset))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (stride))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct DescriptorUpdateTemplateEntry where
  peekCStruct p = do
    dstBinding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 12 :: Ptr DescriptorType))
    offset <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    stride <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pure $ DescriptorUpdateTemplateEntry
             dstBinding dstArrayElement descriptorCount descriptorType ((\(CSize a) -> a) offset) ((\(CSize a) -> a) stride)

instance Storable DescriptorUpdateTemplateEntry where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorUpdateTemplateEntry where
  zero = DescriptorUpdateTemplateEntry
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorUpdateTemplateCreateInfo - Structure specifying parameters
-- of a newly created descriptor update template
--
-- == Valid Usage
--
-- -   If @templateType@ is
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET',
--     @descriptorSetLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- -   If @templateType@ is
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   If @templateType@ is
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @pipelineLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   If @templateType@ is
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR',
--     @set@ /must/ be the unique set number in the pipeline layout that
--     uses a descriptor set layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @pDescriptorUpdateEntries@ /must/ be a valid pointer to an array of
--     @descriptorUpdateEntryCount@ valid 'DescriptorUpdateTemplateEntry'
--     structures
--
-- -   @templateType@ /must/ be a valid
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType'
--     value
--
-- -   @descriptorUpdateEntryCount@ /must/ be greater than @0@
--
-- -   Both of @descriptorSetLayout@, and @pipelineLayout@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags.DescriptorUpdateTemplateCreateFlags',
-- 'DescriptorUpdateTemplateEntry',
-- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR'
data DescriptorUpdateTemplateCreateInfo = DescriptorUpdateTemplateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: DescriptorUpdateTemplateCreateFlags
  , -- | @pDescriptorUpdateEntries@ is a pointer to an array of
    -- 'DescriptorUpdateTemplateEntry' structures describing the descriptors to
    -- be updated by the descriptor update template.
    descriptorUpdateEntries :: Vector DescriptorUpdateTemplateEntry
  , -- | @templateType@ Specifies the type of the descriptor update template. If
    -- set to
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET'
    -- it /can/ only be used to update descriptor sets with a fixed
    -- @descriptorSetLayout@. If set to
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
    -- it /can/ only be used to push descriptor sets using the provided
    -- @pipelineBindPoint@, @pipelineLayout@, and @set@ number.
    templateType :: DescriptorUpdateTemplateType
  , -- | @descriptorSetLayout@ is the descriptor set layout the parameter update
    -- template will be used with. All descriptor sets which are going to be
    -- updated through the newly created descriptor update template /must/ be
    -- created with this layout. @descriptorSetLayout@ is the descriptor set
    -- layout used to build the descriptor update template. All descriptor sets
    -- which are going to be updated through the newly created descriptor
    -- update template /must/ be created with a layout that matches (is the
    -- same as, or defined identically to) this layout. This parameter is
    -- ignored if @templateType@ is not
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET'.
    descriptorSetLayout :: DescriptorSetLayout
  , -- | @pipelineBindPoint@ is a
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' indicating
    -- whether the descriptors will be used by graphics pipelines or compute
    -- pipelines. This parameter is ignored if @templateType@ is not
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pipelineLayout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used
    -- to program the bindings. This parameter is ignored if @templateType@ is
    -- not
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
    pipelineLayout :: PipelineLayout
  , -- | @set@ is the set number of the descriptor set in the pipeline layout
    -- that will be updated. This parameter is ignored if @templateType@ is not
    -- 'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
    set :: Word32
  }
  deriving (Typeable)
deriving instance Show DescriptorUpdateTemplateCreateInfo

instance ToCStruct DescriptorUpdateTemplateCreateInfo where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorUpdateTemplateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorUpdateEntries)) :: Word32))
    pPDescriptorUpdateEntries' <- ContT $ allocaBytesAligned @DescriptorUpdateTemplateEntry ((Data.Vector.length (descriptorUpdateEntries)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDescriptorUpdateEntries' `plusPtr` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry) (e) . ($ ())) (descriptorUpdateEntries)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry))) (pPDescriptorUpdateEntries')
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType)) (templateType)
    lift $ poke ((p `plusPtr` 40 :: Ptr DescriptorSetLayout)) (descriptorSetLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (pipelineLayout)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (set)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDescriptorUpdateEntries' <- ContT $ allocaBytesAligned @DescriptorUpdateTemplateEntry ((Data.Vector.length (mempty)) * 32) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDescriptorUpdateEntries' `plusPtr` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry))) (pPDescriptorUpdateEntries')
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DescriptorSetLayout)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct DescriptorUpdateTemplateCreateInfo where
  peekCStruct p = do
    flags <- peek @DescriptorUpdateTemplateCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplateCreateFlags))
    descriptorUpdateEntryCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDescriptorUpdateEntries <- peek @(Ptr DescriptorUpdateTemplateEntry) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry)))
    pDescriptorUpdateEntries' <- generateM (fromIntegral descriptorUpdateEntryCount) (\i -> peekCStruct @DescriptorUpdateTemplateEntry ((pDescriptorUpdateEntries `advancePtrBytes` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry)))
    templateType <- peek @DescriptorUpdateTemplateType ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType))
    descriptorSetLayout <- peek @DescriptorSetLayout ((p `plusPtr` 40 :: Ptr DescriptorSetLayout))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 48 :: Ptr PipelineBindPoint))
    pipelineLayout <- peek @PipelineLayout ((p `plusPtr` 56 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pure $ DescriptorUpdateTemplateCreateInfo
             flags pDescriptorUpdateEntries' templateType descriptorSetLayout pipelineBindPoint pipelineLayout set

instance Zero DescriptorUpdateTemplateCreateInfo where
  zero = DescriptorUpdateTemplateCreateInfo
           zero
           mempty
           zero
           zero
           zero
           zero
           zero

