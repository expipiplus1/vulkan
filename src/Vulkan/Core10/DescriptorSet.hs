{-# language CPP #-}
-- No documentation found for Chapter "DescriptorSet"
module Vulkan.Core10.DescriptorSet  ( createDescriptorSetLayout
                                    , withDescriptorSetLayout
                                    , destroyDescriptorSetLayout
                                    , createDescriptorPool
                                    , withDescriptorPool
                                    , destroyDescriptorPool
                                    , resetDescriptorPool
                                    , allocateDescriptorSets
                                    , withDescriptorSets
                                    , freeDescriptorSets
                                    , updateDescriptorSets
                                    , DescriptorBufferInfo(..)
                                    , DescriptorImageInfo(..)
                                    , WriteDescriptorSet(..)
                                    , CopyDescriptorSet(..)
                                    , DescriptorSetLayoutBinding(..)
                                    , DescriptorSetLayoutCreateInfo(..)
                                    , DescriptorPoolSize(..)
                                    , DescriptorPoolCreateInfo(..)
                                    , DescriptorSetAllocateInfo(..)
                                    , DescriptorSet(..)
                                    , DescriptorSetLayout(..)
                                    , DescriptorPool(..)
                                    , DescriptorPoolResetFlags(..)
                                    , DescriptorType(..)
                                    , DescriptorPoolCreateFlagBits(..)
                                    , DescriptorPoolCreateFlags
                                    , DescriptorSetLayoutCreateFlagBits(..)
                                    , DescriptorSetLayoutCreateFlags
                                    ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (BufferView)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (DescriptorPool)
import Vulkan.Core10.Handles (DescriptorPool(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (DescriptorPoolInlineUniformBlockCreateInfoEXT)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags(..))
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetLayoutBindingFlagsCreateInfo)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountAllocateInfo)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAllocateDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorSetLayout))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorSetLayout))
import Vulkan.Dynamic (DeviceCmds(pVkFreeDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkResetDescriptorPool))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateDescriptorSets))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_mutable_descriptor_type (MutableDescriptorTypeCreateInfoVALVE)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Sampler)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (WriteDescriptorSetAccelerationStructureKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (WriteDescriptorSetAccelerationStructureNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (WriteDescriptorSetInlineUniformBlockEXT)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (DescriptorPool(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags(..))
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorSetLayout
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorSetLayout -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorSetLayout -> IO Result

-- | vkCreateDescriptorSetLayout - Create a new descriptor set layout
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDescriptorSetLayout-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateDescriptorSetLayout-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DescriptorSetLayoutCreateInfo' structure
--
-- -   #VUID-vkCreateDescriptorSetLayout-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDescriptorSetLayout-pSetLayout-parameter# @pSetLayout@
--     /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handle
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
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'DescriptorSetLayoutCreateInfo', 'Vulkan.Core10.Handles.Device'
createDescriptorSetLayout :: forall a io
                           . (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, MonadIO io)
                          => -- | @device@ is the logical device that creates the descriptor set layout.
                             Device
                          -> -- | @pCreateInfo@ is a pointer to a 'DescriptorSetLayoutCreateInfo'
                             -- structure specifying the state of the descriptor set layout object.
                             (DescriptorSetLayoutCreateInfo a)
                          -> -- | @pAllocator@ controls host memory allocation as described in the
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                             -- chapter.
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io (DescriptorSetLayout)
createDescriptorSetLayout device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorSetLayoutPtr = pVkCreateDescriptorSetLayout (deviceCmds (device :: Device))
  lift $ unless (vkCreateDescriptorSetLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDescriptorSetLayout is null" Nothing Nothing
  let vkCreateDescriptorSetLayout' = mkVkCreateDescriptorSetLayout vkCreateDescriptorSetLayoutPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSetLayout <- ContT $ bracket (callocBytes @DescriptorSetLayout 8) free
  r <- lift $ traceAroundEvent "vkCreateDescriptorSetLayout" (vkCreateDescriptorSetLayout' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSetLayout))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSetLayout <- lift $ peek @DescriptorSetLayout pPSetLayout
  pure $ (pSetLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorSetLayout' and 'destroyDescriptorSetLayout'
--
-- To ensure that 'destroyDescriptorSetLayout' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorSetLayout :: forall a io r . (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorSetLayoutCreateInfo a -> Maybe AllocationCallbacks -> (io DescriptorSetLayout -> (DescriptorSetLayout -> io ()) -> r) -> r
withDescriptorSetLayout device pCreateInfo pAllocator b =
  b (createDescriptorSetLayout device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorSetLayout device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorSetLayout
  :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorSetLayout -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDescriptorSetLayout - Destroy a descriptor set layout object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDescriptorSetLayout-descriptorSetLayout-00284# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorSetLayout@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyDescriptorSetLayout-descriptorSetLayout-00285# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorSetLayout@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDescriptorSetLayout-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyDescriptorSetLayout-descriptorSetLayout-parameter# If
--     @descriptorSetLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @descriptorSetLayout@
--     /must/ be a valid 'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- -   #VUID-vkDestroyDescriptorSetLayout-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyDescriptorSetLayout-descriptorSetLayout-parent# If
--     @descriptorSetLayout@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorSetLayout@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.Handles.Device'
destroyDescriptorSetLayout :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device that destroys the descriptor set layout.
                              Device
                           -> -- | @descriptorSetLayout@ is the descriptor set layout to destroy.
                              DescriptorSetLayout
                           -> -- | @pAllocator@ controls host memory allocation as described in the
                              -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                              -- chapter.
                              ("allocator" ::: Maybe AllocationCallbacks)
                           -> io ()
destroyDescriptorSetLayout device descriptorSetLayout allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorSetLayoutPtr = pVkDestroyDescriptorSetLayout (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDescriptorSetLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDescriptorSetLayout is null" Nothing Nothing
  let vkDestroyDescriptorSetLayout' = mkVkDestroyDescriptorSetLayout vkDestroyDescriptorSetLayoutPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDescriptorSetLayout" (vkDestroyDescriptorSetLayout' (deviceHandle (device)) (descriptorSetLayout) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorPool
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorPool -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr DescriptorPool -> IO Result

-- | vkCreateDescriptorPool - Creates a descriptor pool object
--
-- = Description
--
-- @pAllocator@ controls host memory allocation as described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
-- chapter.
--
-- The created descriptor pool is returned in @pDescriptorPool@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDescriptorPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateDescriptorPool-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'DescriptorPoolCreateInfo'
--     structure
--
-- -   #VUID-vkCreateDescriptorPool-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateDescriptorPool-pDescriptorPool-parameter#
--     @pDescriptorPool@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.DescriptorPool' handle
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
--     -   'Vulkan.Extensions.VK_EXT_descriptor_indexing.ERROR_FRAGMENTATION_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.DescriptorPool', 'DescriptorPoolCreateInfo',
-- 'Vulkan.Core10.Handles.Device'
createDescriptorPool :: forall a io
                      . (Extendss DescriptorPoolCreateInfo a, PokeChain a, MonadIO io)
                     => -- | @device@ is the logical device that creates the descriptor pool.
                        Device
                     -> -- | @pCreateInfo@ is a pointer to a 'DescriptorPoolCreateInfo' structure
                        -- specifying the state of the descriptor pool object.
                        (DescriptorPoolCreateInfo a)
                     -> -- | @pAllocator@ controls host memory allocation as described in the
                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                        -- chapter.
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (DescriptorPool)
createDescriptorPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorPoolPtr = pVkCreateDescriptorPool (deviceCmds (device :: Device))
  lift $ unless (vkCreateDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDescriptorPool is null" Nothing Nothing
  let vkCreateDescriptorPool' = mkVkCreateDescriptorPool vkCreateDescriptorPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDescriptorPool <- ContT $ bracket (callocBytes @DescriptorPool 8) free
  r <- lift $ traceAroundEvent "vkCreateDescriptorPool" (vkCreateDescriptorPool' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPDescriptorPool))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorPool <- lift $ peek @DescriptorPool pPDescriptorPool
  pure $ (pDescriptorPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorPool' and 'destroyDescriptorPool'
--
-- To ensure that 'destroyDescriptorPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorPool :: forall a io r . (Extendss DescriptorPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorPoolCreateInfo a -> Maybe AllocationCallbacks -> (io DescriptorPool -> (DescriptorPool -> io ()) -> r) -> r
withDescriptorPool device pCreateInfo pAllocator b =
  b (createDescriptorPool device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorPool
  :: FunPtr (Ptr Device_T -> DescriptorPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorPool -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDescriptorPool - Destroy a descriptor pool object
--
-- = Description
--
-- When a pool is destroyed, all descriptor sets allocated from the pool
-- are implicitly freed and become invalid. Descriptor sets allocated from
-- a given pool do not need to be freed before destroying that descriptor
-- pool.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDescriptorPool-descriptorPool-00303# All submitted
--     commands that refer to @descriptorPool@ (via any allocated
--     descriptor sets) /must/ have completed execution
--
-- -   #VUID-vkDestroyDescriptorPool-descriptorPool-00304# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyDescriptorPool-descriptorPool-00305# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @descriptorPool@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDescriptorPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyDescriptorPool-descriptorPool-parameter# If
--     @descriptorPool@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @descriptorPool@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorPool' handle
--
-- -   #VUID-vkDestroyDescriptorPool-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyDescriptorPool-descriptorPool-parent# If
--     @descriptorPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.DescriptorPool', 'Vulkan.Core10.Handles.Device'
destroyDescriptorPool :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the logical device that destroys the descriptor pool.
                         Device
                      -> -- | @descriptorPool@ is the descriptor pool to destroy.
                         DescriptorPool
                      -> -- | @pAllocator@ controls host memory allocation as described in the
                         -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                         -- chapter.
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io ()
destroyDescriptorPool device descriptorPool allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorPoolPtr = pVkDestroyDescriptorPool (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDescriptorPool is null" Nothing Nothing
  let vkDestroyDescriptorPool' = mkVkDestroyDescriptorPool vkDestroyDescriptorPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDescriptorPool" (vkDestroyDescriptorPool' (deviceHandle (device)) (descriptorPool) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetDescriptorPool
  :: FunPtr (Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result) -> Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result

-- | vkResetDescriptorPool - Resets a descriptor pool object
--
-- = Description
--
-- Resetting a descriptor pool recycles all of the resources from all of
-- the descriptor sets allocated from the descriptor pool back to the
-- descriptor pool, and the descriptor sets are implicitly freed.
--
-- == Valid Usage
--
-- -   #VUID-vkResetDescriptorPool-descriptorPool-00313# All uses of
--     @descriptorPool@ (via any allocated descriptor sets) /must/ have
--     completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkResetDescriptorPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkResetDescriptorPool-descriptorPool-parameter#
--     @descriptorPool@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorPool' handle
--
-- -   #VUID-vkResetDescriptorPool-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-vkResetDescriptorPool-descriptorPool-parent# @descriptorPool@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to any 'Vulkan.Core10.Handles.DescriptorSet' objects
--     allocated from @descriptorPool@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorPool',
-- 'Vulkan.Core10.Enums.DescriptorPoolResetFlags.DescriptorPoolResetFlags',
-- 'Vulkan.Core10.Handles.Device'
resetDescriptorPool :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that owns the descriptor pool.
                       Device
                    -> -- | @descriptorPool@ is the descriptor pool to be reset.
                       DescriptorPool
                    -> -- | @flags@ is reserved for future use.
                       DescriptorPoolResetFlags
                    -> io ()
resetDescriptorPool device descriptorPool flags = liftIO $ do
  let vkResetDescriptorPoolPtr = pVkResetDescriptorPool (deviceCmds (device :: Device))
  unless (vkResetDescriptorPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetDescriptorPool is null" Nothing Nothing
  let vkResetDescriptorPool' = mkVkResetDescriptorPool vkResetDescriptorPoolPtr
  _ <- traceAroundEvent "vkResetDescriptorPool" (vkResetDescriptorPool' (deviceHandle (device)) (descriptorPool) (flags))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateDescriptorSets
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetAllocateInfo) -> Ptr DescriptorSet -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetAllocateInfo) -> Ptr DescriptorSet -> IO Result

-- | vkAllocateDescriptorSets - Allocate one or more descriptor sets
--
-- = Description
--
-- The allocated descriptor sets are returned in @pDescriptorSets@.
--
-- When a descriptor set is allocated, the initial state is largely
-- uninitialized and all descriptors are undefined. Descriptors also become
-- undefined if the underlying resource is destroyed. Descriptor sets
-- containing undefined descriptors /can/ still be bound and used, subject
-- to the following conditions:
--
-- -   For descriptor set bindings created with the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
--     bit set, all descriptors in that binding that are dynamically used
--     /must/ have been populated before the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-binding consumed>.
--
-- -   For descriptor set bindings created without the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
--     bit set, all descriptors in that binding that are statically used
--     /must/ have been populated before the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-binding consumed>.
--
-- -   Descriptor bindings with descriptor type of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     /can/ be undefined when the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-binding consumed>;
--     though values in that block will be undefined.
--
-- -   Entries that are not used by a pipeline /can/ have undefined
--     descriptors.
--
-- If a call to 'allocateDescriptorSets' would cause the total number of
-- descriptor sets allocated from the pool to exceed the value of
-- 'DescriptorPoolCreateInfo'::@maxSets@ used to create
-- @pAllocateInfo->descriptorPool@, then the allocation /may/ fail due to
-- lack of space in the descriptor pool. Similarly, the allocation /may/
-- fail due to lack of space if the call to 'allocateDescriptorSets' would
-- cause the number of any given descriptor type to exceed the sum of all
-- the @descriptorCount@ members of each element of
-- 'DescriptorPoolCreateInfo'::@pPoolSizes@ with a @member@ equal to that
-- type.
--
-- Additionally, the allocation /may/ also fail if a call to
-- 'allocateDescriptorSets' would cause the total number of inline uniform
-- block bindings allocated from the pool to exceed the value of
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.DescriptorPoolInlineUniformBlockCreateInfoEXT'::@maxInlineUniformBlockBindings@
-- used to create the descriptor pool.
--
-- If the allocation fails due to no more space in the descriptor pool, and
-- not because of system or device memory exhaustion, then
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_POOL_MEMORY' /must/ be
-- returned.
--
-- 'allocateDescriptorSets' /can/ be used to create multiple descriptor
-- sets. If the creation of any of those descriptor sets fails, then the
-- implementation /must/ destroy all successfully created descriptor set
-- objects from this command, set all entries of the @pDescriptorSets@
-- array to 'Vulkan.Core10.APIConstants.NULL_HANDLE' and return the error.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAllocateDescriptorSets-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAllocateDescriptorSets-pAllocateInfo-parameter#
--     @pAllocateInfo@ /must/ be a valid pointer to a valid
--     'DescriptorSetAllocateInfo' structure
--
-- -   #VUID-vkAllocateDescriptorSets-pDescriptorSets-parameter#
--     @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @pAllocateInfo->descriptorSetCount@
--     'Vulkan.Core10.Handles.DescriptorSet' handles
--
-- -   #VUID-vkAllocateDescriptorSets-pAllocateInfo::descriptorSetCount-arraylength#
--     @pAllocateInfo->descriptorSetCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @pAllocateInfo->descriptorPool@ /must/ be externally
--     synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_FRAGMENTED_POOL'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_POOL_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorSet', 'DescriptorSetAllocateInfo',
-- 'Vulkan.Core10.Handles.Device'
allocateDescriptorSets :: forall a io
                        . (Extendss DescriptorSetAllocateInfo a, PokeChain a, MonadIO io)
                       => -- | @device@ is the logical device that owns the descriptor pool.
                          Device
                       -> -- | @pAllocateInfo@ is a pointer to a 'DescriptorSetAllocateInfo' structure
                          -- describing parameters of the allocation.
                          (DescriptorSetAllocateInfo a)
                       -> io (("descriptorSets" ::: Vector DescriptorSet))
allocateDescriptorSets device allocateInfo = liftIO . evalContT $ do
  let vkAllocateDescriptorSetsPtr = pVkAllocateDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkAllocateDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAllocateDescriptorSets is null" Nothing Nothing
  let vkAllocateDescriptorSets' = mkVkAllocateDescriptorSets vkAllocateDescriptorSetsPtr
  pAllocateInfo <- ContT $ withCStruct (allocateInfo)
  pPDescriptorSets <- ContT $ bracket (callocBytes @DescriptorSet ((fromIntegral . Data.Vector.length . setLayouts $ (allocateInfo)) * 8)) free
  r <- lift $ traceAroundEvent "vkAllocateDescriptorSets" (vkAllocateDescriptorSets' (deviceHandle (device)) (forgetExtensions pAllocateInfo) (pPDescriptorSets))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorSets <- lift $ generateM (fromIntegral . Data.Vector.length . setLayouts $ (allocateInfo)) (\i -> peek @DescriptorSet ((pPDescriptorSets `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSet)))
  pure $ (pDescriptorSets)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateDescriptorSets' and 'freeDescriptorSets'
--
-- To ensure that 'freeDescriptorSets' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorSets :: forall a io r . (Extendss DescriptorSetAllocateInfo a, PokeChain a, MonadIO io) => Device -> DescriptorSetAllocateInfo a -> (io (Vector DescriptorSet) -> (Vector DescriptorSet -> io ()) -> r) -> r
withDescriptorSets device pAllocateInfo b =
  b (allocateDescriptorSets device pAllocateInfo)
    (\(o0) -> freeDescriptorSets device (descriptorPool (pAllocateInfo :: DescriptorSetAllocateInfo a)) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeDescriptorSets
  :: FunPtr (Ptr Device_T -> DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO Result) -> Ptr Device_T -> DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO Result

-- | vkFreeDescriptorSets - Free one or more descriptor sets
--
-- = Description
--
-- After calling 'freeDescriptorSets', all descriptor sets in
-- @pDescriptorSets@ are invalid.
--
-- == Valid Usage
--
-- -   #VUID-vkFreeDescriptorSets-pDescriptorSets-00309# All submitted
--     commands that refer to any element of @pDescriptorSets@ /must/ have
--     completed execution
--
-- -   #VUID-vkFreeDescriptorSets-pDescriptorSets-00310# @pDescriptorSets@
--     /must/ be a valid pointer to an array of @descriptorSetCount@
--     'Vulkan.Core10.Handles.DescriptorSet' handles, each element of which
--     /must/ either be a valid handle or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkFreeDescriptorSets-descriptorPool-00312# @descriptorPool@
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT'
--     flag
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkFreeDescriptorSets-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkFreeDescriptorSets-descriptorPool-parameter#
--     @descriptorPool@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorPool' handle
--
-- -   #VUID-vkFreeDescriptorSets-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-vkFreeDescriptorSets-descriptorPool-parent# @descriptorPool@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- -   #VUID-vkFreeDescriptorSets-pDescriptorSets-parent# Each element of
--     @pDescriptorSets@ that is a valid handle /must/ have been created,
--     allocated, or retrieved from @descriptorPool@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to each member of @pDescriptorSets@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorPool',
-- 'Vulkan.Core10.Handles.DescriptorSet', 'Vulkan.Core10.Handles.Device'
freeDescriptorSets :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that owns the descriptor pool.
                      Device
                   -> -- | @descriptorPool@ is the descriptor pool from which the descriptor sets
                      -- were allocated.
                      DescriptorPool
                   -> -- | @pDescriptorSets@ is a pointer to an array of handles to
                      -- 'Vulkan.Core10.Handles.DescriptorSet' objects.
                      ("descriptorSets" ::: Vector DescriptorSet)
                   -> io ()
freeDescriptorSets device descriptorPool descriptorSets = liftIO . evalContT $ do
  let vkFreeDescriptorSetsPtr = pVkFreeDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkFreeDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkFreeDescriptorSets is null" Nothing Nothing
  let vkFreeDescriptorSets' = mkVkFreeDescriptorSets vkFreeDescriptorSetsPtr
  pPDescriptorSets <- ContT $ allocaBytesAligned @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  _ <- lift $ traceAroundEvent "vkFreeDescriptorSets" (vkFreeDescriptorSets' (deviceHandle (device)) (descriptorPool) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32)) (pPDescriptorSets))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSets
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> Word32 -> Ptr CopyDescriptorSet -> IO ()) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> Word32 -> Ptr CopyDescriptorSet -> IO ()

-- | vkUpdateDescriptorSets - Update the contents of a descriptor set object
--
-- = Description
--
-- The operations described by @pDescriptorWrites@ are performed first,
-- followed by the operations described by @pDescriptorCopies@. Within each
-- array, the operations are performed in the order they appear in the
-- array.
--
-- Each element in the @pDescriptorWrites@ array describes an operation
-- updating the descriptor set using descriptors for resources specified in
-- the structure.
--
-- Each element in the @pDescriptorCopies@ array is a 'CopyDescriptorSet'
-- structure describing an operation copying descriptors between sets.
--
-- If the @dstSet@ member of any element of @pDescriptorWrites@ or
-- @pDescriptorCopies@ is bound, accessed, or modified by any command that
-- was recorded to a command buffer which is currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>,
-- and any of the descriptor bindings that are updated were not created
-- with the
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
-- or
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
-- bits set, that command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   #VUID-vkUpdateDescriptorSets-None-03047# Descriptor bindings updated
--     by this command which were created without the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     or
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
--     bits set /must/ not be used by any command that was recorded to a
--     command buffer which is in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkUpdateDescriptorSets-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkUpdateDescriptorSets-pDescriptorWrites-parameter# If
--     @descriptorWriteCount@ is not @0@, @pDescriptorWrites@ /must/ be a
--     valid pointer to an array of @descriptorWriteCount@ valid
--     'WriteDescriptorSet' structures
--
-- -   #VUID-vkUpdateDescriptorSets-pDescriptorCopies-parameter# If
--     @descriptorCopyCount@ is not @0@, @pDescriptorCopies@ /must/ be a
--     valid pointer to an array of @descriptorCopyCount@ valid
--     'CopyDescriptorSet' structures
--
-- == Host Synchronization
--
-- -   Host access to @pDescriptorWrites@[].dstSet /must/ be externally
--     synchronized
--
-- -   Host access to @pDescriptorCopies@[].dstSet /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'CopyDescriptorSet', 'Vulkan.Core10.Handles.Device',
-- 'WriteDescriptorSet'
updateDescriptorSets :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that updates the descriptor sets.
                        Device
                     -> -- | @pDescriptorWrites@ is a pointer to an array of 'WriteDescriptorSet'
                        -- structures describing the descriptor sets to write to.
                        ("descriptorWrites" ::: Vector (SomeStruct WriteDescriptorSet))
                     -> -- | @pDescriptorCopies@ is a pointer to an array of 'CopyDescriptorSet'
                        -- structures describing the descriptor sets to copy between.
                        ("descriptorCopies" ::: Vector CopyDescriptorSet)
                     -> io ()
updateDescriptorSets device descriptorWrites descriptorCopies = liftIO . evalContT $ do
  let vkUpdateDescriptorSetsPtr = pVkUpdateDescriptorSets (deviceCmds (device :: Device))
  lift $ unless (vkUpdateDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUpdateDescriptorSets is null" Nothing Nothing
  let vkUpdateDescriptorSets' = mkVkUpdateDescriptorSets vkUpdateDescriptorSetsPtr
  pPDescriptorWrites <- ContT $ allocaBytesAligned @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
  pPDescriptorCopies <- ContT $ allocaBytesAligned @CopyDescriptorSet ((Data.Vector.length (descriptorCopies)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorCopies `plusPtr` (56 * (i)) :: Ptr CopyDescriptorSet) (e)) (descriptorCopies)
  lift $ traceAroundEvent "vkUpdateDescriptorSets" (vkUpdateDescriptorSets' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32)) (forgetExtensions (pPDescriptorWrites)) ((fromIntegral (Data.Vector.length $ (descriptorCopies)) :: Word32)) (pPDescriptorCopies))
  pure $ ()


-- | VkDescriptorBufferInfo - Structure specifying descriptor buffer info
--
-- = Description
--
-- Note
--
-- When setting @range@ to 'Vulkan.Core10.APIConstants.WHOLE_SIZE', the
-- effective range /must/ not be larger than the maximum range for the
-- descriptor type
-- (<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxUniformBufferRange maxUniformBufferRange>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxStorageBufferRange maxStorageBufferRange>).
-- This means that 'Vulkan.Core10.APIConstants.WHOLE_SIZE' is not typically
-- useful in the common case where uniform buffer descriptors are
-- suballocated from a buffer that is much larger than
-- @maxUniformBufferRange@.
--
-- For
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
-- and
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
-- descriptor types, @offset@ is the base offset from which the dynamic
-- offset is applied and @range@ is the static size used for all dynamic
-- offsets.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorBufferInfo-offset-00340# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-VkDescriptorBufferInfo-range-00341# If @range@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkDescriptorBufferInfo-range-00342# If @range@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be less than
--     or equal to the size of @buffer@ minus @offset@
--
-- -   #VUID-VkDescriptorBufferInfo-buffer-02998# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, @buffer@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDescriptorBufferInfo-buffer-02999# If @buffer@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @offset@ /must/ be zero
--     and @range@ /must/ be 'Vulkan.Core10.APIConstants.WHOLE_SIZE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorBufferInfo-buffer-parameter# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'WriteDescriptorSet'
data DescriptorBufferInfo = DescriptorBufferInfo
  { -- | @buffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the buffer
    -- resource.
    buffer :: Buffer
  , -- | @offset@ is the offset in bytes from the start of @buffer@. Access to
    -- buffer memory via this descriptor uses addressing that is relative to
    -- this starting offset.
    offset :: DeviceSize
  , -- | @range@ is the size in bytes that is used for this descriptor update, or
    -- 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to use the range from @offset@
    -- to the end of the buffer.
    range :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorBufferInfo)
#endif
deriving instance Show DescriptorBufferInfo

instance ToCStruct DescriptorBufferInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorBufferInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (range)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DescriptorBufferInfo where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    range <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ DescriptorBufferInfo
             buffer offset range

instance Storable DescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorBufferInfo where
  zero = DescriptorBufferInfo
           zero
           zero
           zero


-- | VkDescriptorImageInfo - Structure specifying descriptor image info
--
-- = Description
--
-- Members of 'DescriptorImageInfo' that are not used in an update (as
-- described above) are ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorImageInfo-imageView-00343# @imageView@ /must/ not
--     be 2D or 2D array image view created from a 3D image
--
-- -   #VUID-VkDescriptorImageInfo-imageView-01976# If @imageView@ is
--     created from a depth\/stencil image, the @aspectMask@ used to create
--     the @imageView@ /must/ include either
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     but not both
--
-- -   #VUID-VkDescriptorImageInfo-imageLayout-00344# @imageLayout@ /must/
--     match the actual 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' of
--     each subresource accessible from @imageView@ at the time this
--     descriptor is accessed as defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- -   #VUID-VkDescriptorImageInfo-sampler-01564# If @sampler@ is used and
--     the 'Vulkan.Core10.Enums.Format.Format' of the image is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the image /must/ have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     and the @aspectMask@ of the @imageView@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or (for three-plane formats only)
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkDescriptorImageInfo-mutableComparisonSamplers-04450# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@mutableComparisonSamplers@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @sampler@ /must/
--     have been created with
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@compareEnable@ set to
--     'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorImageInfo-commonparent# Both of @imageView@, and
--     @sampler@ that are valid handles of non-ignored parameters /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView', 'Vulkan.Core10.Handles.Sampler',
-- 'WriteDescriptorSet'
data DescriptorImageInfo = DescriptorImageInfo
  { -- | @sampler@ is a sampler handle, and is used in descriptor updates for
    -- types 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' and
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- if the binding being updated does not use immutable samplers.
    sampler :: Sampler
  , -- | @imageView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or an image view
    -- handle, and is used in descriptor updates for types
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- and
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that the image subresources accessible from
    -- @imageView@ will be in at the time this descriptor is accessed.
    -- @imageLayout@ is used in descriptor updates for types
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- and
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'.
    imageLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorImageInfo)
#endif
deriving instance Show DescriptorImageInfo

instance ToCStruct DescriptorImageInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorImageInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Sampler)) (sampler)
    poke ((p `plusPtr` 8 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (imageLayout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Sampler)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ImageView)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct DescriptorImageInfo where
  peekCStruct p = do
    sampler <- peek @Sampler ((p `plusPtr` 0 :: Ptr Sampler))
    imageView <- peek @ImageView ((p `plusPtr` 8 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 16 :: Ptr ImageLayout))
    pure $ DescriptorImageInfo
             sampler imageView imageLayout

instance Storable DescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorImageInfo where
  zero = DescriptorImageInfo
           zero
           zero
           zero


-- | VkWriteDescriptorSet - Structure specifying the parameters of a
-- descriptor set write operation
--
-- = Description
--
-- Only one of @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- is used according to the descriptor type specified in the
-- @descriptorType@ member of the containing 'WriteDescriptorSet'
-- structure, or none of them in case @descriptorType@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
-- in which case the source data for the descriptor writes is taken from
-- the
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT'
-- structure included in the @pNext@ chain of 'WriteDescriptorSet', or if
-- @descriptorType@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
-- in which case the source data for the descriptor writes is taken from
-- the
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR'
-- structure in the @pNext@ chain of 'WriteDescriptorSet', or if
-- @descriptorType@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
-- in which case the source data for the descriptor writes is taken from
-- the
-- 'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV'
-- structure in the @pNext@ chain of 'WriteDescriptorSet', as specified
-- below.
--
-- If the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, the buffer, acceleration structure, imageView, or
-- bufferView /can/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'. Loads from
-- a null descriptor return zero values and stores and atomics to a null
-- descriptor are discarded. A null acceleration structure descriptor
-- results in the miss shader being invoked.
--
-- If the destination descriptor is a mutable descriptor, the active
-- descriptor type for the destination descriptor becomes @descriptorType@.
--
-- If the @dstBinding@ has fewer than @descriptorCount@ array elements
-- remaining starting from @dstArrayElement@, then the remainder will be
-- used to update the subsequent binding - @dstBinding@+1 starting at array
-- element zero. If a binding has a @descriptorCount@ of zero, it is
-- skipped. This behavior applies recursively, with the update affecting
-- consecutive bindings as needed to update all @descriptorCount@
-- descriptors.
--
-- Note
--
-- The same behavior applies to bindings with a descriptor type of
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
-- where @descriptorCount@ specifies the number of bytes to update while
-- @dstArrayElement@ specifies the starting byte offset, thus in this case
-- if the @dstBinding@ has a smaller byte size than the sum of
-- @dstArrayElement@ and @descriptorCount@, then the remainder will be used
-- to update the subsequent binding - @dstBinding@+1 starting at offset
-- zero. This falls out as a special case of the above rule.
--
-- == Valid Usage
--
-- -   #VUID-VkWriteDescriptorSet-dstBinding-00315# @dstBinding@ /must/ be
--     less than or equal to the maximum value of @binding@ of all
--     'DescriptorSetLayoutBinding' structures specified when @dstSet@’s
--     descriptor set layout was created
--
-- -   #VUID-VkWriteDescriptorSet-dstBinding-00316# @dstBinding@ /must/ be
--     a binding with a non-zero @descriptorCount@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorCount-00317# All consecutive
--     bindings updated via a single 'WriteDescriptorSet' structure, except
--     those with a @descriptorCount@ of zero, /must/ have identical
--     @descriptorType@ and @stageFlags@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorCount-00318# All consecutive
--     bindings updated via a single 'WriteDescriptorSet' structure, except
--     those with a @descriptorCount@ of zero, /must/ all either use
--     immutable samplers or /must/ all not use immutable samplers
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00319# @descriptorType@
--     /must/ match the type of @dstBinding@ within @dstSet@
--
-- -   #VUID-VkWriteDescriptorSet-dstSet-00320# @dstSet@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   #VUID-VkWriteDescriptorSet-dstArrayElement-00321# The sum of
--     @dstArrayElement@ and @descriptorCount@ /must/ be less than or equal
--     to the number of array elements in the descriptor set binding
--     specified by @dstBinding@, and all applicable consecutive bindings,
--     as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02219# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02220# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00322# If @descriptorType@
--     is 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @pImageInfo@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid 'DescriptorImageInfo' structures
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02994# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     each element of @pTexelBufferView@ /must/ be either a valid
--     'Vulkan.Core10.Handles.BufferView' handle or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02995# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, each element of @pTexelBufferView@ /must/
--     not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00324# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     @pBufferInfo@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid 'DescriptorBufferInfo' structures
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00325# If @descriptorType@
--     is 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @dstSet@ was not allocated with a layout that included immutable
--     samplers for @dstBinding@ with @descriptorType@, the @sampler@
--     member of each element of @pImageInfo@ /must/ be a valid
--     'Vulkan.Core10.Handles.Sampler' object
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02996# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ member of each element of @pImageInfo@ /must/ be
--     either a valid 'Vulkan.Core10.Handles.ImageView' handle or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02997# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, the @imageView@ member of each element of
--     @pImageInfo@ /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02221# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT'
--     structure whose @dataSize@ member equals @descriptorCount@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02382# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR'
--     structure whose @accelerationStructureCount@ member equals
--     @descriptorCount@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-03817# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV'
--     structure whose @accelerationStructureCount@ member equals
--     @descriptorCount@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-01946# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     then the @imageView@ member of each @pImageInfo@ element /must/ have
--     been created without a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02738# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and if any element of @pImageInfo@ has a @imageView@ member that was
--     created with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain, then @dstSet@ /must/ have been
--     allocated with a layout that included immutable samplers for
--     @dstBinding@, and the corresponding immutable sampler /must/ have
--     been created with an /identically defined/
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     object
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-01948# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @dstSet@ was allocated with a layout that included immutable
--     samplers for @dstBinding@, then the @imageView@ member of each
--     element of @pImageInfo@ which corresponds to an immutable sampler
--     that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     /must/ have been created with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain with an /identically defined/
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     to the corresponding immutable sampler
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00327# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @offset@ member of each element of @pBufferInfo@ /must/ be a
--     multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00328# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @offset@ member of each element of @pBufferInfo@ /must/ be a
--     multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00329# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     and the @buffer@ member of any element of @pBufferInfo@ is the
--     handle of a non-sparse buffer, then that buffer /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00330# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @buffer@ member of each element of @pBufferInfo@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00331# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @buffer@ member of each element of @pBufferInfo@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00332# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @range@ member of each element of @pBufferInfo@, or the
--     effective range if @range@ is
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxUniformBufferRange@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00333# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @range@ member of each element of @pBufferInfo@, or the
--     effective range if @range@ is
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxStorageBufferRange@
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00334# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     the 'Vulkan.Core10.Handles.Buffer' that each element of
--     @pTexelBufferView@ was created from /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00335# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     the 'Vulkan.Core10.Handles.Buffer' that each element of
--     @pTexelBufferView@ was created from /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--     set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00336# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with the identity swizzle
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00337# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-04149# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     the @imageLayout@ member of each element of @pImageInfo@ /must/ be a
--     member of the list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage Sampled Image>
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-04150# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     the @imageLayout@ member of each element of @pImageInfo@ /must/ be a
--     member of the list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler Combined Image Sampler>
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-04151# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     the @imageLayout@ member of each element of @pImageInfo@ /must/ be a
--     member of the list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inputattachment Input Attachment>
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-04152# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     the @imageLayout@ member of each element of @pImageInfo@ /must/ be a
--     member of the list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage Storage Image>
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00338# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-00339# If @descriptorType@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT' set
--
-- -   #VUID-VkWriteDescriptorSet-descriptorCount-03048# All consecutive
--     bindings updated via a single 'WriteDescriptorSet' structure, except
--     those with a @descriptorCount@ of zero, /must/ have identical
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlagBits'
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-02752# If @descriptorType@
--     is 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     then @dstSet@ /must/ not have been allocated with a layout that
--     included immutable samplers for @dstBinding@
--
-- -   #VUID-VkWriteDescriptorSet-dstSet-04611# If the
--     'DescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     the new active descriptor type @descriptorType@ /must/ exist in the
--     corresponding @pMutableDescriptorTypeLists@ list for @dstBinding@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteDescriptorSet-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET'
--
-- -   #VUID-VkWriteDescriptorSet-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV',
--     or
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT'
--
-- -   #VUID-VkWriteDescriptorSet-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkWriteDescriptorSet-descriptorType-parameter#
--     @descriptorType@ /must/ be a valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- -   #VUID-VkWriteDescriptorSet-descriptorCount-arraylength#
--     @descriptorCount@ /must/ be greater than @0@
--
-- -   #VUID-VkWriteDescriptorSet-commonparent# Both of @dstSet@, and the
--     elements of @pTexelBufferView@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.BufferView', 'DescriptorBufferInfo',
-- 'DescriptorImageInfo', 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'updateDescriptorSets'
data WriteDescriptorSet (es :: [Type]) = WriteDescriptorSet
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @dstSet@ is the destination descriptor set to update.
    dstSet :: DescriptorSet
  , -- | @dstBinding@ is the descriptor binding within that set.
    dstBinding :: Word32
  , -- | @dstArrayElement@ is the starting element in that array. If the
    -- descriptor binding identified by @dstSet@ and @dstBinding@ has a
    -- descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @dstArrayElement@ specifies the starting byte offset within the
    -- binding.
    dstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to update (the number of
    -- elements in @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ , or a
    -- value matching the @dataSize@ member of a
    -- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT'
    -- structure in the @pNext@ chain , or a value matching the
    -- @accelerationStructureCount@ of a
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR'
    -- structure in the @pNext@ chain ). If the descriptor binding identified
    -- by @dstSet@ and @dstBinding@ has a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @descriptorCount@ specifies the number of bytes to update.
    descriptorCount :: Word32
  , -- | @descriptorType@ is a
    -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' specifying the type
    -- of each descriptor in @pImageInfo@, @pBufferInfo@, or
    -- @pTexelBufferView@, as described below. If 'DescriptorSetLayoutBinding'
    -- for @dstSet@ at @dstBinding@ is not equal to
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
    -- @descriptorType@ /must/ be the same type as that specified in
    -- 'DescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@. The type of
    -- the descriptor also controls which array the descriptors are taken from.
    descriptorType :: DescriptorType
  , -- | @pImageInfo@ is a pointer to an array of 'DescriptorImageInfo'
    -- structures or is ignored, as described below.
    imageInfo :: Vector DescriptorImageInfo
  , -- | @pBufferInfo@ is a pointer to an array of 'DescriptorBufferInfo'
    -- structures or is ignored, as described below.
    bufferInfo :: Vector DescriptorBufferInfo
  , -- | @pTexelBufferView@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.BufferView' handles as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-views Buffer Views>
    -- section or is ignored, as described below.
    texelBufferView :: Vector BufferView
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSet (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (WriteDescriptorSet es)

instance Extensible WriteDescriptorSet where
  extensibleTypeName = "WriteDescriptorSet"
  setNext x next = x{next = next}
  getNext WriteDescriptorSet{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends WriteDescriptorSet e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @WriteDescriptorSetAccelerationStructureNV = Just f
    | Just Refl <- eqT @e @WriteDescriptorSetAccelerationStructureKHR = Just f
    | Just Refl <- eqT @e @WriteDescriptorSetInlineUniformBlockEXT = Just f
    | otherwise = Nothing

instance (Extendss WriteDescriptorSet es, PokeChain es) => ToCStruct (WriteDescriptorSet es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSet{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (dstSet)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (dstBinding)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (dstArrayElement)
    let pImageInfoLength = Data.Vector.length $ (imageInfo)
    lift $ unless (fromIntegral pImageInfoLength == (descriptorCount) || pImageInfoLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pImageInfo must be empty or have 'descriptorCount' elements" Nothing Nothing
    let pBufferInfoLength = Data.Vector.length $ (bufferInfo)
    lift $ unless (fromIntegral pBufferInfoLength == (descriptorCount) || pBufferInfoLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pBufferInfo must be empty or have 'descriptorCount' elements" Nothing Nothing
    let pTexelBufferViewLength = Data.Vector.length $ (texelBufferView)
    lift $ unless (fromIntegral pTexelBufferViewLength == (descriptorCount) || pTexelBufferViewLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pTexelBufferView must be empty or have 'descriptorCount' elements" Nothing Nothing
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((descriptorCount))
    lift $ poke ((p `plusPtr` 36 :: Ptr DescriptorType)) (descriptorType)
    pImageInfo'' <- if Data.Vector.null (imageInfo)
      then pure nullPtr
      else do
        pPImageInfo <- ContT $ allocaBytesAligned @DescriptorImageInfo (((Data.Vector.length (imageInfo))) * 24) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPImageInfo `plusPtr` (24 * (i)) :: Ptr DescriptorImageInfo) (e)) ((imageInfo))
        pure $ pPImageInfo
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr DescriptorImageInfo))) pImageInfo''
    pBufferInfo'' <- if Data.Vector.null (bufferInfo)
      then pure nullPtr
      else do
        pPBufferInfo <- ContT $ allocaBytesAligned @DescriptorBufferInfo (((Data.Vector.length (bufferInfo))) * 24) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferInfo `plusPtr` (24 * (i)) :: Ptr DescriptorBufferInfo) (e)) ((bufferInfo))
        pure $ pPBufferInfo
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr DescriptorBufferInfo))) pBufferInfo''
    pTexelBufferView'' <- if Data.Vector.null (texelBufferView)
      then pure nullPtr
      else do
        pPTexelBufferView <- ContT $ allocaBytesAligned @BufferView (((Data.Vector.length (texelBufferView))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPTexelBufferView `plusPtr` (8 * (i)) :: Ptr BufferView) (e)) ((texelBufferView))
        pure $ pPTexelBufferView
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr BufferView))) pTexelBufferView''
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr DescriptorType)) (zero)
    lift $ f

instance (Extendss WriteDescriptorSet es, PeekChain es) => FromCStruct (WriteDescriptorSet es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    dstSet <- peek @DescriptorSet ((p `plusPtr` 16 :: Ptr DescriptorSet))
    dstBinding <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 36 :: Ptr DescriptorType))
    pImageInfo <- peek @(Ptr DescriptorImageInfo) ((p `plusPtr` 40 :: Ptr (Ptr DescriptorImageInfo)))
    let pImageInfoLength = if pImageInfo == nullPtr then 0 else (fromIntegral descriptorCount)
    pImageInfo' <- generateM pImageInfoLength (\i -> peekCStruct @DescriptorImageInfo ((pImageInfo `advancePtrBytes` (24 * (i)) :: Ptr DescriptorImageInfo)))
    pBufferInfo <- peek @(Ptr DescriptorBufferInfo) ((p `plusPtr` 48 :: Ptr (Ptr DescriptorBufferInfo)))
    let pBufferInfoLength = if pBufferInfo == nullPtr then 0 else (fromIntegral descriptorCount)
    pBufferInfo' <- generateM pBufferInfoLength (\i -> peekCStruct @DescriptorBufferInfo ((pBufferInfo `advancePtrBytes` (24 * (i)) :: Ptr DescriptorBufferInfo)))
    pTexelBufferView <- peek @(Ptr BufferView) ((p `plusPtr` 56 :: Ptr (Ptr BufferView)))
    let pTexelBufferViewLength = if pTexelBufferView == nullPtr then 0 else (fromIntegral descriptorCount)
    pTexelBufferView' <- generateM pTexelBufferViewLength (\i -> peek @BufferView ((pTexelBufferView `advancePtrBytes` (8 * (i)) :: Ptr BufferView)))
    pure $ WriteDescriptorSet
             next dstSet dstBinding dstArrayElement descriptorCount descriptorType pImageInfo' pBufferInfo' pTexelBufferView'

instance es ~ '[] => Zero (WriteDescriptorSet es) where
  zero = WriteDescriptorSet
           ()
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty
           mempty


-- | VkCopyDescriptorSet - Structure specifying a copy descriptor set
-- operation
--
-- = Description
--
-- If the 'DescriptorSetLayoutBinding' for @dstBinding@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE' and
-- @srcBinding@ is not
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE', the
-- new active descriptor type becomes the descriptor type of @srcBinding@.
-- If both 'DescriptorSetLayoutBinding' for @srcBinding@ and @dstBinding@
-- are 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
-- the active descriptor type in each source descriptor is copied into the
-- corresponding destination descriptor. The active descriptor type /can/
-- be different for each source descriptor.
--
-- Note
--
-- The intention is that copies to and from mutable descriptors is a simple
-- memcpy. Copies between non-mutable and mutable descriptors are expected
-- to require one memcpy per descriptor to handle the difference in size,
-- but this use case with more than one @descriptorCount@ is considered
-- rare.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyDescriptorSet-srcBinding-00345# @srcBinding@ /must/ be a
--     valid binding within @srcSet@
--
-- -   #VUID-VkCopyDescriptorSet-srcArrayElement-00346# The sum of
--     @srcArrayElement@ and @descriptorCount@ /must/ be less than or equal
--     to the number of array elements in the descriptor set binding
--     specified by @srcBinding@, and all applicable consecutive bindings,
--     as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   #VUID-VkCopyDescriptorSet-dstBinding-00347# @dstBinding@ /must/ be a
--     valid binding within @dstSet@
--
-- -   #VUID-VkCopyDescriptorSet-dstArrayElement-00348# The sum of
--     @dstArrayElement@ and @descriptorCount@ /must/ be less than or equal
--     to the number of array elements in the descriptor set binding
--     specified by @dstBinding@, and all applicable consecutive bindings,
--     as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   #VUID-VkCopyDescriptorSet-dstBinding-02632# The type of @dstBinding@
--     within @dstSet@ /must/ be equal to the type of @srcBinding@ within
--     @srcSet@
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-00349# If @srcSet@ is equal to
--     @dstSet@, then the source and destination ranges of descriptors
--     /must/ not overlap, where the ranges /may/ include array elements
--     from consecutive bindings as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   #VUID-VkCopyDescriptorSet-srcBinding-02223# If the descriptor type
--     of the descriptor set binding specified by @srcBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @srcArrayElement@ /must/ be an integer multiple of @4@
--
-- -   #VUID-VkCopyDescriptorSet-dstBinding-02224# If the descriptor type
--     of the descriptor set binding specified by @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   #VUID-VkCopyDescriptorSet-srcBinding-02225# If the descriptor type
--     of the descriptor set binding specified by either @srcBinding@ or
--     @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-01918# If @srcSet@’s layout was
--     created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     flag set, then @dstSet@’s layout /must/ also have been created with
--     the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     flag set
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-01919# If @srcSet@’s layout was
--     created without the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     flag set, then @dstSet@’s layout /must/ also have been created
--     without the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     flag set
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-01920# If the descriptor pool from
--     which @srcSet@ was allocated was created with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     flag set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     flag set
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-01921# If the descriptor pool from
--     which @srcSet@ was allocated was created without the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     flag set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created without the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     flag set
--
-- -   #VUID-VkCopyDescriptorSet-dstBinding-02753# If the descriptor type
--     of the descriptor set binding specified by @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER', then
--     @dstSet@ /must/ not have been allocated with a layout that included
--     immutable samplers for @dstBinding@
--
-- -   #VUID-VkCopyDescriptorSet-dstSet-04612# If
--     'DescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     the new active descriptor type /must/ exist in the corresponding
--     @pMutableDescriptorTypeLists@ list for @dstBinding@ if the new
--     active descriptor type is not
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-04613# If
--     'DescriptorSetLayoutBinding' for @srcSet@ at @srcBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--     and the 'DescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@ is
--     not
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     the active descriptor type for the source descriptor /must/ match
--     the descriptor type of @dstBinding@
--
-- -   #VUID-VkCopyDescriptorSet-dstSet-04614# If
--     'DescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     and the new active descriptor type is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     the @pMutableDescriptorTypeLists@ for @srcBinding@ and @dstBinding@
--     /must/ match exactly
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyDescriptorSet-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_DESCRIPTOR_SET'
--
-- -   #VUID-VkCopyDescriptorSet-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyDescriptorSet-srcSet-parameter# @srcSet@ /must/ be a
--     valid 'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   #VUID-VkCopyDescriptorSet-dstSet-parameter# @dstSet@ /must/ be a
--     valid 'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   #VUID-VkCopyDescriptorSet-commonparent# Both of @dstSet@, and
--     @srcSet@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'updateDescriptorSets'
data CopyDescriptorSet = CopyDescriptorSet
  { -- | @srcSet@, @srcBinding@, and @srcArrayElement@ are the source set,
    -- binding, and array element, respectively. If the descriptor binding
    -- identified by @srcSet@ and @srcBinding@ has a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @srcArrayElement@ specifies the starting byte offset within the
    -- binding to copy from.
    srcSet :: DescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcBinding"
    srcBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcArrayElement"
    srcArrayElement :: Word32
  , -- | @dstSet@, @dstBinding@, and @dstArrayElement@ are the destination set,
    -- binding, and array element, respectively. If the descriptor binding
    -- identified by @dstSet@ and @dstBinding@ has a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @dstArrayElement@ specifies the starting byte offset within the
    -- binding to copy to.
    dstSet :: DescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstBinding"
    dstBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstArrayElement"
    dstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to copy from the source
    -- to destination. If @descriptorCount@ is greater than the number of
    -- remaining array elements in the source or destination binding, those
    -- affect consecutive bindings in a manner similar to 'WriteDescriptorSet'
    -- above. If the descriptor binding identified by @srcSet@ and @srcBinding@
    -- has a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @descriptorCount@ specifies the number of bytes to copy and the
    -- remaining array elements in the source or destination binding refer to
    -- the remaining number of bytes in those.
    descriptorCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyDescriptorSet)
#endif
deriving instance Show CopyDescriptorSet

instance ToCStruct CopyDescriptorSet where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyDescriptorSet{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (srcSet)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (srcBinding)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (srcArrayElement)
    poke ((p `plusPtr` 32 :: Ptr DescriptorSet)) (dstSet)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (dstBinding)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (dstArrayElement)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (descriptorCount)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSet)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DescriptorSet)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct CopyDescriptorSet where
  peekCStruct p = do
    srcSet <- peek @DescriptorSet ((p `plusPtr` 16 :: Ptr DescriptorSet))
    srcBinding <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    srcArrayElement <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    dstSet <- peek @DescriptorSet ((p `plusPtr` 32 :: Ptr DescriptorSet))
    dstBinding <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ CopyDescriptorSet
             srcSet srcBinding srcArrayElement dstSet dstBinding dstArrayElement descriptorCount

instance Storable CopyDescriptorSet where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyDescriptorSet where
  zero = CopyDescriptorSet
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorSetLayoutBinding - Structure specifying a descriptor set
-- layout binding
--
-- = Description
--
-- -   @pImmutableSamplers@ affects initialization of samplers. If
--     @descriptorType@ specifies a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     type descriptor, then @pImmutableSamplers@ /can/ be used to
--     initialize a set of /immutable samplers/. Immutable samplers are
--     permanently bound into the set layout and /must/ not be changed;
--     updating a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER'
--     descriptor with immutable samplers is not allowed and updates to a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     descriptor with immutable samplers does not modify the samplers (the
--     image views are updated, but the sampler updates are ignored). If
--     @pImmutableSamplers@ is not @NULL@, then it points to an array of
--     sampler handles that will be copied into the set layout and used for
--     the corresponding binding. Only the sampler handles are copied; the
--     sampler objects /must/ not be destroyed before the final use of the
--     set layout and any descriptor pools and sets created using it. If
--     @pImmutableSamplers@ is @NULL@, then the sampler slots are dynamic
--     and sampler handles /must/ be bound into descriptor sets using this
--     layout. If @descriptorType@ is not one of these descriptor types,
--     then @pImmutableSamplers@ is ignored.
--
-- The above layout definition allows the descriptor bindings to be
-- specified sparsely such that not all binding numbers between 0 and the
-- maximum binding number need to be specified in the @pBindings@ array.
-- Bindings that are not specified have a @descriptorCount@ and
-- @stageFlags@ of zero, and the value of @descriptorType@ is undefined.
-- However, all binding numbers between 0 and the maximum binding number in
-- the 'DescriptorSetLayoutCreateInfo'::@pBindings@ array /may/ consume
-- memory in the descriptor set layout even if not all descriptor bindings
-- are used, though it /should/ not consume additional memory from the
-- descriptor pool.
--
-- Note
--
-- The maximum binding number specified /should/ be as compact as possible
-- to avoid wasted memory.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-00282# If
--     @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @descriptorCount@ is not @0@ and @pImmutableSamplers@ is not
--     @NULL@, @pImmutableSamplers@ /must/ be a valid pointer to an array
--     of @descriptorCount@ valid 'Vulkan.Core10.Handles.Sampler' handles
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-04604# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inlineUniformBlock inlineUniformBlock>
--     feature is not enabled, @descriptorType@ /must/ not be
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-02209# If
--     @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be a multiple of @4@
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-02210# If
--     @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be less than or equal to
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxInlineUniformBlockSize@
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorCount-00283# If
--     @descriptorCount@ is not @0@, @stageFlags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-01510# If
--     @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     and @descriptorCount@ is not @0@, then @stageFlags@ /must/ be @0@ or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkDescriptorSetLayoutBinding-pImmutableSamplers-04009# The
--     sampler objects indicated by @pImmutableSamplers@ /must/ not have a
--     @borderColor@ with one of the values
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-04605# If
--     @descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     then @pImmutableSamplers@ /must/ be @NULL@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetLayoutBinding-descriptorType-parameter#
--     @descriptorType@ /must/ be a valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- = See Also
--
-- 'DescriptorSetLayoutCreateInfo',
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Vulkan.Core10.Handles.Sampler',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { -- | @binding@ is the binding number of this entry and corresponds to a
    -- resource of the same binding number in the shader stages.
    binding :: Word32
  , -- | @descriptorType@ is a
    -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' specifying which
    -- type of resource descriptors are used for this binding.
    descriptorType :: DescriptorType
  , -- | @descriptorCount@ is the number of descriptors contained in the binding,
    -- accessed in a shader as an array , except if @descriptorType@ is
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- in which case @descriptorCount@ is the size in bytes of the inline
    -- uniform block . If @descriptorCount@ is zero this binding entry is
    -- reserved and the resource /must/ not be accessed from any stage via this
    -- binding within any pipeline using the set layout.
    descriptorCount :: Word32
  , -- | @stageFlags@ member is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- which pipeline shader stages /can/ access a resource for this binding.
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL' is a
    -- shorthand specifying that all defined shader stages, including any
    -- additional stages defined by extensions, /can/ access the resource.
    --
    -- If a shader stage is not included in @stageFlags@, then a resource
    -- /must/ not be accessed from that stage via this binding within any
    -- pipeline using the set layout. Other than input attachments which are
    -- limited to the fragment shader, there are no limitations on what
    -- combinations of stages /can/ use a descriptor binding, and in particular
    -- a binding /can/ be used by both graphics stages and the compute stage.
    stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "pImmutableSamplers"
    immutableSamplers :: Vector Sampler
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutBinding)
#endif
deriving instance Show DescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutBinding where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutBinding{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    lift $ poke ((p `plusPtr` 4 :: Ptr DescriptorType)) (descriptorType)
    let pImmutableSamplersLength = Data.Vector.length $ (immutableSamplers)
    descriptorCount'' <- lift $ if (descriptorCount) == 0
      then pure $ fromIntegral pImmutableSamplersLength
      else do
        unless (fromIntegral pImmutableSamplersLength == (descriptorCount) || pImmutableSamplersLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pImmutableSamplers must be empty or have 'descriptorCount' elements" Nothing Nothing
        pure (descriptorCount)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (descriptorCount'')
    lift $ poke ((p `plusPtr` 12 :: Ptr ShaderStageFlags)) (stageFlags)
    pImmutableSamplers'' <- if Data.Vector.null (immutableSamplers)
      then pure nullPtr
      else do
        pPImmutableSamplers <- ContT $ allocaBytesAligned @Sampler (((Data.Vector.length (immutableSamplers))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPImmutableSamplers `plusPtr` (8 * (i)) :: Ptr Sampler) (e)) ((immutableSamplers))
        pure $ pPImmutableSamplers
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Sampler))) pImmutableSamplers''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct DescriptorSetLayoutBinding where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 4 :: Ptr DescriptorType))
    descriptorCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 12 :: Ptr ShaderStageFlags))
    pImmutableSamplers <- peek @(Ptr Sampler) ((p `plusPtr` 16 :: Ptr (Ptr Sampler)))
    let pImmutableSamplersLength = if pImmutableSamplers == nullPtr then 0 else (fromIntegral descriptorCount)
    pImmutableSamplers' <- generateM pImmutableSamplersLength (\i -> peek @Sampler ((pImmutableSamplers `advancePtrBytes` (8 * (i)) :: Ptr Sampler)))
    pure $ DescriptorSetLayoutBinding
             binding descriptorType descriptorCount stageFlags pImmutableSamplers'

instance Zero DescriptorSetLayoutBinding where
  zero = DescriptorSetLayoutBinding
           zero
           zero
           zero
           zero
           mempty


-- | VkDescriptorSetLayoutCreateInfo - Structure specifying parameters of a
-- newly created descriptor set layout
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-binding-00279# The
--     'DescriptorSetLayoutBinding'::@binding@ members of the elements of
--     the @pBindings@ array /must/ each have different values
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-00280# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindings@ /must/ not have a @descriptorType@
--     of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-02208# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindings@ /must/ not have a @descriptorType@
--     of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-00281# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then the total number of elements of all bindings /must/ be less
--     than or equal to
--     'Vulkan.Extensions.VK_KHR_push_descriptor.PhysicalDevicePushDescriptorPropertiesKHR'::@maxPushDescriptors@
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-04590# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-04591# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     @pBindings@ /must/ not have a @descriptorType@ of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-03000# If any binding
--     has the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     bit set, @flags@ /must/ include
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-descriptorType-03001# If any
--     binding has the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     bit set, then all bindings /must/ not have @descriptorType@ of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-04592# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-descriptorType-04593# If any
--     binding has a @descriptorType@ of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     then a
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'
--     /must/ be present in the @pNext@ chain
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-descriptorType-04594# If a
--     binding has a @descriptorType@ value of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE',
--     then @pImmutableSamplers@ /must/ be @NULL@
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-mutableDescriptorType-04595#
--     If
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'::@mutableDescriptorType@
--     is not enabled, @pBindings@ /must/ not contain a @descriptorType@ of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-04596# If @flags@
--     contains
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE',
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'::@mutableDescriptorType@
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetLayoutBindingFlagsCreateInfo'
--     or
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits'
--     values
--
-- -   #VUID-VkDescriptorSetLayoutCreateInfo-pBindings-parameter# If
--     @bindingCount@ is not @0@, @pBindings@ /must/ be a valid pointer to
--     an array of @bindingCount@ valid 'DescriptorSetLayoutBinding'
--     structures
--
-- = See Also
--
-- 'DescriptorSetLayoutBinding',
-- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDescriptorSetLayout',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.getDescriptorSetLayoutSupport',
-- 'Vulkan.Extensions.VK_KHR_maintenance3.getDescriptorSetLayoutSupportKHR'
data DescriptorSetLayoutCreateInfo (es :: [Type]) = DescriptorSetLayoutCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits'
    -- specifying options for descriptor set layout creation.
    flags :: DescriptorSetLayoutCreateFlags
  , -- | @pBindings@ is a pointer to an array of 'DescriptorSetLayoutBinding'
    -- structures.
    bindings :: Vector DescriptorSetLayoutBinding
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetLayoutCreateInfo es)

instance Extensible DescriptorSetLayoutCreateInfo where
  extensibleTypeName = "DescriptorSetLayoutCreateInfo"
  setNext x next = x{next = next}
  getNext DescriptorSetLayoutCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetLayoutCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MutableDescriptorTypeCreateInfoVALVE = Just f
    | Just Refl <- eqT @e @DescriptorSetLayoutBindingFlagsCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetLayoutCreateInfo es, PokeChain es) => ToCStruct (DescriptorSetLayoutCreateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorSetLayoutCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bindings)) :: Word32))
    pPBindings' <- ContT $ allocaBytesAligned @DescriptorSetLayoutBinding ((Data.Vector.length (bindings)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindings' `plusPtr` (24 * (i)) :: Ptr DescriptorSetLayoutBinding) (e) . ($ ())) (bindings)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayoutBinding))) (pPBindings')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss DescriptorSetLayoutCreateInfo es, PeekChain es) => FromCStruct (DescriptorSetLayoutCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DescriptorSetLayoutCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorSetLayoutCreateFlags))
    bindingCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pBindings <- peek @(Ptr DescriptorSetLayoutBinding) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayoutBinding)))
    pBindings' <- generateM (fromIntegral bindingCount) (\i -> peekCStruct @DescriptorSetLayoutBinding ((pBindings `advancePtrBytes` (24 * (i)) :: Ptr DescriptorSetLayoutBinding)))
    pure $ DescriptorSetLayoutCreateInfo
             next flags pBindings'

instance es ~ '[] => Zero (DescriptorSetLayoutCreateInfo es) where
  zero = DescriptorSetLayoutCreateInfo
           ()
           zero
           mempty


-- | VkDescriptorPoolSize - Structure specifying descriptor pool size
--
-- = Description
--
-- Note
--
-- When creating a descriptor pool that will contain descriptors for
-- combined image samplers of multi-planar formats, an application needs to
-- account for non-trivial descriptor consumption when choosing the
-- @descriptorCount@ value, as indicated by
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionImageFormatProperties'::@combinedImageSamplerDescriptorCount@.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorPoolSize-descriptorCount-00302# @descriptorCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkDescriptorPoolSize-type-02218# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorPoolSize-type-parameter# @type@ /must/ be a valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- = See Also
--
-- 'DescriptorPoolCreateInfo',
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType'
data DescriptorPoolSize = DescriptorPoolSize
  { -- | @type@ is the type of descriptor.
    type' :: DescriptorType
  , -- | @descriptorCount@ is the number of descriptors of that type to allocate.
    -- If @type@ is
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @descriptorCount@ is the number of bytes to allocate for
    -- descriptors of this type.
    descriptorCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolSize)
#endif
deriving instance Show DescriptorPoolSize

instance ToCStruct DescriptorPoolSize where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolSize{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DescriptorType)) (type')
    poke ((p `plusPtr` 4 :: Ptr Word32)) (descriptorCount)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorPoolSize where
  peekCStruct p = do
    type' <- peek @DescriptorType ((p `plusPtr` 0 :: Ptr DescriptorType))
    descriptorCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DescriptorPoolSize
             type' descriptorCount

instance Storable DescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorPoolSize where
  zero = DescriptorPoolSize
           zero
           zero


-- | VkDescriptorPoolCreateInfo - Structure specifying parameters of a newly
-- created descriptor pool
--
-- = Description
--
-- If multiple 'DescriptorPoolSize' structures appear in the @pPoolSizes@
-- array then the pool will be created with enough storage for the total
-- number of descriptors of each type.
--
-- Fragmentation of a descriptor pool is possible and /may/ lead to
-- descriptor set allocation failures. A failure due to fragmentation is
-- defined as failing a descriptor set allocation despite the sum of all
-- outstanding descriptor set allocations from the pool plus the requested
-- allocation requiring no more than the total number of descriptors
-- requested at pool creation. Implementations provide certain guarantees
-- of when fragmentation /must/ not cause allocation failure, as described
-- below.
--
-- If a descriptor pool has not had any descriptor sets freed since it was
-- created or most recently reset then fragmentation /must/ not cause an
-- allocation failure (note that this is always the case for a pool created
-- without the
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT'
-- bit set). Additionally, if all sets allocated from the pool since it was
-- created or most recently reset use the same number of descriptors (of
-- each type) and the requested allocation also uses that same number of
-- descriptors (of each type), then fragmentation /must/ not cause an
-- allocation failure.
--
-- If an allocation failure occurs due to fragmentation, an application
-- /can/ create an additional descriptor pool to perform further descriptor
-- set allocations.
--
-- If @flags@ has the
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
-- bit set, descriptor pool creation /may/ fail with the error
-- 'Vulkan.Core10.Enums.Result.ERROR_FRAGMENTATION' if the total number of
-- descriptors across all pools (including this one) created with this bit
-- set exceeds @maxUpdateAfterBindDescriptorsInAllPools@, or if
-- fragmentation of the underlying hardware resources occurs.
--
-- If a @pPoolSizes@[i]::@type@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE', a
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'
-- struct in the @pNext@ chain /can/ be used to specify which mutable
-- descriptor types /can/ be allocated from the pool. If present in the
-- @pNext@ chain,
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'::@pMutableDescriptorTypeLists@[i]
-- specifies which kind of
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
-- descriptors /can/ be allocated from this pool entry. If
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'
-- does not exist in the @pNext@ chain, or
-- 'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'::@pMutableDescriptorTypeLists@[i]
-- is out of range, the descriptor pool allocates enough memory to be able
-- to allocate a
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
-- descriptor with any supported
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' as a mutable
-- descriptor. A mutable descriptor /can/ be allocated from a pool entry if
-- the type list in 'DescriptorSetLayoutCreateInfo' is a subset of the type
-- list declared in the descriptor pool, or if the pool entry is created
-- without a descriptor type list.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorPoolCreateInfo-maxSets-00301# @maxSets@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkDescriptorPoolCreateInfo-flags-04607# If @flags@ has the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--     bit set, then the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     bit /must/ not be set
--
-- -   #VUID-VkDescriptorPoolCreateInfo-mutableDescriptorType-04608# If
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'::@mutableDescriptorType@
--     is not enabled, @pPoolSizes@ /must/ not contain a @descriptorType@
--     of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkDescriptorPoolCreateInfo-flags-04609# If @flags@ has the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--     bit set,
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'::@mutableDescriptorType@
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorPoolCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO'
--
-- -   #VUID-VkDescriptorPoolCreateInfo-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.DescriptorPoolInlineUniformBlockCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.MutableDescriptorTypeCreateInfoVALVE'
--
-- -   #VUID-VkDescriptorPoolCreateInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDescriptorPoolCreateInfo-flags-parameter# @flags@ /must/ be
--     a valid combination of
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits'
--     values
--
-- -   #VUID-VkDescriptorPoolCreateInfo-pPoolSizes-parameter# @pPoolSizes@
--     /must/ be a valid pointer to an array of @poolSizeCount@ valid
--     'DescriptorPoolSize' structures
--
-- -   #VUID-VkDescriptorPoolCreateInfo-poolSizeCount-arraylength#
--     @poolSizeCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlags',
-- 'DescriptorPoolSize', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDescriptorPool'
data DescriptorPoolCreateInfo (es :: [Type]) = DescriptorPoolCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits'
    -- specifying certain supported operations on the pool.
    flags :: DescriptorPoolCreateFlags
  , -- | @maxSets@ is the maximum number of descriptor sets that /can/ be
    -- allocated from the pool.
    maxSets :: Word32
  , -- | @pPoolSizes@ is a pointer to an array of 'DescriptorPoolSize'
    -- structures, each containing a descriptor type and number of descriptors
    -- of that type to be allocated in the pool.
    poolSizes :: Vector DescriptorPoolSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorPoolCreateInfo es)

instance Extensible DescriptorPoolCreateInfo where
  extensibleTypeName = "DescriptorPoolCreateInfo"
  setNext x next = x{next = next}
  getNext DescriptorPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MutableDescriptorTypeCreateInfoVALVE = Just f
    | Just Refl <- eqT @e @DescriptorPoolInlineUniformBlockCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss DescriptorPoolCreateInfo es, PokeChain es) => ToCStruct (DescriptorPoolCreateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPoolCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (maxSets)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (poolSizes)) :: Word32))
    pPPoolSizes' <- ContT $ allocaBytesAligned @DescriptorPoolSize ((Data.Vector.length (poolSizes)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPoolSizes' `plusPtr` (8 * (i)) :: Ptr DescriptorPoolSize) (e)) (poolSizes)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorPoolSize))) (pPPoolSizes')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss DescriptorPoolCreateInfo es, PeekChain es) => FromCStruct (DescriptorPoolCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DescriptorPoolCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorPoolCreateFlags))
    maxSets <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    poolSizeCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPoolSizes <- peek @(Ptr DescriptorPoolSize) ((p `plusPtr` 32 :: Ptr (Ptr DescriptorPoolSize)))
    pPoolSizes' <- generateM (fromIntegral poolSizeCount) (\i -> peekCStruct @DescriptorPoolSize ((pPoolSizes `advancePtrBytes` (8 * (i)) :: Ptr DescriptorPoolSize)))
    pure $ DescriptorPoolCreateInfo
             next flags maxSets pPoolSizes'

instance es ~ '[] => Zero (DescriptorPoolCreateInfo es) where
  zero = DescriptorPoolCreateInfo
           ()
           zero
           zero
           mempty


-- | VkDescriptorSetAllocateInfo - Structure specifying the allocation
-- parameters for descriptor sets
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorSetAllocateInfo-pSetLayouts-00308# Each element of
--     @pSetLayouts@ /must/ not have been created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--     set
--
-- -   #VUID-VkDescriptorSetAllocateInfo-pSetLayouts-03044# If any element
--     of @pSetLayouts@ was created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     bit set, @descriptorPool@ /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
--     flag set
--
-- -   #VUID-VkDescriptorSetAllocateInfo-pSetLayouts-04610# If any element
--     of @pSetLayouts@ was created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--     bit set, @descriptorPool@ /must/ have been created with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetAllocateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO'
--
-- -   #VUID-VkDescriptorSetAllocateInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountAllocateInfo'
--
-- -   #VUID-VkDescriptorSetAllocateInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDescriptorSetAllocateInfo-descriptorPool-parameter#
--     @descriptorPool@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorPool' handle
--
-- -   #VUID-VkDescriptorSetAllocateInfo-pSetLayouts-parameter#
--     @pSetLayouts@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handles
--
-- -   #VUID-VkDescriptorSetAllocateInfo-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-VkDescriptorSetAllocateInfo-commonparent# Both of
--     @descriptorPool@, and the elements of @pSetLayouts@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DescriptorPool',
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'allocateDescriptorSets'
data DescriptorSetAllocateInfo (es :: [Type]) = DescriptorSetAllocateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @descriptorPool@ is the pool which the sets will be allocated from.
    descriptorPool :: DescriptorPool
  , -- | @pSetLayouts@ is a pointer to an array of descriptor set layouts, with
    -- each member specifying how the corresponding descriptor set is
    -- allocated.
    setLayouts :: Vector DescriptorSetLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetAllocateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetAllocateInfo es)

instance Extensible DescriptorSetAllocateInfo where
  extensibleTypeName = "DescriptorSetAllocateInfo"
  setNext x next = x{next = next}
  getNext DescriptorSetAllocateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetAllocateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetVariableDescriptorCountAllocateInfo = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetAllocateInfo es, PokeChain es) => ToCStruct (DescriptorSetAllocateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetAllocateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPool)) (descriptorPool)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (setLayouts)) :: Word32))
    pPSetLayouts' <- ContT $ allocaBytesAligned @DescriptorSetLayout ((Data.Vector.length (setLayouts)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (setLayouts)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorPool)) (zero)
    lift $ f

instance (Extendss DescriptorSetAllocateInfo es, PeekChain es) => FromCStruct (DescriptorSetAllocateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    descriptorPool <- peek @DescriptorPool ((p `plusPtr` 16 :: Ptr DescriptorPool))
    descriptorSetCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pSetLayouts <- peek @(Ptr DescriptorSetLayout) ((p `plusPtr` 32 :: Ptr (Ptr DescriptorSetLayout)))
    pSetLayouts' <- generateM (fromIntegral descriptorSetCount) (\i -> peek @DescriptorSetLayout ((pSetLayouts `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSetLayout)))
    pure $ DescriptorSetAllocateInfo
             next descriptorPool pSetLayouts'

instance es ~ '[] => Zero (DescriptorSetAllocateInfo es) where
  zero = DescriptorSetAllocateInfo
           ()
           zero
           mempty

