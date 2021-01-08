{-# language CPP #-}
-- No documentation found for Chapter "SparseResourceMemoryManagement"
module Vulkan.Core10.SparseResourceMemoryManagement  ( getImageSparseMemoryRequirements
                                                     , getPhysicalDeviceSparseImageFormatProperties
                                                     , queueBindSparse
                                                     , SparseImageFormatProperties(..)
                                                     , SparseImageMemoryRequirements(..)
                                                     , ImageSubresource(..)
                                                     , SparseMemoryBind(..)
                                                     , SparseImageMemoryBind(..)
                                                     , SparseBufferMemoryBindInfo(..)
                                                     , SparseImageOpaqueMemoryBindInfo(..)
                                                     , SparseImageMemoryBindInfo(..)
                                                     , BindSparseInfo(..)
                                                     , ImageAspectFlagBits(..)
                                                     , ImageAspectFlags
                                                     , SparseImageFormatFlagBits(..)
                                                     , SparseImageFormatFlags
                                                     , SparseMemoryBindFlagBits(..)
                                                     , SparseMemoryBindFlags
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
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSparseMemoryRequirements))
import Vulkan.Dynamic (DeviceCmds(pVkQueueBindSparse))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageType (ImageType(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSparseImageFormatProperties))
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlags)
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlagBits(..))
import Vulkan.Core10.Enums.SparseImageFormatFlagBits (SparseImageFormatFlags)
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlagBits(..))
import Vulkan.Core10.Enums.SparseMemoryBindFlagBits (SparseMemoryBindFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements
  :: FunPtr (Ptr Device_T -> Image -> Ptr Word32 -> Ptr SparseImageMemoryRequirements -> IO ()) -> Ptr Device_T -> Image -> Ptr Word32 -> Ptr SparseImageMemoryRequirements -> IO ()

-- | vkGetImageSparseMemoryRequirements - Query the memory requirements for a
-- sparse image
--
-- = Description
--
-- If @pSparseMemoryRequirements@ is @NULL@, then the number of sparse
-- memory requirements available is returned in
-- @pSparseMemoryRequirementCount@. Otherwise,
-- @pSparseMemoryRequirementCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSparseMemoryRequirements@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pSparseMemoryRequirements@. If
-- @pSparseMemoryRequirementCount@ is less than the number of sparse memory
-- requirements available, at most @pSparseMemoryRequirementCount@
-- structures will be written.
--
-- If the image was not created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- then @pSparseMemoryRequirementCount@ will be set to zero and
-- @pSparseMemoryRequirements@ will not be written to.
--
-- Note
--
-- It is legal for an implementation to report a larger value in
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ than would
-- be obtained by adding together memory sizes for all
-- 'SparseImageMemoryRequirements' returned by
-- 'getImageSparseMemoryRequirements'. This /may/ occur when the
-- implementation requires unused padding in the address range describing
-- the resource.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageSparseMemoryRequirements-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageSparseMemoryRequirements-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkGetImageSparseMemoryRequirements-pSparseMemoryRequirementCount-parameter#
--     @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetImageSparseMemoryRequirements-pSparseMemoryRequirements-parameter#
--     If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@ 'SparseImageMemoryRequirements'
--     structures
--
-- -   #VUID-vkGetImageSparseMemoryRequirements-image-parent# @image@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'SparseImageMemoryRequirements'
getImageSparseMemoryRequirements :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the logical device that owns the image.
                                    Device
                                 -> -- | @image@ is the 'Vulkan.Core10.Handles.Image' object to get the memory
                                    -- requirements for.
                                    Image
                                 -> io (("sparseMemoryRequirements" ::: Vector SparseImageMemoryRequirements))
getImageSparseMemoryRequirements device image = liftIO . evalContT $ do
  let vkGetImageSparseMemoryRequirementsPtr = pVkGetImageSparseMemoryRequirements (deviceCmds (device :: Device))
  lift $ unless (vkGetImageSparseMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSparseMemoryRequirements is null" Nothing Nothing
  let vkGetImageSparseMemoryRequirements' = mkVkGetImageSparseMemoryRequirements vkGetImageSparseMemoryRequirementsPtr
  let device' = deviceHandle (device)
  pPSparseMemoryRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetImageSparseMemoryRequirements" (vkGetImageSparseMemoryRequirements' device' (image) (pPSparseMemoryRequirementCount) (nullPtr))
  pSparseMemoryRequirementCount <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pPSparseMemoryRequirements <- ContT $ bracket (callocBytes @SparseImageMemoryRequirements ((fromIntegral (pSparseMemoryRequirementCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSparseMemoryRequirements `advancePtrBytes` (i * 48) :: Ptr SparseImageMemoryRequirements) . ($ ())) [0..(fromIntegral (pSparseMemoryRequirementCount)) - 1]
  lift $ traceAroundEvent "vkGetImageSparseMemoryRequirements" (vkGetImageSparseMemoryRequirements' device' (image) (pPSparseMemoryRequirementCount) ((pPSparseMemoryRequirements)))
  pSparseMemoryRequirementCount' <- lift $ peek @Word32 pPSparseMemoryRequirementCount
  pSparseMemoryRequirements' <- lift $ generateM (fromIntegral (pSparseMemoryRequirementCount')) (\i -> peekCStruct @SparseImageMemoryRequirements (((pPSparseMemoryRequirements) `advancePtrBytes` (48 * (i)) :: Ptr SparseImageMemoryRequirements)))
  pure $ (pSparseMemoryRequirements')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> SampleCountFlagBits -> ImageUsageFlags -> ImageTiling -> Ptr Word32 -> Ptr SparseImageFormatProperties -> IO ()) -> Ptr PhysicalDevice_T -> Format -> ImageType -> SampleCountFlagBits -> ImageUsageFlags -> ImageTiling -> Ptr Word32 -> Ptr SparseImageFormatProperties -> IO ()

-- | vkGetPhysicalDeviceSparseImageFormatProperties - Retrieve properties of
-- an image format applied to sparse images
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of sparse format properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of sparse format properties
-- available, at most @pPropertyCount@ structures will be written.
--
-- If
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- is not supported for the given arguments, @pPropertyCount@ will be set
-- to zero upon return, and no data will be written to @pProperties@.
--
-- Multiple aspects are returned for depth\/stencil images that are
-- implemented as separate planes by the implementation. The depth and
-- stencil data planes each have unique 'SparseImageFormatProperties' data.
--
-- Depth\/stencil images with depth and stencil data interleaved into a
-- single plane will return a single 'SparseImageFormatProperties'
-- structure with the @aspectMask@ set to
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' |
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-samples-01094#
--     @samples@ /must/ be a bit value that is set in
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
--     with @format@, @type@, @tiling@, and @usage@ equal to those in this
--     command and @flags@ equal to the value that is set in
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ when the image is
--     created
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-format-parameter#
--     @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-type-parameter#
--     @type@ /must/ be a valid 'Vulkan.Core10.Enums.ImageType.ImageType'
--     value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-samples-parameter#
--     @samples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-usage-parameter#
--     @usage@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-usage-requiredbitmask#
--     @usage@ /must/ not be @0@
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-tiling-parameter#
--     @tiling@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'SparseImageFormatProperties'
--     structures
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'SparseImageFormatProperties'
getPhysicalDeviceSparseImageFormatProperties :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the sparse
                                                -- image capabilities.
                                                PhysicalDevice
                                             -> -- | @format@ is the image format.
                                                Format
                                             -> -- | @type@ is the dimensionality of image.
                                                ImageType
                                             -> -- | @samples@ is the number of samples per texel as defined in
                                                -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'.
                                                ("samples" ::: SampleCountFlagBits)
                                             -> -- | @usage@ is a bitmask describing the intended usage of the image.
                                                ImageUsageFlags
                                             -> -- | @tiling@ is the tiling arrangement of the texel blocks in memory.
                                                ImageTiling
                                             -> io (("properties" ::: Vector SparseImageFormatProperties))
getPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSparseImageFormatPropertiesPtr = pVkGetPhysicalDeviceSparseImageFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSparseImageFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSparseImageFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceSparseImageFormatProperties' = mkVkGetPhysicalDeviceSparseImageFormatProperties vkGetPhysicalDeviceSparseImageFormatPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetPhysicalDeviceSparseImageFormatProperties" (vkGetPhysicalDeviceSparseImageFormatProperties' physicalDevice' (format) (type') (samples) (usage) (tiling) (pPPropertyCount) (nullPtr))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @SparseImageFormatProperties ((fromIntegral (pPropertyCount)) * 20)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 20) :: Ptr SparseImageFormatProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  lift $ traceAroundEvent "vkGetPhysicalDeviceSparseImageFormatProperties" (vkGetPhysicalDeviceSparseImageFormatProperties' physicalDevice' (format) (type') (samples) (usage) (tiling) (pPPropertyCount) ((pPProperties)))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @SparseImageFormatProperties (((pPProperties) `advancePtrBytes` (20 * (i)) :: Ptr SparseImageFormatProperties)))
  pure $ (pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBindSparse
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct BindSparseInfo) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct BindSparseInfo) -> Fence -> IO Result

-- | vkQueueBindSparse - Bind device memory to a sparse resource object
--
-- = Description
--
-- 'queueBindSparse' is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pBindInfo@ as a
-- 'BindSparseInfo' structure. Batches begin execution in the order they
-- appear in @pBindInfo@, but /may/ complete out of order.
--
-- Within a batch, a given range of a resource /must/ not be bound more
-- than once. Across batches, if a range is to be bound to one allocation
-- and offset and then to another allocation and offset, then the
-- application /must/ guarantee (usually using semaphores) that the binding
-- operations are executed in the correct order, as well as to order
-- binding operations against the execution of command buffer submissions.
--
-- As no operation to 'queueBindSparse' causes any pipeline stage to access
-- memory, synchronization primitives used in this command effectively only
-- define execution dependencies.
--
-- Additional information about fence and semaphore operation is described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- == Valid Usage
--
-- -   #VUID-vkQueueBindSparse-fence-01113# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be
--     unsignaled
--
-- -   #VUID-vkQueueBindSparse-fence-01114# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ not be
--     associated with any other queue command that has not yet completed
--     execution on that queue
--
-- -   #VUID-vkQueueBindSparse-pSignalSemaphores-01115# Each element of the
--     @pSignalSemaphores@ member of each element of @pBindInfo@ /must/ be
--     unsignaled when the semaphore signal operation it defines is
--     executed on the device
--
-- -   #VUID-vkQueueBindSparse-pWaitSemaphores-01116# When a semaphore wait
--     operation referring to a binary semaphore defined by any element of
--     the @pWaitSemaphores@ member of any element of @pBindInfo@ executes
--     on @queue@, there /must/ be no other queues waiting on the same
--     semaphore
--
-- -   #VUID-vkQueueBindSparse-pWaitSemaphores-01117# All elements of the
--     @pWaitSemaphores@ member of all elements of @pBindInfo@ member
--     referring to a binary semaphore /must/ be semaphores that are
--     signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution
--
-- -   #VUID-vkQueueBindSparse-pWaitSemaphores-03245# All elements of the
--     @pWaitSemaphores@ member of all elements of @pBindInfo@ created with
--     a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY' /must/
--     reference a semaphore signal operation that has been submitted for
--     execution and any semaphore signal operations on which it depends
--     (if any) /must/ have also been submitted for execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueBindSparse-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueueBindSparse-pBindInfo-parameter# If @bindInfoCount@ is
--     not @0@, @pBindInfo@ /must/ be a valid pointer to an array of
--     @bindInfoCount@ valid 'BindSparseInfo' structures
--
-- -   #VUID-vkQueueBindSparse-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkQueueBindSparse-queuetype# The @queue@ /must/ support sparse
--     binding operations
--
-- -   #VUID-vkQueueBindSparse-commonparent# Both of @fence@, and @queue@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @pBindInfo@[].pBufferBinds[].buffer /must/ be
--     externally synchronized
--
-- -   Host access to @pBindInfo@[].pImageOpaqueBinds[].image /must/ be
--     externally synchronized
--
-- -   Host access to @pBindInfo@[].pImageBinds[].image /must/ be
--     externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | SPARSE_BINDING                                                                                                        | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'BindSparseInfo', 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Handles.Queue'
queueBindSparse :: forall io
                 . (MonadIO io)
                => -- | @queue@ is the queue that the sparse binding operations will be
                   -- submitted to.
                   Queue
                -> -- | @pBindInfo@ is a pointer to an array of 'BindSparseInfo' structures,
                   -- each specifying a sparse binding submission batch.
                   ("bindInfo" ::: Vector (SomeStruct BindSparseInfo))
                -> -- | @fence@ is an /optional/ handle to a fence to be signaled. If @fence@ is
                   -- not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it defines a
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
                   Fence
                -> io ()
queueBindSparse queue bindInfo fence = liftIO . evalContT $ do
  let vkQueueBindSparsePtr = pVkQueueBindSparse (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueBindSparsePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueBindSparse is null" Nothing Nothing
  let vkQueueBindSparse' = mkVkQueueBindSparse vkQueueBindSparsePtr
  pPBindInfo <- ContT $ allocaBytesAligned @(BindSparseInfo _) ((Data.Vector.length (bindInfo)) * 96) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfo `plusPtr` (96 * (i)) :: Ptr (BindSparseInfo _))) (e) . ($ ())) (bindInfo)
  r <- lift $ traceAroundEvent "vkQueueBindSparse" (vkQueueBindSparse' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (bindInfo)) :: Word32)) (forgetExtensions (pPBindInfo)) (fence))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkSparseImageFormatProperties - Structure specifying sparse image format
-- properties
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SparseImageFormatFlags',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.SparseImageFormatProperties2',
-- 'SparseImageMemoryRequirements',
-- 'getPhysicalDeviceSparseImageFormatProperties'
data SparseImageFormatProperties = SparseImageFormatProperties
  { -- | @aspectMask@ is a bitmask
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' specifying
    -- which aspects of the image the properties apply to.
    aspectMask :: ImageAspectFlags
  , -- | @imageGranularity@ is the width, height, and depth of the sparse image
    -- block in texels or compressed texel blocks.
    imageGranularity :: Extent3D
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SparseImageFormatFlagBits'
    -- specifying additional information about the sparse resource.
    flags :: SparseImageFormatFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageFormatProperties)
#endif
deriving instance Show SparseImageFormatProperties

instance ToCStruct SparseImageFormatProperties where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Extent3D)) (imageGranularity)
    poke ((p `plusPtr` 16 :: Ptr SparseImageFormatFlags)) (flags)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct SparseImageFormatProperties where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    imageGranularity <- peekCStruct @Extent3D ((p `plusPtr` 4 :: Ptr Extent3D))
    flags <- peek @SparseImageFormatFlags ((p `plusPtr` 16 :: Ptr SparseImageFormatFlags))
    pure $ SparseImageFormatProperties
             aspectMask imageGranularity flags

instance Storable SparseImageFormatProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageFormatProperties where
  zero = SparseImageFormatProperties
           zero
           zero
           zero


-- | VkSparseImageMemoryRequirements - Structure specifying sparse image
-- memory requirements
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'SparseImageFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.SparseImageMemoryRequirements2',
-- 'getImageSparseMemoryRequirements'
data SparseImageMemoryRequirements = SparseImageMemoryRequirements
  { -- No documentation found for Nested "VkSparseImageMemoryRequirements" "formatProperties"
    formatProperties :: SparseImageFormatProperties
  , -- | @imageMipTailFirstLod@ is the first mip level at which image
    -- subresources are included in the mip tail region.
    imageMipTailFirstLod :: Word32
  , -- | @imageMipTailSize@ is the memory size (in bytes) of the mip tail region.
    -- If @formatProperties.flags@ contains
    -- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT',
    -- this is the size of the whole mip tail, otherwise this is the size of
    -- the mip tail of a single array layer. This value is guaranteed to be a
    -- multiple of the sparse block size in bytes.
    imageMipTailSize :: DeviceSize
  , -- | @imageMipTailOffset@ is the opaque memory offset used with
    -- 'SparseImageOpaqueMemoryBindInfo' to bind the mip tail region(s).
    imageMipTailOffset :: DeviceSize
  , -- | @imageMipTailStride@ is the offset stride between each array-layer’s mip
    -- tail, if @formatProperties.flags@ does not contain
    -- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT'
    -- (otherwise the value is undefined).
    imageMipTailStride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryRequirements)
#endif
deriving instance Show SparseImageMemoryRequirements

instance ToCStruct SparseImageMemoryRequirements where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryRequirements{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties)) (formatProperties)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (imageMipTailFirstLod)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (imageMipTailSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (imageMipTailOffset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (imageMipTailStride)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseImageMemoryRequirements where
  peekCStruct p = do
    formatProperties <- peekCStruct @SparseImageFormatProperties ((p `plusPtr` 0 :: Ptr SparseImageFormatProperties))
    imageMipTailFirstLod <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    imageMipTailSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    imageMipTailOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    imageMipTailStride <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ SparseImageMemoryRequirements
             formatProperties imageMipTailFirstLod imageMipTailSize imageMipTailOffset imageMipTailStride

instance Storable SparseImageMemoryRequirements where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageMemoryRequirements where
  zero = SparseImageMemoryRequirements
           zero
           zero
           zero
           zero
           zero


-- | VkImageSubresource - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'SparseImageMemoryBind', 'Vulkan.Core10.Image.getImageSubresourceLayout'
data ImageSubresource = ImageSubresource
  { -- | @aspectMask@ is a
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags' selecting the
    -- image /aspect/.
    --
    -- #VUID-VkImageSubresource-aspectMask-parameter# @aspectMask@ /must/ be a
    -- valid combination of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
    --
    -- #VUID-VkImageSubresource-aspectMask-requiredbitmask# @aspectMask@ /must/
    -- not be @0@
    aspectMask :: ImageAspectFlags
  , -- | @mipLevel@ selects the mipmap level.
    mipLevel :: Word32
  , -- | @arrayLayer@ selects the array layer.
    arrayLayer :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresource)
#endif
deriving instance Show ImageSubresource

instance ToCStruct ImageSubresource where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (arrayLayer)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresource where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    arrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ ImageSubresource
             aspectMask mipLevel arrayLayer

instance Storable ImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource where
  zero = ImageSubresource
           zero
           zero
           zero


-- | VkSparseMemoryBind - Structure specifying a sparse memory bind operation
--
-- = Description
--
-- The /binding range/ [@resourceOffset@, @resourceOffset@ + @size@) has
-- different constraints based on @flags@. If @flags@ contains
-- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SPARSE_MEMORY_BIND_METADATA_BIT',
-- the binding range /must/ be within the mip tail region of the metadata
-- aspect. This metadata region is defined by:
--
-- -   metadataRegion = [base, base + @imageMipTailSize@)
--
-- -   base = @imageMipTailOffset@ + @imageMipTailStride@ × n
--
-- and @imageMipTailOffset@, @imageMipTailSize@, and @imageMipTailStride@
-- values are from the 'SparseImageMemoryRequirements' corresponding to the
-- metadata aspect of the image, and n is a valid array layer index for the
-- image,
--
-- @imageMipTailStride@ is considered to be zero for aspects where
-- 'SparseImageMemoryRequirements'::@formatProperties.flags@ contains
-- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT'.
--
-- If @flags@ does not contain
-- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SPARSE_MEMORY_BIND_METADATA_BIT',
-- the binding range /must/ be within the range
-- [0,'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@).
--
-- == Valid Usage
--
-- -   #VUID-VkSparseMemoryBind-memory-01096# If @memory@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @memory@ and
--     @memoryOffset@ /must/ match the memory requirements of the resource,
--     as described in section
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-association>
--
-- -   #VUID-VkSparseMemoryBind-memory-01097# If @memory@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @memory@ /must/ not have
--     been created with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--     bit set
--
-- -   #VUID-VkSparseMemoryBind-size-01098# @size@ /must/ be greater than
--     @0@
--
-- -   #VUID-VkSparseMemoryBind-resourceOffset-01099# @resourceOffset@
--     /must/ be less than the size of the resource
--
-- -   #VUID-VkSparseMemoryBind-size-01100# @size@ /must/ be less than or
--     equal to the size of the resource minus @resourceOffset@
--
-- -   #VUID-VkSparseMemoryBind-memoryOffset-01101# @memoryOffset@ /must/
--     be less than the size of @memory@
--
-- -   #VUID-VkSparseMemoryBind-size-01102# @size@ /must/ be less than or
--     equal to the size of @memory@ minus @memoryOffset@
--
-- -   #VUID-VkSparseMemoryBind-memory-02730# If @memory@ was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     not equal to @0@, at least one handle type it contained /must/ also
--     have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when the resource was created
--
-- -   #VUID-VkSparseMemoryBind-memory-02731# If @memory@ was created by a
--     memory import operation, the external handle type of the imported
--     memory /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when the resource was created
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSparseMemoryBind-memory-parameter# If @memory@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @memory@ /must/ be a valid
--     'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-VkSparseMemoryBind-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlagBits'
--     values
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'SparseBufferMemoryBindInfo', 'SparseImageOpaqueMemoryBindInfo',
-- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlags'
data SparseMemoryBind = SparseMemoryBind
  { -- | @resourceOffset@ is the offset into the resource.
    resourceOffset :: DeviceSize
  , -- | @size@ is the size of the memory region to be bound.
    size :: DeviceSize
  , -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object that the
    -- range of the resource is bound to. If @memory@ is
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the range is unbound.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the offset into the
    -- 'Vulkan.Core10.Handles.DeviceMemory' object to bind the resource range
    -- to. If @memory@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', this value
    -- is ignored.
    memoryOffset :: DeviceSize
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlagBits'
    -- specifying usage of the binding operation.
    flags :: SparseMemoryBindFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseMemoryBind)
#endif
deriving instance Show SparseMemoryBind

instance ToCStruct SparseMemoryBind where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseMemoryBind{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (resourceOffset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (memoryOffset)
    poke ((p `plusPtr` 32 :: Ptr SparseMemoryBindFlags)) (flags)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseMemoryBind where
  peekCStruct p = do
    resourceOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    flags <- peek @SparseMemoryBindFlags ((p `plusPtr` 32 :: Ptr SparseMemoryBindFlags))
    pure $ SparseMemoryBind
             resourceOffset size memory memoryOffset flags

instance Storable SparseMemoryBind where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseMemoryBind where
  zero = SparseMemoryBind
           zero
           zero
           zero
           zero
           zero


-- | VkSparseImageMemoryBind - Structure specifying sparse image memory bind
--
-- == Valid Usage
--
-- -   #VUID-VkSparseImageMemoryBind-memory-01104# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, and if any other resources are bound to
--     ranges of @memory@, the range of @memory@ being bound /must/ not
--     overlap with those bound ranges
--
-- -   #VUID-VkSparseImageMemoryBind-memory-01105# @memory@ and
--     @memoryOffset@ /must/ match the memory requirements of the calling
--     command’s @image@, as described in section
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-association>
--
-- -   #VUID-VkSparseImageMemoryBind-subresource-01106# @subresource@
--     /must/ be a valid image subresource for @image@ (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views>)
--
-- -   #VUID-VkSparseImageMemoryBind-offset-01107# @offset.x@ /must/ be a
--     multiple of the sparse image block width
--     ('SparseImageFormatProperties'::@imageGranularity.width@) of the
--     image
--
-- -   #VUID-VkSparseImageMemoryBind-extent-01108# @extent.width@ /must/
--     either be a multiple of the sparse image block width of the image,
--     or else (@extent.width@ + @offset.x@) /must/ equal the width of the
--     image subresource
--
-- -   #VUID-VkSparseImageMemoryBind-offset-01109# @offset.y@ /must/ be a
--     multiple of the sparse image block height
--     ('SparseImageFormatProperties'::@imageGranularity.height@) of the
--     image
--
-- -   #VUID-VkSparseImageMemoryBind-extent-01110# @extent.height@ /must/
--     either be a multiple of the sparse image block height of the image,
--     or else (@extent.height@ + @offset.y@) /must/ equal the height of
--     the image subresource
--
-- -   #VUID-VkSparseImageMemoryBind-offset-01111# @offset.z@ /must/ be a
--     multiple of the sparse image block depth
--     ('SparseImageFormatProperties'::@imageGranularity.depth@) of the
--     image
--
-- -   #VUID-VkSparseImageMemoryBind-extent-01112# @extent.depth@ /must/
--     either be a multiple of the sparse image block depth of the image,
--     or else (@extent.depth@ + @offset.z@) /must/ equal the depth of the
--     image subresource
--
-- -   #VUID-VkSparseImageMemoryBind-memory-02732# If @memory@ was created
--     with
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     not equal to @0@, at least one handle type it contained /must/ also
--     have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when the image was created
--
-- -   #VUID-VkSparseImageMemoryBind-memory-02733# If @memory@ was created
--     by a memory import operation, the external handle type of the
--     imported memory /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when @image@ was created
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSparseImageMemoryBind-subresource-parameter# @subresource@
--     /must/ be a valid 'ImageSubresource' structure
--
-- -   #VUID-VkSparseImageMemoryBind-memory-parameter# If @memory@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @memory@ /must/ be a valid
--     'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-VkSparseImageMemoryBind-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlagBits'
--     values
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D', 'ImageSubresource',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'SparseImageMemoryBindInfo',
-- 'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SparseMemoryBindFlags'
data SparseImageMemoryBind = SparseImageMemoryBind
  { -- | @subresource@ is the image /aspect/ and region of interest in the image.
    subresource :: ImageSubresource
  , -- | @offset@ are the coordinates of the first texel within the image
    -- subresource to bind.
    offset :: Offset3D
  , -- | @extent@ is the size in texels of the region within the image
    -- subresource to bind. The extent /must/ be a multiple of the sparse image
    -- block dimensions, except when binding sparse image blocks along the edge
    -- of an image subresource it /can/ instead be such that any coordinate of
    -- @offset@ + @extent@ equals the corresponding dimensions of the image
    -- subresource.
    extent :: Extent3D
  , -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object that the
    -- sparse image blocks of the image are bound to. If @memory@ is
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the sparse image blocks are
    -- unbound.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is an offset into 'Vulkan.Core10.Handles.DeviceMemory'
    -- object. If @memory@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', this
    -- value is ignored.
    memoryOffset :: DeviceSize
  , -- | @flags@ are sparse memory binding flags.
    flags :: SparseMemoryBindFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryBind)
#endif
deriving instance Show SparseImageMemoryBind

instance ToCStruct SparseImageMemoryBind where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryBind{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresource)) (subresource)
    poke ((p `plusPtr` 12 :: Ptr Offset3D)) (offset)
    poke ((p `plusPtr` 24 :: Ptr Extent3D)) (extent)
    poke ((p `plusPtr` 40 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (memoryOffset)
    poke ((p `plusPtr` 56 :: Ptr SparseMemoryBindFlags)) (flags)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresource)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent3D)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SparseImageMemoryBind where
  peekCStruct p = do
    subresource <- peekCStruct @ImageSubresource ((p `plusPtr` 0 :: Ptr ImageSubresource))
    offset <- peekCStruct @Offset3D ((p `plusPtr` 12 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 24 :: Ptr Extent3D))
    memory <- peek @DeviceMemory ((p `plusPtr` 40 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    flags <- peek @SparseMemoryBindFlags ((p `plusPtr` 56 :: Ptr SparseMemoryBindFlags))
    pure $ SparseImageMemoryBind
             subresource offset extent memory memoryOffset flags

instance Storable SparseImageMemoryBind where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageMemoryBind where
  zero = SparseImageMemoryBind
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkSparseBufferMemoryBindInfo - Structure specifying a sparse buffer
-- memory bind operation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'BindSparseInfo', 'Vulkan.Core10.Handles.Buffer', 'SparseMemoryBind'
data SparseBufferMemoryBindInfo = SparseBufferMemoryBindInfo
  { -- | @buffer@ is the 'Vulkan.Core10.Handles.Buffer' object to be bound.
    --
    -- #VUID-VkSparseBufferMemoryBindInfo-buffer-parameter# @buffer@ /must/ be
    -- a valid 'Vulkan.Core10.Handles.Buffer' handle
    buffer :: Buffer
  , -- | @pBinds@ is a pointer to array of 'SparseMemoryBind' structures.
    --
    -- #VUID-VkSparseBufferMemoryBindInfo-pBinds-parameter# @pBinds@ /must/ be
    -- a valid pointer to an array of @bindCount@ valid 'SparseMemoryBind'
    -- structures
    binds :: Vector SparseMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseBufferMemoryBindInfo)
#endif
deriving instance Show SparseBufferMemoryBindInfo

instance ToCStruct SparseBufferMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseBufferMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (binds)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (zero)
    f

instance FromCStruct SparseBufferMemoryBindInfo where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseMemoryBind ((pBinds `advancePtrBytes` (40 * (i)) :: Ptr SparseMemoryBind)))
    pure $ SparseBufferMemoryBindInfo
             buffer pBinds'

instance Zero SparseBufferMemoryBindInfo where
  zero = SparseBufferMemoryBindInfo
           zero
           mempty


-- | VkSparseImageOpaqueMemoryBindInfo - Structure specifying sparse image
-- opaque memory bind info
--
-- == Valid Usage
--
-- -   #VUID-VkSparseImageOpaqueMemoryBindInfo-pBinds-01103# If the @flags@
--     member of any element of @pBinds@ contains
--     'Vulkan.Core10.Enums.SparseMemoryBindFlagBits.SPARSE_MEMORY_BIND_METADATA_BIT',
--     the binding range defined /must/ be within the mip tail region of
--     the metadata aspect of @image@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSparseImageOpaqueMemoryBindInfo-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkSparseImageOpaqueMemoryBindInfo-pBinds-parameter# @pBinds@
--     /must/ be a valid pointer to an array of @bindCount@ valid
--     'SparseMemoryBind' structures
--
-- -   #VUID-VkSparseImageOpaqueMemoryBindInfo-bindCount-arraylength#
--     @bindCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'BindSparseInfo', 'Vulkan.Core10.Handles.Image', 'SparseMemoryBind'
data SparseImageOpaqueMemoryBindInfo = SparseImageOpaqueMemoryBindInfo
  { -- | @image@ is the 'Vulkan.Core10.Handles.Image' object to be bound.
    image :: Image
  , -- | @pBinds@ is a pointer to an array of 'SparseMemoryBind' structures.
    binds :: Vector SparseMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageOpaqueMemoryBindInfo)
#endif
deriving instance Show SparseImageOpaqueMemoryBindInfo

instance ToCStruct SparseImageOpaqueMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageOpaqueMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseMemoryBind ((Data.Vector.length (binds)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (40 * (i)) :: Ptr SparseMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Image)) (zero)
    f

instance FromCStruct SparseImageOpaqueMemoryBindInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 0 :: Ptr Image))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseMemoryBind ((pBinds `advancePtrBytes` (40 * (i)) :: Ptr SparseMemoryBind)))
    pure $ SparseImageOpaqueMemoryBindInfo
             image pBinds'

instance Zero SparseImageOpaqueMemoryBindInfo where
  zero = SparseImageOpaqueMemoryBindInfo
           zero
           mempty


-- | VkSparseImageMemoryBindInfo - Structure specifying sparse image memory
-- bind info
--
-- == Valid Usage
--
-- -   #VUID-VkSparseImageMemoryBindInfo-subresource-01722# The
--     @subresource.mipLevel@ member of each element of @pBinds@ /must/ be
--     less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkSparseImageMemoryBindInfo-subresource-01723# The
--     @subresource.arrayLayer@ member of each element of @pBinds@ /must/
--     be less than the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkSparseImageMemoryBindInfo-image-02901# @image@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSparseImageMemoryBindInfo-image-parameter# @image@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkSparseImageMemoryBindInfo-pBinds-parameter# @pBinds@ /must/
--     be a valid pointer to an array of @bindCount@ valid
--     'SparseImageMemoryBind' structures
--
-- -   #VUID-VkSparseImageMemoryBindInfo-bindCount-arraylength# @bindCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- 'BindSparseInfo', 'Vulkan.Core10.Handles.Image', 'SparseImageMemoryBind'
data SparseImageMemoryBindInfo = SparseImageMemoryBindInfo
  { -- | @image@ is the 'Vulkan.Core10.Handles.Image' object to be bound
    image :: Image
  , -- | @pBinds@ is a pointer to an array of 'SparseImageMemoryBind' structures
    binds :: Vector SparseImageMemoryBind
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageMemoryBindInfo)
#endif
deriving instance Show SparseImageMemoryBindInfo

instance ToCStruct SparseImageMemoryBindInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageMemoryBindInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (binds)) :: Word32))
    pPBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBind ((Data.Vector.length (binds)) * 64) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBinds' `plusPtr` (64 * (i)) :: Ptr SparseImageMemoryBind) (e)) (binds)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr SparseImageMemoryBind))) (pPBinds')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Image)) (zero)
    f

instance FromCStruct SparseImageMemoryBindInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 0 :: Ptr Image))
    bindCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pBinds <- peek @(Ptr SparseImageMemoryBind) ((p `plusPtr` 16 :: Ptr (Ptr SparseImageMemoryBind)))
    pBinds' <- generateM (fromIntegral bindCount) (\i -> peekCStruct @SparseImageMemoryBind ((pBinds `advancePtrBytes` (64 * (i)) :: Ptr SparseImageMemoryBind)))
    pure $ SparseImageMemoryBindInfo
             image pBinds'

instance Zero SparseImageMemoryBindInfo where
  zero = SparseImageMemoryBindInfo
           zero
           mempty


-- | VkBindSparseInfo - Structure specifying a sparse binding operation
--
-- == Valid Usage
--
-- -   #VUID-VkBindSparseInfo-pWaitSemaphores-03246# If any element of
--     @pWaitSemaphores@ or @pSignalSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' then the
--     @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure
--
-- -   #VUID-VkBindSparseInfo-pNext-03247# If the @pNext@ chain of this
--     structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pWaitSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' then its
--     @waitSemaphoreValueCount@ member /must/ equal @waitSemaphoreCount@
--
-- -   #VUID-VkBindSparseInfo-pNext-03248# If the @pNext@ chain of this
--     structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pSignalSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' then its
--     @signalSemaphoreValueCount@ member /must/ equal
--     @signalSemaphoreCount@
--
-- -   #VUID-VkBindSparseInfo-pSignalSemaphores-03249# For each element of
--     @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value greater than the current value of the semaphore
--     when the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     is executed
--
-- -   #VUID-VkBindSparseInfo-pWaitSemaphores-03250# For each element of
--     @pWaitSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pWaitSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or from the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkBindSparseInfo-pSignalSemaphores-03251# For each element of
--     @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or from the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindSparseInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_SPARSE_INFO'
--
-- -   #VUID-VkBindSparseInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupBindSparseInfo'
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--
-- -   #VUID-VkBindSparseInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBindSparseInfo-pWaitSemaphores-parameter# If
--     @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a valid
--     pointer to an array of @waitSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkBindSparseInfo-pBufferBinds-parameter# If @bufferBindCount@
--     is not @0@, @pBufferBinds@ /must/ be a valid pointer to an array of
--     @bufferBindCount@ valid 'SparseBufferMemoryBindInfo' structures
--
-- -   #VUID-VkBindSparseInfo-pImageOpaqueBinds-parameter# If
--     @imageOpaqueBindCount@ is not @0@, @pImageOpaqueBinds@ /must/ be a
--     valid pointer to an array of @imageOpaqueBindCount@ valid
--     'SparseImageOpaqueMemoryBindInfo' structures
--
-- -   #VUID-VkBindSparseInfo-pImageBinds-parameter# If @imageBindCount@ is
--     not @0@, @pImageBinds@ /must/ be a valid pointer to an array of
--     @imageBindCount@ valid 'SparseImageMemoryBindInfo' structures
--
-- -   #VUID-VkBindSparseInfo-pSignalSemaphores-parameter# If
--     @signalSemaphoreCount@ is not @0@, @pSignalSemaphores@ /must/ be a
--     valid pointer to an array of @signalSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkBindSparseInfo-commonparent# Both of the elements of
--     @pSignalSemaphores@, and the elements of @pWaitSemaphores@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Semaphore', 'SparseBufferMemoryBindInfo',
-- 'SparseImageMemoryBindInfo', 'SparseImageOpaqueMemoryBindInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'queueBindSparse'
data BindSparseInfo (es :: [Type]) = BindSparseInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @pWaitSemaphores@ is a pointer to an array of semaphores upon which to
    -- wait on before the sparse binding operations for this batch begin
    -- execution. If semaphores to wait on are provided, they define a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>.
    waitSemaphores :: Vector Semaphore
  , -- | @pBufferBinds@ is a pointer to an array of 'SparseBufferMemoryBindInfo'
    -- structures.
    bufferBinds :: Vector SparseBufferMemoryBindInfo
  , -- | @pImageOpaqueBinds@ is a pointer to an array of
    -- 'SparseImageOpaqueMemoryBindInfo' structures, indicating opaque sparse
    -- image bindings to perform.
    imageOpaqueBinds :: Vector SparseImageOpaqueMemoryBindInfo
  , -- | @pImageBinds@ is a pointer to an array of 'SparseImageMemoryBindInfo'
    -- structures, indicating sparse image bindings to perform.
    imageBinds :: Vector SparseImageMemoryBindInfo
  , -- | @pSignalSemaphores@ is a pointer to an array of semaphores which will be
    -- signaled when the sparse binding operations for this batch have
    -- completed execution. If semaphores to be signaled are provided, they
    -- define a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>.
    signalSemaphores :: Vector Semaphore
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindSparseInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindSparseInfo es)

instance Extensible BindSparseInfo where
  extensibleTypeName = "BindSparseInfo"
  setNext x next = x{next = next}
  getNext BindSparseInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindSparseInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @TimelineSemaphoreSubmitInfo = Just f
    | Just Refl <- eqT @e @DeviceGroupBindSparseInfo = Just f
    | otherwise = Nothing

instance (Extendss BindSparseInfo es, PokeChain es) => ToCStruct (BindSparseInfo es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindSparseInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_SPARSE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphores)) :: Word32))
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (waitSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (waitSemaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bufferBinds)) :: Word32))
    pPBufferBinds' <- ContT $ allocaBytesAligned @SparseBufferMemoryBindInfo ((Data.Vector.length (bufferBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBufferBinds' `plusPtr` (24 * (i)) :: Ptr SparseBufferMemoryBindInfo) (e) . ($ ())) (bufferBinds)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SparseBufferMemoryBindInfo))) (pPBufferBinds')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageOpaqueBinds)) :: Word32))
    pPImageOpaqueBinds' <- ContT $ allocaBytesAligned @SparseImageOpaqueMemoryBindInfo ((Data.Vector.length (imageOpaqueBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageOpaqueBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageOpaqueMemoryBindInfo) (e) . ($ ())) (imageOpaqueBinds)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SparseImageOpaqueMemoryBindInfo))) (pPImageOpaqueBinds')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageBinds)) :: Word32))
    pPImageBinds' <- ContT $ allocaBytesAligned @SparseImageMemoryBindInfo ((Data.Vector.length (imageBinds)) * 24) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageBinds' `plusPtr` (24 * (i)) :: Ptr SparseImageMemoryBindInfo) (e) . ($ ())) (imageBinds)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr SparseImageMemoryBindInfo))) (pPImageBinds')
    lift $ poke ((p `plusPtr` 80 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphores)) :: Word32))
    pPSignalSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (signalSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (signalSemaphores)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr Semaphore))) (pPSignalSemaphores')
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_SPARSE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss BindSparseInfo es, PeekChain es) => FromCStruct (BindSparseInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pWaitSemaphores' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Semaphore ((pWaitSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    bufferBindCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pBufferBinds <- peek @(Ptr SparseBufferMemoryBindInfo) ((p `plusPtr` 40 :: Ptr (Ptr SparseBufferMemoryBindInfo)))
    pBufferBinds' <- generateM (fromIntegral bufferBindCount) (\i -> peekCStruct @SparseBufferMemoryBindInfo ((pBufferBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseBufferMemoryBindInfo)))
    imageOpaqueBindCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pImageOpaqueBinds <- peek @(Ptr SparseImageOpaqueMemoryBindInfo) ((p `plusPtr` 56 :: Ptr (Ptr SparseImageOpaqueMemoryBindInfo)))
    pImageOpaqueBinds' <- generateM (fromIntegral imageOpaqueBindCount) (\i -> peekCStruct @SparseImageOpaqueMemoryBindInfo ((pImageOpaqueBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseImageOpaqueMemoryBindInfo)))
    imageBindCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pImageBinds <- peek @(Ptr SparseImageMemoryBindInfo) ((p `plusPtr` 72 :: Ptr (Ptr SparseImageMemoryBindInfo)))
    pImageBinds' <- generateM (fromIntegral imageBindCount) (\i -> peekCStruct @SparseImageMemoryBindInfo ((pImageBinds `advancePtrBytes` (24 * (i)) :: Ptr SparseImageMemoryBindInfo)))
    signalSemaphoreCount <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pSignalSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 88 :: Ptr (Ptr Semaphore)))
    pSignalSemaphores' <- generateM (fromIntegral signalSemaphoreCount) (\i -> peek @Semaphore ((pSignalSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    pure $ BindSparseInfo
             next pWaitSemaphores' pBufferBinds' pImageOpaqueBinds' pImageBinds' pSignalSemaphores'

instance es ~ '[] => Zero (BindSparseInfo es) where
  zero = BindSparseInfo
           ()
           mempty
           mempty
           mempty
           mempty
           mempty

