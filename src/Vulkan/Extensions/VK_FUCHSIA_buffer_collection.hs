{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_buffer_collection - device extension
--
-- == VK_FUCHSIA_buffer_collection
--
-- [__Name String__]
--     @VK_FUCHSIA_buffer_collection@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     367
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_FUCHSIA_external_memory@
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_FUCHSIA_buffer_collection] @rosasco%0A<<Here describe the issue or question you have about the VK_FUCHSIA_buffer_collection extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Craig Stout, Google
--
--     -   John Bauman, Google
--
--     -   John Rosasco, Google
--
-- == Description
--
-- A buffer collection is a collection of one or more buffers which were
-- allocated together as a group and which all have the same properties.
-- These properties describe the buffers\' internal representation such as
-- its dimensions and memory layout. This ensures that all of the buffers
-- can be used interchangeably by tasks that require swapping among
-- multiple buffers, such as double-buffered graphics rendering.
--
-- By sharing such a collection of buffers between components,
-- communication about buffer lifecycle can be made much simpler and more
-- efficient. For example, when a content producer finishes writing to a
-- buffer, it can message the consumer of the buffer with the buffer index,
-- rather than passing a handle to the shared memory.
--
-- On Fuchsia, the Sysmem service uses buffer collections as a core
-- construct in its design. VK_FUCHSIA_buffer_collection is the Vulkan
-- extension that allows Vulkan applications to interoperate with the
-- Sysmem service on Fuchsia.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
--
-- == New Commands
--
-- -   'createBufferCollectionFUCHSIA'
--
-- -   'destroyBufferCollectionFUCHSIA'
--
-- -   'getBufferCollectionPropertiesFUCHSIA'
--
-- -   'setBufferCollectionBufferConstraintsFUCHSIA'
--
-- -   'setBufferCollectionImageConstraintsFUCHSIA'
--
-- == New Structures
--
-- -   'BufferCollectionConstraintsInfoFUCHSIA'
--
-- -   'BufferCollectionCreateInfoFUCHSIA'
--
-- -   'BufferCollectionPropertiesFUCHSIA'
--
-- -   'BufferConstraintsInfoFUCHSIA'
--
-- -   'ImageConstraintsInfoFUCHSIA'
--
-- -   'ImageFormatConstraintsInfoFUCHSIA'
--
-- -   'SysmemColorSpaceFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'BufferCollectionBufferCreateInfoFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'BufferCollectionImageCreateInfoFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryBufferCollectionFUCHSIA'
--
-- == New Enums
--
-- -   'ImageConstraintsInfoFlagBitsFUCHSIA'
--
-- == New Bitmasks
--
-- -   'ImageConstraintsInfoFlagsFUCHSIA'
--
-- -   'ImageFormatConstraintsFlagsFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME'
--
-- -   'FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_BUFFER_COLLECTION_FUCHSIA_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_BUFFER_COLLECTION_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA'
--
-- == Issues
--
-- 1) When configuring a 'ImageConstraintsInfoFUCHSIA' structure for
-- constraint setting, should a NULL @pFormatConstraints@ parameter be
-- allowed ?
--
-- __RESOLVED__: No. Specifying a NULL @pFormatConstraints@ results in
-- logical complexity of interpreting the relationship between the
-- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ settings of the elements
-- of the @pImageCreateInfos@ array and the implied or desired
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags'.
--
-- The explicit requirement for @pFormatConstraints@ to be non-NULL
-- simplifies the implied logic of the implementation and expectations for
-- the Vulkan application.
--
-- == Version History
--
-- -   Revision 2, 2021-09-23 (John Rosasco)
--
--     -   Review passes
--
-- -   Revision 1, 2021-03-09 (John Rosasco)
--
--     -   Initial revision
--
-- == See Also
--
-- 'BufferCollectionBufferCreateInfoFUCHSIA',
-- 'BufferCollectionConstraintsInfoFUCHSIA',
-- 'BufferCollectionCreateInfoFUCHSIA',
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'BufferCollectionImageCreateInfoFUCHSIA',
-- 'BufferCollectionPropertiesFUCHSIA', 'BufferConstraintsInfoFUCHSIA',
-- 'ImageConstraintsInfoFUCHSIA', 'ImageConstraintsInfoFlagBitsFUCHSIA',
-- 'ImageConstraintsInfoFlagsFUCHSIA',
-- 'ImageFormatConstraintsFlagsFUCHSIA',
-- 'ImageFormatConstraintsInfoFUCHSIA',
-- 'ImportMemoryBufferCollectionFUCHSIA', 'SysmemColorSpaceFUCHSIA',
-- 'createBufferCollectionFUCHSIA', 'destroyBufferCollectionFUCHSIA',
-- 'getBufferCollectionPropertiesFUCHSIA',
-- 'setBufferCollectionBufferConstraintsFUCHSIA',
-- 'setBufferCollectionImageConstraintsFUCHSIA'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_buffer_collection  ( createBufferCollectionFUCHSIA
                                                       , withBufferCollectionFUCHSIA
                                                       , setBufferCollectionBufferConstraintsFUCHSIA
                                                       , setBufferCollectionImageConstraintsFUCHSIA
                                                       , destroyBufferCollectionFUCHSIA
                                                       , getBufferCollectionPropertiesFUCHSIA
                                                       , ImportMemoryBufferCollectionFUCHSIA(..)
                                                       , BufferCollectionImageCreateInfoFUCHSIA(..)
                                                       , BufferCollectionBufferCreateInfoFUCHSIA(..)
                                                       , BufferCollectionCreateInfoFUCHSIA(..)
                                                       , BufferCollectionPropertiesFUCHSIA(..)
                                                       , BufferConstraintsInfoFUCHSIA(..)
                                                       , SysmemColorSpaceFUCHSIA(..)
                                                       , ImageFormatConstraintsInfoFUCHSIA(..)
                                                       , ImageConstraintsInfoFUCHSIA(..)
                                                       , BufferCollectionConstraintsInfoFUCHSIA(..)
                                                       , ImageFormatConstraintsFlagsFUCHSIA(..)
                                                       , ImageConstraintsInfoFlagsFUCHSIA
                                                       , ImageConstraintsInfoFlagBitsFUCHSIA( IMAGE_CONSTRAINTS_INFO_CPU_READ_RARELY_FUCHSIA
                                                                                            , IMAGE_CONSTRAINTS_INFO_CPU_READ_OFTEN_FUCHSIA
                                                                                            , IMAGE_CONSTRAINTS_INFO_CPU_WRITE_RARELY_FUCHSIA
                                                                                            , IMAGE_CONSTRAINTS_INFO_CPU_WRITE_OFTEN_FUCHSIA
                                                                                            , IMAGE_CONSTRAINTS_INFO_PROTECTED_OPTIONAL_FUCHSIA
                                                                                            , ..
                                                                                            )
                                                       , FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION
                                                       , pattern FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION
                                                       , FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME
                                                       , pattern FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME
                                                       , BufferCollectionFUCHSIA(..)
                                                       , DebugReportObjectTypeEXT(..)
                                                       , Zx_handle_t
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Extensions.Handles (BufferCollectionFUCHSIA)
import Vulkan.Extensions.Handles (BufferCollectionFUCHSIA(..))
import Vulkan.Core10.Buffer (BufferCreateInfo)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core10.ImageView (ComponentMapping)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateBufferCollectionFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyBufferCollectionFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferCollectionPropertiesFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkSetBufferCollectionBufferConstraintsFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkSetBufferCollectionImageConstraintsFUCHSIA))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Image (ImageCreateInfo)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (BufferCollectionFUCHSIA(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBufferCollectionFUCHSIA
  :: FunPtr (Ptr Device_T -> Ptr BufferCollectionCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr BufferCollectionFUCHSIA -> IO Result) -> Ptr Device_T -> Ptr BufferCollectionCreateInfoFUCHSIA -> Ptr AllocationCallbacks -> Ptr BufferCollectionFUCHSIA -> IO Result

-- | vkCreateBufferCollectionFUCHSIA - Create a new buffer collection
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateBufferCollectionFUCHSIA-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateBufferCollectionFUCHSIA-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'BufferCollectionCreateInfoFUCHSIA' structure
--
-- -   #VUID-vkCreateBufferCollectionFUCHSIA-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateBufferCollectionFUCHSIA-pCollection-parameter#
--     @pCollection@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_EXTERNAL_HANDLE'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- == Host Access
--
-- All functions referencing a
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' /must/ be externally
-- synchronized with the exception of 'createBufferCollectionFUCHSIA'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'BufferCollectionCreateInfoFUCHSIA',
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Handles.Device'
createBufferCollectionFUCHSIA :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that creates the
                                 -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                 Device
                              -> -- | @pCreateInfo@ is a pointer to a 'BufferCollectionCreateInfoFUCHSIA'
                                 -- structure containing parameters affecting creation of the buffer
                                 -- collection
                                 BufferCollectionCreateInfoFUCHSIA
                              -> -- | @pAllocator@ is a pointer to a
                                 -- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
                                 -- controlling host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io (BufferCollectionFUCHSIA)
createBufferCollectionFUCHSIA device createInfo allocator = liftIO . evalContT $ do
  let vkCreateBufferCollectionFUCHSIAPtr = pVkCreateBufferCollectionFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateBufferCollectionFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateBufferCollectionFUCHSIA is null" Nothing Nothing
  let vkCreateBufferCollectionFUCHSIA' = mkVkCreateBufferCollectionFUCHSIA vkCreateBufferCollectionFUCHSIAPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCollection <- ContT $ bracket (callocBytes @BufferCollectionFUCHSIA 8) free
  r <- lift $ traceAroundEvent "vkCreateBufferCollectionFUCHSIA" (vkCreateBufferCollectionFUCHSIA' (deviceHandle (device)) pCreateInfo pAllocator (pPCollection))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCollection <- lift $ peek @BufferCollectionFUCHSIA pPCollection
  pure $ (pCollection)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBufferCollectionFUCHSIA' and 'destroyBufferCollectionFUCHSIA'
--
-- To ensure that 'destroyBufferCollectionFUCHSIA' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBufferCollectionFUCHSIA :: forall io r . MonadIO io => Device -> BufferCollectionCreateInfoFUCHSIA -> Maybe AllocationCallbacks -> (io BufferCollectionFUCHSIA -> (BufferCollectionFUCHSIA -> io ()) -> r) -> r
withBufferCollectionFUCHSIA device pCreateInfo pAllocator b =
  b (createBufferCollectionFUCHSIA device pCreateInfo pAllocator)
    (\(o0) -> destroyBufferCollectionFUCHSIA device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetBufferCollectionBufferConstraintsFUCHSIA
  :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr BufferConstraintsInfoFUCHSIA -> IO Result) -> Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr BufferConstraintsInfoFUCHSIA -> IO Result

-- | vkSetBufferCollectionBufferConstraintsFUCHSIA - Set buffer-based
-- constraints for a buffer collection
--
-- = Description
--
-- 'setBufferCollectionBufferConstraintsFUCHSIA' /may/ fail if the
-- implementation does not support the constraints specified in the
-- @bufferCollectionConstraints@ structure. If that occurs,
-- 'setBufferCollectionBufferConstraintsFUCHSIA' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'BufferConstraintsInfoFUCHSIA', 'Vulkan.Core10.Handles.Device'
setBufferCollectionBufferConstraintsFUCHSIA :: forall io
                                             . (MonadIO io)
                                            => -- | @device@ is the logical device
                                               --
                                               -- #VUID-vkSetBufferCollectionBufferConstraintsFUCHSIA-device-parameter#
                                               -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                               Device
                                            -> -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                               -- handle
                                               --
                                               -- #VUID-vkSetBufferCollectionBufferConstraintsFUCHSIA-collection-parameter#
                                               -- @collection@ /must/ be a valid
                                               -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
                                               --
                                               -- #VUID-vkSetBufferCollectionBufferConstraintsFUCHSIA-collection-parent#
                                               -- @collection@ /must/ have been created, allocated, or retrieved from
                                               -- @device@
                                               BufferCollectionFUCHSIA
                                            -> -- | @pBufferConstraintsInfo@ is a pointer to a
                                               -- 'BufferConstraintsInfoFUCHSIA' structure
                                               --
                                               -- #VUID-vkSetBufferCollectionBufferConstraintsFUCHSIA-pBufferConstraintsInfo-parameter#
                                               -- @pBufferConstraintsInfo@ /must/ be a valid pointer to a valid
                                               -- 'BufferConstraintsInfoFUCHSIA' structure
                                               BufferConstraintsInfoFUCHSIA
                                            -> io ()
setBufferCollectionBufferConstraintsFUCHSIA device collection bufferConstraintsInfo = liftIO . evalContT $ do
  let vkSetBufferCollectionBufferConstraintsFUCHSIAPtr = pVkSetBufferCollectionBufferConstraintsFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetBufferCollectionBufferConstraintsFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetBufferCollectionBufferConstraintsFUCHSIA is null" Nothing Nothing
  let vkSetBufferCollectionBufferConstraintsFUCHSIA' = mkVkSetBufferCollectionBufferConstraintsFUCHSIA vkSetBufferCollectionBufferConstraintsFUCHSIAPtr
  pBufferConstraintsInfo <- ContT $ withCStruct (bufferConstraintsInfo)
  r <- lift $ traceAroundEvent "vkSetBufferCollectionBufferConstraintsFUCHSIA" (vkSetBufferCollectionBufferConstraintsFUCHSIA' (deviceHandle (device)) (collection) pBufferConstraintsInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetBufferCollectionImageConstraintsFUCHSIA
  :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr ImageConstraintsInfoFUCHSIA -> IO Result) -> Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr ImageConstraintsInfoFUCHSIA -> IO Result

-- | vkSetBufferCollectionImageConstraintsFUCHSIA - Set image-based
-- constraints for a buffer collection
--
-- = Description
--
-- 'setBufferCollectionImageConstraintsFUCHSIA' /may/ fail if
-- @pImageConstraintsInfo@::@formatConstraintsCount@ is larger than the
-- implementation-defined limit. If that occurs,
-- 'setBufferCollectionImageConstraintsFUCHSIA' will return
-- VK_ERROR_INITIALIZATION_FAILED.
--
-- 'setBufferCollectionImageConstraintsFUCHSIA' /may/ fail if the
-- implementation does not support any of the formats described by the
-- @pImageConstraintsInfo@ structure. If that occurs,
-- 'setBufferCollectionImageConstraintsFUCHSIA' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Handles.Device', 'ImageConstraintsInfoFUCHSIA'
setBufferCollectionImageConstraintsFUCHSIA :: forall io
                                            . (MonadIO io)
                                           => -- | @device@ is the logical device
                                              --
                                              -- #VUID-vkSetBufferCollectionImageConstraintsFUCHSIA-device-parameter#
                                              -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                              Device
                                           -> -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                              -- handle
                                              --
                                              -- #VUID-vkSetBufferCollectionImageConstraintsFUCHSIA-collection-parameter#
                                              -- @collection@ /must/ be a valid
                                              -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
                                              --
                                              -- #VUID-vkSetBufferCollectionImageConstraintsFUCHSIA-collection-parent#
                                              -- @collection@ /must/ have been created, allocated, or retrieved from
                                              -- @device@
                                              BufferCollectionFUCHSIA
                                           -> -- | @pImageConstraintsInfo@ is a pointer to a 'ImageConstraintsInfoFUCHSIA'
                                              -- structure
                                              --
                                              -- #VUID-vkSetBufferCollectionImageConstraintsFUCHSIA-pImageConstraintsInfo-parameter#
                                              -- @pImageConstraintsInfo@ /must/ be a valid pointer to a valid
                                              -- 'ImageConstraintsInfoFUCHSIA' structure
                                              ImageConstraintsInfoFUCHSIA
                                           -> io ()
setBufferCollectionImageConstraintsFUCHSIA device collection imageConstraintsInfo = liftIO . evalContT $ do
  let vkSetBufferCollectionImageConstraintsFUCHSIAPtr = pVkSetBufferCollectionImageConstraintsFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetBufferCollectionImageConstraintsFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetBufferCollectionImageConstraintsFUCHSIA is null" Nothing Nothing
  let vkSetBufferCollectionImageConstraintsFUCHSIA' = mkVkSetBufferCollectionImageConstraintsFUCHSIA vkSetBufferCollectionImageConstraintsFUCHSIAPtr
  pImageConstraintsInfo <- ContT $ withCStruct (imageConstraintsInfo)
  r <- lift $ traceAroundEvent "vkSetBufferCollectionImageConstraintsFUCHSIA" (vkSetBufferCollectionImageConstraintsFUCHSIA' (deviceHandle (device)) (collection) pImageConstraintsInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBufferCollectionFUCHSIA
  :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyBufferCollectionFUCHSIA - Destroy a buffer collection
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyBufferCollectionFUCHSIA-collection-06407#
--     'Vulkan.Core10.Handles.Image' and 'Vulkan.Core10.Handles.Buffer'
--     objects that referenced @collection@ upon creation by inclusion of a
--     'BufferCollectionImageCreateInfoFUCHSIA' or
--     'BufferCollectionBufferCreateInfoFUCHSIA' chained to their
--     'Vulkan.Core10.Image.ImageCreateInfo' or
--     'Vulkan.Core10.Buffer.BufferCreateInfo' structures respectively,
--     /may/ outlive @collection@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyBufferCollectionFUCHSIA-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyBufferCollectionFUCHSIA-collection-parameter#
--     @collection@ /must/ be a valid
--     'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
--
-- -   #VUID-vkDestroyBufferCollectionFUCHSIA-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyBufferCollectionFUCHSIA-collection-parent#
--     @collection@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Handles.Device'
destroyBufferCollectionFUCHSIA :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the
                                  -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                  Device
                               -> -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                  -- handle
                                  BufferCollectionFUCHSIA
                               -> -- | @pAllocator@ is a pointer to a
                                  -- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
                                  -- controlling host memory allocation as described in the
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io ()
destroyBufferCollectionFUCHSIA device collection allocator = liftIO . evalContT $ do
  let vkDestroyBufferCollectionFUCHSIAPtr = pVkDestroyBufferCollectionFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyBufferCollectionFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyBufferCollectionFUCHSIA is null" Nothing Nothing
  let vkDestroyBufferCollectionFUCHSIA' = mkVkDestroyBufferCollectionFUCHSIA vkDestroyBufferCollectionFUCHSIAPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyBufferCollectionFUCHSIA" (vkDestroyBufferCollectionFUCHSIA' (deviceHandle (device)) (collection) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferCollectionPropertiesFUCHSIA
  :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr BufferCollectionPropertiesFUCHSIA -> IO Result) -> Ptr Device_T -> BufferCollectionFUCHSIA -> Ptr BufferCollectionPropertiesFUCHSIA -> IO Result

-- | vkGetBufferCollectionPropertiesFUCHSIA - Retrieve properties from a
-- buffer collection
--
-- = Description
--
-- For image-based buffer collections, upon calling
-- 'getBufferCollectionPropertiesFUCHSIA', Sysmem will choose an element of
-- the 'ImageConstraintsInfoFUCHSIA'::@pImageCreateInfos@ established by
-- the preceding call to 'setBufferCollectionImageConstraintsFUCHSIA'. The
-- index of the element chosen is stored in and can be retrieved from
-- 'BufferCollectionPropertiesFUCHSIA'::@createInfoIndex@.
--
-- For buffer-based buffer collections, a single
-- 'Vulkan.Core10.Buffer.BufferCreateInfo' is specified as
-- 'BufferConstraintsInfoFUCHSIA'::@createInfo@.
-- 'BufferCollectionPropertiesFUCHSIA'::@createInfoIndex@ will therefore
-- always be zero.
--
-- 'getBufferCollectionPropertiesFUCHSIA' /may/ fail if Sysmem is unable to
-- resolve the constraints of all of the participants in the buffer
-- collection. If that occurs, 'getBufferCollectionPropertiesFUCHSIA' will
-- return 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetBufferCollectionPropertiesFUCHSIA-None-06405# Prior to
--     calling 'getBufferCollectionPropertiesFUCHSIA', the constraints on
--     the buffer collection /must/ have been set by either
--     'setBufferCollectionImageConstraintsFUCHSIA' or
--     'setBufferCollectionBufferConstraintsFUCHSIA'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetBufferCollectionPropertiesFUCHSIA-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetBufferCollectionPropertiesFUCHSIA-collection-parameter#
--     @collection@ /must/ be a valid
--     'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
--
-- -   #VUID-vkGetBufferCollectionPropertiesFUCHSIA-pProperties-parameter#
--     @pProperties@ /must/ be a valid pointer to a
--     'BufferCollectionPropertiesFUCHSIA' structure
--
-- -   #VUID-vkGetBufferCollectionPropertiesFUCHSIA-collection-parent#
--     @collection@ /must/ have been created, allocated, or retrieved from
--     @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'BufferCollectionPropertiesFUCHSIA', 'Vulkan.Core10.Handles.Device'
getBufferCollectionPropertiesFUCHSIA :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device handle
                                        Device
                                     -> -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
                                        -- handle
                                        BufferCollectionFUCHSIA
                                     -> io (BufferCollectionPropertiesFUCHSIA)
getBufferCollectionPropertiesFUCHSIA device collection = liftIO . evalContT $ do
  let vkGetBufferCollectionPropertiesFUCHSIAPtr = pVkGetBufferCollectionPropertiesFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetBufferCollectionPropertiesFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferCollectionPropertiesFUCHSIA is null" Nothing Nothing
  let vkGetBufferCollectionPropertiesFUCHSIA' = mkVkGetBufferCollectionPropertiesFUCHSIA vkGetBufferCollectionPropertiesFUCHSIAPtr
  pPProperties <- ContT (withZeroCStruct @BufferCollectionPropertiesFUCHSIA)
  r <- lift $ traceAroundEvent "vkGetBufferCollectionPropertiesFUCHSIA" (vkGetBufferCollectionPropertiesFUCHSIA' (deviceHandle (device)) (collection) (pPProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @BufferCollectionPropertiesFUCHSIA pPProperties
  pure $ (pProperties)


-- | VkImportMemoryBufferCollectionFUCHSIA - Structure to specify the Sysmem
-- buffer to import
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryBufferCollectionFUCHSIA = ImportMemoryBufferCollectionFUCHSIA
  { -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
    -- handle
    --
    -- #VUID-VkImportMemoryBufferCollectionFUCHSIA-collection-parameter#
    -- @collection@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
    collection :: BufferCollectionFUCHSIA
  , -- | @index@ the index of the buffer to import from @collection@
    --
    -- #VUID-VkImportMemoryBufferCollectionFUCHSIA-index-06406# @index@ /must/
    -- be less than the value retrieved as
    -- 'BufferCollectionPropertiesFUCHSIA':bufferCount
    index :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryBufferCollectionFUCHSIA)
#endif
deriving instance Show ImportMemoryBufferCollectionFUCHSIA

instance ToCStruct ImportMemoryBufferCollectionFUCHSIA where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryBufferCollectionFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (collection)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (index)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImportMemoryBufferCollectionFUCHSIA where
  peekCStruct p = do
    collection <- peek @BufferCollectionFUCHSIA ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA))
    index <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ ImportMemoryBufferCollectionFUCHSIA
             collection index

instance Storable ImportMemoryBufferCollectionFUCHSIA where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryBufferCollectionFUCHSIA where
  zero = ImportMemoryBufferCollectionFUCHSIA
           zero
           zero


-- | VkBufferCollectionImageCreateInfoFUCHSIA - Create a
-- VkBufferCollectionFUCHSIA-compatible VkImage
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferCollectionImageCreateInfoFUCHSIA = BufferCollectionImageCreateInfoFUCHSIA
  { -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
    -- handle
    --
    -- #VUID-VkBufferCollectionImageCreateInfoFUCHSIA-collection-parameter#
    -- @collection@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
    collection :: BufferCollectionFUCHSIA
  , -- | @index@ is the index of the buffer in the buffer collection from which
    -- the memory will be imported
    --
    -- #VUID-VkBufferCollectionImageCreateInfoFUCHSIA-index-06391# @index@
    -- /must/ be less than 'BufferCollectionPropertiesFUCHSIA'::@bufferCount@
    index :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCollectionImageCreateInfoFUCHSIA)
#endif
deriving instance Show BufferCollectionImageCreateInfoFUCHSIA

instance ToCStruct BufferCollectionImageCreateInfoFUCHSIA where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCollectionImageCreateInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (collection)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (index)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct BufferCollectionImageCreateInfoFUCHSIA where
  peekCStruct p = do
    collection <- peek @BufferCollectionFUCHSIA ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA))
    index <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ BufferCollectionImageCreateInfoFUCHSIA
             collection index

instance Storable BufferCollectionImageCreateInfoFUCHSIA where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCollectionImageCreateInfoFUCHSIA where
  zero = BufferCollectionImageCreateInfoFUCHSIA
           zero
           zero


-- | VkBufferCollectionBufferCreateInfoFUCHSIA - Create a
-- VkBufferCollectionFUCHSIA-compatible VkBuffer
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferCollectionBufferCreateInfoFUCHSIA = BufferCollectionBufferCreateInfoFUCHSIA
  { -- | @collection@ is the 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
    -- handle
    --
    -- #VUID-VkBufferCollectionBufferCreateInfoFUCHSIA-collection-parameter#
    -- @collection@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' handle
    collection :: BufferCollectionFUCHSIA
  , -- | @index@ is the index of the buffer in the buffer collection from which
    -- the memory will be imported
    --
    -- #VUID-VkBufferCollectionBufferCreateInfoFUCHSIA-index-06388# @index@
    -- /must/ be less than 'BufferCollectionPropertiesFUCHSIA'::@bufferCount@
    index :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCollectionBufferCreateInfoFUCHSIA)
#endif
deriving instance Show BufferCollectionBufferCreateInfoFUCHSIA

instance ToCStruct BufferCollectionBufferCreateInfoFUCHSIA where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCollectionBufferCreateInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (collection)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (index)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct BufferCollectionBufferCreateInfoFUCHSIA where
  peekCStruct p = do
    collection <- peek @BufferCollectionFUCHSIA ((p `plusPtr` 16 :: Ptr BufferCollectionFUCHSIA))
    index <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ BufferCollectionBufferCreateInfoFUCHSIA
             collection index

instance Storable BufferCollectionBufferCreateInfoFUCHSIA where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCollectionBufferCreateInfoFUCHSIA where
  zero = BufferCollectionBufferCreateInfoFUCHSIA
           zero
           zero


-- | VkBufferCollectionCreateInfoFUCHSIA - Structure specifying desired
-- parameters to create the buffer collection
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createBufferCollectionFUCHSIA'
data BufferCollectionCreateInfoFUCHSIA = BufferCollectionCreateInfoFUCHSIA
  { -- | @collectionToken@ is a @zx_handle_t@ containing the Sysmem client’s
    -- buffer collection token
    --
    -- #VUID-VkBufferCollectionCreateInfoFUCHSIA-collectionToken-06393#
    -- @collectionToken@ /must/ be a valid @zx_handle_t@ to a Zircon channel
    -- allocated from Sysmem
    -- (@fuchsia.sysmem.Allocator@\/AllocateSharedCollection) with
    -- @ZX_DEFAULT_CHANNEL_RIGHTS@ rights
    collectionToken :: Zx_handle_t }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCollectionCreateInfoFUCHSIA)
#endif
deriving instance Show BufferCollectionCreateInfoFUCHSIA

instance ToCStruct BufferCollectionCreateInfoFUCHSIA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCollectionCreateInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Zx_handle_t)) (collectionToken)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Zx_handle_t)) (zero)
    f

instance FromCStruct BufferCollectionCreateInfoFUCHSIA where
  peekCStruct p = do
    collectionToken <- peek @Zx_handle_t ((p `plusPtr` 16 :: Ptr Zx_handle_t))
    pure $ BufferCollectionCreateInfoFUCHSIA
             collectionToken

instance Storable BufferCollectionCreateInfoFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCollectionCreateInfoFUCHSIA where
  zero = BufferCollectionCreateInfoFUCHSIA
           zero


-- | VkBufferCollectionPropertiesFUCHSIA - Structure specifying the
-- negotiated format chosen by Sysmem
--
-- = Description
--
-- @sysmemColorSpace@ is only set for image-based buffer collections where
-- the constraints were specified using 'ImageConstraintsInfoFUCHSIA' in a
-- call to 'setBufferCollectionImageConstraintsFUCHSIA'.
--
-- For image-based buffer collections, @createInfoIndex@ will identify both
-- the 'ImageConstraintsInfoFUCHSIA'::@pImageCreateInfos@ element and the
-- 'ImageConstraintsInfoFUCHSIA'::@pFormatConstraints@ element chosen by
-- Sysmem when 'setBufferCollectionImageConstraintsFUCHSIA' was called. The
-- value of @sysmemColorSpaceIndex@ will be an index to one of the color
-- spaces provided in the
-- 'ImageFormatConstraintsInfoFUCHSIA'::@pColorSpaces@ array.
--
-- The implementation must have @formatFeatures@ with all bits set that
-- were set in
-- 'ImageFormatConstraintsInfoFUCHSIA'::@requiredFormatFeatures@, by the
-- call to 'setBufferCollectionImageConstraintsFUCHSIA', at
-- @createInfoIndex@ (other bits could be set as well).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation',
-- 'Vulkan.Core10.ImageView.ComponentMapping',
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion',
-- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SysmemColorSpaceFUCHSIA', 'getBufferCollectionPropertiesFUCHSIA'
data BufferCollectionPropertiesFUCHSIA = BufferCollectionPropertiesFUCHSIA
  { -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the buffer collection can be imported as buffer collection
    memoryTypeBits :: Word32
  , -- | @bufferCount@ is the number of buffers in the collection
    bufferCount :: Word32
  , -- | @createInfoIndex@ as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#sysmem-chosen-create-infos Sysmem chosen create infos>
    createInfoIndex :: Word32
  , -- | @sysmemPixelFormat@ is the Sysmem @PixelFormatType@ as defined in
    -- @fuchsia.sysmem\/image_formats.fidl@
    sysmemPixelFormat :: Word64
  , -- | @formatFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' shared
    -- by the buffer collection
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-formatFeatures-parameter#
    -- @formatFeatures@ /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' values
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-formatFeatures-requiredbitmask#
    -- @formatFeatures@ /must/ not be @0@
    formatFeatures :: FormatFeatureFlags
  , -- | @sysmemColorSpaceIndex@ is a 'SysmemColorSpaceFUCHSIA' struct specifying
    -- the color space
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-sysmemColorSpaceIndex-parameter#
    -- @sysmemColorSpaceIndex@ /must/ be a valid 'SysmemColorSpaceFUCHSIA'
    -- structure
    sysmemColorSpaceIndex :: SysmemColorSpaceFUCHSIA
  , -- | @samplerYcbcrConversionComponents@ is a
    -- 'Vulkan.Core10.ImageView.ComponentMapping' struct specifying the
    -- component mapping
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-samplerYcbcrConversionComponents-parameter#
    -- @samplerYcbcrConversionComponents@ /must/ be a valid
    -- 'Vulkan.Core10.ImageView.ComponentMapping' structure
    samplerYcbcrConversionComponents :: ComponentMapping
  , -- | @suggestedYcbcrModel@ is a
    -- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion'
    -- value specifying the suggested Y′CBCR model
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-suggestedYcbcrModel-parameter#
    -- @suggestedYcbcrModel@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion'
    -- value
    suggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- | @suggestedYcbcrRange@ is a
    -- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange' value
    -- specifying the suggested Y′CBCR range
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-suggestedYcbcrRange-parameter#
    -- @suggestedYcbcrRange@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange' value
    suggestedYcbcrRange :: SamplerYcbcrRange
  , -- | @suggestedXChromaOffset@ is a
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value specifying the
    -- suggested X chroma offset
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-suggestedXChromaOffset-parameter#
    -- @suggestedXChromaOffset@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
    suggestedXChromaOffset :: ChromaLocation
  , -- | @suggestedYChromaOffset@ is a
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value specifying the
    -- suggested Y chroma offset
    --
    -- #VUID-VkBufferCollectionPropertiesFUCHSIA-suggestedYChromaOffset-parameter#
    -- @suggestedYChromaOffset@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation' value
    suggestedYChromaOffset :: ChromaLocation
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCollectionPropertiesFUCHSIA)
#endif
deriving instance Show BufferCollectionPropertiesFUCHSIA

instance ToCStruct BufferCollectionPropertiesFUCHSIA where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCollectionPropertiesFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (bufferCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (createInfoIndex)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (sysmemPixelFormat)
    poke ((p `plusPtr` 40 :: Ptr FormatFeatureFlags)) (formatFeatures)
    poke ((p `plusPtr` 48 :: Ptr SysmemColorSpaceFUCHSIA)) (sysmemColorSpaceIndex)
    poke ((p `plusPtr` 72 :: Ptr ComponentMapping)) (samplerYcbcrConversionComponents)
    poke ((p `plusPtr` 88 :: Ptr SamplerYcbcrModelConversion)) (suggestedYcbcrModel)
    poke ((p `plusPtr` 92 :: Ptr SamplerYcbcrRange)) (suggestedYcbcrRange)
    poke ((p `plusPtr` 96 :: Ptr ChromaLocation)) (suggestedXChromaOffset)
    poke ((p `plusPtr` 100 :: Ptr ChromaLocation)) (suggestedYChromaOffset)
    f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr FormatFeatureFlags)) (zero)
    poke ((p `plusPtr` 48 :: Ptr SysmemColorSpaceFUCHSIA)) (zero)
    poke ((p `plusPtr` 72 :: Ptr ComponentMapping)) (zero)
    poke ((p `plusPtr` 88 :: Ptr SamplerYcbcrModelConversion)) (zero)
    poke ((p `plusPtr` 92 :: Ptr SamplerYcbcrRange)) (zero)
    poke ((p `plusPtr` 96 :: Ptr ChromaLocation)) (zero)
    poke ((p `plusPtr` 100 :: Ptr ChromaLocation)) (zero)
    f

instance FromCStruct BufferCollectionPropertiesFUCHSIA where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    bufferCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    createInfoIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    sysmemPixelFormat <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    formatFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 40 :: Ptr FormatFeatureFlags))
    sysmemColorSpaceIndex <- peekCStruct @SysmemColorSpaceFUCHSIA ((p `plusPtr` 48 :: Ptr SysmemColorSpaceFUCHSIA))
    samplerYcbcrConversionComponents <- peekCStruct @ComponentMapping ((p `plusPtr` 72 :: Ptr ComponentMapping))
    suggestedYcbcrModel <- peek @SamplerYcbcrModelConversion ((p `plusPtr` 88 :: Ptr SamplerYcbcrModelConversion))
    suggestedYcbcrRange <- peek @SamplerYcbcrRange ((p `plusPtr` 92 :: Ptr SamplerYcbcrRange))
    suggestedXChromaOffset <- peek @ChromaLocation ((p `plusPtr` 96 :: Ptr ChromaLocation))
    suggestedYChromaOffset <- peek @ChromaLocation ((p `plusPtr` 100 :: Ptr ChromaLocation))
    pure $ BufferCollectionPropertiesFUCHSIA
             memoryTypeBits bufferCount createInfoIndex sysmemPixelFormat formatFeatures sysmemColorSpaceIndex samplerYcbcrConversionComponents suggestedYcbcrModel suggestedYcbcrRange suggestedXChromaOffset suggestedYChromaOffset

instance Storable BufferCollectionPropertiesFUCHSIA where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCollectionPropertiesFUCHSIA where
  zero = BufferCollectionPropertiesFUCHSIA
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkBufferConstraintsInfoFUCHSIA - Structure buffer-based buffer
-- collection constraints
--
-- == Valid Usage
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-requiredFormatFeatures-06404#
--     The @requiredFormatFeatures@ bitmask of
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
--     /must/ be chosen from among the buffer compatible format features
--     listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#buffer-compatible-format-features buffer compatible format features>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA'
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-createInfo-parameter#
--     @createInfo@ /must/ be a valid
--     'Vulkan.Core10.Buffer.BufferCreateInfo' structure
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-requiredFormatFeatures-parameter#
--     @requiredFormatFeatures@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
--     values
--
-- -   #VUID-VkBufferConstraintsInfoFUCHSIA-bufferCollectionConstraints-parameter#
--     @bufferCollectionConstraints@ /must/ be a valid
--     'BufferCollectionConstraintsInfoFUCHSIA' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'BufferCollectionConstraintsInfoFUCHSIA',
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setBufferCollectionBufferConstraintsFUCHSIA'
data BufferConstraintsInfoFUCHSIA = BufferConstraintsInfoFUCHSIA
  { -- No documentation found for Nested "VkBufferConstraintsInfoFUCHSIA" "createInfo"
    createInfo :: SomeStruct BufferCreateInfo
  , -- | @requiredFormatFeatures@ bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- required features of the buffers in the buffer collection
    requiredFormatFeatures :: FormatFeatureFlags
  , -- | @bufferCollectionConstraints@ is used to supply parameters for the
    -- negotiation and allocation of the buffer collection
    bufferCollectionConstraints :: BufferCollectionConstraintsInfoFUCHSIA
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferConstraintsInfoFUCHSIA)
#endif
deriving instance Show BufferConstraintsInfoFUCHSIA

instance ToCStruct BufferConstraintsInfoFUCHSIA where
  withCStruct x f = allocaBytes 120 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferConstraintsInfoFUCHSIA{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (BufferCreateInfo _)))) (createInfo) . ($ ())
    lift $ poke ((p `plusPtr` 72 :: Ptr FormatFeatureFlags)) (requiredFormatFeatures)
    lift $ poke ((p `plusPtr` 80 :: Ptr BufferCollectionConstraintsInfoFUCHSIA)) (bufferCollectionConstraints)
    lift $ f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (BufferCreateInfo _)))) ((SomeStruct zero)) . ($ ())
    lift $ poke ((p `plusPtr` 80 :: Ptr BufferCollectionConstraintsInfoFUCHSIA)) (zero)
    lift $ f

instance FromCStruct BufferConstraintsInfoFUCHSIA where
  peekCStruct p = do
    createInfo <- peekSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (BufferCreateInfo _))))
    requiredFormatFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 72 :: Ptr FormatFeatureFlags))
    bufferCollectionConstraints <- peekCStruct @BufferCollectionConstraintsInfoFUCHSIA ((p `plusPtr` 80 :: Ptr BufferCollectionConstraintsInfoFUCHSIA))
    pure $ BufferConstraintsInfoFUCHSIA
             createInfo requiredFormatFeatures bufferCollectionConstraints

instance Zero BufferConstraintsInfoFUCHSIA where
  zero = BufferConstraintsInfoFUCHSIA
           (SomeStruct zero)
           zero
           zero


-- | VkSysmemColorSpaceFUCHSIA - Structure describing the buffer collections
-- color space
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'BufferCollectionPropertiesFUCHSIA',
-- 'ImageFormatConstraintsInfoFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SysmemColorSpaceFUCHSIA = SysmemColorSpaceFUCHSIA
  { -- | @colorSpace@ value of the Sysmem @ColorSpaceType@
    --
    -- #VUID-VkSysmemColorSpaceFUCHSIA-colorSpace-06402# @colorSpace@ /must/ be
    -- a @ColorSpaceType@ as defined in @fuchsia.sysmem\/image_formats.fidl@
    colorSpace :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SysmemColorSpaceFUCHSIA)
#endif
deriving instance Show SysmemColorSpaceFUCHSIA

instance ToCStruct SysmemColorSpaceFUCHSIA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SysmemColorSpaceFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (colorSpace)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SysmemColorSpaceFUCHSIA where
  peekCStruct p = do
    colorSpace <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SysmemColorSpaceFUCHSIA
             colorSpace

instance Storable SysmemColorSpaceFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SysmemColorSpaceFUCHSIA where
  zero = SysmemColorSpaceFUCHSIA
           zero


-- | VkImageFormatConstraintsInfoFUCHSIA - Structure image-based buffer
-- collection constraints
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'ImageConstraintsInfoFUCHSIA', 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'ImageFormatConstraintsFlagsFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'SysmemColorSpaceFUCHSIA'
data ImageFormatConstraintsInfoFUCHSIA = ImageFormatConstraintsInfoFUCHSIA
  { -- | @imageCreateInfo@ is the 'Vulkan.Core10.Image.ImageCreateInfo' used to
    -- create a 'Vulkan.Core10.Handles.Image' that is to use memory from the
    -- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
    --
    -- #VUID-VkImageFormatConstraintsInfoFUCHSIA-imageCreateInfo-parameter#
    -- @imageCreateInfo@ /must/ be a valid
    -- 'Vulkan.Core10.Image.ImageCreateInfo' structure
    imageCreateInfo :: SomeStruct ImageCreateInfo
  , -- | @requiredFormatFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- specifying required features of the buffers in the buffer collection
    --
    -- #VUID-VkImageFormatConstraintsInfoFUCHSIA-requiredFormatFeatures-parameter#
    -- @requiredFormatFeatures@ /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' values
    --
    -- #VUID-VkImageFormatConstraintsInfoFUCHSIA-requiredFormatFeatures-requiredbitmask#
    -- @requiredFormatFeatures@ /must/ not be @0@
    requiredFormatFeatures :: FormatFeatureFlags
  , -- | @flags@ is reserved for future use
    --
    -- #VUID-VkImageFormatConstraintsInfoFUCHSIA-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: ImageFormatConstraintsFlagsFUCHSIA
  , -- | @sysmemPixelFormat@ is a @PixelFormatType@ value from the
    -- @fuchsia.sysmem\/image_formats.fidl@ FIDL interface
    sysmemPixelFormat :: Word64
  , -- | @colorSpaceCount@ the element count of @pColorSpaces@
    colorSpaceCount :: Word32
  , -- | @pColorSpaces@ is a pointer to an array of 'SysmemColorSpaceFUCHSIA'
    -- structs of size @colorSpaceCount@
    --
    -- #VUID-VkImageFormatConstraintsInfoFUCHSIA-pColorSpaces-parameter#
    -- @pColorSpaces@ /must/ be a valid pointer to a valid
    -- 'SysmemColorSpaceFUCHSIA' structure
    colorSpaces :: SysmemColorSpaceFUCHSIA
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageFormatConstraintsInfoFUCHSIA)
#endif
deriving instance Show ImageFormatConstraintsInfoFUCHSIA

instance ToCStruct ImageFormatConstraintsInfoFUCHSIA where
  withCStruct x f = allocaBytes 136 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageFormatConstraintsInfoFUCHSIA{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (ImageCreateInfo _)))) (imageCreateInfo) . ($ ())
    lift $ poke ((p `plusPtr` 104 :: Ptr FormatFeatureFlags)) (requiredFormatFeatures)
    lift $ poke ((p `plusPtr` 108 :: Ptr ImageFormatConstraintsFlagsFUCHSIA)) (flags)
    lift $ poke ((p `plusPtr` 112 :: Ptr Word64)) (sysmemPixelFormat)
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (colorSpaceCount)
    pColorSpaces'' <- ContT $ withCStruct (colorSpaces)
    lift $ poke ((p `plusPtr` 128 :: Ptr (Ptr SysmemColorSpaceFUCHSIA))) pColorSpaces''
    lift $ f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (ImageCreateInfo _)))) ((SomeStruct zero)) . ($ ())
    lift $ poke ((p `plusPtr` 104 :: Ptr FormatFeatureFlags)) (zero)
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (zero)
    pColorSpaces'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 128 :: Ptr (Ptr SysmemColorSpaceFUCHSIA))) pColorSpaces''
    lift $ f

instance FromCStruct ImageFormatConstraintsInfoFUCHSIA where
  peekCStruct p = do
    imageCreateInfo <- peekSomeCStruct (forgetExtensions ((p `plusPtr` 16 :: Ptr (ImageCreateInfo _))))
    requiredFormatFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 104 :: Ptr FormatFeatureFlags))
    flags <- peek @ImageFormatConstraintsFlagsFUCHSIA ((p `plusPtr` 108 :: Ptr ImageFormatConstraintsFlagsFUCHSIA))
    sysmemPixelFormat <- peek @Word64 ((p `plusPtr` 112 :: Ptr Word64))
    colorSpaceCount <- peek @Word32 ((p `plusPtr` 120 :: Ptr Word32))
    pColorSpaces <- peekCStruct @SysmemColorSpaceFUCHSIA =<< peek ((p `plusPtr` 128 :: Ptr (Ptr SysmemColorSpaceFUCHSIA)))
    pure $ ImageFormatConstraintsInfoFUCHSIA
             imageCreateInfo requiredFormatFeatures flags sysmemPixelFormat colorSpaceCount pColorSpaces

instance Zero ImageFormatConstraintsInfoFUCHSIA where
  zero = ImageFormatConstraintsInfoFUCHSIA
           (SomeStruct zero)
           zero
           zero
           zero
           zero
           zero


-- | VkImageConstraintsInfoFUCHSIA - Structure of image-based buffer
-- collection constraints
--
-- == Valid Usage
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06395# All
--     elements of @pFormatConstraints@ /must/ have at least one bit set in
--     its 'ImageFormatConstraintsInfoFUCHSIA'::@requiredFormatFeatures@
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06396# If
--     @pFormatConstraints@::@imageCreateInfo@::@usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06397# If
--     @pFormatConstraints@::@imageCreateInfo@::@usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_BIT'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06398# If
--     @pFormatConstraints@::@imageCreateInfo@::@usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06399# If
--     @pFormatConstraints@::@imageCreateInfo@::@usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-06400# If
--     @pFormatConstraints@::@imageCreateInfo@::@usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     at least one of
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-attachmentFragmentShadingRate-06401#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is enabled, and @pFormatConstraints@::@imageCreateInfo@::@usage@
--     contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     then @pFormatConstraints@::@requiredFormatFeatures@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA'
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-pFormatConstraints-parameter#
--     @pFormatConstraints@ /must/ be a valid pointer to an array of
--     @formatConstraintsCount@ valid 'ImageFormatConstraintsInfoFUCHSIA'
--     structures
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-bufferCollectionConstraints-parameter#
--     @bufferCollectionConstraints@ /must/ be a valid
--     'BufferCollectionConstraintsInfoFUCHSIA' structure
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-flags-parameter# @flags@ /must/
--     be a valid combination of 'ImageConstraintsInfoFlagBitsFUCHSIA'
--     values
--
-- -   #VUID-VkImageConstraintsInfoFUCHSIA-formatConstraintsCount-arraylength#
--     @formatConstraintsCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'BufferCollectionConstraintsInfoFUCHSIA',
-- 'ImageConstraintsInfoFlagsFUCHSIA', 'ImageFormatConstraintsInfoFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setBufferCollectionImageConstraintsFUCHSIA'
data ImageConstraintsInfoFUCHSIA = ImageConstraintsInfoFUCHSIA
  { -- | @pFormatConstraints@ is a pointer to an array of
    -- 'ImageFormatConstraintsInfoFUCHSIA' structures of size
    -- @formatConstraintsCount@ that is used to further constrain buffer
    -- collection format selection for image-based buffer collections.
    formatConstraints :: Vector ImageFormatConstraintsInfoFUCHSIA
  , -- | @bufferCollectionConstraints@ is a
    -- 'BufferCollectionConstraintsInfoFUCHSIA' structure used to supply
    -- parameters for the negotiation and allocation for buffer-based buffer
    -- collections.
    bufferCollectionConstraints :: BufferCollectionConstraintsInfoFUCHSIA
  , -- | @flags@ is a 'ImageConstraintsInfoFlagBitsFUCHSIA' value specifying
    -- hints about the type of memory Sysmem should allocate for the buffer
    -- collection.
    flags :: ImageConstraintsInfoFlagsFUCHSIA
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageConstraintsInfoFUCHSIA)
#endif
deriving instance Show ImageConstraintsInfoFUCHSIA

instance ToCStruct ImageConstraintsInfoFUCHSIA where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageConstraintsInfoFUCHSIA{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (formatConstraints)) :: Word32))
    pPFormatConstraints' <- ContT $ allocaBytes @ImageFormatConstraintsInfoFUCHSIA ((Data.Vector.length (formatConstraints)) * 136)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPFormatConstraints' `plusPtr` (136 * (i)) :: Ptr ImageFormatConstraintsInfoFUCHSIA) (e) . ($ ())) (formatConstraints)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageFormatConstraintsInfoFUCHSIA))) (pPFormatConstraints')
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferCollectionConstraintsInfoFUCHSIA)) (bufferCollectionConstraints)
    lift $ poke ((p `plusPtr` 72 :: Ptr ImageConstraintsInfoFlagsFUCHSIA)) (flags)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr BufferCollectionConstraintsInfoFUCHSIA)) (zero)
    f

instance FromCStruct ImageConstraintsInfoFUCHSIA where
  peekCStruct p = do
    formatConstraintsCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pFormatConstraints <- peek @(Ptr ImageFormatConstraintsInfoFUCHSIA) ((p `plusPtr` 24 :: Ptr (Ptr ImageFormatConstraintsInfoFUCHSIA)))
    pFormatConstraints' <- generateM (fromIntegral formatConstraintsCount) (\i -> peekCStruct @ImageFormatConstraintsInfoFUCHSIA ((pFormatConstraints `advancePtrBytes` (136 * (i)) :: Ptr ImageFormatConstraintsInfoFUCHSIA)))
    bufferCollectionConstraints <- peekCStruct @BufferCollectionConstraintsInfoFUCHSIA ((p `plusPtr` 32 :: Ptr BufferCollectionConstraintsInfoFUCHSIA))
    flags <- peek @ImageConstraintsInfoFlagsFUCHSIA ((p `plusPtr` 72 :: Ptr ImageConstraintsInfoFlagsFUCHSIA))
    pure $ ImageConstraintsInfoFUCHSIA
             pFormatConstraints' bufferCollectionConstraints flags

instance Zero ImageConstraintsInfoFUCHSIA where
  zero = ImageConstraintsInfoFUCHSIA
           mempty
           zero
           zero


-- | VkBufferCollectionConstraintsInfoFUCHSIA - Structure of general buffer
-- collection constraints
--
-- = Description
--
-- Sysmem uses all buffer count parameters in combination to determine the
-- number of buffers it will allocate. Sysmem defines buffer count
-- constraints in @fuchsia.sysmem\/constraints.fidl@.
--
-- /Camping/ as referred to by @minBufferCountForCamping@, is the number of
-- buffers that should be available for the participant that are not for
-- transient use. This number of buffers is required for the participant to
-- logically operate.
--
-- /Slack/ as referred to by @minBufferCountForDedicatedSlack@ and
-- @minBufferCountForSharedSlack@, refers to the number of buffers desired
-- by participants for optimal performance.
-- @minBufferCountForDedicatedSlack@ refers to the current participant.
-- @minBufferCountForSharedSlack@ refers to buffer slack for all
-- participants in the collection.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'BufferConstraintsInfoFUCHSIA', 'ImageConstraintsInfoFUCHSIA',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferCollectionConstraintsInfoFUCHSIA = BufferCollectionConstraintsInfoFUCHSIA
  { -- | @minBufferCount@ is the minimum number of buffers available in the
    -- collection
    minBufferCount :: Word32
  , -- | @maxBufferCount@ is the maximum number of buffers allowed in the
    -- collection
    maxBufferCount :: Word32
  , -- | @minBufferCountForCamping@ is the per-participant minimum buffers for
    -- camping
    minBufferCountForCamping :: Word32
  , -- | @minBufferCountForDedicatedSlack@ is the per-participant minimum buffers
    -- for dedicated slack
    minBufferCountForDedicatedSlack :: Word32
  , -- | @minBufferCountForSharedSlack@ is the per-participant minimum buffers
    -- for shared slack
    minBufferCountForSharedSlack :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCollectionConstraintsInfoFUCHSIA)
#endif
deriving instance Show BufferCollectionConstraintsInfoFUCHSIA

instance ToCStruct BufferCollectionConstraintsInfoFUCHSIA where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCollectionConstraintsInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minBufferCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxBufferCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (minBufferCountForCamping)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (minBufferCountForDedicatedSlack)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (minBufferCountForSharedSlack)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct BufferCollectionConstraintsInfoFUCHSIA where
  peekCStruct p = do
    minBufferCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxBufferCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    minBufferCountForCamping <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    minBufferCountForDedicatedSlack <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    minBufferCountForSharedSlack <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ BufferCollectionConstraintsInfoFUCHSIA
             minBufferCount maxBufferCount minBufferCountForCamping minBufferCountForDedicatedSlack minBufferCountForSharedSlack

instance Storable BufferCollectionConstraintsInfoFUCHSIA where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCollectionConstraintsInfoFUCHSIA where
  zero = BufferCollectionConstraintsInfoFUCHSIA
           zero
           zero
           zero
           zero
           zero


-- | VkImageFormatConstraintsFlagsFUCHSIA - Reserved for future use
--
-- = Description
--
-- 'ImageFormatConstraintsFlagsFUCHSIA' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'ImageFormatConstraintsInfoFUCHSIA'
newtype ImageFormatConstraintsFlagsFUCHSIA = ImageFormatConstraintsFlagsFUCHSIA Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameImageFormatConstraintsFlagsFUCHSIA :: String
conNameImageFormatConstraintsFlagsFUCHSIA = "ImageFormatConstraintsFlagsFUCHSIA"

enumPrefixImageFormatConstraintsFlagsFUCHSIA :: String
enumPrefixImageFormatConstraintsFlagsFUCHSIA = ""

showTableImageFormatConstraintsFlagsFUCHSIA :: [(ImageFormatConstraintsFlagsFUCHSIA, String)]
showTableImageFormatConstraintsFlagsFUCHSIA = []

instance Show ImageFormatConstraintsFlagsFUCHSIA where
  showsPrec = enumShowsPrec enumPrefixImageFormatConstraintsFlagsFUCHSIA
                            showTableImageFormatConstraintsFlagsFUCHSIA
                            conNameImageFormatConstraintsFlagsFUCHSIA
                            (\(ImageFormatConstraintsFlagsFUCHSIA x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ImageFormatConstraintsFlagsFUCHSIA where
  readPrec = enumReadPrec enumPrefixImageFormatConstraintsFlagsFUCHSIA
                          showTableImageFormatConstraintsFlagsFUCHSIA
                          conNameImageFormatConstraintsFlagsFUCHSIA
                          ImageFormatConstraintsFlagsFUCHSIA


type ImageConstraintsInfoFlagsFUCHSIA = ImageConstraintsInfoFlagBitsFUCHSIA

-- | VkImageConstraintsInfoFlagBitsFUCHSIA - Bitmask specifying image
-- constraints flags
--
-- = Description
--
-- General hints about the type of memory that should be allocated by
-- Sysmem based on the expected usage of the images in the buffer
-- collection include:
--
-- For protected memory:
--
-- Note that if all participants in the buffer collection (Vulkan or
-- otherwise) specify that protected memory is optional, Sysmem will not
-- allocate protected memory.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection VK_FUCHSIA_buffer_collection>,
-- 'ImageConstraintsInfoFlagsFUCHSIA'
newtype ImageConstraintsInfoFlagBitsFUCHSIA = ImageConstraintsInfoFlagBitsFUCHSIA Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_CONSTRAINTS_INFO_CPU_READ_RARELY_FUCHSIA'
pattern IMAGE_CONSTRAINTS_INFO_CPU_READ_RARELY_FUCHSIA    = ImageConstraintsInfoFlagBitsFUCHSIA 0x00000001
-- | 'IMAGE_CONSTRAINTS_INFO_CPU_READ_OFTEN_FUCHSIA'
pattern IMAGE_CONSTRAINTS_INFO_CPU_READ_OFTEN_FUCHSIA     = ImageConstraintsInfoFlagBitsFUCHSIA 0x00000002
-- | 'IMAGE_CONSTRAINTS_INFO_CPU_WRITE_RARELY_FUCHSIA'
pattern IMAGE_CONSTRAINTS_INFO_CPU_WRITE_RARELY_FUCHSIA   = ImageConstraintsInfoFlagBitsFUCHSIA 0x00000004
-- | 'IMAGE_CONSTRAINTS_INFO_CPU_WRITE_OFTEN_FUCHSIA'
pattern IMAGE_CONSTRAINTS_INFO_CPU_WRITE_OFTEN_FUCHSIA    = ImageConstraintsInfoFlagBitsFUCHSIA 0x00000008
-- | 'IMAGE_CONSTRAINTS_INFO_PROTECTED_OPTIONAL_FUCHSIA' specifies that
-- protected memory is optional for the buffer collection.
pattern IMAGE_CONSTRAINTS_INFO_PROTECTED_OPTIONAL_FUCHSIA = ImageConstraintsInfoFlagBitsFUCHSIA 0x00000010

conNameImageConstraintsInfoFlagBitsFUCHSIA :: String
conNameImageConstraintsInfoFlagBitsFUCHSIA = "ImageConstraintsInfoFlagBitsFUCHSIA"

enumPrefixImageConstraintsInfoFlagBitsFUCHSIA :: String
enumPrefixImageConstraintsInfoFlagBitsFUCHSIA = "IMAGE_CONSTRAINTS_INFO_"

showTableImageConstraintsInfoFlagBitsFUCHSIA :: [(ImageConstraintsInfoFlagBitsFUCHSIA, String)]
showTableImageConstraintsInfoFlagBitsFUCHSIA =
  [ (IMAGE_CONSTRAINTS_INFO_CPU_READ_RARELY_FUCHSIA   , "CPU_READ_RARELY_FUCHSIA")
  , (IMAGE_CONSTRAINTS_INFO_CPU_READ_OFTEN_FUCHSIA    , "CPU_READ_OFTEN_FUCHSIA")
  , (IMAGE_CONSTRAINTS_INFO_CPU_WRITE_RARELY_FUCHSIA  , "CPU_WRITE_RARELY_FUCHSIA")
  , (IMAGE_CONSTRAINTS_INFO_CPU_WRITE_OFTEN_FUCHSIA   , "CPU_WRITE_OFTEN_FUCHSIA")
  , (IMAGE_CONSTRAINTS_INFO_PROTECTED_OPTIONAL_FUCHSIA, "PROTECTED_OPTIONAL_FUCHSIA")
  ]

instance Show ImageConstraintsInfoFlagBitsFUCHSIA where
  showsPrec = enumShowsPrec enumPrefixImageConstraintsInfoFlagBitsFUCHSIA
                            showTableImageConstraintsInfoFlagBitsFUCHSIA
                            conNameImageConstraintsInfoFlagBitsFUCHSIA
                            (\(ImageConstraintsInfoFlagBitsFUCHSIA x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ImageConstraintsInfoFlagBitsFUCHSIA where
  readPrec = enumReadPrec enumPrefixImageConstraintsInfoFlagBitsFUCHSIA
                          showTableImageConstraintsInfoFlagBitsFUCHSIA
                          conNameImageConstraintsInfoFlagBitsFUCHSIA
                          ImageConstraintsInfoFlagBitsFUCHSIA


type FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION"
pattern FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION :: forall a . Integral a => a
pattern FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION = 2


type FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME = "VK_FUCHSIA_buffer_collection"

-- No documentation found for TopLevel "VK_FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME"
pattern FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME = "VK_FUCHSIA_buffer_collection"

