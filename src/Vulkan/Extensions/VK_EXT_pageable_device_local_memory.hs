{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pageable_device_local_memory - device extension
--
-- == VK_EXT_pageable_device_local_memory
--
-- [__Name String__]
--     @VK_EXT_pageable_device_local_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     413
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_priority VK_EXT_memory_priority>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pageable_device_local_memory] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_pageable_device_local_memory extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-08-24
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Axel Gneiting, id Software
--
--     -   Billy Khan, id Software
--
--     -   Daniel Koch, NVIDIA
--
--     -   Chris Lentini, NVIDIA
--
--     -   Joshua Schnarr, NVIDIA
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- Vulkan is frequently implemented on multi-user and multi-process
-- operating systems where the device-local memory can be shared by more
-- than one process. On such systems the size of the device-local memory
-- available to the application may not be the full size of the memory heap
-- at all times. In order for these operating systems to support multiple
-- applications the device-local memory is virtualized and paging is used
-- to move memory between device-local and host-local memory heaps,
-- transparent to the application.
--
-- The current Vulkan specification does not expose this behavior well and
-- may cause applications to make suboptimal memory choices when allocating
-- memory. For example, in a system with multiple applications running, the
-- application may think that device-local memory is full and revert to
-- making performance-sensitive allocations from host-local memory. In
-- reality the memory heap might not have been full, it just appeared to be
-- at the time memory consumption was queried, and a device-local
-- allocation would have succeeded. A well designed operating system that
-- implements pageable device-local memory will try to have all memory
-- allocations for the foreground application paged into device-local
-- memory, and paged out for other applications as needed when not in use.
--
-- When this extension is exposed by the Vulkan implementation it indicates
-- to the application that the operating system implements pageable
-- device-local memory and the application should adjust its memory
-- allocation strategy accordingly. The extension also exposes a new
-- 'setDeviceMemoryPriorityEXT' function to allow the application to
-- dynamically adjust the priority of existing memory allocations based on
-- its current needs. This will help the operating system page out lower
-- priority memory allocations before higher priority allocations when
-- needed. It will also help the operating system decide which memory
-- allocations to page back into device-local memory first.
--
-- To take best advantage of pageable device-local memory the application
-- must create the Vulkan device with the
-- 'PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT'::@pageableDeviceLocalMemory@
-- feature enabled. When enabled the Vulkan implementation will allow
-- device-local memory allocations to be paged in and out by the operating
-- system, and /may/ not return VK_ERROR_OUT_OF_DEVICE_MEMORY even if
-- device-local memory appears to be full, but will instead page this, or
-- other allocations, out to make room. The Vulkan implementation will also
-- ensure that host-local memory allocations will never be promoted to
-- device-local memory by the operating system, or consume device-local
-- memory.
--
-- == New Commands
--
-- -   'setDeviceMemoryPriorityEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME'
--
-- -   'EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-08-24 (Piers Daniell)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT',
-- 'setDeviceMemoryPriorityEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pageable_device_local_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pageable_device_local_memory  ( setDeviceMemoryPriorityEXT
                                                              , PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT(..)
                                                              , EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION
                                                              , pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION
                                                              , EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME
                                                              , pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME
                                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Foreign.C.Types (CFloat(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkSetDeviceMemoryPriorityEXT))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDeviceMemoryPriorityEXT
  :: FunPtr (Ptr Device_T -> DeviceMemory -> CFloat -> IO ()) -> Ptr Device_T -> DeviceMemory -> CFloat -> IO ()

-- | vkSetDeviceMemoryPriorityEXT - Change a memory allocation priority
--
-- = Description
--
-- Memory allocations with higher priority /may/ be more likely to stay in
-- device-local memory when the system is under memory pressure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pageable_device_local_memory VK_EXT_pageable_device_local_memory>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.DeviceMemory'
setDeviceMemoryPriorityEXT :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device that owns the memory.
                              --
                              -- #VUID-vkSetDeviceMemoryPriorityEXT-device-parameter# @device@ /must/ be
                              -- a valid 'Vulkan.Core10.Handles.Device' handle
                              Device
                           -> -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object to which the
                              -- new priority will be applied.
                              --
                              -- #VUID-vkSetDeviceMemoryPriorityEXT-memory-parameter# @memory@ /must/ be
                              -- a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
                              --
                              -- #VUID-vkSetDeviceMemoryPriorityEXT-memory-parent# @memory@ /must/ have
                              -- been created, allocated, or retrieved from @device@
                              DeviceMemory
                           -> -- | @priority@ is a floating-point value between @0@ and @1@, indicating the
                              -- priority of the allocation relative to other memory allocations. Larger
                              -- values are higher priority. The granularity of the priorities is
                              -- implementation-dependent.
                              --
                              -- #VUID-vkSetDeviceMemoryPriorityEXT-priority-06258# @priority@ /must/ be
                              -- between @0@ and @1@, inclusive
                              ("priority" ::: Float)
                           -> io ()
setDeviceMemoryPriorityEXT device memory priority = liftIO $ do
  let vkSetDeviceMemoryPriorityEXTPtr = pVkSetDeviceMemoryPriorityEXT (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkSetDeviceMemoryPriorityEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetDeviceMemoryPriorityEXT is null" Nothing Nothing
  let vkSetDeviceMemoryPriorityEXT' = mkVkSetDeviceMemoryPriorityEXT vkSetDeviceMemoryPriorityEXTPtr
  traceAroundEvent "vkSetDeviceMemoryPriorityEXT" (vkSetDeviceMemoryPriorityEXT'
                                                     (deviceHandle (device))
                                                     (memory)
                                                     (CFloat (priority)))
  pure $ ()


-- | VkPhysicalDevicePageableDeviceLocalMemoryFeaturesEXT - Structure
-- describing whether the implementation supports pageable device-local
-- memory
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pageable_device_local_memory VK_EXT_pageable_device_local_memory>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT = PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT
  { -- | #features-pageableDeviceLocalMemory# @pageableDeviceLocalMemory@
    -- indicates that the implementation supports pageable device-local memory
    -- and /may/ transparently move device-local memory allocations to
    -- host-local memory to better share device-local memory with other
    -- applications.
    pageableDeviceLocalMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT

instance ToCStruct PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pageableDeviceLocalMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT where
  peekCStruct p = do
    pageableDeviceLocalMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT
             (bool32ToBool pageableDeviceLocalMemory)

instance Storable PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT where
  zero = PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT
           zero


type EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION"
pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_SPEC_VERSION = 1


type EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME = "VK_EXT_pageable_device_local_memory"

-- No documentation found for TopLevel "VK_EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME"
pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PAGEABLE_DEVICE_LOCAL_MEMORY_EXTENSION_NAME = "VK_EXT_pageable_device_local_memory"

