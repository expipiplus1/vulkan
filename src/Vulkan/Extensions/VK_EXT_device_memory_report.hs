{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_memory_report - device extension
--
-- == VK_EXT_device_memory_report
--
-- [__Name String__]
--     @VK_EXT_device_memory_report@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     285
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Yiwei Zhang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_device_memory_report:%20&body=@zhangyiwei%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-01-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Yiwei Zhang, Google
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- This device extension allows registration of device memory event
-- callbacks upon device creation, so that applications or middleware can
-- obtain detailed information about memory usage and how memory is
-- associated with Vulkan objects. This extension exposes the actual
-- underlying device memory usage, including allocations that are not
-- normally visible to the application, such as memory consumed by
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines'. It is intended
-- primarily for use by debug tooling rather than for production
-- applications.
--
-- == New Structures
--
-- -   'DeviceMemoryReportCallbackDataEXT'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceDeviceMemoryReportCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceMemoryReportFeaturesEXT'
--
-- == New Function Pointers
--
-- -   'PFN_vkDeviceMemoryReportCallbackEXT'
--
-- == New Enums
--
-- -   'DeviceMemoryReportEventTypeEXT'
--
-- == New Bitmasks
--
-- -   'DeviceMemoryReportFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME'
--
-- -   'EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should this be better expressed as an extension to VK_EXT_debug_utils
-- and its general-purpose messenger construct?
--
-- __RESOLVED__: No. The intended lifecycle is quite different. We want to
-- make this extension tied to the device’s lifecycle. Each ICD just
-- handles its own implementation of this extension, and this extension
-- will only be directly exposed from the ICD. So we can avoid the extra
-- implementation complexity used to accommodate the flexibility of
-- @VK_EXT_debug_utils@ extension.
--
-- 2) Can we extend and use the existing internal allocation callbacks
-- instead of adding the new callback structure in this extension?
--
-- __RESOLVED__: No. Our memory reporting layer that combines this
-- information with other memory information it collects directly (e.g.
-- bindings of resources to 'Vulkan.Core10.Handles.DeviceMemory') would
-- have to intercept all entry points that take a
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' parameter and
-- inject its own @pfnInternalAllocation@ and @pfnInternalFree@. That’s
-- maybe doable for the extensions we know about, but not for ones we
-- don’t. The proposal would work fine in the face of most unknown
-- extensions. But even for ones we know about, since apps can provide a
-- different set of callbacks and userdata and those can be retained by the
-- driver and used later (esp. for pool object, but not just those), we’d
-- have to dynamically allocate the interception trampoline every time.
-- That’s getting to be an unreasonably large amount of complexity and
-- (possibly) overhead.
--
-- We’re interested in both alloc\/free and import\/unimport. The latter is
-- fairly important for tracking (and avoiding double-counting) of
-- swapchain images (still true with “native swapchains” based on external
-- memory) and media\/camera interop. Though we might be able to handle
-- this with additional
-- 'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
-- values, for import\/export we do want to be able to tie this to the
-- external resource, which is one thing that the @memoryObjectId@ is for.
--
-- The internal alloc\/free callbacks are not extensible except via new
-- 'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
-- values. The 'DeviceMemoryReportCallbackDataEXT' in this extension is
-- extensible. That was deliberate: there’s a real possibility we’ll want
-- to get extra information in the future. As one example, currently this
-- reports only physical allocations, but we believe there are interesting
-- cases for tracking how populated that VA region is.
--
-- The callbacks are clearly specified as only callable within the context
-- of a call from the app into Vulkan. We believe there are some cases
-- where drivers can allocate device memory asynchronously. This was one of
-- the sticky issues that derailed the internal device memory allocation
-- reporting design (which is essentially what this extension is trying to
-- do) leading up to 1.0.
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' is described in
-- a section called “Host memory” and the intro to it is very explicitly
-- about host memory. The other callbacks are all inherently about host
-- memory. But this extension is very focused on device memory.
--
-- 3) Should the callback be reporting which heap is used?
--
-- __RESOLVED__: Yes. It’s important for non-UMA systems to have all the
-- device memory allocations attributed to the corresponding device memory
-- heaps. For internally-allocated device memory, @heapIndex@ will always
-- correspond to an advertised heap, rather than having a magic value
-- indicating a non-advertised heap. Drivers can advertise heaps that don’t
-- have any corresponding memory types if they need to.
--
-- 4) Should we use an array of callback for the layers to intercept
-- instead of chaining multiple of the
-- 'DeviceDeviceMemoryReportCreateInfoEXT' structures in the @pNext@ of
-- 'Vulkan.Core10.Device.DeviceCreateInfo'?
--
-- __RESOLVED__ No. The pointer to the
-- 'DeviceDeviceMemoryReportCreateInfoEXT' structure itself is const and
-- you can’t just cast it away. Thus we can’t update the callback array
-- inside the structure. In addition, we can’t drop this @pNext@ chain
-- either, so making a copy of this whole structure doesn’t work either.
--
-- 5) Should we track bulk allocations shared among multiple objects?
--
-- __RESOLVED__ No. Take the shader heap as an example. Some
-- implementations will let multiple 'Vulkan.Core10.Handles.Pipeline'
-- objects share the same shader heap. We are not asking the implementation
-- to report 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PIPELINE' along
-- with a 'Vulkan.Core10.APIConstants.NULL_HANDLE' for this bulk
-- allocation. Instead, this bulk allocation is considered as a layer below
-- what this extension is interested in. Later, when the actual
-- 'Vulkan.Core10.Handles.Pipeline' objects are created by suballocating
-- from the bulk allocation, we ask the implementation to report the valid
-- handles of the 'Vulkan.Core10.Handles.Pipeline' objects along with the
-- actual suballocated sizes and different @memoryObjectId@.
--
-- 6) Can we require the callbacks to be always called in the same thread
-- with the Vulkan commands?
--
-- __RESOLVED__ No. Some implementations might choose to multiplex work
-- from multiple application threads into a single backend thread and
-- perform JIT allocations as a part of that flow. Since this behavior is
-- theoretically legit, we can’t require the callbacks to be always called
-- in the same thread with the Vulkan commands, and the note is to remind
-- the applications to handle this case properly.
--
-- 7) Should we add an additional “allocation failed” event type with
-- things like size and heap index reported?
--
-- __RESOLVED__ Yes. This fits in well with the callback infrastructure
-- added in this extension, and implementation touches the same code and
-- has the same overheads as the rest of the extension. It could help
-- debugging things like getting an
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' error when ending
-- a command buffer. Right now the allocation failure could have happened
-- anywhere during recording, and a callback would be really useful to
-- understand where and why.
--
-- == Version History
--
-- -   Revision 1, 2020-08-26 (Yiwei Zhang)
--
--     -   Initial version
--
-- -   Revision 2, 2021-01-06 (Yiwei Zhang)
--
--     -   Minor description update
--
-- = See Also
--
-- 'PFN_vkDeviceMemoryReportCallbackEXT',
-- 'DeviceDeviceMemoryReportCreateInfoEXT',
-- 'DeviceMemoryReportCallbackDataEXT', 'DeviceMemoryReportEventTypeEXT',
-- 'DeviceMemoryReportFlagsEXT',
-- 'PhysicalDeviceDeviceMemoryReportFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_memory_report Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_memory_report  ( PhysicalDeviceDeviceMemoryReportFeaturesEXT(..)
                                                      , DeviceDeviceMemoryReportCreateInfoEXT(..)
                                                      , DeviceMemoryReportCallbackDataEXT(..)
                                                      , DeviceMemoryReportFlagsEXT(..)
                                                      , DeviceMemoryReportEventTypeEXT( DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT
                                                                                      , ..
                                                                                      )
                                                      , PFN_vkDeviceMemoryReportCallbackEXT
                                                      , FN_vkDeviceMemoryReportCallbackEXT
                                                      , EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION
                                                      , pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION
                                                      , EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME
                                                      , pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME
                                                      ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT))
-- | VkPhysicalDeviceDeviceMemoryReportFeaturesEXT - Structure describing
-- whether device memory report callback can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceMemoryReportFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDeviceMemoryReportFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceMemoryReportFeaturesEXT = PhysicalDeviceDeviceMemoryReportFeaturesEXT
  { -- | #features-deviceMemoryReport# @deviceMemoryReport@ indicates whether the
    -- implementation supports the ability to register device memory report
    -- callbacks.
    deviceMemoryReport :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceMemoryReportFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceMemoryReport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  peekCStruct p = do
    deviceMemoryReport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceMemoryReportFeaturesEXT
             (bool32ToBool deviceMemoryReport)

instance Storable PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  zero = PhysicalDeviceDeviceMemoryReportFeaturesEXT
           zero


-- | VkDeviceDeviceMemoryReportCreateInfoEXT - Register device memory report
-- callbacks for a Vulkan device
--
-- = Description
--
-- The callback /may/ be called from multiple threads simultaneously.
--
-- The callback /must/ be called only once by the implementation when a
-- 'DeviceMemoryReportEventTypeEXT' event occurs.
--
-- Note
--
-- The callback could be called from a background thread other than the
-- thread calling the Vulkan commands.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PFN_vkDeviceMemoryReportCallbackEXT', 'DeviceMemoryReportFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceDeviceMemoryReportCreateInfoEXT = DeviceDeviceMemoryReportCreateInfoEXT
  { -- | @flags@ is 0 and reserved for future use.
    --
    -- #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: DeviceMemoryReportFlagsEXT
  , -- | @pfnUserCallback@ is the application callback function to call.
    --
    -- #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-pfnUserCallback-parameter#
    -- @pfnUserCallback@ /must/ be a valid
    -- 'PFN_vkDeviceMemoryReportCallbackEXT' value
    pfnUserCallback :: PFN_vkDeviceMemoryReportCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
    --
    -- #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-pUserData-parameter#
    -- @pUserData@ /must/ be a pointer value
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceDeviceMemoryReportCreateInfoEXT)
#endif
deriving instance Show DeviceDeviceMemoryReportCreateInfoEXT

instance ToCStruct DeviceDeviceMemoryReportCreateInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceDeviceMemoryReportCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT)) (pfnUserCallback)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DeviceDeviceMemoryReportCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DeviceMemoryReportFlagsEXT ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT))
    pfnUserCallback <- peek @PFN_vkDeviceMemoryReportCallbackEXT ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ DeviceDeviceMemoryReportCreateInfoEXT
             flags pfnUserCallback pUserData

instance Storable DeviceDeviceMemoryReportCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceDeviceMemoryReportCreateInfoEXT where
  zero = DeviceDeviceMemoryReportCreateInfoEXT
           zero
           zero
           zero


-- | VkDeviceMemoryReportCallbackDataEXT - Structure specifying parameters
-- returned to the callback
--
-- = Description
--
-- @memoryObjectId@ is used to avoid double-counting on the same memory
-- object.
--
-- If an internally-allocated device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' /cannot/ be exported,
-- @memoryObjectId@ /must/ be unique in the 'Vulkan.Core10.Handles.Device'.
--
-- If an internally-allocated device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' supports being exported,
-- @memoryObjectId@ /must/ be unique system wide.
--
-- If an internal device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' is backed by an imported external
-- memory object, @memoryObjectId@ /must/ be unique system wide.
--
-- Note
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- For 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT' and
-- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' events, @objectHandle@
-- usually will not yet exist when the application or tool receives the
-- callback. @objectHandle@ will only exist when the create or allocate
-- call that triggered the event returns, and if the allocation or import
-- ends up failing @objectHandle@ will not ever exist.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DeviceMemoryReportEventTypeEXT', 'DeviceMemoryReportFlagsEXT',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceMemoryReportCallbackDataEXT = DeviceMemoryReportCallbackDataEXT
  { -- | @flags@ is 0 and reserved for future use.
    flags :: DeviceMemoryReportFlagsEXT
  , -- | @type@ is a 'DeviceMemoryReportEventTypeEXT' type specifying the type of
    -- event reported in this 'DeviceMemoryReportCallbackDataEXT' structure.
    type' :: DeviceMemoryReportEventTypeEXT
  , -- | @memoryObjectId@ is the unique id for the underlying memory object as
    -- described below.
    memoryObjectId :: Word64
  , -- | @size@ is the size of the memory object in bytes. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @size@ is a
    -- valid 'Vulkan.Core10.FundamentalTypes.DeviceSize' value. Otherwise,
    -- @size@ is undefined.
    size :: DeviceSize
  , -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
    -- specifying the type of the object associated with this device memory
    -- report event. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @objectType@ is
    -- a valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' enum. Otherwise,
    -- @objectType@ is undefined.
    objectType :: ObjectType
  , -- | @objectHandle@ is the object this device memory report event is
    -- attributed to. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT', @objectHandle@ is a
    -- valid Vulkan handle of the type associated with @objectType@ as defined
    -- in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
    -- table. Otherwise, @objectHandle@ is undefined.
    objectHandle :: Word64
  , -- | @heapIndex@ describes which memory heap this device memory allocation is
    -- made from. If @type@ is 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT'
    -- or 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @heapIndex@
    -- corresponds to one of the valid heaps from the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'
    -- structure. Otherwise, @heapIndex@ is undefined.
    heapIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryReportCallbackDataEXT)
#endif
deriving instance Show DeviceMemoryReportCallbackDataEXT

instance ToCStruct DeviceMemoryReportCallbackDataEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceMemoryReportCallbackDataEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT)) (type')
    poke ((p `plusPtr` 24 :: Ptr Word64)) (memoryObjectId)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 40 :: Ptr ObjectType)) (objectType)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (objectHandle)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (heapIndex)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceMemoryReportCallbackDataEXT where
  peekCStruct p = do
    flags <- peek @DeviceMemoryReportFlagsEXT ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT))
    type' <- peek @DeviceMemoryReportEventTypeEXT ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT))
    memoryObjectId <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    objectType <- peek @ObjectType ((p `plusPtr` 40 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    heapIndex <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ DeviceMemoryReportCallbackDataEXT
             flags type' memoryObjectId size objectType objectHandle heapIndex

instance Storable DeviceMemoryReportCallbackDataEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceMemoryReportCallbackDataEXT where
  zero = DeviceMemoryReportCallbackDataEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkDeviceMemoryReportFlagsEXT"
newtype DeviceMemoryReportFlagsEXT = DeviceMemoryReportFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDeviceMemoryReportFlagsEXT :: String
conNameDeviceMemoryReportFlagsEXT = "DeviceMemoryReportFlagsEXT"

enumPrefixDeviceMemoryReportFlagsEXT :: String
enumPrefixDeviceMemoryReportFlagsEXT = ""

showTableDeviceMemoryReportFlagsEXT :: [(DeviceMemoryReportFlagsEXT, String)]
showTableDeviceMemoryReportFlagsEXT = []

instance Show DeviceMemoryReportFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixDeviceMemoryReportFlagsEXT
                            showTableDeviceMemoryReportFlagsEXT
                            conNameDeviceMemoryReportFlagsEXT
                            (\(DeviceMemoryReportFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DeviceMemoryReportFlagsEXT where
  readPrec = enumReadPrec enumPrefixDeviceMemoryReportFlagsEXT
                          showTableDeviceMemoryReportFlagsEXT
                          conNameDeviceMemoryReportFlagsEXT
                          DeviceMemoryReportFlagsEXT


-- | VkDeviceMemoryReportEventTypeEXT - Events that can occur on a device
-- memory object
--
-- = See Also
--
-- 'DeviceMemoryReportCallbackDataEXT'
newtype DeviceMemoryReportEventTypeEXT = DeviceMemoryReportEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT' specifies this event
-- corresponds to the allocation of an internal device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT          = DeviceMemoryReportEventTypeEXT 0
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT' specifies this event
-- corresponds to the deallocation of an internally-allocated device memory
-- object or a 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT              = DeviceMemoryReportEventTypeEXT 1
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' specifies this event
-- corresponds to the import of an external memory object.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT            = DeviceMemoryReportEventTypeEXT 2
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT' specifies this event is
-- the release of an imported external memory object.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT          = DeviceMemoryReportEventTypeEXT 3
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT' specifies this
-- event corresponds to the failed allocation of an internal device memory
-- object or a 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT = DeviceMemoryReportEventTypeEXT 4
{-# complete DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT :: DeviceMemoryReportEventTypeEXT #-}

conNameDeviceMemoryReportEventTypeEXT :: String
conNameDeviceMemoryReportEventTypeEXT = "DeviceMemoryReportEventTypeEXT"

enumPrefixDeviceMemoryReportEventTypeEXT :: String
enumPrefixDeviceMemoryReportEventTypeEXT = "DEVICE_MEMORY_REPORT_EVENT_TYPE_"

showTableDeviceMemoryReportEventTypeEXT :: [(DeviceMemoryReportEventTypeEXT, String)]
showTableDeviceMemoryReportEventTypeEXT =
  [ (DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT         , "ALLOCATE_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT             , "FREE_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT           , "IMPORT_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT         , "UNIMPORT_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT, "ALLOCATION_FAILED_EXT")
  ]

instance Show DeviceMemoryReportEventTypeEXT where
  showsPrec = enumShowsPrec enumPrefixDeviceMemoryReportEventTypeEXT
                            showTableDeviceMemoryReportEventTypeEXT
                            conNameDeviceMemoryReportEventTypeEXT
                            (\(DeviceMemoryReportEventTypeEXT x) -> x)
                            (showsPrec 11)

instance Read DeviceMemoryReportEventTypeEXT where
  readPrec = enumReadPrec enumPrefixDeviceMemoryReportEventTypeEXT
                          showTableDeviceMemoryReportEventTypeEXT
                          conNameDeviceMemoryReportEventTypeEXT
                          DeviceMemoryReportEventTypeEXT


type FN_vkDeviceMemoryReportCallbackEXT = ("pCallbackData" ::: Ptr DeviceMemoryReportCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO ()
-- | PFN_vkDeviceMemoryReportCallbackEXT - Application-defined device memory
-- report callback function
--
-- = Description
--
-- The callback /must/ not make calls to any Vulkan commands.
--
-- = See Also
--
-- 'DeviceDeviceMemoryReportCreateInfoEXT'
type PFN_vkDeviceMemoryReportCallbackEXT = FunPtr FN_vkDeviceMemoryReportCallbackEXT


type EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION"
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 2


type EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME"
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

