{-# language CPP #-}
-- | = Name
--
-- VK_EXT_private_data - device extension
--
-- == VK_EXT_private_data
--
-- [__Name String__]
--     @VK_EXT_private_data@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     296
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Matthew Rusch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_private_data:%20&body=@mattruschnv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-03-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthew Rusch, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- The \'VK_EXT_private_data\' extension is a device extension which
-- enables attaching arbitrary payloads to Vulkan objects. It introduces
-- the idea of private data slots as a means of storing a 64-bit unsigned
-- integer of application defined data. Private data slots can be created
-- or destroyed any time an associated device is available. Private data
-- slots can be reserved at device creation time, and limiting use to the
-- amount reserved will allow the extension to exhibit better performance
-- characteristics.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
--
-- == New Commands
--
-- -   'createPrivateDataSlotEXT'
--
-- -   'destroyPrivateDataSlotEXT'
--
-- -   'getPrivateDataEXT'
--
-- -   'setPrivateDataEXT'
--
-- == New Structures
--
-- -   'PrivateDataSlotCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DevicePrivateDataCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrivateDataFeaturesEXT'
--
-- == New Enums
--
-- -   'PrivateDataSlotCreateFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PrivateDataSlotCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIVATE_DATA_EXTENSION_NAME'
--
-- -   'EXT_PRIVATE_DATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT'
--
-- == Examples
--
-- -   In progress
--
-- == Version History
--
-- -   Revision 1, 2020-01-15 (Matthew Rusch)
--
--     -   Initial draft
--
-- = See Also
--
-- 'DevicePrivateDataCreateInfoEXT',
-- 'PhysicalDevicePrivateDataFeaturesEXT',
-- 'PrivateDataSlotCreateFlagBitsEXT', 'PrivateDataSlotCreateFlagsEXT',
-- 'PrivateDataSlotCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT',
-- 'createPrivateDataSlotEXT', 'destroyPrivateDataSlotEXT',
-- 'getPrivateDataEXT', 'setPrivateDataEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_private_data  ( createPrivateDataSlotEXT
                                              , withPrivateDataSlotEXT
                                              , destroyPrivateDataSlotEXT
                                              , setPrivateDataEXT
                                              , getPrivateDataEXT
                                              , DevicePrivateDataCreateInfoEXT(..)
                                              , PrivateDataSlotCreateInfoEXT(..)
                                              , PhysicalDevicePrivateDataFeaturesEXT(..)
                                              , PrivateDataSlotCreateFlagsEXT
                                              , PrivateDataSlotCreateFlagBitsEXT(..)
                                              , EXT_PRIVATE_DATA_SPEC_VERSION
                                              , pattern EXT_PRIVATE_DATA_SPEC_VERSION
                                              , EXT_PRIVATE_DATA_EXTENSION_NAME
                                              , pattern EXT_PRIVATE_DATA_EXTENSION_NAME
                                              , PrivateDataSlotEXT(..)
                                              ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePrivateDataSlotEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPrivateDataSlotEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetPrivateDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetPrivateDataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Extensions.Handles (PrivateDataSlotEXT)
import Vulkan.Extensions.Handles (PrivateDataSlotEXT(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (PrivateDataSlotEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePrivateDataSlotEXT
  :: FunPtr (Ptr Device_T -> Ptr PrivateDataSlotCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr PrivateDataSlotEXT -> IO Result) -> Ptr Device_T -> Ptr PrivateDataSlotCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr PrivateDataSlotEXT -> IO Result

-- | vkCreatePrivateDataSlotEXT - Create a slot for private data storage
--
-- == Valid Usage
--
-- -   #VUID-vkCreatePrivateDataSlotEXT-privateData-04564# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-privateData privateData>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreatePrivateDataSlotEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreatePrivateDataSlotEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'PrivateDataSlotCreateInfoEXT' structure
--
-- -   #VUID-vkCreatePrivateDataSlotEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreatePrivateDataSlotEXT-pPrivateDataSlot-parameter#
--     @pPrivateDataSlot@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.PrivateDataSlotEXT' handle
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
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'PrivateDataSlotCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
createPrivateDataSlotEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device associated with the creation of the
                            -- object(s) holding the private data slot.
                            Device
                         -> -- | @pCreateInfo@ is a pointer to a 'PrivateDataSlotCreateInfoEXT'
                            PrivateDataSlotCreateInfoEXT
                         -> -- | @pAllocator@ controls host memory allocation as described in the
                            -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                            -- chapter.
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (PrivateDataSlotEXT)
createPrivateDataSlotEXT device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePrivateDataSlotEXTPtr = pVkCreatePrivateDataSlotEXT (deviceCmds (device :: Device))
  lift $ unless (vkCreatePrivateDataSlotEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePrivateDataSlotEXT is null" Nothing Nothing
  let vkCreatePrivateDataSlotEXT' = mkVkCreatePrivateDataSlotEXT vkCreatePrivateDataSlotEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPrivateDataSlot <- ContT $ bracket (callocBytes @PrivateDataSlotEXT 8) free
  r <- lift $ traceAroundEvent "vkCreatePrivateDataSlotEXT" (vkCreatePrivateDataSlotEXT' (deviceHandle (device)) pCreateInfo pAllocator (pPPrivateDataSlot))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPrivateDataSlot <- lift $ peek @PrivateDataSlotEXT pPPrivateDataSlot
  pure $ (pPrivateDataSlot)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPrivateDataSlotEXT' and 'destroyPrivateDataSlotEXT'
--
-- To ensure that 'destroyPrivateDataSlotEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPrivateDataSlotEXT :: forall io r . MonadIO io => Device -> PrivateDataSlotCreateInfoEXT -> Maybe AllocationCallbacks -> (io PrivateDataSlotEXT -> (PrivateDataSlotEXT -> io ()) -> r) -> r
withPrivateDataSlotEXT device pCreateInfo pAllocator b =
  b (createPrivateDataSlotEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyPrivateDataSlotEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPrivateDataSlotEXT
  :: FunPtr (Ptr Device_T -> PrivateDataSlotEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PrivateDataSlotEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPrivateDataSlotEXT - Destroy a private data slot
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-privateDataSlot-04062# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @privateDataSlot@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-privateDataSlot-04063# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @privateDataSlot@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-privateDataSlot-parameter# If
--     @privateDataSlot@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @privateDataSlot@ /must/ be a valid
--     'Vulkan.Extensions.Handles.PrivateDataSlotEXT' handle
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyPrivateDataSlotEXT-privateDataSlot-parent# If
--     @privateDataSlot@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @privateDataSlot@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
destroyPrivateDataSlotEXT :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device associated with the creation of the
                             -- object(s) holding the private data slot.
                             Device
                          -> -- | @privateDataSlot@ is the private data slot to destroy.
                             PrivateDataSlotEXT
                          -> -- | @pAllocator@ controls host memory allocation as described in the
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                             -- chapter.
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io ()
destroyPrivateDataSlotEXT device privateDataSlot allocator = liftIO . evalContT $ do
  let vkDestroyPrivateDataSlotEXTPtr = pVkDestroyPrivateDataSlotEXT (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPrivateDataSlotEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPrivateDataSlotEXT is null" Nothing Nothing
  let vkDestroyPrivateDataSlotEXT' = mkVkDestroyPrivateDataSlotEXT vkDestroyPrivateDataSlotEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyPrivateDataSlotEXT" (vkDestroyPrivateDataSlotEXT' (deviceHandle (device)) (privateDataSlot) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetPrivateDataEXT
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Word64 -> IO Result) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Word64 -> IO Result

-- | vkSetPrivateDataEXT - Associate data with a Vulkan object
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
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
setPrivateDataEXT :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the device that created the object.
                     --
                     -- #VUID-vkSetPrivateDataEXT-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
                     -- the type of object to associate data with.
                     --
                     -- #VUID-vkSetPrivateDataEXT-objectType-parameter# @objectType@ /must/ be a
                     -- valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
                     ObjectType
                  -> -- | @objectHandle@ is a handle to the object to associate data with.
                     --
                     -- #VUID-vkSetPrivateDataEXT-objectHandle-04016# @objectHandle@ /must/ be
                     -- @device@ or a child of @device@
                     --
                     -- #VUID-vkSetPrivateDataEXT-objectHandle-04017# @objectHandle@ /must/ be a
                     -- valid handle to an object of type @objectType@
                     ("objectHandle" ::: Word64)
                  -> -- | @privateDataSlot@ is a handle to a
                     -- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT' specifying location of
                     -- private data storage.
                     --
                     -- #VUID-vkSetPrivateDataEXT-privateDataSlot-parameter# @privateDataSlot@
                     -- /must/ be a valid 'Vulkan.Extensions.Handles.PrivateDataSlotEXT' handle
                     --
                     -- #VUID-vkSetPrivateDataEXT-privateDataSlot-parent# @privateDataSlot@
                     -- /must/ have been created, allocated, or retrieved from @device@
                     PrivateDataSlotEXT
                  -> -- | @data@ is user defined data to associate the object with. This data will
                     -- be stored at @privateDataSlot@.
                     ("data" ::: Word64)
                  -> io ()
setPrivateDataEXT device objectType objectHandle privateDataSlot data' = liftIO $ do
  let vkSetPrivateDataEXTPtr = pVkSetPrivateDataEXT (deviceCmds (device :: Device))
  unless (vkSetPrivateDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetPrivateDataEXT is null" Nothing Nothing
  let vkSetPrivateDataEXT' = mkVkSetPrivateDataEXT vkSetPrivateDataEXTPtr
  r <- traceAroundEvent "vkSetPrivateDataEXT" (vkSetPrivateDataEXT' (deviceHandle (device)) (objectType) (objectHandle) (privateDataSlot) (data'))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPrivateDataEXT
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Ptr Word64 -> IO ()) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlotEXT -> Ptr Word64 -> IO ()

-- | vkGetPrivateDataEXT - Retrieve data associated with a Vulkan object
--
-- = Description
--
-- Note
--
-- Due to platform details on Android, implementations might not be able to
-- reliably return @0@ from calls to 'getPrivateDataEXT' for
-- 'Vulkan.Extensions.Handles.SwapchainKHR' objects on which
-- 'setPrivateDataEXT' has not previously been called. This erratum is
-- exclusive to the Android platform and objects of type
-- 'Vulkan.Extensions.Handles.SwapchainKHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
getPrivateDataEXT :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the device that created the object
                     --
                     -- #VUID-vkGetPrivateDataEXT-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
                     -- the type of object data is associated with.
                     --
                     -- #VUID-vkGetPrivateDataEXT-objectType-04018# @objectType@ /must/ be
                     -- 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DEVICE', or an object type
                     -- whose parent is 'Vulkan.Core10.Handles.Device'
                     --
                     -- #VUID-vkGetPrivateDataEXT-objectType-parameter# @objectType@ /must/ be a
                     -- valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
                     ObjectType
                  -> -- | @objectHandle@ is a handle to the object data is associated with.
                     ("objectHandle" ::: Word64)
                  -> -- | @privateDataSlot@ is a handle to a
                     -- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT' specifying location of
                     -- private data pointer storage.
                     --
                     -- #VUID-vkGetPrivateDataEXT-privateDataSlot-parameter# @privateDataSlot@
                     -- /must/ be a valid 'Vulkan.Extensions.Handles.PrivateDataSlotEXT' handle
                     --
                     -- #VUID-vkGetPrivateDataEXT-privateDataSlot-parent# @privateDataSlot@
                     -- /must/ have been created, allocated, or retrieved from @device@
                     PrivateDataSlotEXT
                  -> io (("data" ::: Word64))
getPrivateDataEXT device objectType objectHandle privateDataSlot = liftIO . evalContT $ do
  let vkGetPrivateDataEXTPtr = pVkGetPrivateDataEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetPrivateDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPrivateDataEXT is null" Nothing Nothing
  let vkGetPrivateDataEXT' = mkVkGetPrivateDataEXT vkGetPrivateDataEXTPtr
  pPData <- ContT $ bracket (callocBytes @Word64 8) free
  lift $ traceAroundEvent "vkGetPrivateDataEXT" (vkGetPrivateDataEXT' (deviceHandle (device)) (objectType) (objectHandle) (privateDataSlot) (pPData))
  pData <- lift $ peek @Word64 pPData
  pure $ (pData)


-- | VkDevicePrivateDataCreateInfoEXT - Reserve private data slots
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DevicePrivateDataCreateInfoEXT = DevicePrivateDataCreateInfoEXT
  { -- | @privateDataSlotRequestCount@ is the amount of slots to reserve.
    privateDataSlotRequestCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DevicePrivateDataCreateInfoEXT)
#endif
deriving instance Show DevicePrivateDataCreateInfoEXT

instance ToCStruct DevicePrivateDataCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DevicePrivateDataCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (privateDataSlotRequestCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DevicePrivateDataCreateInfoEXT where
  peekCStruct p = do
    privateDataSlotRequestCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DevicePrivateDataCreateInfoEXT
             privateDataSlotRequestCount

instance Storable DevicePrivateDataCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DevicePrivateDataCreateInfoEXT where
  zero = DevicePrivateDataCreateInfoEXT
           zero


-- | VkPrivateDataSlotCreateInfoEXT - Structure specifying the parameters of
-- private data slot construction
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PrivateDataSlotCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createPrivateDataSlotEXT'
data PrivateDataSlotCreateInfoEXT = PrivateDataSlotCreateInfoEXT
  { -- | @flags@ is a bitmask of 'PrivateDataSlotCreateFlagsEXT' specifying
    -- additional parameters of the new private data slot
    --
    -- #VUID-VkPrivateDataSlotCreateInfoEXT-flags-zerobitmask# @flags@ /must/
    -- be @0@
    flags :: PrivateDataSlotCreateFlagsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PrivateDataSlotCreateInfoEXT)
#endif
deriving instance Show PrivateDataSlotCreateInfoEXT

instance ToCStruct PrivateDataSlotCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PrivateDataSlotCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT)) (zero)
    f

instance FromCStruct PrivateDataSlotCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PrivateDataSlotCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlagsEXT))
    pure $ PrivateDataSlotCreateInfoEXT
             flags

instance Storable PrivateDataSlotCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PrivateDataSlotCreateInfoEXT where
  zero = PrivateDataSlotCreateInfoEXT
           zero


-- | VkPhysicalDevicePrivateDataFeaturesEXT - Structure specifying physical
-- device support
--
-- = Members
--
-- The members of the 'PhysicalDevicePrivateDataFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePrivateDataFeaturesEXT' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDevicePrivateDataFeaturesEXT' /can/ also be used in the @pNext@
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePrivateDataFeaturesEXT = PhysicalDevicePrivateDataFeaturesEXT
  { -- | #features-privateData# @privateData@ indicates whether the
    -- implementation supports private data. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#private-data Private Data>.
    privateData :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrivateDataFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePrivateDataFeaturesEXT

instance ToCStruct PhysicalDevicePrivateDataFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrivateDataFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (privateData))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrivateDataFeaturesEXT where
  peekCStruct p = do
    privateData <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePrivateDataFeaturesEXT
             (bool32ToBool privateData)

instance Storable PhysicalDevicePrivateDataFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrivateDataFeaturesEXT where
  zero = PhysicalDevicePrivateDataFeaturesEXT
           zero


type PrivateDataSlotCreateFlagsEXT = PrivateDataSlotCreateFlagBitsEXT

-- | VkPrivateDataSlotCreateFlagBitsEXT - Bitmask specifying additional
-- parameters for private data slot creation
--
-- = See Also
--
-- 'PrivateDataSlotCreateFlagsEXT'
newtype PrivateDataSlotCreateFlagBitsEXT = PrivateDataSlotCreateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePrivateDataSlotCreateFlagBitsEXT :: String
conNamePrivateDataSlotCreateFlagBitsEXT = "PrivateDataSlotCreateFlagBitsEXT"

enumPrefixPrivateDataSlotCreateFlagBitsEXT :: String
enumPrefixPrivateDataSlotCreateFlagBitsEXT = ""

showTablePrivateDataSlotCreateFlagBitsEXT :: [(PrivateDataSlotCreateFlagBitsEXT, String)]
showTablePrivateDataSlotCreateFlagBitsEXT = []

instance Show PrivateDataSlotCreateFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixPrivateDataSlotCreateFlagBitsEXT
                            showTablePrivateDataSlotCreateFlagBitsEXT
                            conNamePrivateDataSlotCreateFlagBitsEXT
                            (\(PrivateDataSlotCreateFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PrivateDataSlotCreateFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixPrivateDataSlotCreateFlagBitsEXT
                          showTablePrivateDataSlotCreateFlagBitsEXT
                          conNamePrivateDataSlotCreateFlagBitsEXT
                          PrivateDataSlotCreateFlagBitsEXT


type EXT_PRIVATE_DATA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_SPEC_VERSION"
pattern EXT_PRIVATE_DATA_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIVATE_DATA_SPEC_VERSION = 1


type EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_EXTENSION_NAME"
pattern EXT_PRIVATE_DATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

