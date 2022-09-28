{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_private_data"
module Vulkan.Core13.Promoted_From_VK_EXT_private_data  ( createPrivateDataSlot
                                                        , withPrivateDataSlot
                                                        , destroyPrivateDataSlot
                                                        , setPrivateData
                                                        , getPrivateData
                                                        , DevicePrivateDataCreateInfo(..)
                                                        , PrivateDataSlotCreateInfo(..)
                                                        , PhysicalDevicePrivateDataFeatures(..)
                                                        , PrivateDataSlot(..)
                                                        , PrivateDataSlotCreateFlags(..)
                                                        , StructureType(..)
                                                        , ObjectType(..)
                                                        ) where

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePrivateDataSlot))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPrivateDataSlot))
import Vulkan.Dynamic (DeviceCmds(pVkGetPrivateData))
import Vulkan.Dynamic (DeviceCmds(pVkSetPrivateData))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core13.Handles (PrivateDataSlot)
import Vulkan.Core13.Handles (PrivateDataSlot(..))
import Vulkan.Core13.Enums.PrivateDataSlotCreateFlags (PrivateDataSlotCreateFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core13.Handles (PrivateDataSlot(..))
import Vulkan.Core13.Enums.PrivateDataSlotCreateFlags (PrivateDataSlotCreateFlags(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePrivateDataSlot
  :: FunPtr (Ptr Device_T -> Ptr PrivateDataSlotCreateInfo -> Ptr AllocationCallbacks -> Ptr PrivateDataSlot -> IO Result) -> Ptr Device_T -> Ptr PrivateDataSlotCreateInfo -> Ptr AllocationCallbacks -> Ptr PrivateDataSlot -> IO Result

-- | vkCreatePrivateDataSlot - Create a slot for private data storage
--
-- == Valid Usage
--
-- -   #VUID-vkCreatePrivateDataSlot-privateData-04564# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-privateData privateData>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreatePrivateDataSlot-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreatePrivateDataSlot-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'PrivateDataSlotCreateInfo'
--     structure
--
-- -   #VUID-vkCreatePrivateDataSlot-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreatePrivateDataSlot-pPrivateDataSlot-parameter#
--     @pPrivateDataSlot@ /must/ be a valid pointer to a
--     'Vulkan.Core13.Handles.PrivateDataSlot' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core13.Handles.PrivateDataSlot',
-- 'PrivateDataSlotCreateInfo'
createPrivateDataSlot :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the logical device associated with the creation of the
                         -- object(s) holding the private data slot.
                         Device
                      -> -- | @pCreateInfo@ is a pointer to a 'PrivateDataSlotCreateInfo'
                         PrivateDataSlotCreateInfo
                      -> -- | @pAllocator@ controls host memory allocation as described in the
                         -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                         -- chapter.
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io (PrivateDataSlot)
createPrivateDataSlot device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePrivateDataSlotPtr = pVkCreatePrivateDataSlot (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreatePrivateDataSlotPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePrivateDataSlot is null" Nothing Nothing
  let vkCreatePrivateDataSlot' = mkVkCreatePrivateDataSlot vkCreatePrivateDataSlotPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPrivateDataSlot <- ContT $ bracket (callocBytes @PrivateDataSlot 8) free
  r <- lift $ traceAroundEvent "vkCreatePrivateDataSlot" (vkCreatePrivateDataSlot'
                                                            (deviceHandle (device))
                                                            pCreateInfo
                                                            pAllocator
                                                            (pPPrivateDataSlot))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPrivateDataSlot <- lift $ peek @PrivateDataSlot pPPrivateDataSlot
  pure $ (pPrivateDataSlot)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPrivateDataSlot' and 'destroyPrivateDataSlot'
--
-- To ensure that 'destroyPrivateDataSlot' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPrivateDataSlot :: forall io r . MonadIO io => Device -> PrivateDataSlotCreateInfo -> Maybe AllocationCallbacks -> (io PrivateDataSlot -> (PrivateDataSlot -> io ()) -> r) -> r
withPrivateDataSlot device pCreateInfo pAllocator b =
  b (createPrivateDataSlot device pCreateInfo pAllocator)
    (\(o0) -> destroyPrivateDataSlot device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPrivateDataSlot
  :: FunPtr (Ptr Device_T -> PrivateDataSlot -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PrivateDataSlot -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPrivateDataSlot - Destroy a private data slot
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyPrivateDataSlot-privateDataSlot-04062# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @privateDataSlot@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyPrivateDataSlot-privateDataSlot-04063# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @privateDataSlot@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyPrivateDataSlot-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyPrivateDataSlot-privateDataSlot-parameter# If
--     @privateDataSlot@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @privateDataSlot@ /must/ be a valid
--     'Vulkan.Core13.Handles.PrivateDataSlot' handle
--
-- -   #VUID-vkDestroyPrivateDataSlot-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyPrivateDataSlot-privateDataSlot-parent# If
--     @privateDataSlot@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @privateDataSlot@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core13.Handles.PrivateDataSlot'
destroyPrivateDataSlot :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device associated with the creation of the
                          -- object(s) holding the private data slot.
                          Device
                       -> -- | @privateDataSlot@ is the private data slot to destroy.
                          PrivateDataSlot
                       -> -- | @pAllocator@ controls host memory allocation as described in the
                          -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                          -- chapter.
                          ("allocator" ::: Maybe AllocationCallbacks)
                       -> io ()
destroyPrivateDataSlot device
                         privateDataSlot
                         allocator = liftIO . evalContT $ do
  let vkDestroyPrivateDataSlotPtr = pVkDestroyPrivateDataSlot (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyPrivateDataSlotPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPrivateDataSlot is null" Nothing Nothing
  let vkDestroyPrivateDataSlot' = mkVkDestroyPrivateDataSlot vkDestroyPrivateDataSlotPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyPrivateDataSlot" (vkDestroyPrivateDataSlot'
                                                        (deviceHandle (device))
                                                        (privateDataSlot)
                                                        pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetPrivateData
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlot -> Word64 -> IO Result) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlot -> Word64 -> IO Result

-- | vkSetPrivateData - Associate data with a Vulkan object
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core13.Handles.PrivateDataSlot'
setPrivateData :: forall io
                . (MonadIO io)
               => -- | @device@ is the device that created the object.
                  --
                  -- #VUID-vkSetPrivateData-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
                  -- the type of object to associate data with.
                  --
                  -- #VUID-vkSetPrivateData-objectType-parameter# @objectType@ /must/ be a
                  -- valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
                  ObjectType
               -> -- | @objectHandle@ is a handle to the object to associate data with.
                  --
                  -- #VUID-vkSetPrivateData-objectHandle-04016# @objectHandle@ /must/ be
                  -- @device@ or a child of @device@
                  --
                  -- #VUID-vkSetPrivateData-objectHandle-04017# @objectHandle@ /must/ be a
                  -- valid handle to an object of type @objectType@
                  ("objectHandle" ::: Word64)
               -> -- | @privateDataSlot@ is a handle to a
                  -- 'Vulkan.Core13.Handles.PrivateDataSlot' specifying location of private
                  -- data storage.
                  --
                  -- #VUID-vkSetPrivateData-privateDataSlot-parameter# @privateDataSlot@
                  -- /must/ be a valid 'Vulkan.Core13.Handles.PrivateDataSlot' handle
                  --
                  -- #VUID-vkSetPrivateData-privateDataSlot-parent# @privateDataSlot@ /must/
                  -- have been created, allocated, or retrieved from @device@
                  PrivateDataSlot
               -> -- | @data@ is user defined data to associate the object with. This data will
                  -- be stored at @privateDataSlot@.
                  ("data" ::: Word64)
               -> io ()
setPrivateData device
                 objectType
                 objectHandle
                 privateDataSlot
                 data' = liftIO $ do
  let vkSetPrivateDataPtr = pVkSetPrivateData (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkSetPrivateDataPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetPrivateData is null" Nothing Nothing
  let vkSetPrivateData' = mkVkSetPrivateData vkSetPrivateDataPtr
  r <- traceAroundEvent "vkSetPrivateData" (vkSetPrivateData'
                                              (deviceHandle (device))
                                              (objectType)
                                              (objectHandle)
                                              (privateDataSlot)
                                              (data'))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPrivateData
  :: FunPtr (Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlot -> Ptr Word64 -> IO ()) -> Ptr Device_T -> ObjectType -> Word64 -> PrivateDataSlot -> Ptr Word64 -> IO ()

-- | vkGetPrivateData - Retrieve data associated with a Vulkan object
--
-- = Description
--
-- Note
--
-- Due to platform details on Android, implementations might not be able to
-- reliably return @0@ from calls to 'getPrivateData' for
-- 'Vulkan.Extensions.Handles.SwapchainKHR' objects on which
-- 'setPrivateData' has not previously been called. This erratum is
-- exclusive to the Android platform and objects of type
-- 'Vulkan.Extensions.Handles.SwapchainKHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core13.Handles.PrivateDataSlot'
getPrivateData :: forall io
                . (MonadIO io)
               => -- | @device@ is the device that created the object
                  --
                  -- #VUID-vkGetPrivateData-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
                  -- the type of object data is associated with.
                  --
                  -- #VUID-vkGetPrivateData-objectType-04018# @objectType@ /must/ be
                  -- 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DEVICE', or an object type
                  -- whose parent is 'Vulkan.Core10.Handles.Device'
                  --
                  -- #VUID-vkGetPrivateData-objectType-parameter# @objectType@ /must/ be a
                  -- valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
                  ObjectType
               -> -- | @objectHandle@ is a handle to the object data is associated with.
                  ("objectHandle" ::: Word64)
               -> -- | @privateDataSlot@ is a handle to a
                  -- 'Vulkan.Core13.Handles.PrivateDataSlot' specifying location of private
                  -- data pointer storage.
                  --
                  -- #VUID-vkGetPrivateData-privateDataSlot-parameter# @privateDataSlot@
                  -- /must/ be a valid 'Vulkan.Core13.Handles.PrivateDataSlot' handle
                  --
                  -- #VUID-vkGetPrivateData-privateDataSlot-parent# @privateDataSlot@ /must/
                  -- have been created, allocated, or retrieved from @device@
                  PrivateDataSlot
               -> io (("data" ::: Word64))
getPrivateData device
                 objectType
                 objectHandle
                 privateDataSlot = liftIO . evalContT $ do
  let vkGetPrivateDataPtr = pVkGetPrivateData (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPrivateDataPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPrivateData is null" Nothing Nothing
  let vkGetPrivateData' = mkVkGetPrivateData vkGetPrivateDataPtr
  pPData <- ContT $ bracket (callocBytes @Word64 8) free
  lift $ traceAroundEvent "vkGetPrivateData" (vkGetPrivateData'
                                                (deviceHandle (device))
                                                (objectType)
                                                (objectHandle)
                                                (privateDataSlot)
                                                (pPData))
  pData <- lift $ peek @Word64 pPData
  pure $ (pData)


-- | VkDevicePrivateDataCreateInfo - Reserve private data slots
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DevicePrivateDataCreateInfo = DevicePrivateDataCreateInfo
  { -- | @privateDataSlotRequestCount@ is the amount of slots to reserve.
    privateDataSlotRequestCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DevicePrivateDataCreateInfo)
#endif
deriving instance Show DevicePrivateDataCreateInfo

instance ToCStruct DevicePrivateDataCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DevicePrivateDataCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (privateDataSlotRequestCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DevicePrivateDataCreateInfo where
  peekCStruct p = do
    privateDataSlotRequestCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DevicePrivateDataCreateInfo
             privateDataSlotRequestCount

instance Storable DevicePrivateDataCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DevicePrivateDataCreateInfo where
  zero = DevicePrivateDataCreateInfo
           zero


-- | VkPrivateDataSlotCreateInfo - Structure specifying the parameters of
-- private data slot construction
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.PrivateDataSlotCreateFlags.PrivateDataSlotCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createPrivateDataSlot',
-- 'Vulkan.Extensions.VK_EXT_private_data.createPrivateDataSlotEXT'
data PrivateDataSlotCreateInfo = PrivateDataSlotCreateInfo
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkPrivateDataSlotCreateInfo-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: PrivateDataSlotCreateFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PrivateDataSlotCreateInfo)
#endif
deriving instance Show PrivateDataSlotCreateInfo

instance ToCStruct PrivateDataSlotCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PrivateDataSlotCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlags)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlags)) (zero)
    f

instance FromCStruct PrivateDataSlotCreateInfo where
  peekCStruct p = do
    flags <- peek @PrivateDataSlotCreateFlags ((p `plusPtr` 16 :: Ptr PrivateDataSlotCreateFlags))
    pure $ PrivateDataSlotCreateInfo
             flags

instance Storable PrivateDataSlotCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PrivateDataSlotCreateInfo where
  zero = PrivateDataSlotCreateInfo
           zero


-- | VkPhysicalDevicePrivateDataFeatures - Structure specifying physical
-- device support
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePrivateDataFeatures' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePrivateDataFeatures' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePrivateDataFeatures = PhysicalDevicePrivateDataFeatures
  { -- | #extension-features-privateData# @privateData@ indicates whether the
    -- implementation supports private data. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#private-data Private Data>.
    privateData :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrivateDataFeatures)
#endif
deriving instance Show PhysicalDevicePrivateDataFeatures

instance ToCStruct PhysicalDevicePrivateDataFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrivateDataFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (privateData))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrivateDataFeatures where
  peekCStruct p = do
    privateData <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePrivateDataFeatures
             (bool32ToBool privateData)

instance Storable PhysicalDevicePrivateDataFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrivateDataFeatures where
  zero = PhysicalDevicePrivateDataFeatures
           zero

