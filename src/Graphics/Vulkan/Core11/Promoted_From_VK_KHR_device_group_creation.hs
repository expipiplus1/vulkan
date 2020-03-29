{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation  ( enumeratePhysicalDeviceGroups
                                                                          , PhysicalDeviceGroupProperties(..)
                                                                          , DeviceGroupDeviceCreateInfo(..)
                                                                          , StructureType(..)
                                                                          , MemoryHeapFlagBits(..)
                                                                          , MemoryHeapFlags
                                                                          , MAX_DEVICE_GROUP_SIZE
                                                                          , pattern MAX_DEVICE_GROUP_SIZE
                                                                          ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
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
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Instance)
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceGroups))
import Graphics.Vulkan.Core10.Handles (Instance_T)
import Graphics.Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Graphics.Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlagBits(..))
import Graphics.Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
import Graphics.Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result) -> Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result

-- | vkEnumeratePhysicalDeviceGroups - Enumerates groups of physical devices
-- that can be used to create a single logical device
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Instance' is a handle to a Vulkan
--     instance previously created with
--     'Graphics.Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- -   @pPhysicalDeviceGroupCount@ is a pointer to an integer related to
--     the number of device groups available or queried, as described
--     below.
--
-- -   @pPhysicalDeviceGroupProperties@ is either @NULL@ or a pointer to an
--     array of 'PhysicalDeviceGroupProperties' structures.
--
-- = Description
--
-- If @pPhysicalDeviceGroupProperties@ is @NULL@, then the number of device
-- groups available is returned in @pPhysicalDeviceGroupCount@. Otherwise,
-- @pPhysicalDeviceGroupCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pPhysicalDeviceGroupProperties@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pPhysicalDeviceGroupProperties@. If
-- @pPhysicalDeviceGroupCount@ is less than the number of device groups
-- available, at most @pPhysicalDeviceGroupCount@ structures will be
-- written. If @pPhysicalDeviceGroupCount@ is smaller than the number of
-- device groups available,
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available device groups were returned.
--
-- Every physical device /must/ be in exactly one device group.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Instance' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Instance' handle
--
-- -   @pPhysicalDeviceGroupCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pPhysicalDeviceGroupCount@ is not @0@,
--     and @pPhysicalDeviceGroupProperties@ is not @NULL@,
--     @pPhysicalDeviceGroupProperties@ /must/ be a valid pointer to an
--     array of @pPhysicalDeviceGroupCount@ 'PhysicalDeviceGroupProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Instance',
-- 'PhysicalDeviceGroupProperties'
enumeratePhysicalDeviceGroups :: Instance -> IO (Result, ("physicalDeviceGroupProperties" ::: Vector PhysicalDeviceGroupProperties))
enumeratePhysicalDeviceGroups instance' = evalContT $ do
  let vkEnumeratePhysicalDeviceGroups' = mkVkEnumeratePhysicalDeviceGroups (pVkEnumeratePhysicalDeviceGroups (instanceCmds (instance' :: Instance)))
  let instance'' = instanceHandle (instance')
  pPPhysicalDeviceGroupCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPhysicalDeviceGroupCount <- lift $ peek @Word32 pPPhysicalDeviceGroupCount
  pPPhysicalDeviceGroupProperties <- ContT $ bracket (callocBytes @PhysicalDeviceGroupProperties ((fromIntegral (pPhysicalDeviceGroupCount)) * 288)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPPhysicalDeviceGroupProperties `advancePtrBytes` (i * 288) :: Ptr PhysicalDeviceGroupProperties) . ($ ())) [0..(fromIntegral (pPhysicalDeviceGroupCount)) - 1]
  r' <- lift $ vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) ((pPPhysicalDeviceGroupProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPhysicalDeviceGroupCount' <- lift $ peek @Word32 pPPhysicalDeviceGroupCount
  pPhysicalDeviceGroupProperties' <- lift $ generateM (fromIntegral (pPhysicalDeviceGroupCount')) (\i -> peekCStruct @PhysicalDeviceGroupProperties (((pPPhysicalDeviceGroupProperties) `advancePtrBytes` (288 * (i)) :: Ptr PhysicalDeviceGroupProperties)))
  pure $ ((r'), pPhysicalDeviceGroupProperties')


-- | VkPhysicalDeviceGroupProperties - Structure specifying physical device
-- group properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group_creation.enumeratePhysicalDeviceGroupsKHR'
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- | @physicalDeviceCount@ is the number of physical devices in the group.
    physicalDeviceCount :: Word32
  , -- | @physicalDevices@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE'
    -- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handles representing all
    -- physical devices in the group. The first @physicalDeviceCount@ elements
    -- of the array will be valid.
    physicalDevices :: Vector (Ptr PhysicalDevice_T)
  , -- | @subsetAllocation@ specifies whether logical devices created from the
    -- group support allocating device memory on a subset of devices, via the
    -- @deviceMask@ member of the
    -- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo'.
    -- If this is 'Graphics.Vulkan.Core10.BaseType.FALSE', then all device
    -- memory allocations are made across all physical devices in the group. If
    -- @physicalDeviceCount@ is @1@, then @subsetAllocation@ /must/ be
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE'.
    subsetAllocation :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceGroupProperties

instance ToCStruct PhysicalDeviceGroupProperties where
  withCStruct x f = allocaBytesAligned 288 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGroupProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (physicalDeviceCount)
    unless ((Data.Vector.length $ (physicalDevices)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "physicalDevices is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (physicalDevices)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (subsetAllocation))
    f
  cStructSize = 288
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    unless ((Data.Vector.length $ (mempty)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "physicalDevices is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (mempty)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGroupProperties where
  peekCStruct p = do
    physicalDeviceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    physicalDevices <- generateM (MAX_DEVICE_GROUP_SIZE) (\i -> peek @(Ptr PhysicalDevice_T) (((lowerArrayPtr @(Ptr PhysicalDevice_T) ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T))))
    subsetAllocation <- peek @Bool32 ((p `plusPtr` 280 :: Ptr Bool32))
    pure $ PhysicalDeviceGroupProperties
             physicalDeviceCount physicalDevices (bool32ToBool subsetAllocation)

instance Storable PhysicalDeviceGroupProperties where
  sizeOf ~_ = 288
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGroupProperties where
  zero = PhysicalDeviceGroupProperties
           zero
           mempty
           zero


-- | VkDeviceGroupDeviceCreateInfo - Create a logical device from multiple
-- physical devices
--
-- = Description
--
-- The elements of the @pPhysicalDevices@ array are an ordered list of the
-- physical devices that the logical device represents. These /must/ be a
-- subset of a single device group, and need not be in the same order as
-- they were enumerated. The order of the physical devices in the
-- @pPhysicalDevices@ array determines the /device index/ of each physical
-- device, with element i being assigned a device index of i. Certain
-- commands and structures refer to one or more physical devices by using
-- device indices or /device masks/ formed using device indices.
--
-- A logical device created without using 'DeviceGroupDeviceCreateInfo', or
-- with @physicalDeviceCount@ equal to zero, is equivalent to a
-- @physicalDeviceCount@ of one and @pPhysicalDevices@ pointing to the
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice' parameter to
-- 'Graphics.Vulkan.Core10.Device.createDevice'. In particular, the device
-- index of that physical device is zero.
--
-- == Valid Usage
--
-- -   Each element of @pPhysicalDevices@ /must/ be unique
--
-- -   All elements of @pPhysicalDevices@ /must/ be in the same device
--     group as enumerated by 'enumeratePhysicalDeviceGroups'
--
-- -   If @physicalDeviceCount@ is not @0@, the
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' parameter of
--     'Graphics.Vulkan.Core10.Device.createDevice' /must/ be an element of
--     @pPhysicalDevices@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO'
--
-- -   If @physicalDeviceCount@ is not @0@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @physicalDeviceCount@ valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handles
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- | @pPhysicalDevices@ is a pointer to an array of physical device handles
    -- belonging to the same device group.
    physicalDevices :: Vector (Ptr PhysicalDevice_T) }
  deriving (Typeable)
deriving instance Show DeviceGroupDeviceCreateInfo

instance ToCStruct DeviceGroupDeviceCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupDeviceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (physicalDevices)) :: Word32))
    pPPhysicalDevices' <- ContT $ allocaBytesAligned @(Ptr PhysicalDevice_T) ((Data.Vector.length (physicalDevices)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPhysicalDevices' `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (physicalDevices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T)))) (pPPhysicalDevices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPPhysicalDevices' <- ContT $ allocaBytesAligned @(Ptr PhysicalDevice_T) ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPhysicalDevices' `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T)))) (pPPhysicalDevices')
    lift $ f

instance FromCStruct DeviceGroupDeviceCreateInfo where
  peekCStruct p = do
    physicalDeviceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPhysicalDevices <- peek @(Ptr (Ptr PhysicalDevice_T)) ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T))))
    pPhysicalDevices' <- generateM (fromIntegral physicalDeviceCount) (\i -> peek @(Ptr PhysicalDevice_T) ((pPhysicalDevices `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T))))
    pure $ DeviceGroupDeviceCreateInfo
             pPhysicalDevices'

instance Zero DeviceGroupDeviceCreateInfo where
  zero = DeviceGroupDeviceCreateInfo
           mempty

