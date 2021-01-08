{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_group_creation"
module Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation  ( enumeratePhysicalDeviceGroups
                                                                 , PhysicalDeviceGroupProperties(..)
                                                                 , DeviceGroupDeviceCreateInfo(..)
                                                                 , StructureType(..)
                                                                 , MemoryHeapFlagBits(..)
                                                                 , MemoryHeapFlags
                                                                 , MAX_DEVICE_GROUP_SIZE
                                                                 , pattern MAX_DEVICE_GROUP_SIZE
                                                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceGroups))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlagBits(..))
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result) -> Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result

-- | vkEnumeratePhysicalDeviceGroups - Enumerates groups of physical devices
-- that can be used to create a single logical device
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
-- device groups available, 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available device groups were returned.
--
-- Every physical device /must/ be in exactly one device group.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkEnumeratePhysicalDeviceGroups-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkEnumeratePhysicalDeviceGroups-pPhysicalDeviceGroupCount-parameter#
--     @pPhysicalDeviceGroupCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkEnumeratePhysicalDeviceGroups-pPhysicalDeviceGroupProperties-parameter#
--     If the value referenced by @pPhysicalDeviceGroupCount@ is not @0@,
--     and @pPhysicalDeviceGroupProperties@ is not @NULL@,
--     @pPhysicalDeviceGroupProperties@ /must/ be a valid pointer to an
--     array of @pPhysicalDeviceGroupCount@ 'PhysicalDeviceGroupProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Instance', 'PhysicalDeviceGroupProperties'
enumeratePhysicalDeviceGroups :: forall io
                               . (MonadIO io)
                              => -- | @instance@ is a handle to a Vulkan instance previously created with
                                 -- 'Vulkan.Core10.DeviceInitialization.createInstance'.
                                 Instance
                              -> io (Result, ("physicalDeviceGroupProperties" ::: Vector PhysicalDeviceGroupProperties))
enumeratePhysicalDeviceGroups instance' = liftIO . evalContT $ do
  let vkEnumeratePhysicalDeviceGroupsPtr = pVkEnumeratePhysicalDeviceGroups (instanceCmds (instance' :: Instance))
  lift $ unless (vkEnumeratePhysicalDeviceGroupsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDeviceGroups is null" Nothing Nothing
  let vkEnumeratePhysicalDeviceGroups' = mkVkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroupsPtr
  let instance'' = instanceHandle (instance')
  pPPhysicalDeviceGroupCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceGroups" (vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPhysicalDeviceGroupCount <- lift $ peek @Word32 pPPhysicalDeviceGroupCount
  pPPhysicalDeviceGroupProperties <- ContT $ bracket (callocBytes @PhysicalDeviceGroupProperties ((fromIntegral (pPhysicalDeviceGroupCount)) * 288)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPPhysicalDeviceGroupProperties `advancePtrBytes` (i * 288) :: Ptr PhysicalDeviceGroupProperties) . ($ ())) [0..(fromIntegral (pPhysicalDeviceGroupCount)) - 1]
  r' <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceGroups" (vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) ((pPPhysicalDeviceGroupProperties)))
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
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceGroups',
-- 'Vulkan.Extensions.VK_KHR_device_group_creation.enumeratePhysicalDeviceGroupsKHR'
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- | @physicalDeviceCount@ is the number of physical devices in the group.
    physicalDeviceCount :: Word32
  , -- | @physicalDevices@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE'
    -- 'Vulkan.Core10.Handles.PhysicalDevice' handles representing all physical
    -- devices in the group. The first @physicalDeviceCount@ elements of the
    -- array will be valid.
    physicalDevices :: Vector (Ptr PhysicalDevice_T)
  , -- | @subsetAllocation@ specifies whether logical devices created from the
    -- group support allocating device memory on a subset of devices, via the
    -- @deviceMask@ member of the
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo'.
    -- If this is 'Vulkan.Core10.FundamentalTypes.FALSE', then all device
    -- memory allocations are made across all physical devices in the group. If
    -- @physicalDeviceCount@ is @1@, then @subsetAllocation@ /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE'.
    subsetAllocation :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGroupProperties)
#endif
deriving instance Show PhysicalDeviceGroupProperties

instance ToCStruct PhysicalDeviceGroupProperties where
  withCStruct x f = allocaBytesAligned 288 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGroupProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (physicalDeviceCount)
    unless ((Data.Vector.length $ (physicalDevices)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "physicalDevices is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (physicalDevices)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (subsetAllocation))
    f
  cStructSize = 288
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGroupProperties where
  peekCStruct p = do
    physicalDeviceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    physicalDevices <- generateM (MAX_DEVICE_GROUP_SIZE) (\i -> peek @(Ptr PhysicalDevice_T) (((lowerArrayPtr @(Ptr PhysicalDevice_T) ((p `plusPtr` 24 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T))))
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
-- @physicalDevice@ parameter to 'Vulkan.Core10.Device.createDevice'. In
-- particular, the device index of that physical device is zero.
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceGroupDeviceCreateInfo-pPhysicalDevices-00375# Each
--     element of @pPhysicalDevices@ /must/ be unique
--
-- -   #VUID-VkDeviceGroupDeviceCreateInfo-pPhysicalDevices-00376# All
--     elements of @pPhysicalDevices@ /must/ be in the same device group as
--     enumerated by 'enumeratePhysicalDeviceGroups'
--
-- -   #VUID-VkDeviceGroupDeviceCreateInfo-physicalDeviceCount-00377# If
--     @physicalDeviceCount@ is not @0@, the @physicalDevice@ parameter of
--     'Vulkan.Core10.Device.createDevice' /must/ be an element of
--     @pPhysicalDevices@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceGroupDeviceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO'
--
-- -   #VUID-VkDeviceGroupDeviceCreateInfo-pPhysicalDevices-parameter# If
--     @physicalDeviceCount@ is not @0@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @physicalDeviceCount@ valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handles
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- | @pPhysicalDevices@ is a pointer to an array of physical device handles
    -- belonging to the same device group.
    physicalDevices :: Vector (Ptr PhysicalDevice_T) }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupDeviceCreateInfo)
#endif
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
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

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

