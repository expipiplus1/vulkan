{-# language CPP #-}
module Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory  ( getDeviceQueue2
                                                                  , ProtectedSubmitInfo(..)
                                                                  , PhysicalDeviceProtectedMemoryFeatures(..)
                                                                  , PhysicalDeviceProtectedMemoryProperties(..)
                                                                  , DeviceQueueInfo2(..)
                                                                  , StructureType(..)
                                                                  , QueueFlagBits(..)
                                                                  , QueueFlags
                                                                  , DeviceQueueCreateFlagBits(..)
                                                                  , DeviceQueueCreateFlags
                                                                  , MemoryPropertyFlagBits(..)
                                                                  , MemoryPropertyFlags
                                                                  , BufferCreateFlagBits(..)
                                                                  , BufferCreateFlags
                                                                  , ImageCreateFlagBits(..)
                                                                  , ImageCreateFlags
                                                                  , CommandPoolCreateFlagBits(..)
                                                                  , CommandPoolCreateFlags
                                                                  ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceQueue2))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlagBits(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlagBits(..))
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlags)
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlagBits(..))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue2
  :: FunPtr (Ptr Device_T -> Ptr DeviceQueueInfo2 -> Ptr (Ptr Queue_T) -> IO ()) -> Ptr Device_T -> Ptr DeviceQueueInfo2 -> Ptr (Ptr Queue_T) -> IO ()

-- | vkGetDeviceQueue2 - Get a queue handle from a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the queue.
--
-- -   @pQueueInfo@ is a pointer to a 'DeviceQueueInfo2' structure,
--     describing the parameters used to create the device queue.
--
-- -   @pQueue@ is a pointer to a 'Vulkan.Core10.Handles.Queue' object that
--     will be filled with the handle for the requested queue.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'DeviceQueueInfo2',
-- 'Vulkan.Core10.Handles.Queue'
getDeviceQueue2 :: forall io . MonadIO io => Device -> DeviceQueueInfo2 -> io (Queue)
getDeviceQueue2 device queueInfo = liftIO . evalContT $ do
  let cmds = deviceCmds (device :: Device)
  let vkGetDeviceQueue2' = mkVkGetDeviceQueue2 (pVkGetDeviceQueue2 cmds)
  pQueueInfo <- ContT $ withCStruct (queueInfo)
  pPQueue <- ContT $ bracket (callocBytes @(Ptr Queue_T) 8) free
  lift $ vkGetDeviceQueue2' (deviceHandle (device)) pQueueInfo (pPQueue)
  pQueue <- lift $ peek @(Ptr Queue_T) pPQueue
  pure $ (((\h -> Queue h cmds ) pQueue))


-- | VkProtectedSubmitInfo - Structure indicating whether the submission is
-- protected
--
-- == Valid Usage
--
-- -   If the protected memory feature is not enabled, @protectedSubmit@
--     /must/ not be 'Vulkan.Core10.BaseType.TRUE'
--
-- -   If @protectedSubmit@ is 'Vulkan.Core10.BaseType.TRUE', then each
--     element of the @pCommandBuffers@ array /must/ be a protected command
--     buffer
--
-- -   If @protectedSubmit@ is 'Vulkan.Core10.BaseType.FALSE', then each
--     element of the @pCommandBuffers@ array /must/ be an unprotected
--     command buffer
--
-- -   If the 'Vulkan.Core10.Queue.SubmitInfo'::@pNext@ chain does not
--     include a 'ProtectedSubmitInfo' structure, then each element of the
--     command buffer of the @pCommandBuffers@ array /must/ be an
--     unprotected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO'
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ProtectedSubmitInfo = ProtectedSubmitInfo
  { -- | @protectedSubmit@ specifies whether the batch is protected. If
    -- @protectedSubmit@ is 'Vulkan.Core10.BaseType.TRUE', the batch is
    -- protected. If @protectedSubmit@ is 'Vulkan.Core10.BaseType.FALSE', the
    -- batch is unprotected. If the 'Vulkan.Core10.Queue.SubmitInfo'::@pNext@
    -- chain does not include this structure, the batch is unprotected.
    protectedSubmit :: Bool }
  deriving (Typeable)
deriving instance Show ProtectedSubmitInfo

instance ToCStruct ProtectedSubmitInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ProtectedSubmitInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedSubmit))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ProtectedSubmitInfo where
  peekCStruct p = do
    protectedSubmit <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ ProtectedSubmitInfo
             (bool32ToBool protectedSubmit)

instance Storable ProtectedSubmitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ProtectedSubmitInfo where
  zero = ProtectedSubmitInfo
           zero


-- | VkPhysicalDeviceProtectedMemoryFeatures - Structure describing protected
-- memory features that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceProtectedMemoryFeatures' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceProtectedMemoryFeatures = PhysicalDeviceProtectedMemoryFeatures
  { -- | @protectedMemory@ specifies whether protected memory is supported.
    protectedMemory :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceProtectedMemoryFeatures

instance ToCStruct PhysicalDeviceProtectedMemoryFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProtectedMemoryFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProtectedMemoryFeatures where
  peekCStruct p = do
    protectedMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceProtectedMemoryFeatures
             (bool32ToBool protectedMemory)

instance Storable PhysicalDeviceProtectedMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProtectedMemoryFeatures where
  zero = PhysicalDeviceProtectedMemoryFeatures
           zero


-- | VkPhysicalDeviceProtectedMemoryProperties - Structure describing
-- protected memory properties that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceProtectedMemoryProperties' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with a value indicating the implementation-dependent
-- behavior.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceProtectedMemoryProperties = PhysicalDeviceProtectedMemoryProperties
  { -- | @protectedNoFault@ specifies the behavior of the implementation when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-protected-access-rules protected memory access rules>
    -- are broken. If @protectedNoFault@ is 'Vulkan.Core10.BaseType.TRUE',
    -- breaking those rules will not result in process termination or device
    -- loss.
    protectedNoFault :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceProtectedMemoryProperties

instance ToCStruct PhysicalDeviceProtectedMemoryProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProtectedMemoryProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedNoFault))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProtectedMemoryProperties where
  peekCStruct p = do
    protectedNoFault <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceProtectedMemoryProperties
             (bool32ToBool protectedNoFault)

instance Storable PhysicalDeviceProtectedMemoryProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProtectedMemoryProperties where
  zero = PhysicalDeviceProtectedMemoryProperties
           zero


-- | VkDeviceQueueInfo2 - Structure specifying the parameters used for device
-- queue creation
--
-- = Description
--
-- The queue returned by 'getDeviceQueue2' /must/ have the same @flags@
-- value from this structure as that used at device creation time in a
-- 'Vulkan.Core10.Device.DeviceQueueCreateInfo' instance. If no matching
-- @flags@ were specified at device creation time then @pQueue@ will return
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'getDeviceQueue2'
data DeviceQueueInfo2 = DeviceQueueInfo2
  { -- | @flags@ /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits'
    -- values
    flags :: DeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ /must/ be one of the queue family indices specified
    -- when @device@ was created, via the
    -- 'Vulkan.Core10.Device.DeviceQueueCreateInfo' structure
    queueFamilyIndex :: Word32
  , -- | @queueIndex@ /must/ be less than the number of queues created for the
    -- specified queue family index and
    -- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags'
    -- member @flags@ equal to this @flags@ value when @device@ was created,
    -- via the @queueCount@ member of the
    -- 'Vulkan.Core10.Device.DeviceQueueCreateInfo' structure
    queueIndex :: Word32
  }
  deriving (Typeable)
deriving instance Show DeviceQueueInfo2

instance ToCStruct DeviceQueueInfo2 where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueInfo2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (queueIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceQueueInfo2 where
  peekCStruct p = do
    flags <- peek @DeviceQueueCreateFlags ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    queueIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DeviceQueueInfo2
             flags queueFamilyIndex queueIndex

instance Storable DeviceQueueInfo2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueInfo2 where
  zero = DeviceQueueInfo2
           zero
           zero
           zero

