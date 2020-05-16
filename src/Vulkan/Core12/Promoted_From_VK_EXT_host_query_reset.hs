{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset  ( resetQueryPool
                                                            , PhysicalDeviceHostQueryResetFeatures(..)
                                                            , StructureType(..)
                                                            ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkResetQueryPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkResetQueryPool - Reset queries in a query pool
--
-- = Description
--
-- This command sets the status of query indices [@firstQuery@,
-- @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- If @queryPool@ is
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' this
-- command sets the status of query indices [@firstQuery@, @firstQuery@ +
-- @queryCount@ - 1] to unavailable for each pass.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-hostQueryReset hostQueryReset>
--     feature /must/ be enabled
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   Submitted commands that refer to the range specified by @firstQuery@
--     and @queryCount@ in @queryPool@ /must/ have completed execution
--
-- -   The range of queries specified by @firstQuery@ and @queryCount@ in
--     @queryPool@ /must/ not be in use by calls to
--     'Vulkan.Core10.Query.getQueryPoolResults' or 'resetQueryPool' in
--     other threads
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   @queryPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.QueryPool'
resetQueryPool :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that owns the query pool.
                  Device
               -> -- | @queryPool@ is the handle of the query pool managing the queries being
                  -- reset.
                  QueryPool
               -> -- | @firstQuery@ is the initial query index to reset.
                  ("firstQuery" ::: Word32)
               -> -- | @queryCount@ is the number of queries to reset.
                  ("queryCount" ::: Word32)
               -> io ()
resetQueryPool device queryPool firstQuery queryCount = liftIO $ do
  let vkResetQueryPoolPtr = pVkResetQueryPool (deviceCmds (device :: Device))
  unless (vkResetQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetQueryPool is null" Nothing Nothing
  let vkResetQueryPool' = mkVkResetQueryPool vkResetQueryPoolPtr
  vkResetQueryPool' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount)
  pure $ ()


-- | VkPhysicalDeviceHostQueryResetFeatures - Structure describing whether
-- queries can be reset from the host
--
-- = Members
--
-- The members of the 'PhysicalDeviceHostQueryResetFeatures' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceHostQueryResetFeatures' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceHostQueryResetFeatures' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostQueryResetFeatures = PhysicalDeviceHostQueryResetFeatures
  { -- | @hostQueryReset@ indicates that the implementation supports resetting
    -- queries from the host with 'resetQueryPool'.
    hostQueryReset :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostQueryResetFeatures)
#endif
deriving instance Show PhysicalDeviceHostQueryResetFeatures

instance ToCStruct PhysicalDeviceHostQueryResetFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostQueryResetFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hostQueryReset))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostQueryResetFeatures where
  peekCStruct p = do
    hostQueryReset <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceHostQueryResetFeatures
             (bool32ToBool hostQueryReset)

instance Storable PhysicalDeviceHostQueryResetFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostQueryResetFeatures where
  zero = PhysicalDeviceHostQueryResetFeatures
           zero

