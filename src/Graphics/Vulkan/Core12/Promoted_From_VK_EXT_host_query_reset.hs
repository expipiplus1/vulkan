{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset  ( resetQueryPool
                                                                     , PhysicalDeviceHostQueryResetFeatures(..)
                                                                     , StructureType(..)
                                                                     ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkResetQueryPool))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (QueryPool)
import Graphics.Vulkan.Core10.Handles (QueryPool(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr Device_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkResetQueryPool - Reset queries in a query pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the query pool.
--
-- -   @queryPool@ is the handle of the query pool managing the queries
--     being reset.
--
-- -   @firstQuery@ is the initial query index to reset.
--
-- -   @queryCount@ is the number of queries to reset.
--
-- = Description
--
-- This command sets the status of query indices [@firstQuery@,
-- @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- If @queryPool@ is
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
-- this command sets the status of query indices [@firstQuery@,
-- @firstQuery@ + @queryCount@ - 1] to unavailable for each pass.
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
--     'Graphics.Vulkan.Core10.Query.getQueryPoolResults' or
--     'resetQueryPool' in other threads
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   @queryPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
resetQueryPool :: forall io . MonadIO io => Device -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> io ()
resetQueryPool device queryPool firstQuery queryCount = liftIO $ do
  let vkResetQueryPool' = mkVkResetQueryPool (pVkResetQueryPool (deviceCmds (device :: Device)))
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
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceHostQueryResetFeatures' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostQueryResetFeatures = PhysicalDeviceHostQueryResetFeatures
  { -- | @hostQueryReset@ indicates that the implementation supports resetting
    -- queries from the host with 'resetQueryPool'.
    hostQueryReset :: Bool }
  deriving (Typeable)
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

