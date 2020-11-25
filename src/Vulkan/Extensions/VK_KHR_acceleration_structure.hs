{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_acceleration_structure"
module Vulkan.Extensions.VK_KHR_acceleration_structure  ( destroyAccelerationStructureKHR
                                                        , cmdCopyAccelerationStructureKHR
                                                        , copyAccelerationStructureKHR
                                                        , cmdCopyAccelerationStructureToMemoryKHR
                                                        , copyAccelerationStructureToMemoryKHR
                                                        , cmdCopyMemoryToAccelerationStructureKHR
                                                        , copyMemoryToAccelerationStructureKHR
                                                        , cmdWriteAccelerationStructuresPropertiesKHR
                                                        , writeAccelerationStructuresPropertiesKHR
                                                        , getDeviceAccelerationStructureCompatibilityKHR
                                                        , createAccelerationStructureKHR
                                                        , withAccelerationStructureKHR
                                                        , cmdBuildAccelerationStructuresKHR
                                                        , cmdBuildAccelerationStructuresIndirectKHR
                                                        , buildAccelerationStructuresKHR
                                                        , getAccelerationStructureDeviceAddressKHR
                                                        , getAccelerationStructureBuildSizesKHR
                                                        , WriteDescriptorSetAccelerationStructureKHR(..)
                                                        , PhysicalDeviceAccelerationStructureFeaturesKHR(..)
                                                        , PhysicalDeviceAccelerationStructurePropertiesKHR(..)
                                                        , AccelerationStructureGeometryTrianglesDataKHR(..)
                                                        , AccelerationStructureGeometryAabbsDataKHR(..)
                                                        , AccelerationStructureGeometryInstancesDataKHR(..)
                                                        , AccelerationStructureGeometryKHR(..)
                                                        , AccelerationStructureBuildGeometryInfoKHR(..)
                                                        , AccelerationStructureBuildRangeInfoKHR(..)
                                                        , AccelerationStructureCreateInfoKHR(..)
                                                        , AabbPositionsKHR(..)
                                                        , TransformMatrixKHR(..)
                                                        , AccelerationStructureInstanceKHR(..)
                                                        , AccelerationStructureDeviceAddressInfoKHR(..)
                                                        , AccelerationStructureVersionInfoKHR(..)
                                                        , CopyAccelerationStructureInfoKHR(..)
                                                        , CopyAccelerationStructureToMemoryInfoKHR(..)
                                                        , CopyMemoryToAccelerationStructureInfoKHR(..)
                                                        , AccelerationStructureBuildSizesInfoKHR(..)
                                                        , DeviceOrHostAddressKHR(..)
                                                        , DeviceOrHostAddressConstKHR(..)
                                                        , AccelerationStructureGeometryDataKHR(..)
                                                        , GeometryInstanceFlagsKHR
                                                        , GeometryInstanceFlagBitsKHR( GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR
                                                                                     , ..
                                                                                     )
                                                        , GeometryFlagsKHR
                                                        , GeometryFlagBitsKHR( GEOMETRY_OPAQUE_BIT_KHR
                                                                             , GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR
                                                                             , ..
                                                                             )
                                                        , BuildAccelerationStructureFlagsKHR
                                                        , BuildAccelerationStructureFlagBitsKHR( BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR
                                                                                               , ..
                                                                                               )
                                                        , AccelerationStructureCreateFlagsKHR
                                                        , AccelerationStructureCreateFlagBitsKHR( ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR
                                                                                                , ..
                                                                                                )
                                                        , CopyAccelerationStructureModeKHR( COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR
                                                                                          , ..
                                                                                          )
                                                        , BuildAccelerationStructureModeKHR( BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
                                                                                           , BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR
                                                                                           , ..
                                                                                           )
                                                        , AccelerationStructureTypeKHR( ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                                                                                      , ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
                                                                                      , ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR
                                                                                      , ..
                                                                                      )
                                                        , GeometryTypeKHR( GEOMETRY_TYPE_TRIANGLES_KHR
                                                                         , GEOMETRY_TYPE_AABBS_KHR
                                                                         , GEOMETRY_TYPE_INSTANCES_KHR
                                                                         , ..
                                                                         )
                                                        , AccelerationStructureBuildTypeKHR( ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
                                                                                           , ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
                                                                                           , ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR
                                                                                           , ..
                                                                                           )
                                                        , AccelerationStructureCompatibilityKHR( ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR
                                                                                               , ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR
                                                                                               , ..
                                                                                               )
                                                        , KHR_ACCELERATION_STRUCTURE_SPEC_VERSION
                                                        , pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION
                                                        , KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                                                        , pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                                                        , AccelerationStructureKHR(..)
                                                        , DeferredOperationKHR(..)
                                                        , DebugReportObjectTypeEXT(..)
                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import qualified Data.ByteString (length)
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkBuildAccelerationStructuresKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructuresIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructuresKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureBuildSizesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureDeviceAddressKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceAccelerationStructureCompatibilityKHR))
import Vulkan.Dynamic (DeviceCmds(pVkWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (pattern UUID_SIZE)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyAccelerationStructureKHR"
destroyAccelerationStructureKHR :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkDestroyAccelerationStructureKHR" "device"
                                   Device
                                -> -- No documentation found for Nested "vkDestroyAccelerationStructureKHR" "accelerationStructure"
                                   AccelerationStructureKHR
                                -> -- No documentation found for Nested "vkDestroyAccelerationStructureKHR" "pAllocator"
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyAccelerationStructureKHR device accelerationStructure allocator = liftIO . evalContT $ do
  let vkDestroyAccelerationStructureKHRPtr = pVkDestroyAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkDestroyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyAccelerationStructureKHR is null" Nothing Nothing
  let vkDestroyAccelerationStructureKHR' = mkVkDestroyAccelerationStructureKHR vkDestroyAccelerationStructureKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyAccelerationStructureKHR' (deviceHandle (device)) (accelerationStructure) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureInfoKHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyAccelerationStructureKHR"
cmdCopyAccelerationStructureKHR :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkCmdCopyAccelerationStructureKHR" "commandBuffer"
                                   CommandBuffer
                                -> -- No documentation found for Nested "vkCmdCopyAccelerationStructureKHR" "pInfo"
                                   CopyAccelerationStructureInfoKHR
                                -> io ()
cmdCopyAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureKHRPtr = pVkCmdCopyAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureKHR is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureKHR' = mkVkCmdCopyAccelerationStructureKHR vkCmdCopyAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureInfoKHR -> IO Result

-- No documentation found for TopLevel "vkCopyAccelerationStructureKHR"
copyAccelerationStructureKHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCopyAccelerationStructureKHR" "device"
                                Device
                             -> -- No documentation found for Nested "vkCopyAccelerationStructureKHR" "deferredOperation"
                                DeferredOperationKHR
                             -> -- No documentation found for Nested "vkCopyAccelerationStructureKHR" "pInfo"
                                CopyAccelerationStructureInfoKHR
                             -> io (Result)
copyAccelerationStructureKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureKHRPtr = pVkCopyAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyAccelerationStructureKHR is null" Nothing Nothing
  let vkCopyAccelerationStructureKHR' = mkVkCopyAccelerationStructureKHR vkCopyAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyAccelerationStructureKHR' (deviceHandle (device)) (deferredOperation) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyAccelerationStructureToMemoryKHR"
cmdCopyAccelerationStructureToMemoryKHR :: forall io
                                         . (MonadIO io)
                                        => -- No documentation found for Nested "vkCmdCopyAccelerationStructureToMemoryKHR" "commandBuffer"
                                           CommandBuffer
                                        -> -- No documentation found for Nested "vkCmdCopyAccelerationStructureToMemoryKHR" "pInfo"
                                           CopyAccelerationStructureToMemoryInfoKHR
                                        -> io ()
cmdCopyAccelerationStructureToMemoryKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureToMemoryKHRPtr = pVkCmdCopyAccelerationStructureToMemoryKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyAccelerationStructureToMemoryKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureToMemoryKHR is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureToMemoryKHR' = mkVkCmdCopyAccelerationStructureToMemoryKHR vkCmdCopyAccelerationStructureToMemoryKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyAccelerationStructureToMemoryKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO Result

-- No documentation found for TopLevel "vkCopyAccelerationStructureToMemoryKHR"
copyAccelerationStructureToMemoryKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkCopyAccelerationStructureToMemoryKHR" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkCopyAccelerationStructureToMemoryKHR" "deferredOperation"
                                        DeferredOperationKHR
                                     -> -- No documentation found for Nested "vkCopyAccelerationStructureToMemoryKHR" "pInfo"
                                        CopyAccelerationStructureToMemoryInfoKHR
                                     -> io (Result)
copyAccelerationStructureToMemoryKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureToMemoryKHRPtr = pVkCopyAccelerationStructureToMemoryKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyAccelerationStructureToMemoryKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyAccelerationStructureToMemoryKHR is null" Nothing Nothing
  let vkCopyAccelerationStructureToMemoryKHR' = mkVkCopyAccelerationStructureToMemoryKHR vkCopyAccelerationStructureToMemoryKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyAccelerationStructureToMemoryKHR' (deviceHandle (device)) (deferredOperation) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyMemoryToAccelerationStructureKHR"
cmdCopyMemoryToAccelerationStructureKHR :: forall io
                                         . (MonadIO io)
                                        => -- No documentation found for Nested "vkCmdCopyMemoryToAccelerationStructureKHR" "commandBuffer"
                                           CommandBuffer
                                        -> -- No documentation found for Nested "vkCmdCopyMemoryToAccelerationStructureKHR" "pInfo"
                                           CopyMemoryToAccelerationStructureInfoKHR
                                        -> io ()
cmdCopyMemoryToAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMemoryToAccelerationStructureKHRPtr = pVkCmdCopyMemoryToAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyMemoryToAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryToAccelerationStructureKHR is null" Nothing Nothing
  let vkCmdCopyMemoryToAccelerationStructureKHR' = mkVkCmdCopyMemoryToAccelerationStructureKHR vkCmdCopyMemoryToAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyMemoryToAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO Result

-- No documentation found for TopLevel "vkCopyMemoryToAccelerationStructureKHR"
copyMemoryToAccelerationStructureKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkCopyMemoryToAccelerationStructureKHR" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkCopyMemoryToAccelerationStructureKHR" "deferredOperation"
                                        DeferredOperationKHR
                                     -> -- No documentation found for Nested "vkCopyMemoryToAccelerationStructureKHR" "pInfo"
                                        CopyMemoryToAccelerationStructureInfoKHR
                                     -> io (Result)
copyMemoryToAccelerationStructureKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyMemoryToAccelerationStructureKHRPtr = pVkCopyMemoryToAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyMemoryToAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMemoryToAccelerationStructureKHR is null" Nothing Nothing
  let vkCopyMemoryToAccelerationStructureKHR' = mkVkCopyMemoryToAccelerationStructureKHR vkCopyMemoryToAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyMemoryToAccelerationStructureKHR' (deviceHandle (device)) (deferredOperation) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdWriteAccelerationStructuresPropertiesKHR"
cmdWriteAccelerationStructuresPropertiesKHR :: forall io
                                             . (MonadIO io)
                                            => -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesKHR" "commandBuffer"
                                               CommandBuffer
                                            -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesKHR" "pAccelerationStructures"
                                               ("accelerationStructures" ::: Vector AccelerationStructureKHR)
                                            -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesKHR" "queryType"
                                               QueryType
                                            -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesKHR" "queryPool"
                                               QueryPool
                                            -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesKHR" "firstQuery"
                                               ("firstQuery" ::: Word32)
                                            -> io ()
cmdWriteAccelerationStructuresPropertiesKHR commandBuffer accelerationStructures queryType queryPool firstQuery = liftIO . evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesKHRPtr = pVkCmdWriteAccelerationStructuresPropertiesKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWriteAccelerationStructuresPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteAccelerationStructuresPropertiesKHR is null" Nothing Nothing
  let vkCmdWriteAccelerationStructuresPropertiesKHR' = mkVkCmdWriteAccelerationStructuresPropertiesKHR vkCmdWriteAccelerationStructuresPropertiesKHRPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  lift $ vkCmdWriteAccelerationStructuresPropertiesKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result) -> Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result

-- No documentation found for TopLevel "vkWriteAccelerationStructuresPropertiesKHR"
writeAccelerationStructuresPropertiesKHR :: forall io
                                          . (MonadIO io)
                                         => -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "device"
                                            Device
                                         -> -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "pAccelerationStructures"
                                            ("accelerationStructures" ::: Vector AccelerationStructureKHR)
                                         -> -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "queryType"
                                            QueryType
                                         -> -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "dataSize"
                                            ("dataSize" ::: Word64)
                                         -> -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "pData"
                                            ("data" ::: Ptr ())
                                         -> -- No documentation found for Nested "vkWriteAccelerationStructuresPropertiesKHR" "stride"
                                            ("stride" ::: Word64)
                                         -> io ()
writeAccelerationStructuresPropertiesKHR device accelerationStructures queryType dataSize data' stride = liftIO . evalContT $ do
  let vkWriteAccelerationStructuresPropertiesKHRPtr = pVkWriteAccelerationStructuresPropertiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkWriteAccelerationStructuresPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWriteAccelerationStructuresPropertiesKHR is null" Nothing Nothing
  let vkWriteAccelerationStructuresPropertiesKHR' = mkVkWriteAccelerationStructuresPropertiesKHR vkWriteAccelerationStructuresPropertiesKHRPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  r <- lift $ vkWriteAccelerationStructuresPropertiesKHR' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (CSize (dataSize)) (data') (CSize (stride))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceAccelerationStructureCompatibilityKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureVersionInfoKHR -> Ptr AccelerationStructureCompatibilityKHR -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureVersionInfoKHR -> Ptr AccelerationStructureCompatibilityKHR -> IO ()

-- No documentation found for TopLevel "vkGetDeviceAccelerationStructureCompatibilityKHR"
getDeviceAccelerationStructureCompatibilityKHR :: forall io
                                                . (MonadIO io)
                                               => -- No documentation found for Nested "vkGetDeviceAccelerationStructureCompatibilityKHR" "device"
                                                  Device
                                               -> -- No documentation found for Nested "vkGetDeviceAccelerationStructureCompatibilityKHR" "pVersionInfo"
                                                  AccelerationStructureVersionInfoKHR
                                               -> io (AccelerationStructureCompatibilityKHR)
getDeviceAccelerationStructureCompatibilityKHR device versionInfo = liftIO . evalContT $ do
  let vkGetDeviceAccelerationStructureCompatibilityKHRPtr = pVkGetDeviceAccelerationStructureCompatibilityKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceAccelerationStructureCompatibilityKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceAccelerationStructureCompatibilityKHR is null" Nothing Nothing
  let vkGetDeviceAccelerationStructureCompatibilityKHR' = mkVkGetDeviceAccelerationStructureCompatibilityKHR vkGetDeviceAccelerationStructureCompatibilityKHRPtr
  pVersionInfo <- ContT $ withCStruct (versionInfo)
  pPCompatibility <- ContT $ bracket (callocBytes @AccelerationStructureCompatibilityKHR 4) free
  lift $ vkGetDeviceAccelerationStructureCompatibilityKHR' (deviceHandle (device)) pVersionInfo (pPCompatibility)
  pCompatibility <- lift $ peek @AccelerationStructureCompatibilityKHR pPCompatibility
  pure $ (pCompatibility)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result

-- No documentation found for TopLevel "vkCreateAccelerationStructureKHR"
createAccelerationStructureKHR :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCreateAccelerationStructureKHR" "device"
                                  Device
                               -> -- No documentation found for Nested "vkCreateAccelerationStructureKHR" "pCreateInfo"
                                  AccelerationStructureCreateInfoKHR
                               -> -- No documentation found for Nested "vkCreateAccelerationStructureKHR" "pAllocator"
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (AccelerationStructureKHR)
createAccelerationStructureKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreateAccelerationStructureKHRPtr = pVkCreateAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAccelerationStructureKHR is null" Nothing Nothing
  let vkCreateAccelerationStructureKHR' = mkVkCreateAccelerationStructureKHR vkCreateAccelerationStructureKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureKHR 8) free
  r <- lift $ vkCreateAccelerationStructureKHR' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureKHR pPAccelerationStructure
  pure $ (pAccelerationStructure)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAccelerationStructureKHR' and 'destroyAccelerationStructureKHR'
--
-- To ensure that 'destroyAccelerationStructureKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAccelerationStructureKHR :: forall io r . MonadIO io => Device -> AccelerationStructureCreateInfoKHR -> Maybe AllocationCallbacks -> (io AccelerationStructureKHR -> (AccelerationStructureKHR -> io ()) -> r) -> r
withAccelerationStructureKHR device pCreateInfo pAllocator b =
  b (createAccelerationStructureKHR device pCreateInfo pAllocator)
    (\(o0) -> destroyAccelerationStructureKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructuresKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO ()

-- No documentation found for TopLevel "vkCmdBuildAccelerationStructuresKHR"
cmdBuildAccelerationStructuresKHR :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkCmdBuildAccelerationStructuresKHR" "commandBuffer"
                                     CommandBuffer
                                  -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresKHR" "pInfos"
                                     ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                                  -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresKHR" "ppBuildRangeInfos"
                                     ("buildRangeInfos" ::: Vector (Vector AccelerationStructureBuildRangeInfoKHR))
                                  -> io ()
cmdBuildAccelerationStructuresKHR commandBuffer infos buildRangeInfos = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructuresKHRPtr = pVkCmdBuildAccelerationStructuresKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructuresKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructuresKHR is null" Nothing Nothing
  let vkCmdBuildAccelerationStructuresKHR' = mkVkCmdBuildAccelerationStructuresKHR vkCmdBuildAccelerationStructuresKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (buildRangeInfos)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppBuildRangeInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPpBuildRangeInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildRangeInfoKHR) ((Data.Vector.length (buildRangeInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpBuildRangeInfos' <- ContT $ allocaBytesAligned @AccelerationStructureBuildRangeInfoKHR ((Data.Vector.length (e)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPpBuildRangeInfos' `plusPtr` (16 * (i)) :: Ptr AccelerationStructureBuildRangeInfoKHR) (e)) (e)
    lift $ poke (pPpBuildRangeInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) (pPpBuildRangeInfos')) (buildRangeInfos)
  lift $ vkCmdBuildAccelerationStructuresKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpBuildRangeInfos)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructuresIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr DeviceAddress -> Ptr Word32 -> Ptr (Ptr Word32) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr DeviceAddress -> Ptr Word32 -> Ptr (Ptr Word32) -> IO ()

-- No documentation found for TopLevel "vkCmdBuildAccelerationStructuresIndirectKHR"
cmdBuildAccelerationStructuresIndirectKHR :: forall io
                                           . (MonadIO io)
                                          => -- No documentation found for Nested "vkCmdBuildAccelerationStructuresIndirectKHR" "commandBuffer"
                                             CommandBuffer
                                          -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresIndirectKHR" "pInfos"
                                             ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                                          -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresIndirectKHR" "pIndirectDeviceAddresses"
                                             ("indirectDeviceAddresses" ::: Vector DeviceAddress)
                                          -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresIndirectKHR" "pIndirectStrides"
                                             ("indirectStrides" ::: Vector Word32)
                                          -> -- No documentation found for Nested "vkCmdBuildAccelerationStructuresIndirectKHR" "ppMaxPrimitiveCounts"
                                             ("maxPrimitiveCounts" ::: Vector (Vector Word32))
                                          -> io ()
cmdBuildAccelerationStructuresIndirectKHR commandBuffer infos indirectDeviceAddresses indirectStrides maxPrimitiveCounts = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructuresIndirectKHRPtr = pVkCmdBuildAccelerationStructuresIndirectKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructuresIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructuresIndirectKHR is null" Nothing Nothing
  let vkCmdBuildAccelerationStructuresIndirectKHR' = mkVkCmdBuildAccelerationStructuresIndirectKHR vkCmdBuildAccelerationStructuresIndirectKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (indirectDeviceAddresses)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pIndirectDeviceAddresses and pInfos must have the same length" Nothing Nothing
  lift $ unless ((Data.Vector.length $ (indirectStrides)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pIndirectStrides and pInfos must have the same length" Nothing Nothing
  lift $ unless ((Data.Vector.length $ (maxPrimitiveCounts)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppMaxPrimitiveCounts and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPIndirectDeviceAddresses <- ContT $ allocaBytesAligned @DeviceAddress ((Data.Vector.length (indirectDeviceAddresses)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPIndirectDeviceAddresses `plusPtr` (8 * (i)) :: Ptr DeviceAddress) (e)) (indirectDeviceAddresses)
  pPIndirectStrides <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (indirectStrides)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPIndirectStrides `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indirectStrides)
  pPpMaxPrimitiveCounts <- ContT $ allocaBytesAligned @(Ptr Word32) ((Data.Vector.length (maxPrimitiveCounts)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpMaxPrimitiveCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (e)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPpMaxPrimitiveCounts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (e)
    lift $ poke (pPpMaxPrimitiveCounts `plusPtr` (8 * (i)) :: Ptr (Ptr Word32)) (pPpMaxPrimitiveCounts')) (maxPrimitiveCounts)
  lift $ vkCmdBuildAccelerationStructuresIndirectKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPIndirectDeviceAddresses) (pPIndirectStrides) (pPpMaxPrimitiveCounts)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBuildAccelerationStructuresKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO Result

-- No documentation found for TopLevel "vkBuildAccelerationStructuresKHR"
buildAccelerationStructuresKHR :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkBuildAccelerationStructuresKHR" "device"
                                  Device
                               -> -- No documentation found for Nested "vkBuildAccelerationStructuresKHR" "deferredOperation"
                                  DeferredOperationKHR
                               -> -- No documentation found for Nested "vkBuildAccelerationStructuresKHR" "pInfos"
                                  ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                               -> -- No documentation found for Nested "vkBuildAccelerationStructuresKHR" "ppBuildRangeInfos"
                                  ("buildRangeInfos" ::: Vector (Vector AccelerationStructureBuildRangeInfoKHR))
                               -> io (Result)
buildAccelerationStructuresKHR device deferredOperation infos buildRangeInfos = liftIO . evalContT $ do
  let vkBuildAccelerationStructuresKHRPtr = pVkBuildAccelerationStructuresKHR (deviceCmds (device :: Device))
  lift $ unless (vkBuildAccelerationStructuresKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBuildAccelerationStructuresKHR is null" Nothing Nothing
  let vkBuildAccelerationStructuresKHR' = mkVkBuildAccelerationStructuresKHR vkBuildAccelerationStructuresKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (buildRangeInfos)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppBuildRangeInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPpBuildRangeInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildRangeInfoKHR) ((Data.Vector.length (buildRangeInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpBuildRangeInfos' <- ContT $ allocaBytesAligned @AccelerationStructureBuildRangeInfoKHR ((Data.Vector.length (e)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPpBuildRangeInfos' `plusPtr` (16 * (i)) :: Ptr AccelerationStructureBuildRangeInfoKHR) (e)) (e)
    lift $ poke (pPpBuildRangeInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) (pPpBuildRangeInfos')) (buildRangeInfos)
  r <- lift $ vkBuildAccelerationStructuresKHR' (deviceHandle (device)) (deferredOperation) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpBuildRangeInfos)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureDeviceAddressKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress) -> Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress

-- No documentation found for TopLevel "vkGetAccelerationStructureDeviceAddressKHR"
getAccelerationStructureDeviceAddressKHR :: forall io
                                          . (MonadIO io)
                                         => -- No documentation found for Nested "vkGetAccelerationStructureDeviceAddressKHR" "device"
                                            Device
                                         -> -- No documentation found for Nested "vkGetAccelerationStructureDeviceAddressKHR" "pInfo"
                                            AccelerationStructureDeviceAddressInfoKHR
                                         -> io (DeviceAddress)
getAccelerationStructureDeviceAddressKHR device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureDeviceAddressKHRPtr = pVkGetAccelerationStructureDeviceAddressKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureDeviceAddressKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureDeviceAddressKHR is null" Nothing Nothing
  let vkGetAccelerationStructureDeviceAddressKHR' = mkVkGetAccelerationStructureDeviceAddressKHR vkGetAccelerationStructureDeviceAddressKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetAccelerationStructureDeviceAddressKHR' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureBuildSizesKHR
  :: FunPtr (Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr Word32 -> Ptr AccelerationStructureBuildSizesInfoKHR -> IO ()) -> Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr Word32 -> Ptr AccelerationStructureBuildSizesInfoKHR -> IO ()

-- No documentation found for TopLevel "vkGetAccelerationStructureBuildSizesKHR"
getAccelerationStructureBuildSizesKHR :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkGetAccelerationStructureBuildSizesKHR" "device"
                                         Device
                                      -> -- No documentation found for Nested "vkGetAccelerationStructureBuildSizesKHR" "buildType"
                                         AccelerationStructureBuildTypeKHR
                                      -> -- No documentation found for Nested "vkGetAccelerationStructureBuildSizesKHR" "pBuildInfo"
                                         ("buildInfo" ::: AccelerationStructureBuildGeometryInfoKHR)
                                      -> -- No documentation found for Nested "vkGetAccelerationStructureBuildSizesKHR" "pMaxPrimitiveCounts"
                                         ("maxPrimitiveCounts" ::: Vector Word32)
                                      -> io (("sizeInfo" ::: AccelerationStructureBuildSizesInfoKHR))
getAccelerationStructureBuildSizesKHR device buildType buildInfo maxPrimitiveCounts = liftIO . evalContT $ do
  let vkGetAccelerationStructureBuildSizesKHRPtr = pVkGetAccelerationStructureBuildSizesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureBuildSizesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureBuildSizesKHR is null" Nothing Nothing
  let vkGetAccelerationStructureBuildSizesKHR' = mkVkGetAccelerationStructureBuildSizesKHR vkGetAccelerationStructureBuildSizesKHRPtr
  pBuildInfo <- ContT $ withCStruct (buildInfo)
  pPMaxPrimitiveCounts <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (maxPrimitiveCounts)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMaxPrimitiveCounts `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (maxPrimitiveCounts)
  pPSizeInfo <- ContT (withZeroCStruct @AccelerationStructureBuildSizesInfoKHR)
  lift $ vkGetAccelerationStructureBuildSizesKHR' (deviceHandle (device)) (buildType) pBuildInfo (pPMaxPrimitiveCounts) (pPSizeInfo)
  pSizeInfo <- lift $ peekCStruct @AccelerationStructureBuildSizesInfoKHR pPSizeInfo
  pure $ (pSizeInfo)



-- No documentation found for TopLevel "VkWriteDescriptorSetAccelerationStructureKHR"
data WriteDescriptorSetAccelerationStructureKHR = WriteDescriptorSetAccelerationStructureKHR
  { -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureKHR" "pAccelerationStructures"
    accelerationStructures :: Vector AccelerationStructureKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetAccelerationStructureKHR)
#endif
deriving instance Show WriteDescriptorSetAccelerationStructureKHR

instance ToCStruct WriteDescriptorSetAccelerationStructureKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR))) (pPAccelerationStructures')
    lift $ f

instance FromCStruct WriteDescriptorSetAccelerationStructureKHR where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureKHR) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureKHR ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureKHR)))
    pure $ WriteDescriptorSetAccelerationStructureKHR
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureKHR where
  zero = WriteDescriptorSetAccelerationStructureKHR
           mempty



-- No documentation found for TopLevel "VkPhysicalDeviceAccelerationStructureFeaturesKHR"
data PhysicalDeviceAccelerationStructureFeaturesKHR = PhysicalDeviceAccelerationStructureFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructureFeaturesKHR" "accelerationStructure"
    accelerationStructure :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructureFeaturesKHR" "accelerationStructureCaptureReplay"
    accelerationStructureCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructureFeaturesKHR" "accelerationStructureIndirectBuild"
    accelerationStructureIndirectBuild :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructureFeaturesKHR" "accelerationStructureHostCommands"
    accelerationStructureHostCommands :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructureFeaturesKHR" "descriptorBindingAccelerationStructureUpdateAfterBind"
    descriptorBindingAccelerationStructureUpdateAfterBind :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAccelerationStructureFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceAccelerationStructureFeaturesKHR

instance ToCStruct PhysicalDeviceAccelerationStructureFeaturesKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAccelerationStructureFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (accelerationStructure))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (accelerationStructureCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (accelerationStructureIndirectBuild))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (accelerationStructureHostCommands))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (descriptorBindingAccelerationStructureUpdateAfterBind))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAccelerationStructureFeaturesKHR where
  peekCStruct p = do
    accelerationStructure <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    accelerationStructureCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    accelerationStructureIndirectBuild <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    accelerationStructureHostCommands <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    descriptorBindingAccelerationStructureUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceAccelerationStructureFeaturesKHR
             (bool32ToBool accelerationStructure) (bool32ToBool accelerationStructureCaptureReplay) (bool32ToBool accelerationStructureIndirectBuild) (bool32ToBool accelerationStructureHostCommands) (bool32ToBool descriptorBindingAccelerationStructureUpdateAfterBind)


instance Storable PhysicalDeviceAccelerationStructureFeaturesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAccelerationStructureFeaturesKHR where
  zero = PhysicalDeviceAccelerationStructureFeaturesKHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceAccelerationStructurePropertiesKHR"
data PhysicalDeviceAccelerationStructurePropertiesKHR = PhysicalDeviceAccelerationStructurePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxGeometryCount"
    maxGeometryCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxInstanceCount"
    maxInstanceCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxPrimitiveCount"
    maxPrimitiveCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxPerStageDescriptorAccelerationStructures"
    maxPerStageDescriptorAccelerationStructures :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxPerStageDescriptorUpdateAfterBindAccelerationStructures"
    maxPerStageDescriptorUpdateAfterBindAccelerationStructures :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxDescriptorSetAccelerationStructures"
    maxDescriptorSetAccelerationStructures :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "maxDescriptorSetUpdateAfterBindAccelerationStructures"
    maxDescriptorSetUpdateAfterBindAccelerationStructures :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceAccelerationStructurePropertiesKHR" "minAccelerationStructureScratchOffsetAlignment"
    minAccelerationStructureScratchOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAccelerationStructurePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceAccelerationStructurePropertiesKHR

instance ToCStruct PhysicalDeviceAccelerationStructurePropertiesKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAccelerationStructurePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxPrimitiveCount)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxPerStageDescriptorAccelerationStructures)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindAccelerationStructures)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindAccelerationStructures)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (minAccelerationStructureScratchOffsetAlignment)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceAccelerationStructurePropertiesKHR where
  peekCStruct p = do
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    maxPrimitiveCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxPerStageDescriptorAccelerationStructures <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindAccelerationStructures <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindAccelerationStructures <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    minAccelerationStructureScratchOffsetAlignment <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceAccelerationStructurePropertiesKHR
             maxGeometryCount maxInstanceCount maxPrimitiveCount maxPerStageDescriptorAccelerationStructures maxPerStageDescriptorUpdateAfterBindAccelerationStructures maxDescriptorSetAccelerationStructures maxDescriptorSetUpdateAfterBindAccelerationStructures minAccelerationStructureScratchOffsetAlignment


instance Storable PhysicalDeviceAccelerationStructurePropertiesKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAccelerationStructurePropertiesKHR where
  zero = PhysicalDeviceAccelerationStructurePropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureGeometryTrianglesDataKHR"
data AccelerationStructureGeometryTrianglesDataKHR = AccelerationStructureGeometryTrianglesDataKHR
  { -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "vertexFormat"
    vertexFormat :: Format
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "vertexData"
    vertexData :: DeviceOrHostAddressConstKHR
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "vertexStride"
    vertexStride :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "maxVertex"
    maxVertex :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "indexType"
    indexType :: IndexType
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "indexData"
    indexData :: DeviceOrHostAddressConstKHR
  , -- No documentation found for Nested "VkAccelerationStructureGeometryTrianglesDataKHR" "transformData"
    transformData :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryTrianglesDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryTrianglesDataKHR

instance ToCStruct AccelerationStructureGeometryTrianglesDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryTrianglesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (vertexFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (vertexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (maxVertex)
    lift $ poke ((p `plusPtr` 44 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (indexData) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr DeviceOrHostAddressConstKHR)) (transformData) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr IndexType)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryTrianglesDataKHR where
  zero = AccelerationStructureGeometryTrianglesDataKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureGeometryAabbsDataKHR"
data AccelerationStructureGeometryAabbsDataKHR = AccelerationStructureGeometryAabbsDataKHR
  { -- No documentation found for Nested "VkAccelerationStructureGeometryAabbsDataKHR" "data"
    data' :: DeviceOrHostAddressConstKHR
  , -- No documentation found for Nested "VkAccelerationStructureGeometryAabbsDataKHR" "stride"
    stride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryAabbsDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryAabbsDataKHR

instance ToCStruct AccelerationStructureGeometryAabbsDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryAabbsDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (stride)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryAabbsDataKHR where
  zero = AccelerationStructureGeometryAabbsDataKHR
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureGeometryInstancesDataKHR"
data AccelerationStructureGeometryInstancesDataKHR = AccelerationStructureGeometryInstancesDataKHR
  { -- No documentation found for Nested "VkAccelerationStructureGeometryInstancesDataKHR" "arrayOfPointers"
    arrayOfPointers :: Bool
  , -- No documentation found for Nested "VkAccelerationStructureGeometryInstancesDataKHR" "data"
    data' :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryInstancesDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryInstancesDataKHR

instance ToCStruct AccelerationStructureGeometryInstancesDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryInstancesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (arrayOfPointers))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryInstancesDataKHR where
  zero = AccelerationStructureGeometryInstancesDataKHR
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureGeometryKHR"
data AccelerationStructureGeometryKHR = AccelerationStructureGeometryKHR
  { -- No documentation found for Nested "VkAccelerationStructureGeometryKHR" "geometryType"
    geometryType :: GeometryTypeKHR
  , -- No documentation found for Nested "VkAccelerationStructureGeometryKHR" "geometry"
    geometry :: AccelerationStructureGeometryDataKHR
  , -- No documentation found for Nested "VkAccelerationStructureGeometryKHR" "flags"
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryKHR)
#endif
deriving instance Show AccelerationStructureGeometryKHR

instance ToCStruct AccelerationStructureGeometryKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (geometry) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr GeometryFlagsKHR)) (flags)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryKHR where
  zero = AccelerationStructureGeometryKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureBuildGeometryInfoKHR"
data AccelerationStructureBuildGeometryInfoKHR = AccelerationStructureBuildGeometryInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "type"
    type' :: AccelerationStructureTypeKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "flags"
    flags :: BuildAccelerationStructureFlagsKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "mode"
    mode :: BuildAccelerationStructureModeKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "srcAccelerationStructure"
    srcAccelerationStructure :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "dstAccelerationStructure"
    dstAccelerationStructure :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "geometryCount"
    geometryCount :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "pGeometries"
    geometries :: Vector AccelerationStructureGeometryKHR
  , -- No documentation found for Nested "VkAccelerationStructureBuildGeometryInfoKHR" "scratchData"
    scratchData :: DeviceOrHostAddressKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildGeometryInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildGeometryInfoKHR

instance ToCStruct AccelerationStructureBuildGeometryInfoKHR where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildGeometryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildAccelerationStructureModeKHR)) (mode)
    lift $ poke ((p `plusPtr` 32 :: Ptr AccelerationStructureKHR)) (srcAccelerationStructure)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccelerationStructureKHR)) (dstAccelerationStructure)
    let pGeometriesLength = Data.Vector.length $ (geometries)
    geometryCount'' <- lift $ if (geometryCount) == 0
      then pure $ fromIntegral pGeometriesLength
      else do
        unless (fromIntegral pGeometriesLength == (geometryCount) || pGeometriesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pGeometries must be empty or have 'geometryCount' elements" Nothing Nothing
        pure (geometryCount)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (geometryCount'')
    pGeometries'' <- if Data.Vector.null (geometries)
      then pure nullPtr
      else do
        pPGeometries <- ContT $ allocaBytesAligned @AccelerationStructureGeometryKHR (((Data.Vector.length (geometries))) * 96) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries `plusPtr` (96 * (i)) :: Ptr AccelerationStructureGeometryKHR) (e) . ($ ())) ((geometries))
        pure $ pPGeometries
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr AccelerationStructureGeometryKHR))) pGeometries''
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr AccelerationStructureGeometryKHR)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (scratchData) . ($ ())
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildAccelerationStructureModeKHR)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr AccelerationStructureGeometryKHR)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureBuildGeometryInfoKHR where
  zero = AccelerationStructureBuildGeometryInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero



-- No documentation found for TopLevel "VkAccelerationStructureBuildRangeInfoKHR"
data AccelerationStructureBuildRangeInfoKHR = AccelerationStructureBuildRangeInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureBuildRangeInfoKHR" "primitiveCount"
    primitiveCount :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureBuildRangeInfoKHR" "primitiveOffset"
    primitiveOffset :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureBuildRangeInfoKHR" "firstVertex"
    firstVertex :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureBuildRangeInfoKHR" "transformOffset"
    transformOffset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildRangeInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildRangeInfoKHR

instance ToCStruct AccelerationStructureBuildRangeInfoKHR where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildRangeInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (primitiveCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (primitiveOffset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (transformOffset)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct AccelerationStructureBuildRangeInfoKHR where
  peekCStruct p = do
    primitiveCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    primitiveOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    transformOffset <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ AccelerationStructureBuildRangeInfoKHR
             primitiveCount primitiveOffset firstVertex transformOffset


instance Storable AccelerationStructureBuildRangeInfoKHR where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureBuildRangeInfoKHR where
  zero = AccelerationStructureBuildRangeInfoKHR
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureCreateInfoKHR"
data AccelerationStructureCreateInfoKHR = AccelerationStructureCreateInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "createFlags"
    createFlags :: AccelerationStructureCreateFlagsKHR
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "type"
    type' :: AccelerationStructureTypeKHR
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoKHR" "deviceAddress"
    deviceAddress :: DeviceAddress
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureCreateInfoKHR)
#endif
deriving instance Show AccelerationStructureCreateInfoKHR

instance ToCStruct AccelerationStructureCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureCreateFlagsKHR)) (createFlags)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR)) (type')
    poke ((p `plusPtr` 56 :: Ptr DeviceAddress)) (deviceAddress)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR)) (zero)
    f

instance FromCStruct AccelerationStructureCreateInfoKHR where
  peekCStruct p = do
    createFlags <- peek @AccelerationStructureCreateFlagsKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureCreateFlagsKHR))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    type' <- peek @AccelerationStructureTypeKHR ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR))
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 56 :: Ptr DeviceAddress))
    pure $ AccelerationStructureCreateInfoKHR
             createFlags buffer offset size type' deviceAddress


instance Storable AccelerationStructureCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureCreateInfoKHR where
  zero = AccelerationStructureCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAabbPositionsKHR"
data AabbPositionsKHR = AabbPositionsKHR
  { -- No documentation found for Nested "VkAabbPositionsKHR" "minX"
    minX :: Float
  , -- No documentation found for Nested "VkAabbPositionsKHR" "minY"
    minY :: Float
  , -- No documentation found for Nested "VkAabbPositionsKHR" "minZ"
    minZ :: Float
  , -- No documentation found for Nested "VkAabbPositionsKHR" "maxX"
    maxX :: Float
  , -- No documentation found for Nested "VkAabbPositionsKHR" "maxY"
    maxY :: Float
  , -- No documentation found for Nested "VkAabbPositionsKHR" "maxZ"
    maxZ :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AabbPositionsKHR)
#endif
deriving instance Show AabbPositionsKHR

instance ToCStruct AabbPositionsKHR where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AabbPositionsKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (minX))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (minY))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (minZ))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (maxX))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (maxY))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxZ))
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct AabbPositionsKHR where
  peekCStruct p = do
    minX <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    minY <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    minZ <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    maxX <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    maxY <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxZ <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ AabbPositionsKHR
             ((\(CFloat a) -> a) minX) ((\(CFloat a) -> a) minY) ((\(CFloat a) -> a) minZ) ((\(CFloat a) -> a) maxX) ((\(CFloat a) -> a) maxY) ((\(CFloat a) -> a) maxZ)


instance Storable AabbPositionsKHR where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AabbPositionsKHR where
  zero = AabbPositionsKHR
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkTransformMatrixKHR"
data TransformMatrixKHR = TransformMatrixKHR
  { -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow0"
    matrixRow0 :: (Float, Float, Float, Float)
  , -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow1"
    matrixRow1 :: (Float, Float, Float, Float)
  , -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow2"
    matrixRow2 :: (Float, Float, Float, Float)
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TransformMatrixKHR)
#endif
deriving instance Show TransformMatrixKHR

instance ToCStruct TransformMatrixKHR where
  withCStruct x f = allocaBytesAligned 48 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TransformMatrixKHR{..} f = do
    let pMatrixRow0' = lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow0) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow0' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow0' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow0' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow0' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow1' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow1) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow1' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow1' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow1' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow1' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow2' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow2) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow2' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow2' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow2' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow2' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    f
  cStructSize = 48
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    let pMatrixRow0' = lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray 4 CFloat)))
    case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow0' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow0' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow0' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow0' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow1' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 4 CFloat)))
    case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow1' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow1' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow1' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow1' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow2' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 4 CFloat)))
    case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow2' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow2' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow2' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow2' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    f

instance FromCStruct TransformMatrixKHR where
  peekCStruct p = do
    let pmatrixRow0 = lowerArrayPtr @CFloat ((p `plusPtr` 0 :: Ptr (FixedArray 4 CFloat)))
    matrixRow00 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow01 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow02 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow03 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrixRow1 = lowerArrayPtr @CFloat ((p `plusPtr` 16 :: Ptr (FixedArray 4 CFloat)))
    matrixRow10 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow11 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow12 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow13 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrixRow2 = lowerArrayPtr @CFloat ((p `plusPtr` 32 :: Ptr (FixedArray 4 CFloat)))
    matrixRow20 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow21 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow22 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow23 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ TransformMatrixKHR
             ((((\(CFloat a) -> a) matrixRow00), ((\(CFloat a) -> a) matrixRow01), ((\(CFloat a) -> a) matrixRow02), ((\(CFloat a) -> a) matrixRow03))) ((((\(CFloat a) -> a) matrixRow10), ((\(CFloat a) -> a) matrixRow11), ((\(CFloat a) -> a) matrixRow12), ((\(CFloat a) -> a) matrixRow13))) ((((\(CFloat a) -> a) matrixRow20), ((\(CFloat a) -> a) matrixRow21), ((\(CFloat a) -> a) matrixRow22), ((\(CFloat a) -> a) matrixRow23)))


instance Storable TransformMatrixKHR where
  sizeOf ~_ = 48
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TransformMatrixKHR where
  zero = TransformMatrixKHR
           (zero, zero, zero, zero)
           (zero, zero, zero, zero)
           (zero, zero, zero, zero)



-- No documentation found for TopLevel "VkAccelerationStructureInstanceKHR"
data AccelerationStructureInstanceKHR = AccelerationStructureInstanceKHR
  { -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "transform"
    transform :: TransformMatrixKHR
  , -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "instanceCustomIndex"
    instanceCustomIndex :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "mask"
    mask :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "instanceShaderBindingTableRecordOffset"
    instanceShaderBindingTableRecordOffset :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "flags"
    flags :: GeometryInstanceFlagsKHR
  , -- No documentation found for Nested "VkAccelerationStructureInstanceKHR" "accelerationStructureReference"
    accelerationStructureReference :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureInstanceKHR)
#endif
deriving instance Show AccelerationStructureInstanceKHR

instance ToCStruct AccelerationStructureInstanceKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInstanceKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (transform)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (((coerce @_ @Word32 (mask)) `shiftL` 24) .|. (instanceCustomIndex))
    poke ((p `plusPtr` 52 :: Ptr Word32)) (((coerce @_ @Word32 (flags)) `shiftL` 24) .|. (instanceShaderBindingTableRecordOffset))
    poke ((p `plusPtr` 56 :: Ptr Word64)) (accelerationStructureReference)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (zero)
    f

instance FromCStruct AccelerationStructureInstanceKHR where
  peekCStruct p = do
    transform <- peekCStruct @TransformMatrixKHR ((p `plusPtr` 0 :: Ptr TransformMatrixKHR))
    instanceCustomIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let instanceCustomIndex' = ((instanceCustomIndex .&. coerce @Word32 0xffffff))
    mask <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let mask' = ((((mask `shiftR` 24)) .&. coerce @Word32 0xff))
    instanceShaderBindingTableRecordOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    let instanceShaderBindingTableRecordOffset' = ((instanceShaderBindingTableRecordOffset .&. coerce @Word32 0xffffff))
    flags <- peek @GeometryInstanceFlagsKHR ((p `plusPtr` 52 :: Ptr GeometryInstanceFlagsKHR))
    let flags' = ((((flags `shiftR` 24)) .&. coerce @Word32 0xff))
    accelerationStructureReference <- peek @Word64 ((p `plusPtr` 56 :: Ptr Word64))
    pure $ AccelerationStructureInstanceKHR
             transform instanceCustomIndex' mask' instanceShaderBindingTableRecordOffset' flags' accelerationStructureReference


instance Storable AccelerationStructureInstanceKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureInstanceKHR where
  zero = AccelerationStructureInstanceKHR
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureDeviceAddressInfoKHR"
data AccelerationStructureDeviceAddressInfoKHR = AccelerationStructureDeviceAddressInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureDeviceAddressInfoKHR" "accelerationStructure"
    accelerationStructure :: AccelerationStructureKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureDeviceAddressInfoKHR)
#endif
deriving instance Show AccelerationStructureDeviceAddressInfoKHR

instance ToCStruct AccelerationStructureDeviceAddressInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureDeviceAddressInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    f

instance FromCStruct AccelerationStructureDeviceAddressInfoKHR where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    pure $ AccelerationStructureDeviceAddressInfoKHR
             accelerationStructure


instance Storable AccelerationStructureDeviceAddressInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureDeviceAddressInfoKHR where
  zero = AccelerationStructureDeviceAddressInfoKHR
           zero



-- No documentation found for TopLevel "VkAccelerationStructureVersionInfoKHR"
data AccelerationStructureVersionInfoKHR = AccelerationStructureVersionInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureVersionInfoKHR" "pVersionData"
    versionData :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureVersionInfoKHR)
#endif
deriving instance Show AccelerationStructureVersionInfoKHR

instance ToCStruct AccelerationStructureVersionInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureVersionInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ unless (Data.ByteString.length (versionData) == 2 * UUID_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "AccelerationStructureVersionKHR::versionData must be 2*VK_UUID_SIZE bytes" Nothing Nothing
    versionData <- fmap (castPtr @CChar @Word8) . ContT $ unsafeUseAsCString (versionData)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Word8))) versionData
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AccelerationStructureVersionInfoKHR where
  peekCStruct p = do
    versionData <- peek @(Ptr Word8) ((p `plusPtr` 16 :: Ptr (Ptr Word8)))
    versionData' <- packCStringLen (castPtr @Word8 @CChar versionData, 2 * UUID_SIZE)
    pure $ AccelerationStructureVersionInfoKHR
             versionData'

instance Zero AccelerationStructureVersionInfoKHR where
  zero = AccelerationStructureVersionInfoKHR
           mempty



-- No documentation found for TopLevel "VkCopyAccelerationStructureInfoKHR"
data CopyAccelerationStructureInfoKHR = CopyAccelerationStructureInfoKHR
  { -- No documentation found for Nested "VkCopyAccelerationStructureInfoKHR" "src"
    src :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkCopyAccelerationStructureInfoKHR" "dst"
    dst :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkCopyAccelerationStructureInfoKHR" "mode"
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyAccelerationStructureInfoKHR)
#endif
deriving instance Show CopyAccelerationStructureInfoKHR

instance ToCStruct CopyAccelerationStructureInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    f

instance FromCStruct CopyAccelerationStructureInfoKHR where
  peekCStruct p = do
    src <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    dst <- peek @AccelerationStructureKHR ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR))
    mode <- peek @CopyAccelerationStructureModeKHR ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR))
    pure $ CopyAccelerationStructureInfoKHR
             src dst mode


instance Storable CopyAccelerationStructureInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyAccelerationStructureInfoKHR where
  zero = CopyAccelerationStructureInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCopyAccelerationStructureToMemoryInfoKHR"
data CopyAccelerationStructureToMemoryInfoKHR = CopyAccelerationStructureToMemoryInfoKHR
  { -- No documentation found for Nested "VkCopyAccelerationStructureToMemoryInfoKHR" "src"
    src :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkCopyAccelerationStructureToMemoryInfoKHR" "dst"
    dst :: DeviceOrHostAddressKHR
  , -- No documentation found for Nested "VkCopyAccelerationStructureToMemoryInfoKHR" "mode"
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyAccelerationStructureToMemoryInfoKHR)
#endif
deriving instance Show CopyAccelerationStructureToMemoryInfoKHR

instance ToCStruct CopyAccelerationStructureToMemoryInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureToMemoryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (dst) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance Zero CopyAccelerationStructureToMemoryInfoKHR where
  zero = CopyAccelerationStructureToMemoryInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkCopyMemoryToAccelerationStructureInfoKHR"
data CopyMemoryToAccelerationStructureInfoKHR = CopyMemoryToAccelerationStructureInfoKHR
  { -- No documentation found for Nested "VkCopyMemoryToAccelerationStructureInfoKHR" "src"
    src :: DeviceOrHostAddressConstKHR
  , -- No documentation found for Nested "VkCopyMemoryToAccelerationStructureInfoKHR" "dst"
    dst :: AccelerationStructureKHR
  , -- No documentation found for Nested "VkCopyMemoryToAccelerationStructureInfoKHR" "mode"
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToAccelerationStructureInfoKHR)
#endif
deriving instance Show CopyMemoryToAccelerationStructureInfoKHR

instance ToCStruct CopyMemoryToAccelerationStructureInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToAccelerationStructureInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (src) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance Zero CopyMemoryToAccelerationStructureInfoKHR where
  zero = CopyMemoryToAccelerationStructureInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureBuildSizesInfoKHR"
data AccelerationStructureBuildSizesInfoKHR = AccelerationStructureBuildSizesInfoKHR
  { -- No documentation found for Nested "VkAccelerationStructureBuildSizesInfoKHR" "accelerationStructureSize"
    accelerationStructureSize :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureBuildSizesInfoKHR" "updateScratchSize"
    updateScratchSize :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureBuildSizesInfoKHR" "buildScratchSize"
    buildScratchSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildSizesInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildSizesInfoKHR

instance ToCStruct AccelerationStructureBuildSizesInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildSizesInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (accelerationStructureSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (updateScratchSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (buildScratchSize)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct AccelerationStructureBuildSizesInfoKHR where
  peekCStruct p = do
    accelerationStructureSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    updateScratchSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    buildScratchSize <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ AccelerationStructureBuildSizesInfoKHR
             accelerationStructureSize updateScratchSize buildScratchSize


instance Storable AccelerationStructureBuildSizesInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureBuildSizesInfoKHR where
  zero = AccelerationStructureBuildSizesInfoKHR
           zero
           zero
           zero


data DeviceOrHostAddressKHR
  = DeviceAddress DeviceAddress
  | HostAddress (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressKHR -> DeviceOrHostAddressKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddress v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddress v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressKHR where
  zero = DeviceAddress zero


data DeviceOrHostAddressConstKHR
  = DeviceAddressConst DeviceAddress
  | HostAddressConst (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressConstKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressConstKHR -> DeviceOrHostAddressConstKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddressConst v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddressConst v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressConstKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressConstKHR where
  zero = DeviceAddressConst zero


data AccelerationStructureGeometryDataKHR
  = Triangles AccelerationStructureGeometryTrianglesDataKHR
  | Aabbs AccelerationStructureGeometryAabbsDataKHR
  | Instances AccelerationStructureGeometryInstancesDataKHR
  deriving (Show)

instance ToCStruct AccelerationStructureGeometryDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr AccelerationStructureGeometryDataKHR -> AccelerationStructureGeometryDataKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Triangles v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryTrianglesDataKHR p) (v) . ($ ())
    Aabbs v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryAabbsDataKHR p) (v) . ($ ())
    Instances v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryInstancesDataKHR p) (v) . ($ ())
  pokeZeroCStruct :: Ptr AccelerationStructureGeometryDataKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 64
  cStructAlignment = 8

instance Zero AccelerationStructureGeometryDataKHR where
  zero = Triangles zero


type GeometryInstanceFlagsKHR = GeometryInstanceFlagBitsKHR

-- No documentation found for TopLevel "VkGeometryInstanceFlagBitsKHR"
newtype GeometryInstanceFlagBitsKHR = GeometryInstanceFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkGeometryInstanceFlagBitsKHR" "VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR"
pattern GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR    = GeometryInstanceFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkGeometryInstanceFlagBitsKHR" "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR"
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkGeometryInstanceFlagBitsKHR" "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR"
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR                    = GeometryInstanceFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkGeometryInstanceFlagBitsKHR" "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR"
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR                 = GeometryInstanceFlagBitsKHR 0x00000008

conNameGeometryInstanceFlagBitsKHR :: String
conNameGeometryInstanceFlagBitsKHR = "GeometryInstanceFlagBitsKHR"

enumPrefixGeometryInstanceFlagBitsKHR :: String
enumPrefixGeometryInstanceFlagBitsKHR = "GEOMETRY_INSTANCE_"

showTableGeometryInstanceFlagBitsKHR :: [(GeometryInstanceFlagBitsKHR, String)]
showTableGeometryInstanceFlagBitsKHR =
  [ (GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR   , "TRIANGLE_FACING_CULL_DISABLE_BIT_KHR")
  , (GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR, "TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR")
  , (GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR                   , "FORCE_OPAQUE_BIT_KHR")
  , (GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR                , "FORCE_NO_OPAQUE_BIT_KHR")
  ]


instance Show GeometryInstanceFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixGeometryInstanceFlagBitsKHR
                          showTableGeometryInstanceFlagBitsKHR
                          conNameGeometryInstanceFlagBitsKHR
                          (\(GeometryInstanceFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read GeometryInstanceFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixGeometryInstanceFlagBitsKHR
                          showTableGeometryInstanceFlagBitsKHR
                          conNameGeometryInstanceFlagBitsKHR
                          GeometryInstanceFlagBitsKHR


type GeometryFlagsKHR = GeometryFlagBitsKHR

-- No documentation found for TopLevel "VkGeometryFlagBitsKHR"
newtype GeometryFlagBitsKHR = GeometryFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkGeometryFlagBitsKHR" "VK_GEOMETRY_OPAQUE_BIT_KHR"
pattern GEOMETRY_OPAQUE_BIT_KHR                          = GeometryFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkGeometryFlagBitsKHR" "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR"
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR = GeometryFlagBitsKHR 0x00000002

conNameGeometryFlagBitsKHR :: String
conNameGeometryFlagBitsKHR = "GeometryFlagBitsKHR"

enumPrefixGeometryFlagBitsKHR :: String
enumPrefixGeometryFlagBitsKHR = "GEOMETRY_"

showTableGeometryFlagBitsKHR :: [(GeometryFlagBitsKHR, String)]
showTableGeometryFlagBitsKHR =
  [ (GEOMETRY_OPAQUE_BIT_KHR                         , "OPAQUE_BIT_KHR")
  , (GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR, "NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR")
  ]


instance Show GeometryFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixGeometryFlagBitsKHR
                          showTableGeometryFlagBitsKHR
                          conNameGeometryFlagBitsKHR
                          (\(GeometryFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read GeometryFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixGeometryFlagBitsKHR
                          showTableGeometryFlagBitsKHR
                          conNameGeometryFlagBitsKHR
                          GeometryFlagBitsKHR


type BuildAccelerationStructureFlagsKHR = BuildAccelerationStructureFlagBitsKHR

-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagBitsKHR"
newtype BuildAccelerationStructureFlagBitsKHR = BuildAccelerationStructureFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsKHR" "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR      = BuildAccelerationStructureFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsKHR" "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR  = BuildAccelerationStructureFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsKHR" "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000004
-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsKHR" "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000008
-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsKHR" "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR        = BuildAccelerationStructureFlagBitsKHR 0x00000010

conNameBuildAccelerationStructureFlagBitsKHR :: String
conNameBuildAccelerationStructureFlagBitsKHR = "BuildAccelerationStructureFlagBitsKHR"

enumPrefixBuildAccelerationStructureFlagBitsKHR :: String
enumPrefixBuildAccelerationStructureFlagBitsKHR = "BUILD_ACCELERATION_STRUCTURE_"

showTableBuildAccelerationStructureFlagBitsKHR :: [(BuildAccelerationStructureFlagBitsKHR, String)]
showTableBuildAccelerationStructureFlagBitsKHR =
  [ (BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR     , "ALLOW_UPDATE_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR , "ALLOW_COMPACTION_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR, "PREFER_FAST_TRACE_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR, "PREFER_FAST_BUILD_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR       , "LOW_MEMORY_BIT_KHR")
  ]


instance Show BuildAccelerationStructureFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixBuildAccelerationStructureFlagBitsKHR
                          showTableBuildAccelerationStructureFlagBitsKHR
                          conNameBuildAccelerationStructureFlagBitsKHR
                          (\(BuildAccelerationStructureFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read BuildAccelerationStructureFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixBuildAccelerationStructureFlagBitsKHR
                          showTableBuildAccelerationStructureFlagBitsKHR
                          conNameBuildAccelerationStructureFlagBitsKHR
                          BuildAccelerationStructureFlagBitsKHR


type AccelerationStructureCreateFlagsKHR = AccelerationStructureCreateFlagBitsKHR

-- No documentation found for TopLevel "VkAccelerationStructureCreateFlagBitsKHR"
newtype AccelerationStructureCreateFlagBitsKHR = AccelerationStructureCreateFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkAccelerationStructureCreateFlagBitsKHR" "VK_ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR"
pattern ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR =
  AccelerationStructureCreateFlagBitsKHR 0x00000001

conNameAccelerationStructureCreateFlagBitsKHR :: String
conNameAccelerationStructureCreateFlagBitsKHR = "AccelerationStructureCreateFlagBitsKHR"

enumPrefixAccelerationStructureCreateFlagBitsKHR :: String
enumPrefixAccelerationStructureCreateFlagBitsKHR =
  "ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR"

showTableAccelerationStructureCreateFlagBitsKHR :: [(AccelerationStructureCreateFlagBitsKHR, String)]
showTableAccelerationStructureCreateFlagBitsKHR =
  [(ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR, "")]


instance Show AccelerationStructureCreateFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixAccelerationStructureCreateFlagBitsKHR
                          showTableAccelerationStructureCreateFlagBitsKHR
                          conNameAccelerationStructureCreateFlagBitsKHR
                          (\(AccelerationStructureCreateFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read AccelerationStructureCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureCreateFlagBitsKHR
                          showTableAccelerationStructureCreateFlagBitsKHR
                          conNameAccelerationStructureCreateFlagBitsKHR
                          AccelerationStructureCreateFlagBitsKHR


-- No documentation found for TopLevel "VkCopyAccelerationStructureModeKHR"
newtype CopyAccelerationStructureModeKHR = CopyAccelerationStructureModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCopyAccelerationStructureModeKHR" "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR"
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR       = CopyAccelerationStructureModeKHR 0
-- No documentation found for Nested "VkCopyAccelerationStructureModeKHR" "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR"
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR     = CopyAccelerationStructureModeKHR 1
-- No documentation found for Nested "VkCopyAccelerationStructureModeKHR" "VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR"
pattern COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR   = CopyAccelerationStructureModeKHR 2
-- No documentation found for Nested "VkCopyAccelerationStructureModeKHR" "VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR"
pattern COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR = CopyAccelerationStructureModeKHR 3
{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR :: CopyAccelerationStructureModeKHR #-}

conNameCopyAccelerationStructureModeKHR :: String
conNameCopyAccelerationStructureModeKHR = "CopyAccelerationStructureModeKHR"

enumPrefixCopyAccelerationStructureModeKHR :: String
enumPrefixCopyAccelerationStructureModeKHR = "COPY_ACCELERATION_STRUCTURE_MODE_"

showTableCopyAccelerationStructureModeKHR :: [(CopyAccelerationStructureModeKHR, String)]
showTableCopyAccelerationStructureModeKHR =
  [ (COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR      , "CLONE_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR    , "COMPACT_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR  , "SERIALIZE_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR, "DESERIALIZE_KHR")
  ]


instance Show CopyAccelerationStructureModeKHR where
showsPrec = enumShowsPrec enumPrefixCopyAccelerationStructureModeKHR
                          showTableCopyAccelerationStructureModeKHR
                          conNameCopyAccelerationStructureModeKHR
                          (\(CopyAccelerationStructureModeKHR x) -> x)
                          (showsPrec 11)


instance Read CopyAccelerationStructureModeKHR where
  readPrec = enumReadPrec enumPrefixCopyAccelerationStructureModeKHR
                          showTableCopyAccelerationStructureModeKHR
                          conNameCopyAccelerationStructureModeKHR
                          CopyAccelerationStructureModeKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureModeKHR"
newtype BuildAccelerationStructureModeKHR = BuildAccelerationStructureModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBuildAccelerationStructureModeKHR" "VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR  = BuildAccelerationStructureModeKHR 0
-- No documentation found for Nested "VkBuildAccelerationStructureModeKHR" "VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR"
pattern BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR = BuildAccelerationStructureModeKHR 1
{-# complete BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR,
             BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR :: BuildAccelerationStructureModeKHR #-}

conNameBuildAccelerationStructureModeKHR :: String
conNameBuildAccelerationStructureModeKHR = "BuildAccelerationStructureModeKHR"

enumPrefixBuildAccelerationStructureModeKHR :: String
enumPrefixBuildAccelerationStructureModeKHR = "BUILD_ACCELERATION_STRUCTURE_MODE_"

showTableBuildAccelerationStructureModeKHR :: [(BuildAccelerationStructureModeKHR, String)]
showTableBuildAccelerationStructureModeKHR =
  [ (BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR , "BUILD_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR, "UPDATE_KHR")
  ]


instance Show BuildAccelerationStructureModeKHR where
showsPrec = enumShowsPrec enumPrefixBuildAccelerationStructureModeKHR
                          showTableBuildAccelerationStructureModeKHR
                          conNameBuildAccelerationStructureModeKHR
                          (\(BuildAccelerationStructureModeKHR x) -> x)
                          (showsPrec 11)


instance Read BuildAccelerationStructureModeKHR where
  readPrec = enumReadPrec enumPrefixBuildAccelerationStructureModeKHR
                          showTableBuildAccelerationStructureModeKHR
                          conNameBuildAccelerationStructureModeKHR
                          BuildAccelerationStructureModeKHR


-- No documentation found for TopLevel "VkAccelerationStructureTypeKHR"
newtype AccelerationStructureTypeKHR = AccelerationStructureTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAccelerationStructureTypeKHR" "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR"
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR    = AccelerationStructureTypeKHR 0
-- No documentation found for Nested "VkAccelerationStructureTypeKHR" "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR"
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR = AccelerationStructureTypeKHR 1
-- No documentation found for Nested "VkAccelerationStructureTypeKHR" "VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR"
pattern ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR      = AccelerationStructureTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
             ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR,
             ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR :: AccelerationStructureTypeKHR #-}

conNameAccelerationStructureTypeKHR :: String
conNameAccelerationStructureTypeKHR = "AccelerationStructureTypeKHR"

enumPrefixAccelerationStructureTypeKHR :: String
enumPrefixAccelerationStructureTypeKHR = "ACCELERATION_STRUCTURE_TYPE_"

showTableAccelerationStructureTypeKHR :: [(AccelerationStructureTypeKHR, String)]
showTableAccelerationStructureTypeKHR =
  [ (ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR   , "TOP_LEVEL_KHR")
  , (ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR, "BOTTOM_LEVEL_KHR")
  , (ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR     , "GENERIC_KHR")
  ]


instance Show AccelerationStructureTypeKHR where
showsPrec = enumShowsPrec enumPrefixAccelerationStructureTypeKHR
                          showTableAccelerationStructureTypeKHR
                          conNameAccelerationStructureTypeKHR
                          (\(AccelerationStructureTypeKHR x) -> x)
                          (showsPrec 11)


instance Read AccelerationStructureTypeKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureTypeKHR
                          showTableAccelerationStructureTypeKHR
                          conNameAccelerationStructureTypeKHR
                          AccelerationStructureTypeKHR


-- No documentation found for TopLevel "VkGeometryTypeKHR"
newtype GeometryTypeKHR = GeometryTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkGeometryTypeKHR" "VK_GEOMETRY_TYPE_TRIANGLES_KHR"
pattern GEOMETRY_TYPE_TRIANGLES_KHR = GeometryTypeKHR 0
-- No documentation found for Nested "VkGeometryTypeKHR" "VK_GEOMETRY_TYPE_AABBS_KHR"
pattern GEOMETRY_TYPE_AABBS_KHR     = GeometryTypeKHR 1
-- No documentation found for Nested "VkGeometryTypeKHR" "VK_GEOMETRY_TYPE_INSTANCES_KHR"
pattern GEOMETRY_TYPE_INSTANCES_KHR = GeometryTypeKHR 2
{-# complete GEOMETRY_TYPE_TRIANGLES_KHR,
             GEOMETRY_TYPE_AABBS_KHR,
             GEOMETRY_TYPE_INSTANCES_KHR :: GeometryTypeKHR #-}

conNameGeometryTypeKHR :: String
conNameGeometryTypeKHR = "GeometryTypeKHR"

enumPrefixGeometryTypeKHR :: String
enumPrefixGeometryTypeKHR = "GEOMETRY_TYPE_"

showTableGeometryTypeKHR :: [(GeometryTypeKHR, String)]
showTableGeometryTypeKHR =
  [ (GEOMETRY_TYPE_TRIANGLES_KHR, "TRIANGLES_KHR")
  , (GEOMETRY_TYPE_AABBS_KHR    , "AABBS_KHR")
  , (GEOMETRY_TYPE_INSTANCES_KHR, "INSTANCES_KHR")
  ]


instance Show GeometryTypeKHR where
showsPrec = enumShowsPrec enumPrefixGeometryTypeKHR
                          showTableGeometryTypeKHR
                          conNameGeometryTypeKHR
                          (\(GeometryTypeKHR x) -> x)
                          (showsPrec 11)


instance Read GeometryTypeKHR where
  readPrec = enumReadPrec enumPrefixGeometryTypeKHR showTableGeometryTypeKHR conNameGeometryTypeKHR GeometryTypeKHR


-- No documentation found for TopLevel "VkAccelerationStructureBuildTypeKHR"
newtype AccelerationStructureBuildTypeKHR = AccelerationStructureBuildTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAccelerationStructureBuildTypeKHR" "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR"
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR           = AccelerationStructureBuildTypeKHR 0
-- No documentation found for Nested "VkAccelerationStructureBuildTypeKHR" "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR"
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR         = AccelerationStructureBuildTypeKHR 1
-- No documentation found for Nested "VkAccelerationStructureBuildTypeKHR" "VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR"
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR = AccelerationStructureBuildTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR :: AccelerationStructureBuildTypeKHR #-}

conNameAccelerationStructureBuildTypeKHR :: String
conNameAccelerationStructureBuildTypeKHR = "AccelerationStructureBuildTypeKHR"

enumPrefixAccelerationStructureBuildTypeKHR :: String
enumPrefixAccelerationStructureBuildTypeKHR = "ACCELERATION_STRUCTURE_BUILD_TYPE_"

showTableAccelerationStructureBuildTypeKHR :: [(AccelerationStructureBuildTypeKHR, String)]
showTableAccelerationStructureBuildTypeKHR =
  [ (ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR          , "HOST_KHR")
  , (ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR        , "DEVICE_KHR")
  , (ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR, "HOST_OR_DEVICE_KHR")
  ]


instance Show AccelerationStructureBuildTypeKHR where
showsPrec = enumShowsPrec enumPrefixAccelerationStructureBuildTypeKHR
                          showTableAccelerationStructureBuildTypeKHR
                          conNameAccelerationStructureBuildTypeKHR
                          (\(AccelerationStructureBuildTypeKHR x) -> x)
                          (showsPrec 11)


instance Read AccelerationStructureBuildTypeKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureBuildTypeKHR
                          showTableAccelerationStructureBuildTypeKHR
                          conNameAccelerationStructureBuildTypeKHR
                          AccelerationStructureBuildTypeKHR


-- No documentation found for TopLevel "VkAccelerationStructureCompatibilityKHR"
newtype AccelerationStructureCompatibilityKHR = AccelerationStructureCompatibilityKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAccelerationStructureCompatibilityKHR" "VK_ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR"
pattern ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR   = AccelerationStructureCompatibilityKHR 0
-- No documentation found for Nested "VkAccelerationStructureCompatibilityKHR" "VK_ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR"
pattern ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR = AccelerationStructureCompatibilityKHR 1
{-# complete ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR,
             ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR :: AccelerationStructureCompatibilityKHR #-}

conNameAccelerationStructureCompatibilityKHR :: String
conNameAccelerationStructureCompatibilityKHR = "AccelerationStructureCompatibilityKHR"

enumPrefixAccelerationStructureCompatibilityKHR :: String
enumPrefixAccelerationStructureCompatibilityKHR = "ACCELERATION_STRUCTURE_COMPATIBILITY_"

showTableAccelerationStructureCompatibilityKHR :: [(AccelerationStructureCompatibilityKHR, String)]
showTableAccelerationStructureCompatibilityKHR =
  [ (ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR  , "COMPATIBLE_KHR")
  , (ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR, "INCOMPATIBLE_KHR")
  ]


instance Show AccelerationStructureCompatibilityKHR where
showsPrec = enumShowsPrec enumPrefixAccelerationStructureCompatibilityKHR
                          showTableAccelerationStructureCompatibilityKHR
                          conNameAccelerationStructureCompatibilityKHR
                          (\(AccelerationStructureCompatibilityKHR x) -> x)
                          (showsPrec 11)


instance Read AccelerationStructureCompatibilityKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureCompatibilityKHR
                          showTableAccelerationStructureCompatibilityKHR
                          conNameAccelerationStructureCompatibilityKHR
                          AccelerationStructureCompatibilityKHR


type KHR_ACCELERATION_STRUCTURE_SPEC_VERSION = 11

-- No documentation found for TopLevel "VK_KHR_ACCELERATION_STRUCTURE_SPEC_VERSION"
pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION = 11


type KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME = "VK_KHR_acceleration_structure"

-- No documentation found for TopLevel "VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME"
pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME = "VK_KHR_acceleration_structure"

