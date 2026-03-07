{-# language CPP #-}
-- No documentation found for Chapter "PromotedStreamingTransfers'"
module Vulkan.Core14.PromotedStreamingTransfers'  ( copyMemoryToImage
                                                  , copyImageToMemory
                                                  , copyImageToImage
                                                  , transitionImageLayout
                                                  , PhysicalDeviceHostImageCopyFeatures(..)
                                                  , PhysicalDeviceHostImageCopyProperties(..)
                                                  , MemoryToImageCopy(..)
                                                  , ImageToMemoryCopy(..)
                                                  , CopyMemoryToImageInfo(..)
                                                  , CopyImageToMemoryInfo(..)
                                                  , CopyImageToImageInfo(..)
                                                  , HostImageLayoutTransitionInfo(..)
                                                  , SubresourceHostMemcpySize(..)
                                                  , HostImageCopyDevicePerformanceQuery(..)
                                                  , StructureType(..)
                                                  , ImageUsageFlagBits(..)
                                                  , ImageUsageFlags
                                                  , HostImageCopyFlagBits(..)
                                                  , HostImageCopyFlags
                                                  , FormatFeatureFlagBits2(..)
                                                  , FormatFeatureFlags2
                                                  ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCopyImageToImage))
import Vulkan.Dynamic (DeviceCmds(pVkCopyImageToMemory))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToImage))
import Vulkan.Dynamic (DeviceCmds(pVkTransitionImageLayout))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageCopy2)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlagBits2(..))
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlagBits(..))
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToImage
  :: FunPtr (Ptr Device_T -> Ptr CopyMemoryToImageInfo -> IO Result) -> Ptr Device_T -> Ptr CopyMemoryToImageInfo -> IO Result

-- | vkCopyMemoryToImage - Copy data from host memory into an image
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyBufferToImage2',
-- except it is executed on the host and reads from host memory instead of
-- a buffer. The memory of @pCopyMemoryToImageInfo->dstImage@ is accessed
-- by the host as if
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-coherent coherent>.
--
-- Because queue submissions
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-host-writes automatically make host memory visible to the device>,
-- there would not be a need for a memory barrier before using the results
-- of this copy operation on the device.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'CopyMemoryToImageInfo', 'Vulkan.Core10.Handles.Device'
copyMemoryToImage :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the device which owns @pCopyMemoryToImageInfo->dstImage@.
                     --
                     -- #VUID-vkCopyMemoryToImage-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @pCopyMemoryToImageInfo@ is a pointer to a 'CopyMemoryToImageInfo'
                     -- structure describing the copy parameters.
                     --
                     -- #VUID-vkCopyMemoryToImage-pCopyMemoryToImageInfo-parameter#
                     -- @pCopyMemoryToImageInfo@ /must/ be a valid pointer to a valid
                     -- 'CopyMemoryToImageInfo' structure
                     CopyMemoryToImageInfo
                  -> io ()
copyMemoryToImage device copyMemoryToImageInfo = liftIO . evalContT $ do
  let vkCopyMemoryToImagePtr = pVkCopyMemoryToImage (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyMemoryToImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMemoryToImage is null" Nothing Nothing
  let vkCopyMemoryToImage' = mkVkCopyMemoryToImage vkCopyMemoryToImagePtr
  pCopyMemoryToImageInfo <- ContT $ withCStruct (copyMemoryToImageInfo)
  r <- lift $ traceAroundEvent "vkCopyMemoryToImage" (vkCopyMemoryToImage'
                                                        (deviceHandle (device))
                                                        pCopyMemoryToImageInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyImageToMemory
  :: FunPtr (Ptr Device_T -> Ptr CopyImageToMemoryInfo -> IO Result) -> Ptr Device_T -> Ptr CopyImageToMemoryInfo -> IO Result

-- | vkCopyImageToMemory - Copy image data into host memory
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyImageToBuffer2',
-- except it is executed on the host and writes to host memory instead of a
-- buffer. The memory of @pCopyImageToMemoryInfo->srcImage@ is accessed by
-- the host as if
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-coherent coherent>.
--
-- If the device has written to the image memory, it is not automatically
-- made available to the host. Before this copy command can be called, a
-- memory barrier for this image /must/ have been issued on the device with
-- the second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- including
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT' and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'CopyImageToMemoryInfo', 'Vulkan.Core10.Handles.Device'
copyImageToMemory :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the device which owns @pCopyImageToMemoryInfo->srcImage@.
                     --
                     -- #VUID-vkCopyImageToMemory-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @pCopyImageToMemoryInfo@ is a pointer to a 'CopyImageToMemoryInfo'
                     -- structure describing the copy parameters.
                     --
                     -- #VUID-vkCopyImageToMemory-pCopyImageToMemoryInfo-parameter#
                     -- @pCopyImageToMemoryInfo@ /must/ be a valid pointer to a valid
                     -- 'CopyImageToMemoryInfo' structure
                     CopyImageToMemoryInfo
                  -> io ()
copyImageToMemory device copyImageToMemoryInfo = liftIO . evalContT $ do
  let vkCopyImageToMemoryPtr = pVkCopyImageToMemory (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyImageToMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyImageToMemory is null" Nothing Nothing
  let vkCopyImageToMemory' = mkVkCopyImageToMemory vkCopyImageToMemoryPtr
  pCopyImageToMemoryInfo <- ContT $ withCStruct (copyImageToMemoryInfo)
  r <- lift $ traceAroundEvent "vkCopyImageToMemory" (vkCopyImageToMemory'
                                                        (deviceHandle (device))
                                                        pCopyImageToMemoryInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyImageToImage
  :: FunPtr (Ptr Device_T -> Ptr CopyImageToImageInfo -> IO Result) -> Ptr Device_T -> Ptr CopyImageToImageInfo -> IO Result

-- | vkCopyImageToImage - Copy image data using the host
--
-- = Description
--
-- This command is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdCopyImage2',
-- except it is executed on the host. The memory of
-- @pCopyImageToImageInfo->srcImage@ and @pCopyImageToImageInfo->dstImage@
-- is accessed by the host as if
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-coherent coherent>.
--
-- If the device has written to the memory of
-- @pCopyImageToImageInfo->srcImage@, it is not automatically made
-- available to the host. Before this copy command can be called, a memory
-- barrier for this image /must/ have been issued on the device with the
-- second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- including
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT' and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT'.
--
-- Because queue submissions
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-host-writes automatically make host memory visible to the device>,
-- there would not be a need for a memory barrier before using the results
-- of this copy operation in @pCopyMemoryToImageInfo->dstImage@ on the
-- device.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'CopyImageToImageInfo', 'Vulkan.Core10.Handles.Device'
copyImageToImage :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the device which owns @pCopyImageToImageInfo->srcImage@ and
                    -- @pCopyImageToImageInfo->dstImage@.
                    --
                    -- #VUID-vkCopyImageToImage-device-parameter# @device@ /must/ be a valid
                    -- 'Vulkan.Core10.Handles.Device' handle
                    Device
                 -> -- | @pCopyImageToImageInfo@ is a pointer to a 'CopyImageToImageInfo'
                    -- structure describing the copy parameters.
                    --
                    -- #VUID-vkCopyImageToImage-pCopyImageToImageInfo-parameter#
                    -- @pCopyImageToImageInfo@ /must/ be a valid pointer to a valid
                    -- 'CopyImageToImageInfo' structure
                    CopyImageToImageInfo
                 -> io ()
copyImageToImage device copyImageToImageInfo = liftIO . evalContT $ do
  let vkCopyImageToImagePtr = pVkCopyImageToImage (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyImageToImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyImageToImage is null" Nothing Nothing
  let vkCopyImageToImage' = mkVkCopyImageToImage vkCopyImageToImagePtr
  pCopyImageToImageInfo <- ContT $ withCStruct (copyImageToImageInfo)
  r <- lift $ traceAroundEvent "vkCopyImageToImage" (vkCopyImageToImage'
                                                       (deviceHandle (device))
                                                       pCopyImageToImageInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTransitionImageLayout
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr HostImageLayoutTransitionInfo -> IO Result) -> Ptr Device_T -> Word32 -> Ptr HostImageLayoutTransitionInfo -> IO Result

-- | vkTransitionImageLayout - Perform an image layout transition on the host
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device', 'HostImageLayoutTransitionInfo'
transitionImageLayout :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the device which owns @pTransitions@[i].@image@.
                         --
                         -- #VUID-vkTransitionImageLayout-device-parameter# @device@ /must/ be a
                         -- valid 'Vulkan.Core10.Handles.Device' handle
                         Device
                      -> -- | @pTransitions@ is a pointer to an array of
                         -- 'HostImageLayoutTransitionInfo' structures specifying the image and
                         -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-image-views subresource ranges>
                         -- within them to transition.
                         --
                         -- #VUID-vkTransitionImageLayout-pTransitions-parameter# @pTransitions@
                         -- /must/ be a valid pointer to an array of @transitionCount@ valid
                         -- 'HostImageLayoutTransitionInfo' structures
                         ("transitions" ::: Vector HostImageLayoutTransitionInfo)
                      -> io ()
transitionImageLayout device transitions = liftIO . evalContT $ do
  let vkTransitionImageLayoutPtr = pVkTransitionImageLayout (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkTransitionImageLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkTransitionImageLayout is null" Nothing Nothing
  let vkTransitionImageLayout' = mkVkTransitionImageLayout vkTransitionImageLayoutPtr
  pPTransitions <- ContT $ allocaBytes @HostImageLayoutTransitionInfo ((Data.Vector.length (transitions)) * 56)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTransitions `plusPtr` (56 * (i)) :: Ptr HostImageLayoutTransitionInfo) (e)) (transitions)
  r <- lift $ traceAroundEvent "vkTransitionImageLayout" (vkTransitionImageLayout'
                                                            (deviceHandle (device))
                                                            ((fromIntegral (Data.Vector.length $ (transitions)) :: Word32))
                                                            (pPTransitions))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPhysicalDeviceHostImageCopyFeatures - Structure indicating support for
-- copies to or from images from host memory
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceHostImageCopyFeatures' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceHostImageCopyFeatures', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostImageCopyFeatures = PhysicalDeviceHostImageCopyFeatures
  { -- | #extension-features-hostImageCopy# @hostImageCopy@ indicates that the
    -- implementation supports copying from host memory to images using the
    -- 'copyMemoryToImage' command, copying from images to host memory using
    -- the 'copyImageToMemory' command, and copying between images using the
    -- 'copyImageToImage' command.
    hostImageCopy :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostImageCopyFeatures)
#endif
deriving instance Show PhysicalDeviceHostImageCopyFeatures

instance ToCStruct PhysicalDeviceHostImageCopyFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostImageCopyFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hostImageCopy))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostImageCopyFeatures where
  peekCStruct p = do
    hostImageCopy <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceHostImageCopyFeatures
             (bool32ToBool hostImageCopy)

instance Storable PhysicalDeviceHostImageCopyFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostImageCopyFeatures where
  zero = PhysicalDeviceHostImageCopyFeatures
           zero


-- | VkPhysicalDeviceHostImageCopyProperties - Structure enumerating image
-- layouts supported by an implementation for host memory copies
--
-- = Description
--
-- -   @copySrcLayoutCount@ is an integer related to the number of image
--     layouts for host copies from images available or queried, as
--     described below.
--
-- -   @pCopySrcLayouts@ is a pointer to an array of
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported
--     image layouts for use with host copy operations from images are
--     returned.
--
-- -   @copyDstLayoutCount@ is an integer related to the number of image
--     layouts for host copies to images available or queried, as described
--     below.
--
-- -   @pCopyDstLayouts@ is a pointer to an array of
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' in which supported
--     image layouts for use with host copy operations to images are
--     returned.
--
-- -   @optimalTilingLayoutUUID@ is an array of
--     'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values representing
--     a universally unique identifier for the implementation’s swizzling
--     layout of images created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
--
-- -   @identicalMemoryTypeRequirements@ indicates that specifying the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     flag in 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ does not
--     affect the memory type requirements of the image.
--
-- If the 'PhysicalDeviceHostImageCopyProperties' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- If @pCopyDstLayouts@ is @NULL@, then the number of image layouts that
-- are supported in 'CopyMemoryToImageInfo'::@dstImageLayout@ and
-- 'CopyImageToImageInfo'::@dstImageLayout@ is returned in
-- @copyDstLayoutCount@. Otherwise, @copyDstLayoutCount@ /must/ be set by
-- the application to the number of elements in the @pCopyDstLayouts@
-- array, and on return is overwritten with the number of values actually
-- written to @pCopyDstLayouts@. If the value of @copyDstLayoutCount@ is
-- less than the number of image layouts that are supported, at most
-- @copyDstLayoutCount@ values will be written to @pCopyDstLayouts@. The
-- implementation /must/ include the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' image layout in
-- @pCopyDstLayouts@. If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-unifiedImageLayouts unifiedImageLayouts>
-- feature is supported, the implementation /must/ include all the image
-- layouts that are interchangeable with
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' in
-- @pCopyDstLayouts@.
--
-- If @pCopySrcLayouts@ is @NULL@, then the number of image layouts that
-- are supported in 'CopyImageToMemoryInfo'::@srcImageLayout@ and
-- 'CopyImageToImageInfo'::@srcImageLayout@ is returned in
-- @copySrcLayoutCount@. Otherwise, @copySrcLayoutCount@ /must/ be set by
-- the application to the number of elements in the @pCopySrcLayouts@
-- array, and on return is overwritten with the number of values actually
-- written to @pCopySrcLayouts@. If the value of @copySrcLayoutCount@ is
-- less than the number of image layouts that are supported, at most
-- @copySrcLayoutCount@ values will be written to @pCopySrcLayouts@. The
-- implementation /must/ include the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' image layout in
-- @pCopySrcLayouts@. If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-unifiedImageLayouts unifiedImageLayouts>
-- feature is supported, the implementation /must/ include all the image
-- layouts that are interchangeable with
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' in
-- @pCopySrcLayouts@.
--
-- The @optimalTilingLayoutUUID@ value can be used to ensure compatible
-- data layouts when using the
-- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT'
-- flag in 'copyMemoryToImage' and 'copyImageToMemory'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyProperties-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES'
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyProperties-pCopySrcLayouts-parameter#
--     If @copySrcLayoutCount@ is not @0@, and @pCopySrcLayouts@ is not
--     @NULL@, @pCopySrcLayouts@ /must/ be a valid pointer to an array of
--     @copySrcLayoutCount@ 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     values
--
-- -   #VUID-VkPhysicalDeviceHostImageCopyProperties-pCopyDstLayouts-parameter#
--     If @copyDstLayoutCount@ is not @0@, and @pCopyDstLayouts@ is not
--     @NULL@, @pCopyDstLayouts@ /must/ be a valid pointer to an array of
--     @copyDstLayoutCount@ 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceHostImageCopyProperties = PhysicalDeviceHostImageCopyProperties
  { -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "copySrcLayoutCount"
    copySrcLayoutCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "pCopySrcLayouts"
    copySrcLayouts :: Ptr ImageLayout
  , -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "copyDstLayoutCount"
    copyDstLayoutCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "pCopyDstLayouts"
    copyDstLayouts :: Ptr ImageLayout
  , -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "optimalTilingLayoutUUID"
    optimalTilingLayoutUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceHostImageCopyProperties" "identicalMemoryTypeRequirements"
    identicalMemoryTypeRequirements :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceHostImageCopyProperties)
#endif
deriving instance Show PhysicalDeviceHostImageCopyProperties

instance ToCStruct PhysicalDeviceHostImageCopyProperties where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceHostImageCopyProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (copySrcLayoutCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ImageLayout))) (copySrcLayouts)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (copyDstLayoutCount)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ImageLayout))) (copyDstLayouts)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray UUID_SIZE Word8))) (optimalTilingLayoutUUID)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (identicalMemoryTypeRequirements))
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceHostImageCopyProperties where
  peekCStruct p = do
    copySrcLayoutCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pCopySrcLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 24 :: Ptr (Ptr ImageLayout)))
    copyDstLayoutCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCopyDstLayouts <- peek @(Ptr ImageLayout) ((p `plusPtr` 40 :: Ptr (Ptr ImageLayout)))
    optimalTilingLayoutUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (FixedArray UUID_SIZE Word8)))
    identicalMemoryTypeRequirements <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    pure $ PhysicalDeviceHostImageCopyProperties
             copySrcLayoutCount
             pCopySrcLayouts
             copyDstLayoutCount
             pCopyDstLayouts
             optimalTilingLayoutUUID
             (bool32ToBool identicalMemoryTypeRequirements)

instance Storable PhysicalDeviceHostImageCopyProperties where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceHostImageCopyProperties where
  zero = PhysicalDeviceHostImageCopyProperties
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkMemoryToImageCopy - Structure specifying a host memory to image copy
-- operation
--
-- = Description
--
-- This structure is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- except it defines host memory as the source of copy instead of a buffer.
-- In particular, the same data packing rules and restrictions as that
-- structure apply here as well.
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryToImageCopy-pHostPointer-09061# @pHostPointer@ /must/
--     point to memory that is large enough to contain all memory locations
--     that are accessed according to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkMemoryToImageCopy-pRegions-09062# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkMemoryToImageCopy-memoryRowLength-09101# @memoryRowLength@
--     /must/ be @0@, or greater than or equal to the @width@ member of
--     @imageExtent@
--
-- -   #VUID-VkMemoryToImageCopy-memoryImageHeight-09102#
--     @memoryImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkMemoryToImageCopy-aspectMask-09103# The @aspectMask@ member
--     of @imageSubresource@ /must/ only have a single bit set
--
-- -   #VUID-VkMemoryToImageCopy-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkMemoryToImageCopy-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkMemoryToImageCopy-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryToImageCopy-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY'
--
-- -   #VUID-VkMemoryToImageCopy-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMemoryToImageCopy-pHostPointer-parameter# @pHostPointer@
--     /must/ be a pointer value
--
-- -   #VUID-VkMemoryToImageCopy-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'CopyMemoryToImageInfo', 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryToImageCopy = MemoryToImageCopy
  { -- | @pHostPointer@ is the host memory address which is the source of the
    -- copy.
    hostPointer :: Ptr ()
  , -- | @memoryRowLength@ and @memoryImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in host memory, and control
    -- the addressing calculations. If either of these values is zero, that
    -- aspect of the host memory is considered to be tightly packed according
    -- to the @imageExtent@.
    memoryRowLength :: Word32
  , -- No documentation found for Nested "VkMemoryToImageCopy" "memoryImageHeight"
    memoryImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryToImageCopy)
#endif
deriving instance Show MemoryToImageCopy

instance ToCStruct MemoryToImageCopy where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryToImageCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (hostPointer)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryRowLength)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (memoryImageHeight)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct MemoryToImageCopy where
  peekCStruct p = do
    pHostPointer <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    memoryRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    memoryImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ MemoryToImageCopy
             pHostPointer
             memoryRowLength
             memoryImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable MemoryToImageCopy where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryToImageCopy where
  zero = MemoryToImageCopy
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageToMemoryCopy - Structure specifying an image to host memory copy
-- operation
--
-- = Description
--
-- This structure is functionally similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- except it defines host memory as the target of copy instead of a buffer.
-- In particular, the same data packing rules and restrictions as that
-- structure apply here as well.
--
-- == Valid Usage
--
-- -   #VUID-VkImageToMemoryCopy-pHostPointer-09066# @pHostPointer@ /must/
--     point to memory that is large enough to contain all memory locations
--     that are accessed according to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-VkImageToMemoryCopy-pRegions-09067# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-VkImageToMemoryCopy-memoryRowLength-09101# @memoryRowLength@
--     /must/ be @0@, or greater than or equal to the @width@ member of
--     @imageExtent@
--
-- -   #VUID-VkImageToMemoryCopy-memoryImageHeight-09102#
--     @memoryImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkImageToMemoryCopy-aspectMask-09103# The @aspectMask@ member
--     of @imageSubresource@ /must/ only have a single bit set
--
-- -   #VUID-VkImageToMemoryCopy-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkImageToMemoryCopy-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkImageToMemoryCopy-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageToMemoryCopy-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY'
--
-- -   #VUID-VkImageToMemoryCopy-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageToMemoryCopy-pHostPointer-parameter# @pHostPointer@
--     /must/ be a pointer value
--
-- -   #VUID-VkImageToMemoryCopy-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'CopyImageToMemoryInfo', 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageToMemoryCopy = ImageToMemoryCopy
  { -- | @pHostPointer@ is the host memory address which is the destination of
    -- the copy.
    hostPointer :: Ptr ()
  , -- | @memoryRowLength@ and @memoryImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in host memory, and control
    -- the addressing calculations. If either of these values is zero, that
    -- aspect of the host memory is considered to be tightly packed according
    -- to the @imageExtent@.
    memoryRowLength :: Word32
  , -- No documentation found for Nested "VkImageToMemoryCopy" "memoryImageHeight"
    memoryImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the source image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageToMemoryCopy)
#endif
deriving instance Show ImageToMemoryCopy

instance ToCStruct ImageToMemoryCopy where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageToMemoryCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (hostPointer)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryRowLength)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (memoryImageHeight)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageToMemoryCopy where
  peekCStruct p = do
    pHostPointer <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    memoryRowLength <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    memoryImageHeight <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 32 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 48 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 60 :: Ptr Extent3D))
    pure $ ImageToMemoryCopy
             pHostPointer
             memoryRowLength
             memoryImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable ImageToMemoryCopy where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageToMemoryCopy where
  zero = ImageToMemoryCopy
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCopyMemoryToImageInfo - Structure specifying parameters of host memory
-- to image copy command
--
-- = Description
--
-- 'copyMemoryToImage' does not check whether the device memory associated
-- with @dstImage@ is currently in use before performing the copy. The
-- application /must/ guarantee that any previously submitted command that
-- reads from or writes to the copy regions has completed before the host
-- performs the copy.
--
-- Copy regions for the image /must/ be aligned to a multiple of the texel
-- block extent in each dimension, except at the edges of the image, where
-- region extents /must/ match the edge of the image.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-09109# If @dstImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-09111# If the stencil aspect
--     of @dstImage@ is accessed, and @dstImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-09112# If the stencil aspect
--     of @dstImage@ is accessed, and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-09113# If non-stencil aspects
--     of @dstImage@ are accessed, @dstImage@ /must/ have been created with
--     the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-09114# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @x@, @y@, and @z@ members of the @imageOffset@ member of each
--     element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-09115# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @imageExtent@ member of each element of @pRegions@ /must/ equal
--     the extents of @dstImage@ identified by @imageSubresource@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07966# If @dstImage@ is
--     non-sparse then the image or each specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageSubresource-07968# If
--     @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07969# @dstImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageSubresource-07971# For each
--     element of @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageSubresource-07972# For each
--     element of @pRegions@, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07973# @dstImage@ /must/ have
--     a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07979# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-09104# For each element of
--     @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07980# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07274# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10051# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     and @imageOffset.x@ does not equal the width of the subresource
--     specified by @imageSubresource@, @imageOffset.x@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07275# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10052# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     and @imageOffset.y@ does not equal the height of the subresource
--     specified by @imageSubresource@, @imageOffset.y@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-00207# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     the sum of @imageOffset.x@ and @extent.width@ does not equal the
--     width of the subresource specified by @imageSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10053# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the difference of @imageOffset.x@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10054# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @imageOffset.x@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10055# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the sum of @imageOffset.x@ and @extent.height@ does not equal the
--     width of the subresource specified by @imageSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-00208# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     and the sum of @imageOffset.y@ and @extent.height@ does not equal
--     the height of the subresource specified by @imageSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10056# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the sum of @imageOffset.y@ and @extent.width@ does not equal the
--     height of the subresource specified by @imageSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10057# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @imageOffset.y@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageOffset-10058# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the difference of @imageOffset.y@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07981# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-07983# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageInfo-memoryRowLength-09106# For each
--     element of @pRegions@, @memoryRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-memoryImageHeight-09107# For each
--     element of @pRegions@, @memoryImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageInfo-memoryRowLength-09108# For each
--     element of @pRegions@, @memoryRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @dstImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImageLayout-09059# @dstImageLayout@
--     /must/ specify the current layout of the image subresources of
--     @dstImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImageLayout-09060# @dstImageLayout@
--     /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopyDstLayouts@
--
-- -   #VUID-VkCopyMemoryToImageInfo-flags-09393# If @flags@ includes
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     for each region in @pRegions@, @memoryRowLength@ and
--     @memoryImageHeight@ /must/ both be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToImageInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO'
--
-- -   #VUID-VkCopyMemoryToImageInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyMemoryToImageInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits'
--     values
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImage-parameter# @dstImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyMemoryToImageInfo-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyMemoryToImageInfo-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'MemoryToImageCopy' structures
--
-- -   #VUID-VkCopyMemoryToImageInfo-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlags',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'MemoryToImageCopy',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'copyMemoryToImage',
-- 'copyMemoryToImage'
data CopyMemoryToImageInfo = CopyMemoryToImageInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits' values
    -- describing additional copy parameters.
    flags :: HostImageCopyFlags
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'MemoryToImageCopy' structures
    -- specifying the regions to copy.
    regions :: Vector MemoryToImageCopy
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToImageInfo)
#endif
deriving instance Show CopyMemoryToImageInfo

instance ToCStruct CopyMemoryToImageInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToImageInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @MemoryToImageCopy ((Data.Vector.length (regions)) * 72)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (72 * (i)) :: Ptr MemoryToImageCopy) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr MemoryToImageCopy))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyMemoryToImageInfo where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlags ((p `plusPtr` 16 :: Ptr HostImageCopyFlags))
    dstImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr MemoryToImageCopy) ((p `plusPtr` 40 :: Ptr (Ptr MemoryToImageCopy)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @MemoryToImageCopy ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr MemoryToImageCopy)))
    pure $ CopyMemoryToImageInfo
             flags dstImage dstImageLayout pRegions'

instance Zero CopyMemoryToImageInfo where
  zero = CopyMemoryToImageInfo
           zero
           zero
           zero
           mempty


-- | VkCopyImageToMemoryInfo - Structure specifying parameters of an image to
-- host memory copy command
--
-- = Description
--
-- 'copyImageToMemory' does not check whether the device memory associated
-- with @srcImage@ is currently in use before performing the copy. The
-- application /must/ guarantee that any previously submitted command that
-- writes to the copy regions has completed before the host performs the
-- copy.
--
-- Copy regions for the image /must/ be aligned to a multiple of the texel
-- block extent in each dimension, except at the edges of the image, where
-- region extents /must/ match the edge of the image.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-09109# If @srcImage@ is
--     sparse then all memory ranges accessed by the copy command /must/ be
--     bound as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-09111# If the stencil aspect
--     of @srcImage@ is accessed, and @srcImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-09112# If the stencil aspect
--     of @srcImage@ is accessed, and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-09113# If non-stencil aspects
--     of @srcImage@ are accessed, @srcImage@ /must/ have been created with
--     the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-09114# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @x@, @y@, and @z@ members of the @imageOffset@ member of each
--     element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-09115# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @imageExtent@ member of each element of @pRegions@ /must/ equal
--     the extents of @srcImage@ identified by @imageSubresource@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07966# If @srcImage@ is
--     non-sparse then the image or each specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageSubresource-07967# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageSubresource-07968# If
--     @imageSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07969# @srcImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageSubresource-07971# For each
--     element of @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageSubresource-07972# For each
--     element of @pRegions@, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07973# @srcImage@ /must/ have
--     a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07979# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-09104# For each element of
--     @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07980# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07274# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10051# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     and @imageOffset.x@ does not equal the width of the subresource
--     specified by @imageSubresource@, @imageOffset.x@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07275# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10052# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     and @imageOffset.y@ does not equal the height of the subresource
--     specified by @imageSubresource@, @imageOffset.y@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-00207# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     the sum of @imageOffset.x@ and @extent.width@ does not equal the
--     width of the subresource specified by @imageSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10053# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the difference of @imageOffset.x@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10054# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @imageOffset.x@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10055# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the sum of @imageOffset.x@ and @extent.height@ does not equal the
--     width of the subresource specified by @imageSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-00208# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     and the sum of @imageOffset.y@ and @extent.height@ does not equal
--     the height of the subresource specified by @imageSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10056# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the sum of @imageOffset.y@ and @extent.width@ does not equal the
--     height of the subresource specified by @imageSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10057# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @imageOffset.y@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageOffset-10058# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the difference of @imageOffset.y@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-imageSubresource-09105# For each
--     element of @pRegions@, @imageSubresource.aspectMask@ /must/ specify
--     aspects present in @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07981# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-07983# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be @0@ and
--     @imageSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToMemoryInfo-memoryRowLength-09106# For each
--     element of @pRegions@, @memoryRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-memoryImageHeight-09107# For each
--     element of @pRegions@, @memoryImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToMemoryInfo-memoryRowLength-09108# For each
--     element of @pRegions@, @memoryRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @srcImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImageLayout-09064# @srcImageLayout@
--     /must/ specify the current layout of the image subresources of
--     @srcImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImageLayout-09065# @srcImageLayout@
--     /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopySrcLayouts@
--
-- -   #VUID-VkCopyImageToMemoryInfo-flags-09394# If @flags@ includes
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     for each region in @pRegions@, @memoryRowLength@ and
--     @memoryImageHeight@ /must/ both be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToMemoryInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO'
--
-- -   #VUID-VkCopyImageToMemoryInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyImageToMemoryInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits'
--     values
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImage-parameter# @srcImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToMemoryInfo-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToMemoryInfo-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'ImageToMemoryCopy' structures
--
-- -   #VUID-VkCopyImageToMemoryInfo-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlags',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageToMemoryCopy',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'copyImageToMemory',
-- 'copyImageToMemory'
data CopyImageToMemoryInfo = CopyImageToMemoryInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits' values
    -- describing additional copy parameters.
    flags :: HostImageCopyFlags
  , -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of 'ImageToMemoryCopy' structures
    -- specifying the regions to copy.
    regions :: Vector ImageToMemoryCopy
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToMemoryInfo)
#endif
deriving instance Show CopyImageToMemoryInfo

instance ToCStruct CopyImageToMemoryInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToMemoryInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageToMemoryCopy ((Data.Vector.length (regions)) * 72)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (72 * (i)) :: Ptr ImageToMemoryCopy) (e)) (regions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ImageToMemoryCopy))) (pPRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageToMemoryInfo where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlags ((p `plusPtr` 16 :: Ptr HostImageCopyFlags))
    srcImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageToMemoryCopy) ((p `plusPtr` 40 :: Ptr (Ptr ImageToMemoryCopy)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageToMemoryCopy ((pRegions `advancePtrBytes` (72 * (i)) :: Ptr ImageToMemoryCopy)))
    pure $ CopyImageToMemoryInfo
             flags srcImage srcImageLayout pRegions'

instance Zero CopyImageToMemoryInfo where
  zero = CopyImageToMemoryInfo
           zero
           zero
           zero
           mempty


-- | VkCopyImageToImageInfo - Structure specifying parameters of an image to
-- image host copy command
--
-- = Description
--
-- 'copyImageToImage' does not check whether the device memory associated
-- with @srcImage@ or @dstImage@ is currently in use before performing the
-- copy. The application /must/ guarantee that any previously submitted
-- command that writes to the copy regions has completed before the host
-- performs the copy.
--
-- == Valid Usage
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09069# @srcImage@ and
--     @dstImage@ /must/ have been created with identical image creation
--     parameters
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09109# If @srcImage@ is sparse
--     then all memory ranges accessed by the copy command /must/ be bound
--     as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09111# If the stencil aspect
--     of @srcImage@ is accessed, and @srcImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09112# If the stencil aspect
--     of @srcImage@ is accessed, and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @srcImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09113# If non-stencil aspects
--     of @srcImage@ are accessed, @srcImage@ /must/ have been created with
--     the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-09114# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @x@, @y@, and @z@ members of the @srcOffset@ member of each
--     element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-09115# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @extent@ member of each element of @pRegions@ /must/ equal the
--     extents of @srcImage@ identified by @srcSubresource@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07966# If @srcImage@ is
--     non-sparse then the image or each specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToImageInfo-srcSubresource-07967# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfo-srcSubresource-07968# If
--     @srcSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07969# @srcImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToImageInfo-srcSubresource-07971# For each element
--     of @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcSubresource-07972# For each element
--     of @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07979# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @srcOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-09104# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07980# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07274# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     @srcOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10051# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     and @srcOffset.x@ does not equal the width of the subresource
--     specified by @srcSubresource@, @srcOffset.x@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07275# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     @srcOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10052# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     and @srcOffset.y@ does not equal the height of the subresource
--     specified by @srcSubresource@, @srcOffset.y@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07276# For each element of
--     @pRegions@, @srcOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-00207# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     the sum of @srcOffset.x@ and @extent.width@ does not equal the width
--     of the subresource specified by @srcSubresource@, @extent.width@
--     /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10053# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the difference of @srcOffset.x@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10054# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @srcOffset.x@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10055# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the sum of @srcOffset.x@ and @extent.height@ does not equal the
--     width of the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-00208# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     and the sum of @srcOffset.y@ and @extent.height@ does not equal the
--     height of the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10056# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the sum of @srcOffset.y@ and @extent.width@ does not equal the
--     height of the subresource specified by @srcSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10057# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @srcOffset.y@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcOffset-10058# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the difference of @srcOffset.y@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-00209# For each element of
--     @pRegions@, if the sum of @srcOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcSubresource-09105# For each element
--     of @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07981# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-07983# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-09109# If @dstImage@ is sparse
--     then all memory ranges accessed by the copy command /must/ be bound
--     as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-resource-binding Binding Resource Memory>
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-09111# If the stencil aspect
--     of @dstImage@ is accessed, and @dstImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-09112# If the stencil aspect
--     of @dstImage@ is accessed, and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     @dstImage@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-09113# If non-stencil aspects
--     of @dstImage@ are accessed, @dstImage@ /must/ have been created with
--     the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-09114# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @x@, @y@, and @z@ members of the @dstOffset@ member of each
--     element of @pRegions@ /must/ be @0@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-09115# If @flags@ contains
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HOST_IMAGE_COPY_MEMCPY_BIT',
--     the @extent@ member of each element of @pRegions@ /must/ equal the
--     extents of @dstImage@ identified by @dstSubresource@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07966# If @dstImage@ is
--     non-sparse then the image or each specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyImageToImageInfo-dstSubresource-07967# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfo-dstSubresource-07968# If
--     @dstSubresource.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07969# @dstImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyImageToImageInfo-dstSubresource-07971# For each element
--     of @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstSubresource-07972# For each element
--     of @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07979# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @dstOffset.y@ /must/ be @0@ and
--     @extent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-09104# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07980# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07274# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     @dstOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10051# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     and @dstOffset.x@ does not equal the width of the subresource
--     specified by @dstSubresource@, @dstOffset.x@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07275# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     @dstOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10052# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     and @dstOffset.y@ does not equal the height of the subresource
--     specified by @dstSubresource@, @dstOffset.y@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07276# For each element of
--     @pRegions@, @dstOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-00207# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     the sum of @dstOffset.x@ and @extent.width@ does not equal the width
--     of the subresource specified by @dstSubresource@, @extent.width@
--     /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10053# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the difference of @dstOffset.x@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10054# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @dstOffset.x@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10055# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the sum of @dstOffset.x@ and @extent.height@ does not equal the
--     width of the subresource specified by @dstSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-00208# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     and the sum of @dstOffset.y@ and @extent.height@ does not equal the
--     height of the subresource specified by @dstSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10056# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     the sum of @dstOffset.y@ and @extent.width@ does not equal the
--     height of the subresource specified by @dstSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10057# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     the difference of @dstOffset.y@ and @extent.height@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstOffset-10058# For each element of
--     @pRegions@, if
--     'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM'::@transform@
--     is equal to
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR',
--     the difference of @dstOffset.y@ and @extent.width@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-00209# For each element of
--     @pRegions@, if the sum of @dstOffset.z@ and @extent.depth@ does not
--     equal the depth of the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstSubresource-09105# For each element
--     of @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07981# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be a single valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-07983# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImageLayout-09070# @srcImageLayout@
--     /must/ specify the current layout of the image subresources of
--     @srcImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImageLayout-09071# @dstImageLayout@
--     /must/ specify the current layout of the image subresources of
--     @dstImage@ specified in @pRegions@
--
-- -   #VUID-VkCopyImageToImageInfo-srcImageLayout-09072# @srcImageLayout@
--     /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopySrcLayouts@
--
-- -   #VUID-VkCopyImageToImageInfo-dstImageLayout-09073# @dstImageLayout@
--     /must/ be one of the image layouts returned in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopyDstLayouts@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyImageToImageInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO'
--
-- -   #VUID-VkCopyImageToImageInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyImageToImageInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits'
--     values
--
-- -   #VUID-VkCopyImageToImageInfo-srcImage-parameter# @srcImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToImageInfo-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToImageInfo-dstImage-parameter# @dstImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyImageToImageInfo-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyImageToImageInfo-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2'
--     structures
--
-- -   #VUID-VkCopyImageToImageInfo-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkCopyImageToImageInfo-commonparent# Both of @dstImage@, and
--     @srcImage@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlags',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'copyImageToImage',
-- 'copyImageToImage'
data CopyImageToImageInfo = CopyImageToImageInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits' values
    -- describing additional copy parameters.
    flags :: HostImageCopyFlags
  , -- | @srcImage@ is the source image.
    srcImage :: Image
  , -- | @srcImageLayout@ is the layout of the source image subresources for the
    -- copy.
    srcImageLayout :: ImageLayout
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pRegions@ is a pointer to an array of
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2'
    -- structures specifying the regions to copy.
    regions :: Vector ImageCopy2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyImageToImageInfo)
#endif
deriving instance Show CopyImageToImageInfo

instance ToCStruct CopyImageToImageInfo where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyImageToImageInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr HostImageCopyFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (srcImage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (srcImageLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (dstImageLayout)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @ImageCopy2 ((Data.Vector.length (regions)) * 88)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (88 * (i)) :: Ptr ImageCopy2) (e)) (regions)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ImageCopy2))) (pPRegions')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyImageToImageInfo where
  peekCStruct p = do
    flags <- peek @HostImageCopyFlags ((p `plusPtr` 16 :: Ptr HostImageCopyFlags))
    srcImage <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    srcImageLayout <- peek @ImageLayout ((p `plusPtr` 32 :: Ptr ImageLayout))
    dstImage <- peek @Image ((p `plusPtr` 40 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    regionCount <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pRegions <- peek @(Ptr ImageCopy2) ((p `plusPtr` 56 :: Ptr (Ptr ImageCopy2)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @ImageCopy2 ((pRegions `advancePtrBytes` (88 * (i)) :: Ptr ImageCopy2)))
    pure $ CopyImageToImageInfo
             flags srcImage srcImageLayout dstImage dstImageLayout pRegions'

instance Zero CopyImageToImageInfo where
  zero = CopyImageToImageInfo
           zero
           zero
           zero
           zero
           zero
           mempty


-- | VkHostImageLayoutTransitionInfo - Structure specifying the parameters of
-- a host-side image layout transition
--
-- = Description
--
-- 'transitionImageLayout' does not check whether the device memory
-- associated with an image is currently in use before performing the
-- layout transition. The application /must/ guarantee that any previously
-- submitted command that reads from or writes to this subresource has
-- completed before the host performs the layout transition. The memory of
-- @image@ is accessed by the host as if
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-coherent coherent>.
--
-- If @image@ is a 3D image created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
-- and the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, the @baseArrayLayer@ and @layerCount@ members of
-- @subresourceRange@ specify the subset of slices of the 3D image affected
-- by the memory barrier, including the layout transition. Any slices of a
-- 3D image not included in @subresourceRange@ are not affected by the
-- memory barrier and remain in their existing layout.
--
-- Image layout transitions performed on the host do not require queue
-- family ownership transfers as the physical layout of the image will not
-- vary between queue families for the layouts supported by this function.
--
-- If the device has written to the image memory, it is not automatically
-- made available to the host. Before this command can be called, a memory
-- barrier for this image /must/ have been issued on the device with the
-- second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- including
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT' and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT'.
--
-- Because queue submissions
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-host-writes automatically make host memory visible to the device>,
-- there would not be a need for a memory barrier before using the results
-- of this layout transition on the device.
--
-- == Valid Usage
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-09055# @image@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-01486#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-01724# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-01488# If
--     @image@ is not a 3D image or was created without
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is not enabled, @subresourceRange.baseArrayLayer@ /must/ be
--     less than the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-maintenance9-10798# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is enabled and @image@ is a 3D image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, @subresourceRange.baseArrayLayer@ /must/ be less than the depth
--     computed from @baseMipLevel@ and @extent.depth@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created,
--     according to the formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-mip-level-sizing Image Mip Level Sizing>
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-maintenance9-10799# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is enabled and @image@ is a 3D image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set and either @subresourceRange.baseArrayLayer@ is not equal to 0
--     or @subresourceRange.layerCount@ is not equal to
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.levelCount@ /must/ be 1
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-01725# If
--     @image@ is not a 3D image or was created without
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is not enabled, and @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-maintenance9-10800# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is enabled, @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', and @image@ is
--     a 3D image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, @subresourceRange.baseArrayLayer@ +
--     @subresourceRange.layerCount@ /must/ be less than or equal to the
--     depth computed from @baseMipLevel@ and @extent.depth@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created,
--     according to the formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-mip-level-sizing Image Mip Level Sizing>
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-01932# If @image@ is
--     non-sparse then the image or each specified /disjoint/ plane /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-09241# If @image@ has a
--     color format that is single-plane, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-09242# If @image@ has a
--     color format and is not /disjoint/, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-01672# If @image@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>
--     and the image is /disjoint/, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include at least one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-03320# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-03319# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-10749# If @image@ has a
--     depth-only format then the @aspectMask@ member of @subresourceRange@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-10750# If @image@ has a
--     stencil-only format then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-aspectMask-08702# If the
--     @aspectMask@ member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-aspectMask-08703# If the
--     @aspectMask@ member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-09601#
--     @subresourceRange.aspectMask@ /must/ be valid for the @format@ the
--     @image@ was created with
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01208# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01209# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01210# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01211# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01212# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01213# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-10767# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-zeroInitializeDeviceMemory zeroInitializeDeviceMemory>
--     feature is not enabled, @oldLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ZERO_INITIALIZED_EXT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-10768# If
--     @oldLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ZERO_INITIALIZED_EXT',
--     then all subresources /must/ be included in the barrier
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01658# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-01659# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-04065# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flags set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-04066# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-04067# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flags set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-04068# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-synchronization2-07793# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @oldLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-synchronization2-07794# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-03938# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL',
--     @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-03939# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL',
--     @image@ /must/ have been created with at least one of the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flags set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-02088# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--     then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07120# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_SRC_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_SRC_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07121# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_DST_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07122# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07123# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_SRC_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07124# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_DST_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_DST_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07125# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR>
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-10287# If
--     @oldLayout@ or @newLayout@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageLayout VK_IMAGE_LAYOUT_VIDEO_ENCODE_QUANTIZATION_MAP_KHR>
--     then @image@ /must/ have been created with the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_QUANTIZATION_DELTA_MAP_BIT_KHR>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_EMPHASIS_MAP_BIT_KHR>
--     usage flags set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-07006# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then @image@ /must/ have been created with either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flags set, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage flags set, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     usage flag set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-attachmentFeedbackLoopLayout-07313#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-srcQueueFamilyIndex-09550# If
--     @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_RENDERING_LOCAL_READ'
--     then @image@ /must/ have been created with either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT'
--     usage flag set, or with both the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flag and either of the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage flags set
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-dynamicRenderingLocalRead-09551#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @oldLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_RENDERING_LOCAL_READ'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-dynamicRenderingLocalRead-09552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_RENDERING_LOCAL_READ'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-09229# @oldLayout@
--     /must/ be either
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources as specified in
--     @subresourceRange@
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-09230# If
--     @oldLayout@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ZERO_INITIALIZED_EXT',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED', it
--     /must/ be one of the layouts in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopySrcLayouts@
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-newLayout-09057# @newLayout@
--     /must/ be one of the layouts in
--     'PhysicalDeviceHostImageCopyProperties'::@pCopyDstLayouts@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO'
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-oldLayout-parameter#
--     @oldLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-newLayout-parameter#
--     @newLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkHostImageLayoutTransitionInfo-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'transitionImageLayout', 'transitionImageLayout'
data HostImageLayoutTransitionInfo = HostImageLayoutTransitionInfo
  { -- | @image@ is a handle to the image affected by this layout transition.
    image :: Image
  , -- | @oldLayout@ is the old layout in an
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @subresourceRange@ describes the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-image-views image subresource range>
    -- within @image@ that is affected by this layout transition.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostImageLayoutTransitionInfo)
#endif
deriving instance Show HostImageLayoutTransitionInfo

instance ToCStruct HostImageLayoutTransitionInfo where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostImageLayoutTransitionInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (oldLayout)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (newLayout)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceRange)) (subresourceRange)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageSubresourceRange)) (zero)
    f

instance FromCStruct HostImageLayoutTransitionInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 28 :: Ptr ImageLayout))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 32 :: Ptr ImageSubresourceRange))
    pure $ HostImageLayoutTransitionInfo
             image oldLayout newLayout subresourceRange

instance Storable HostImageLayoutTransitionInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostImageLayoutTransitionInfo where
  zero = HostImageLayoutTransitionInfo
           zero
           zero
           zero
           zero


-- | VkSubresourceHostMemcpySize - Memory size needed to copy to or from an
-- image on the host with VK_HOST_IMAGE_COPY_MEMCPY_BIT
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubresourceHostMemcpySize = SubresourceHostMemcpySize
  { -- | @size@ is the size in bytes of the image subresource.
    size :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceHostMemcpySize)
#endif
deriving instance Show SubresourceHostMemcpySize

instance ToCStruct SubresourceHostMemcpySize where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceHostMemcpySize{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SubresourceHostMemcpySize where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ SubresourceHostMemcpySize
             size

instance Storable SubresourceHostMemcpySize where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubresourceHostMemcpySize where
  zero = SubresourceHostMemcpySize
           zero


-- | VkHostImageCopyDevicePerformanceQuery - Struct containing information
-- about optimality of device access
--
-- = Description
--
-- The implementation /may/ return 'Vulkan.Core10.FundamentalTypes.FALSE'
-- in @optimalDeviceAccess@ if @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'. If @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', @optimalDeviceAccess@ /must/ be
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- The implementation /may/ return 'Vulkan.Core10.FundamentalTypes.TRUE' in
-- @optimalDeviceAccess@ while @identicalMemoryLayout@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'. In this situation, any device
-- performance impact /should/ not be measurable.
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@format@
-- is a block-compressed format and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- returns 'Vulkan.Core10.Enums.Result.SUCCESS', the implementation /must/
-- return 'Vulkan.Core10.FundamentalTypes.TRUE' in @optimalDeviceAccess@.
--
-- Applications can make use of @optimalDeviceAccess@ to determine their
-- resource copying strategy. If a resource is expected to be accessed more
-- on device than on the host, and the implementation considers the
-- resource sub-optimally accessed, it is likely better to use device
-- copies instead.
--
-- Layout not being identical yet still considered optimal for device
-- access could happen if the implementation has different memory layout
-- patterns, some of which are easier to access on the host.
--
-- The most practical reason for @optimalDeviceAccess@ to be
-- 'Vulkan.Core10.FundamentalTypes.FALSE' is that host image access may
-- disable framebuffer compression where it would otherwise have been
-- enabled. This represents far more efficient host image access since no
-- compression algorithm is required to read or write to the image, but it
-- would impact device access performance. Some implementations may only
-- set @optimalDeviceAccess@ to 'Vulkan.Core10.FundamentalTypes.FALSE' if
-- certain conditions are met, such as specific image usage flags or
-- creation flags.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data HostImageCopyDevicePerformanceQuery = HostImageCopyDevicePerformanceQuery
  { -- | @optimalDeviceAccess@ returns 'Vulkan.Core10.FundamentalTypes.TRUE' if
    -- use of host image copy has no adverse effect on device access
    -- performance, compared to an image that is created with exact same
    -- creation parameters, and bound to the same
    -- 'Vulkan.Core10.Handles.DeviceMemory', except that the
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
    -- usage flag is replaced with
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'.
    optimalDeviceAccess :: Bool
  , -- | @identicalMemoryLayout@ returns 'Vulkan.Core10.FundamentalTypes.TRUE' if
    -- use of host image copy has no impact on memory layout compared to an
    -- image that is created with exact same creation parameters, and bound to
    -- the same 'Vulkan.Core10.Handles.DeviceMemory', except that the
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT'
    -- usage flag is replaced with
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'.
    identicalMemoryLayout :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostImageCopyDevicePerformanceQuery)
#endif
deriving instance Show HostImageCopyDevicePerformanceQuery

instance ToCStruct HostImageCopyDevicePerformanceQuery where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostImageCopyDevicePerformanceQuery{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (optimalDeviceAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (identicalMemoryLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct HostImageCopyDevicePerformanceQuery where
  peekCStruct p = do
    optimalDeviceAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    identicalMemoryLayout <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ HostImageCopyDevicePerformanceQuery
             (bool32ToBool optimalDeviceAccess)
             (bool32ToBool identicalMemoryLayout)

instance Storable HostImageCopyDevicePerformanceQuery where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostImageCopyDevicePerformanceQuery where
  zero = HostImageCopyDevicePerformanceQuery
           zero
           zero

