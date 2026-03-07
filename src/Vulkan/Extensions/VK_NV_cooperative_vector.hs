{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_vector - device extension
--
-- = VK_NV_cooperative_vector
--
-- [__Name String__]
--     @VK_NV_cooperative_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     492
--
-- [__Revision__]
--     4
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_vector extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_cooperative_vector.adoc VK_NV_cooperative_vector>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-23
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_vector.txt GL_NV_cooperative_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds support for using cooperative vector types in
-- SPIR-V. Unlike cooperative matrix types, a variable with a cooperative
-- vector type is logically stored in the invocation it belongs to, but
-- they can cooperate behind the scenes when performing matrix-vector
-- multiplies. Cooperative vectors do not require a fully occupied subgroup
-- or uniform control flow like cooperative matrices, although these do
-- increase the likelihood of being on the fast path. And unlike normal
-- vector types, they have arbitrary length and support a relatively
-- limited set of operations. These types are intended to help accelerate
-- the evaluation of small neural networks, where each invocation is
-- performing its own independent evaluation of the network.
--
-- Cooperative vector types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_vector.txt GL_NV_cooperative_vector>
-- GLSL extension.
--
-- This extension includes support for enumerating the combinations of
-- types that are supported by the implementation, and for converting
-- matrix data to and from an optimized opaque layout.
--
-- == New Commands
--
-- -   'cmdConvertCooperativeVectorMatrixNV'
--
-- -   'convertCooperativeVectorMatrixNV'
--
-- -   'getPhysicalDeviceCooperativeVectorPropertiesNV'
--
-- == New Structures
--
-- -   'ConvertCooperativeVectorMatrixInfoNV'
--
-- -   'CooperativeVectorPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeVectorFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeVectorPropertiesNV'
--
-- == New Unions
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR'
--
-- == New Enums
--
-- -   'ComponentTypeKHR'
--
-- -   'CooperativeVectorMatrixLayoutNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_VECTOR_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_VECTOR_SPEC_VERSION'
--
-- -   Extending 'ComponentTypeKHR':
--
--     -   'COMPONENT_TYPE_FLOAT_E4M3_NV'
--
--     -   'COMPONENT_TYPE_FLOAT_E5M2_NV'
--
--     -   'COMPONENT_TYPE_SINT8_PACKED_NV'
--
--     -   'COMPONENT_TYPE_UINT8_PACKED_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeVectorNV CooperativeVectorNV>
--
-- == Version History
--
-- -   Revision 4, 2024-05-23 (Jeff Bolz)
--
--     -   Add maxCooperativeVectorComponents
--
-- -   Revision 3, 2024-05-23 (Jeff Bolz)
--
--     -   Add training functions
--
-- -   Revision 2, 2024-02-10 (Jeff Bolz)
--
--     -   Add device-side matrix conversion
--
-- -   Revision 1, 2023-12-13 (Jeff Bolz)
--
--     -   Initial revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_cooperative_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_vector  ( getPhysicalDeviceCooperativeVectorPropertiesNV
                                                   , convertCooperativeVectorMatrixNV
                                                   , cmdConvertCooperativeVectorMatrixNV
                                                   , pattern COMPONENT_TYPE_FLOAT_E4M3_NV
                                                   , pattern COMPONENT_TYPE_FLOAT_E5M2_NV
                                                   , PhysicalDeviceCooperativeVectorFeaturesNV(..)
                                                   , CooperativeVectorPropertiesNV(..)
                                                   , PhysicalDeviceCooperativeVectorPropertiesNV(..)
                                                   , ConvertCooperativeVectorMatrixInfoNV(..)
                                                   , ComponentTypeKHR( COMPONENT_TYPE_FLOAT16_KHR
                                                                     , COMPONENT_TYPE_FLOAT32_KHR
                                                                     , COMPONENT_TYPE_FLOAT64_KHR
                                                                     , COMPONENT_TYPE_SINT8_KHR
                                                                     , COMPONENT_TYPE_SINT16_KHR
                                                                     , COMPONENT_TYPE_SINT32_KHR
                                                                     , COMPONENT_TYPE_SINT64_KHR
                                                                     , COMPONENT_TYPE_UINT8_KHR
                                                                     , COMPONENT_TYPE_UINT16_KHR
                                                                     , COMPONENT_TYPE_UINT32_KHR
                                                                     , COMPONENT_TYPE_UINT64_KHR
                                                                     , COMPONENT_TYPE_FLOAT8_E5M2_EXT
                                                                     , COMPONENT_TYPE_FLOAT8_E4M3_EXT
                                                                     , COMPONENT_TYPE_UINT8_PACKED_NV
                                                                     , COMPONENT_TYPE_SINT8_PACKED_NV
                                                                     , COMPONENT_TYPE_BFLOAT16_KHR
                                                                     , ..
                                                                     )
                                                   , CooperativeVectorMatrixLayoutNV( COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV
                                                                                    , COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV
                                                                                    , COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV
                                                                                    , COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV
                                                                                    , ..
                                                                                    )
                                                   , NV_COOPERATIVE_VECTOR_SPEC_VERSION
                                                   , pattern NV_COOPERATIVE_VECTOR_SPEC_VERSION
                                                   , NV_COOPERATIVE_VECTOR_EXTENSION_NAME
                                                   , pattern NV_COOPERATIVE_VECTOR_EXTENSION_NAME
                                                   , DeviceOrHostAddressKHR(..)
                                                   , DeviceOrHostAddressConstKHR(..)
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdConvertCooperativeVectorMatrixNV))
import Vulkan.Dynamic (DeviceCmds(pVkConvertCooperativeVectorMatrixNV))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressKHR)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCooperativeVectorPropertiesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeVectorPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeVectorPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeVectorPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceCooperativeVectorPropertiesNV - Returns properties
-- describing what cooperative vector types are supported
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of cooperative vector
-- properties available is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pPropertyCount@ is less than the number of
-- cooperative vector properties available, at most @pPropertyCount@
-- structures will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available cooperative vector properties were
-- returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeVectorPropertiesNV-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeVectorPropertiesNV-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeVectorPropertiesNV-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'CooperativeVectorPropertiesNV'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'CooperativeVectorPropertiesNV', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceCooperativeVectorPropertiesNV :: forall io
                                                . (MonadIO io)
                                               => -- | @physicalDevice@ is the physical device.
                                                  PhysicalDevice
                                               -> io (Result, ("properties" ::: Vector CooperativeVectorPropertiesNV))
getPhysicalDeviceCooperativeVectorPropertiesNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCooperativeVectorPropertiesNVPtr = pVkGetPhysicalDeviceCooperativeVectorPropertiesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCooperativeVectorPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCooperativeVectorPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceCooperativeVectorPropertiesNV' = mkVkGetPhysicalDeviceCooperativeVectorPropertiesNV vkGetPhysicalDeviceCooperativeVectorPropertiesNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeVectorPropertiesNV" (vkGetPhysicalDeviceCooperativeVectorPropertiesNV'
                                                                                     physicalDevice'
                                                                                     (pPPropertyCount)
                                                                                     (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @CooperativeVectorPropertiesNV ((fromIntegral (pPropertyCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 40) :: Ptr CooperativeVectorPropertiesNV) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeVectorPropertiesNV" (vkGetPhysicalDeviceCooperativeVectorPropertiesNV'
                                                                                      physicalDevice'
                                                                                      (pPPropertyCount)
                                                                                      ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @CooperativeVectorPropertiesNV (((pPProperties) `advancePtrBytes` (40 * (i)) :: Ptr CooperativeVectorPropertiesNV)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkConvertCooperativeVectorMatrixNV
  :: FunPtr (Ptr Device_T -> Ptr ConvertCooperativeVectorMatrixInfoNV -> IO Result) -> Ptr Device_T -> Ptr ConvertCooperativeVectorMatrixInfoNV -> IO Result

-- | vkConvertCooperativeVectorMatrixNV - Convert a cooperative vector matrix
-- from one layout and type to another
--
-- = Description
--
-- If @pInfo->dstData@ is @NULL@, then the number of bytes required to
-- store the converted matrix is returned in @pDstSize@. Otherwise,
-- @pInfo->pDstSize@ /must/ point to a variable set by the user to the
-- number of bytes in @pInfo->dstData@, and on return the variable is
-- overwritten with the number of bytes actually written to
-- @pInfo->dstData@. @pInfo->srcData@ /can/ be @NULL@ when @pInfo->dstData@
-- is @NULL@. If @pInfo->pDstSize@ is less than the number of bytes
-- required to store the converted matrix, no bytes will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not enough space
-- was provided.
--
-- == Valid Usage
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-pInfo-10073# If
--     @pInfo->srcData.hostAddress@ is @NULL@, then
--     @pInfo->dstData.hostAddress@ /must/ be @NULL@
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-pInfo-10074# If
--     @pInfo->srcData.hostAddress@ is not @NULL@, then @pInfo->srcSize@
--     /must/ be large enough to contain the source matrix, based either on
--     the standard matrix layout or based on the size filled out by this
--     command
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-pInfo-10075# If
--     @pInfo->dstData.hostAddress@ is not @NULL@, then the value pointed
--     to by @pInfo->pDstSize@ /must/ be large enough to contain the
--     destination matrix, based either on the standard matrix layout or
--     based on the size filled out by this command
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-pInfo-10076# If
--     @pInfo->dstData.hostAddress@ is not @NULL@, the source and
--     destination memory ranges /must/ not overlap
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkConvertCooperativeVectorMatrixNV-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid
--     'ConvertCooperativeVectorMatrixInfoNV' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'ConvertCooperativeVectorMatrixInfoNV', 'Vulkan.Core10.Handles.Device'
convertCooperativeVectorMatrixNV :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the device.
                                    Device
                                 -> -- | @pInfo@ is a pointer to a 'ConvertCooperativeVectorMatrixInfoNV'
                                    -- structure containing information about the layout conversion.
                                    ConvertCooperativeVectorMatrixInfoNV
                                 -> io (Result)
convertCooperativeVectorMatrixNV device info = liftIO . evalContT $ do
  let vkConvertCooperativeVectorMatrixNVPtr = pVkConvertCooperativeVectorMatrixNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkConvertCooperativeVectorMatrixNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkConvertCooperativeVectorMatrixNV is null" Nothing Nothing
  let vkConvertCooperativeVectorMatrixNV' = mkVkConvertCooperativeVectorMatrixNV vkConvertCooperativeVectorMatrixNVPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkConvertCooperativeVectorMatrixNV" (vkConvertCooperativeVectorMatrixNV'
                                                                       (deviceHandle (device))
                                                                       pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdConvertCooperativeVectorMatrixNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr ConvertCooperativeVectorMatrixInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr ConvertCooperativeVectorMatrixInfoNV -> IO ()

-- | vkCmdConvertCooperativeVectorMatrixNV - Convert a cooperative vector
-- matrix from one layout and type to another
--
-- = Description
--
-- This command does the same conversions as
-- 'convertCooperativeVectorMatrixNV', but executes on the device. One
-- conversion is performed for each of the @infoCount@ elements of
-- @pInfos@.
--
-- This command’s execution is synchronized using
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10083# For each
--     element of @pInfo@, @srcData.deviceAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10895# For each
--     element of @pInfo@, @dstData.deviceAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10084# For each
--     element of @pInfo@, @srcData.deviceAddress@ /must/ be 64 byte
--     aligned
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10085# For each
--     element of @pInfo@, @dstData.deviceAddress@ /must/ be 64 byte
--     aligned
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10086# For each
--     element of @pInfo@, @srcSize@ /must/ be large enough to contain the
--     source matrix, based either on the standard matrix layout or based
--     on the size filled out by 'convertCooperativeVectorMatrixNV'
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfo-10087# For each
--     element of @pInfo@, the value pointed to by @pDstSize@ /must/ be
--     large enough to contain the destination matrix, based either on the
--     standard matrix layout or based on the size filled out by
--     'convertCooperativeVectorMatrixNV'
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-None-10088# Memory
--     accessed by the sources and destinations of all of the conversions
--     /must/ not overlap
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-pInfos-parameter#
--     @pInfos@ /must/ be a valid pointer to an array of @infoCount@ valid
--     'ConvertCooperativeVectorMatrixInfoNV' structures
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdConvertCooperativeVectorMatrixNV-infoCount-arraylength#
--     @infoCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdConvertCooperativeVectorMatrixNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'ConvertCooperativeVectorMatrixInfoNV'
cmdConvertCooperativeVectorMatrixNV :: forall io
                                     . (MonadIO io)
                                    => -- | @commandBuffer@ is the command buffer into which the command will be
                                       -- recorded.
                                       CommandBuffer
                                    -> -- | @pInfos@ is a pointer to an array of
                                       -- 'ConvertCooperativeVectorMatrixInfoNV' structures containing information
                                       -- about the layout conversion.
                                       ("infos" ::: Vector ConvertCooperativeVectorMatrixInfoNV)
                                    -> io ()
cmdConvertCooperativeVectorMatrixNV commandBuffer
                                      infos = liftIO . evalContT $ do
  let vkCmdConvertCooperativeVectorMatrixNVPtr = pVkCmdConvertCooperativeVectorMatrixNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdConvertCooperativeVectorMatrixNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdConvertCooperativeVectorMatrixNV is null" Nothing Nothing
  let vkCmdConvertCooperativeVectorMatrixNV' = mkVkCmdConvertCooperativeVectorMatrixNV vkCmdConvertCooperativeVectorMatrixNVPtr
  pPInfos <- ContT $ allocaBytes @ConvertCooperativeVectorMatrixInfoNV ((Data.Vector.length (infos)) * 96)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (96 * (i)) :: Ptr ConvertCooperativeVectorMatrixInfoNV) (e) . ($ ())) (infos)
  lift $ traceAroundEvent "vkCmdConvertCooperativeVectorMatrixNV" (vkCmdConvertCooperativeVectorMatrixNV'
                                                                     (commandBufferHandle (commandBuffer))
                                                                     ((fromIntegral (Data.Vector.length $ (infos)) :: Word32))
                                                                     (pPInfos))
  pure $ ()


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_FLOAT_E4M3_NV"
pattern COMPONENT_TYPE_FLOAT_E4M3_NV = COMPONENT_TYPE_FLOAT8_E4M3_EXT


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_FLOAT_E5M2_NV"
pattern COMPONENT_TYPE_FLOAT_E5M2_NV = COMPONENT_TYPE_FLOAT8_E5M2_EXT


-- | VkPhysicalDeviceCooperativeVectorFeaturesNV - Structure describing
-- cooperative vector features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeVectorFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceCooperativeVectorFeaturesNV', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeVectorFeaturesNV = PhysicalDeviceCooperativeVectorFeaturesNV
  { -- | #features-cooperativeVector# @cooperativeVector@ indicates that the
    -- implementation supports the @CooperativeVectorNV@ SPIR-V capability.
    cooperativeVector :: Bool
  , -- | #features-cooperativeVectorTraining# @cooperativeVectorTraining@
    -- indicates that the implementation supports the
    -- @CooperativeVectorTrainingNV@ SPIR-V capability.
    cooperativeVectorTraining :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeVectorFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeVectorFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeVectorFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeVectorFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeVector))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeVectorTraining))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeVectorFeaturesNV where
  peekCStruct p = do
    cooperativeVector <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    cooperativeVectorTraining <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeVectorFeaturesNV
             (bool32ToBool cooperativeVector)
             (bool32ToBool cooperativeVectorTraining)

instance Storable PhysicalDeviceCooperativeVectorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeVectorFeaturesNV where
  zero = PhysicalDeviceCooperativeVectorFeaturesNV
           zero
           zero


-- | VkCooperativeVectorPropertiesNV - Structure specifying cooperative
-- vector properties
--
-- = Description
--
-- 'COMPONENT_TYPE_SINT8_PACKED_NV' and 'COMPONENT_TYPE_UINT8_PACKED_NV'
-- /must/ not be used for members other than @inputInterpretation@.
--
-- The following combinations /must/ be supported (each row is a required
-- combination):
--
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | inputType   | inputInterpretation | matrixInterpretation | biasInterpretation | resultType  |
-- +=============+=====================+======================+====================+=============+
-- | FLOAT16     | FLOAT16             | FLOAT16              | FLOAT16            | FLOAT16     |
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | UINT32      | SINT8_PACKED        | SINT8                | SINT32             | SINT32      |
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | SINT8       | SINT8               | SINT8                | SINT32             | SINT32      |
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | FLOAT32     | SINT8               | SINT8                | SINT32             | SINT32      |
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | FLOAT16     | FLOAT_E4M3          | FLOAT_E4M3           | FLOAT16            | FLOAT16     |
-- +-------------+---------------------+----------------------+--------------------+-------------+
-- | FLOAT16     | FLOAT_E5M2          | FLOAT_E5M2           | FLOAT16            | FLOAT16     |
-- +-------------+---------------------+----------------------+--------------------+-------------+
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'ComponentTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceCooperativeVectorPropertiesNV'
data CooperativeVectorPropertiesNV = CooperativeVectorPropertiesNV
  { -- | @inputType@ is the component type of vector @Input@, of type
    -- 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeVectorPropertiesNV-inputType-parameter# @inputType@
    -- /must/ be a valid 'ComponentTypeKHR' value
    inputType :: ComponentTypeKHR
  , -- | @inputInterpretation@ is the value of @InputInterpretation@, of type
    -- 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeVectorPropertiesNV-inputInterpretation-parameter#
    -- @inputInterpretation@ /must/ be a valid 'ComponentTypeKHR' value
    inputInterpretation :: ComponentTypeKHR
  , -- | @matrixInterpretation@ is the value of @MatrixInterpretation@, of type
    -- 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeVectorPropertiesNV-matrixInterpretation-parameter#
    -- @matrixInterpretation@ /must/ be a valid 'ComponentTypeKHR' value
    matrixInterpretation :: ComponentTypeKHR
  , -- | @biasInterpretation@ is the value of @BiasInterpretation@, of type
    -- 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeVectorPropertiesNV-biasInterpretation-parameter#
    -- @biasInterpretation@ /must/ be a valid 'ComponentTypeKHR' value
    biasInterpretation :: ComponentTypeKHR
  , -- | @resultType@ is the component type of
    -- 'Vulkan.Core10.Enums.Result.Result' @Type@, of type 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeVectorPropertiesNV-resultType-parameter# @resultType@
    -- /must/ be a valid 'ComponentTypeKHR' value
    resultType :: ComponentTypeKHR
  , -- | @transpose@ is a boolean indicating whether opaque layout matrices with
    -- this combination of input and output types supports transposition.
    transpose :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CooperativeVectorPropertiesNV)
#endif
deriving instance Show CooperativeVectorPropertiesNV

instance ToCStruct CooperativeVectorPropertiesNV where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CooperativeVectorPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ComponentTypeKHR)) (inputType)
    poke ((p `plusPtr` 20 :: Ptr ComponentTypeKHR)) (inputInterpretation)
    poke ((p `plusPtr` 24 :: Ptr ComponentTypeKHR)) (matrixInterpretation)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeKHR)) (biasInterpretation)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeKHR)) (resultType)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (transpose))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct CooperativeVectorPropertiesNV where
  peekCStruct p = do
    inputType <- peek @ComponentTypeKHR ((p `plusPtr` 16 :: Ptr ComponentTypeKHR))
    inputInterpretation <- peek @ComponentTypeKHR ((p `plusPtr` 20 :: Ptr ComponentTypeKHR))
    matrixInterpretation <- peek @ComponentTypeKHR ((p `plusPtr` 24 :: Ptr ComponentTypeKHR))
    biasInterpretation <- peek @ComponentTypeKHR ((p `plusPtr` 28 :: Ptr ComponentTypeKHR))
    resultType <- peek @ComponentTypeKHR ((p `plusPtr` 32 :: Ptr ComponentTypeKHR))
    transpose <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ CooperativeVectorPropertiesNV
             inputType
             inputInterpretation
             matrixInterpretation
             biasInterpretation
             resultType
             (bool32ToBool transpose)

instance Storable CooperativeVectorPropertiesNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CooperativeVectorPropertiesNV where
  zero = CooperativeVectorPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceCooperativeVectorPropertiesNV - Structure describing
-- cooperative vector properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeVectorPropertiesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeVectorPropertiesNV = PhysicalDeviceCooperativeVectorPropertiesNV
  { -- | #limits-cooperativeVectorSupportedStages#
    -- @cooperativeVectorSupportedStages@ is a bitfield of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' describing
    -- the shader stages that cooperative vector instructions are supported in.
    -- @cooperativeVectorSupportedStages@ will have the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' bit
    -- set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    cooperativeVectorSupportedStages :: ShaderStageFlags
  , -- | #limits-cooperativeVectorTrainingFloat16Accumulation#
    -- @cooperativeVectorTrainingFloat16Accumulation@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- cooperative vector training functions accumulating 16-bit floating-point
    -- results.
    cooperativeVectorTrainingFloat16Accumulation :: Bool
  , -- | #limits-cooperativeVectorTrainingFloat32Accumulation#
    -- @cooperativeVectorTrainingFloat32Accumulation@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- cooperative vector training functions accumulating 32-bit floating-point
    -- results.
    cooperativeVectorTrainingFloat32Accumulation :: Bool
  , -- | #limits-maxCooperativeVectorComponents# @maxCooperativeVectorComponents@
    -- indicates the maximum number of components that /can/ be in a
    -- cooperative vector.
    maxCooperativeVectorComponents :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeVectorPropertiesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeVectorPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeVectorPropertiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeVectorPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (cooperativeVectorSupportedStages)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeVectorTrainingFloat16Accumulation))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (cooperativeVectorTrainingFloat32Accumulation))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxCooperativeVectorComponents)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceCooperativeVectorPropertiesNV where
  peekCStruct p = do
    cooperativeVectorSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    cooperativeVectorTrainingFloat16Accumulation <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    cooperativeVectorTrainingFloat32Accumulation <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    maxCooperativeVectorComponents <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceCooperativeVectorPropertiesNV
             cooperativeVectorSupportedStages
             (bool32ToBool cooperativeVectorTrainingFloat16Accumulation)
             (bool32ToBool cooperativeVectorTrainingFloat32Accumulation)
             maxCooperativeVectorComponents

instance Storable PhysicalDeviceCooperativeVectorPropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeVectorPropertiesNV where
  zero = PhysicalDeviceCooperativeVectorPropertiesNV
           zero
           zero
           zero
           zero


-- | VkConvertCooperativeVectorMatrixInfoNV - Structure specifying a request
-- to convert the layout and type of a cooperative vector matrix
--
-- = Description
--
-- When called from 'cmdConvertCooperativeVectorMatrixNV', the
-- @deviceAddress@ members of @srcData@ and @dstData@ are used. When called
-- from 'convertCooperativeVectorMatrixNV', the @hostAddress@ members of
-- @srcData@ and @dstData@ are used.
--
-- For each of the source and destination matrix, if the layout is not
-- either 'COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV' or
-- 'COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV', then the
-- corresponding stride parameter is ignored.
--
-- The size of the destination is only a function of the destination layout
-- information, and does not depend on the source layout information.
--
-- Conversion /can/ be used to convert between 'COMPONENT_TYPE_FLOAT32_KHR'
-- or 'COMPONENT_TYPE_FLOAT16_KHR' and any supported lower-precision
-- floating-point type. In this case, the conversion uses
-- round-to-nearest-even rounding.
--
-- == Valid Usage
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcLayout-10077# If
--     @srcLayout@ is row-major or column-major, then @srcStride@ /must/ be
--     greater than the length of a row\/column, and a multiple of the
--     element size
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstLayout-10078# If
--     @dstLayout@ is row-major or column-major, then @dstStride@ /must/ be
--     greater than the length of a row\/column, and a multiple of the
--     element size
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcComponentType-10079#
--     If @srcComponentType@ is not a supported
--     'CooperativeVectorPropertiesNV'::@matrixInterpretation@ value as
--     reported by 'getPhysicalDeviceCooperativeVectorPropertiesNV', then
--     @srcComponentType@ /must/ be 'COMPONENT_TYPE_FLOAT32_KHR'
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstComponentType-10080#
--     If @dstComponentType@ is not a supported
--     'CooperativeVectorPropertiesNV'::@matrixInterpretation@ value as
--     reported by 'getPhysicalDeviceCooperativeVectorPropertiesNV', then
--     @dstComponentType@ /must/ be 'COMPONENT_TYPE_FLOAT32_KHR'
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcComponentType-10081#
--     If @srcComponentType@ and @dstComponentType@ are not equal, then one
--     /must/ be 'COMPONENT_TYPE_FLOAT32_KHR' or
--     'COMPONENT_TYPE_FLOAT16_KHR' and the other /must/ be a
--     lower-precision floating-point type
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstComponentType-10082#
--     If @dstComponentType@ is 'COMPONENT_TYPE_FLOAT_E4M3_NV' or
--     'COMPONENT_TYPE_FLOAT_E5M2_NV', then @dstLayout@ /must/ be
--     'COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV' or
--     'COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV'
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcData-parameter#
--     @srcData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--     union
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-pDstSize-parameter#
--     @pDstSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstData-parameter#
--     @dstData@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR'
--     union
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcComponentType-parameter#
--     @srcComponentType@ /must/ be a valid 'ComponentTypeKHR' value
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstComponentType-parameter#
--     @dstComponentType@ /must/ be a valid 'ComponentTypeKHR' value
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-srcLayout-parameter#
--     @srcLayout@ /must/ be a valid 'CooperativeVectorMatrixLayoutNV'
--     value
--
-- -   #VUID-VkConvertCooperativeVectorMatrixInfoNV-dstLayout-parameter#
--     @dstLayout@ /must/ be a valid 'CooperativeVectorMatrixLayoutNV'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'ComponentTypeKHR', 'CooperativeVectorMatrixLayoutNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdConvertCooperativeVectorMatrixNV',
-- 'convertCooperativeVectorMatrixNV'
data ConvertCooperativeVectorMatrixInfoNV = ConvertCooperativeVectorMatrixInfoNV
  { -- | @srcSize@ is the length in bytes of @srcData@.
    srcSize :: Word64
  , -- | @srcData@ is either @NULL@ or a pointer to the source data in the source
    -- layout.
    srcData :: DeviceOrHostAddressConstKHR
  , -- | @pDstSize@ is a pointer to an integer related to the number of bytes
    -- required or requested to convert.
    dstSize :: Ptr CSize
  , -- | @dstData@ is either @NULL@ or a pointer to the destination data in the
    -- destination layout.
    dstData :: DeviceOrHostAddressKHR
  , -- | @srcComponentType@ is the type of a source matrix element.
    srcComponentType :: ComponentTypeKHR
  , -- | @dstComponentType@ is the type of a destination matrix element.
    dstComponentType :: ComponentTypeKHR
  , -- | @numRows@ is the number of rows in the matrix.
    numRows :: Word32
  , -- | @numColumns@ is the number of columns in the matrix.
    numColumns :: Word32
  , -- | @srcLayout@ is the layout of the source matrix.
    srcLayout :: CooperativeVectorMatrixLayoutNV
  , -- | @srcStride@ is the number of bytes between a consecutive row or column
    -- (depending on @srcLayout@) of the source matrix, if it is row-major or
    -- column-major.
    srcStride :: Word64
  , -- | @dstLayout@ is the layout the matrix is converted to.
    dstLayout :: CooperativeVectorMatrixLayoutNV
  , -- | @dstStride@ is the number of bytes between a consecutive row or column
    -- (depending on @dstLayout@) of destination matrix, if it is row-major or
    -- column-major.
    dstStride :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ConvertCooperativeVectorMatrixInfoNV)
#endif
deriving instance Show ConvertCooperativeVectorMatrixInfoNV

instance ToCStruct ConvertCooperativeVectorMatrixInfoNV where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ConvertCooperativeVectorMatrixInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (srcSize))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (srcData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CSize))) (dstSize)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DeviceOrHostAddressKHR)) (dstData) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr ComponentTypeKHR)) (srcComponentType)
    lift $ poke ((p `plusPtr` 52 :: Ptr ComponentTypeKHR)) (dstComponentType)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (numRows)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (numColumns)
    lift $ poke ((p `plusPtr` 64 :: Ptr CooperativeVectorMatrixLayoutNV)) (srcLayout)
    lift $ poke ((p `plusPtr` 72 :: Ptr CSize)) (CSize (srcStride))
    lift $ poke ((p `plusPtr` 80 :: Ptr CooperativeVectorMatrixLayoutNV)) (dstLayout)
    lift $ poke ((p `plusPtr` 88 :: Ptr CSize)) (CSize (dstStride))
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CSize))) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr ComponentTypeKHR)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr ComponentTypeKHR)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr CooperativeVectorMatrixLayoutNV)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr CSize)) (CSize (zero))
    lift $ poke ((p `plusPtr` 80 :: Ptr CooperativeVectorMatrixLayoutNV)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr CSize)) (CSize (zero))
    lift $ f

instance Zero ConvertCooperativeVectorMatrixInfoNV where
  zero = ConvertCooperativeVectorMatrixInfoNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkComponentTypeKHR - Specify SPIR-V cooperative matrix component type
--
-- = Description
--
-- -   'COMPONENT_TYPE_FLOAT16_KHR' corresponds to SPIR-V @OpTypeFloat@ 16.
--
-- -   'COMPONENT_TYPE_FLOAT32_KHR' corresponds to SPIR-V @OpTypeFloat@ 32.
--
-- -   'COMPONENT_TYPE_FLOAT64_KHR' corresponds to SPIR-V @OpTypeFloat@ 64.
--
-- -   'COMPONENT_TYPE_SINT8_KHR' corresponds to SPIR-V @OpTypeInt@ 8 0\/1.
--
-- -   'COMPONENT_TYPE_SINT16_KHR' corresponds to SPIR-V @OpTypeInt@ 16
--     0\/1.
--
-- -   'COMPONENT_TYPE_SINT32_KHR' corresponds to SPIR-V @OpTypeInt@ 32
--     0\/1.
--
-- -   'COMPONENT_TYPE_SINT64_KHR' corresponds to SPIR-V @OpTypeInt@ 64
--     0\/1.
--
-- -   'COMPONENT_TYPE_UINT8_KHR' corresponds to SPIR-V @OpTypeInt@ 8 0\/1.
--
-- -   'COMPONENT_TYPE_UINT16_KHR' corresponds to SPIR-V @OpTypeInt@ 16
--     0\/1.
--
-- -   'COMPONENT_TYPE_UINT32_KHR' corresponds to SPIR-V @OpTypeInt@ 32
--     0\/1.
--
-- -   'COMPONENT_TYPE_UINT64_KHR' corresponds to SPIR-V @OpTypeInt@ 64
--     0\/1.
--
-- -   'COMPONENT_TYPE_BFLOAT16_KHR' corresponds to SPIR-V @OpTypeFloat@ 16
--     BFloat16KHR.
--
-- -   'COMPONENT_TYPE_SINT8_PACKED_NV' corresponds to four 8-bit signed
--     integers packed in a 32-bit unsigned integer.
--
-- -   'COMPONENT_TYPE_UINT8_PACKED_NV' corresponds to four 8-bit unsigned
--     integers packed in a 32-bit unsigned integer.
--
-- -   'COMPONENT_TYPE_FLOAT_E4M3_NV' corresponds to a floating-point type
--     with a sign bit in the most significant bit, followed by four
--     exponent bits, followed by three mantissa bits.
--
-- -   'COMPONENT_TYPE_FLOAT_E5M2_NV' corresponds to a floating-point type
--     with a sign bit in the most significant bit, followed by five
--     exponent bits, followed by two mantissa bits.
--
-- -   'COMPONENT_TYPE_FLOAT8_E4M3_EXT' corresponds to SPIR-V @OpTypeFloat@
--     8 Float8E4M3EXT.
--
-- -   'COMPONENT_TYPE_FLOAT8_E5M2_EXT' corresponds to SPIR-V @OpTypeFloat@
--     8 Float8E5M2EXT.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix VK_NV_cooperative_matrix>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'ConvertCooperativeVectorMatrixInfoNV',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix2.CooperativeMatrixFlexibleDimensionsPropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.CooperativeMatrixPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.CooperativeMatrixPropertiesNV',
-- 'CooperativeVectorPropertiesNV'
newtype ComponentTypeKHR = ComponentTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_FLOAT16_KHR"
pattern COMPONENT_TYPE_FLOAT16_KHR = ComponentTypeKHR 0

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_FLOAT32_KHR"
pattern COMPONENT_TYPE_FLOAT32_KHR = ComponentTypeKHR 1

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_FLOAT64_KHR"
pattern COMPONENT_TYPE_FLOAT64_KHR = ComponentTypeKHR 2

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_SINT8_KHR"
pattern COMPONENT_TYPE_SINT8_KHR = ComponentTypeKHR 3

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_SINT16_KHR"
pattern COMPONENT_TYPE_SINT16_KHR = ComponentTypeKHR 4

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_SINT32_KHR"
pattern COMPONENT_TYPE_SINT32_KHR = ComponentTypeKHR 5

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_SINT64_KHR"
pattern COMPONENT_TYPE_SINT64_KHR = ComponentTypeKHR 6

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_UINT8_KHR"
pattern COMPONENT_TYPE_UINT8_KHR = ComponentTypeKHR 7

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_UINT16_KHR"
pattern COMPONENT_TYPE_UINT16_KHR = ComponentTypeKHR 8

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_UINT32_KHR"
pattern COMPONENT_TYPE_UINT32_KHR = ComponentTypeKHR 9

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_UINT64_KHR"
pattern COMPONENT_TYPE_UINT64_KHR = ComponentTypeKHR 10

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_FLOAT8_E5M2_EXT"
pattern COMPONENT_TYPE_FLOAT8_E5M2_EXT = ComponentTypeKHR 1000491003

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_FLOAT8_E4M3_EXT"
pattern COMPONENT_TYPE_FLOAT8_E4M3_EXT = ComponentTypeKHR 1000491002

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_UINT8_PACKED_NV"
pattern COMPONENT_TYPE_UINT8_PACKED_NV = ComponentTypeKHR 1000491001

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_SINT8_PACKED_NV"
pattern COMPONENT_TYPE_SINT8_PACKED_NV = ComponentTypeKHR 1000491000

-- No documentation found for Nested "VkComponentTypeKHR" "VK_COMPONENT_TYPE_BFLOAT16_KHR"
pattern COMPONENT_TYPE_BFLOAT16_KHR = ComponentTypeKHR 1000141000

{-# COMPLETE
  COMPONENT_TYPE_FLOAT16_KHR
  , COMPONENT_TYPE_FLOAT32_KHR
  , COMPONENT_TYPE_FLOAT64_KHR
  , COMPONENT_TYPE_SINT8_KHR
  , COMPONENT_TYPE_SINT16_KHR
  , COMPONENT_TYPE_SINT32_KHR
  , COMPONENT_TYPE_SINT64_KHR
  , COMPONENT_TYPE_UINT8_KHR
  , COMPONENT_TYPE_UINT16_KHR
  , COMPONENT_TYPE_UINT32_KHR
  , COMPONENT_TYPE_UINT64_KHR
  , COMPONENT_TYPE_FLOAT8_E5M2_EXT
  , COMPONENT_TYPE_FLOAT8_E4M3_EXT
  , COMPONENT_TYPE_UINT8_PACKED_NV
  , COMPONENT_TYPE_SINT8_PACKED_NV
  , COMPONENT_TYPE_BFLOAT16_KHR ::
    ComponentTypeKHR
  #-}

conNameComponentTypeKHR :: String
conNameComponentTypeKHR = "ComponentTypeKHR"

enumPrefixComponentTypeKHR :: String
enumPrefixComponentTypeKHR = "COMPONENT_TYPE_"

showTableComponentTypeKHR :: [(ComponentTypeKHR, String)]
showTableComponentTypeKHR =
  [ (COMPONENT_TYPE_FLOAT16_KHR, "FLOAT16_KHR")
  , (COMPONENT_TYPE_FLOAT32_KHR, "FLOAT32_KHR")
  , (COMPONENT_TYPE_FLOAT64_KHR, "FLOAT64_KHR")
  , (COMPONENT_TYPE_SINT8_KHR, "SINT8_KHR")
  , (COMPONENT_TYPE_SINT16_KHR, "SINT16_KHR")
  , (COMPONENT_TYPE_SINT32_KHR, "SINT32_KHR")
  , (COMPONENT_TYPE_SINT64_KHR, "SINT64_KHR")
  , (COMPONENT_TYPE_UINT8_KHR, "UINT8_KHR")
  , (COMPONENT_TYPE_UINT16_KHR, "UINT16_KHR")
  , (COMPONENT_TYPE_UINT32_KHR, "UINT32_KHR")
  , (COMPONENT_TYPE_UINT64_KHR, "UINT64_KHR")
  ,
    ( COMPONENT_TYPE_FLOAT8_E5M2_EXT
    , "FLOAT8_E5M2_EXT"
    )
  ,
    ( COMPONENT_TYPE_FLOAT8_E4M3_EXT
    , "FLOAT8_E4M3_EXT"
    )
  ,
    ( COMPONENT_TYPE_UINT8_PACKED_NV
    , "UINT8_PACKED_NV"
    )
  ,
    ( COMPONENT_TYPE_SINT8_PACKED_NV
    , "SINT8_PACKED_NV"
    )
  , (COMPONENT_TYPE_BFLOAT16_KHR, "BFLOAT16_KHR")
  ]

instance Show ComponentTypeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixComponentTypeKHR
      showTableComponentTypeKHR
      conNameComponentTypeKHR
      (\(ComponentTypeKHR x) -> x)
      (showsPrec 11)

instance Read ComponentTypeKHR where
  readPrec =
    enumReadPrec
      enumPrefixComponentTypeKHR
      showTableComponentTypeKHR
      conNameComponentTypeKHR
      ComponentTypeKHR

-- | VkCooperativeVectorMatrixLayoutNV - Specify cooperative vector matrix
-- layout
--
-- = Description
--
-- -   'COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV' corresponds to
--     SPIR-V @RowMajorNV@ layout.
--
-- -   'COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV' corresponds to
--     SPIR-V @ColumnMajorNV@ layout.
--
-- -   'COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV'
--     corresponds to SPIR-V @InferencingOptimalNV@ layout.
--
-- -   'COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV' corresponds
--     to SPIR-V @TrainingOptimalNV@ layout.
--
-- All enum values match the corresponding SPIR-V value.
--
-- Row-major layout has elements of each row stored consecutively in
-- memory, with a controllable stride from the start of one row to the
-- start of the next row. Column-major layout has elements of each column
-- stored consecutively in memory, with a controllable stride from the
-- start of one column to the start of the next column. Inferencing-optimal
-- and Training-optimal layouts are implementation-dependent, and the
-- application /can/ convert a matrix to those layouts using
-- 'convertCooperativeVectorMatrixNV' or
-- 'cmdConvertCooperativeVectorMatrixNV'. Training-optimal layout with
-- 'COMPONENT_TYPE_FLOAT16_KHR' or 'COMPONENT_TYPE_FLOAT32_KHR' type has
-- the additional guarantee that the application /can/ reinterpret the data
-- as an array of elements and perform element-wise operations on the data,
-- and finite values in any padding elements do not affect the result of a
-- matrix-vector multiply (inf\/NaN values /may/ still cause NaN values in
-- the result).
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_vector VK_NV_cooperative_vector>,
-- 'ConvertCooperativeVectorMatrixInfoNV'
newtype CooperativeVectorMatrixLayoutNV = CooperativeVectorMatrixLayoutNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCooperativeVectorMatrixLayoutNV" "VK_COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV"
pattern COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV = CooperativeVectorMatrixLayoutNV 0

-- No documentation found for Nested "VkCooperativeVectorMatrixLayoutNV" "VK_COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV"
pattern COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV = CooperativeVectorMatrixLayoutNV 1

-- No documentation found for Nested "VkCooperativeVectorMatrixLayoutNV" "VK_COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV"
pattern COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV = CooperativeVectorMatrixLayoutNV 2

-- No documentation found for Nested "VkCooperativeVectorMatrixLayoutNV" "VK_COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV"
pattern COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV = CooperativeVectorMatrixLayoutNV 3

{-# COMPLETE
  COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV
  , COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV
  , COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV
  , COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV ::
    CooperativeVectorMatrixLayoutNV
  #-}

conNameCooperativeVectorMatrixLayoutNV :: String
conNameCooperativeVectorMatrixLayoutNV = "CooperativeVectorMatrixLayoutNV"

enumPrefixCooperativeVectorMatrixLayoutNV :: String
enumPrefixCooperativeVectorMatrixLayoutNV = "COOPERATIVE_VECTOR_MATRIX_LAYOUT_"

showTableCooperativeVectorMatrixLayoutNV :: [(CooperativeVectorMatrixLayoutNV, String)]
showTableCooperativeVectorMatrixLayoutNV =
  [
    ( COOPERATIVE_VECTOR_MATRIX_LAYOUT_ROW_MAJOR_NV
    , "ROW_MAJOR_NV"
    )
  ,
    ( COOPERATIVE_VECTOR_MATRIX_LAYOUT_COLUMN_MAJOR_NV
    , "COLUMN_MAJOR_NV"
    )
  ,
    ( COOPERATIVE_VECTOR_MATRIX_LAYOUT_INFERENCING_OPTIMAL_NV
    , "INFERENCING_OPTIMAL_NV"
    )
  ,
    ( COOPERATIVE_VECTOR_MATRIX_LAYOUT_TRAINING_OPTIMAL_NV
    , "TRAINING_OPTIMAL_NV"
    )
  ]

instance Show CooperativeVectorMatrixLayoutNV where
  showsPrec =
    enumShowsPrec
      enumPrefixCooperativeVectorMatrixLayoutNV
      showTableCooperativeVectorMatrixLayoutNV
      conNameCooperativeVectorMatrixLayoutNV
      (\(CooperativeVectorMatrixLayoutNV x) -> x)
      (showsPrec 11)

instance Read CooperativeVectorMatrixLayoutNV where
  readPrec =
    enumReadPrec
      enumPrefixCooperativeVectorMatrixLayoutNV
      showTableCooperativeVectorMatrixLayoutNV
      conNameCooperativeVectorMatrixLayoutNV
      CooperativeVectorMatrixLayoutNV

type NV_COOPERATIVE_VECTOR_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_VECTOR_SPEC_VERSION"
pattern NV_COOPERATIVE_VECTOR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COOPERATIVE_VECTOR_SPEC_VERSION = 4


type NV_COOPERATIVE_VECTOR_EXTENSION_NAME = "VK_NV_cooperative_vector"

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_VECTOR_EXTENSION_NAME"
pattern NV_COOPERATIVE_VECTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COOPERATIVE_VECTOR_EXTENSION_NAME = "VK_NV_cooperative_vector"

