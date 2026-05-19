{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_optical_flow - device extension
--
-- = VK_ARM_data_graph_optical_flow
--
-- [__Name String__]
--     @VK_ARM_data_graph_optical_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     632
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_optical_flow] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_optical_flow extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-08
--
-- [__Contributors__]
--
--     -   Contributors to VK_NV_optical_flow
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Steve Suzuki, Arm Ltd.
--
--     -   Liam O’Neil, Arm Ltd.
--
-- == Description
--
-- This extension allows applications to estimate the 2D displacement of
-- pixels between two images.
--
-- == New Commands
--
-- -   'Vulkan.Extensions.VK_ARM_data_graph_instruction_set_tosa.getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
--
-- == New Structures
--
-- -   'DataGraphOpticalFlowImageFormatPropertiesARM'
--
-- -   'DataGraphPipelineSingleNodeConnectionARM'
--
-- -   'QueueFamilyDataGraphOpticalFlowPropertiesARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineOpticalFlowCreateInfoARM'
--
--     -   'DataGraphPipelineSingleNodeCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineDispatchInfoARM':
--
--     -   'DataGraphPipelineOpticalFlowDispatchInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineResourceInfoARM':
--
--     -   'DataGraphPipelineResourceInfoImageLayoutARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphOpticalFlowFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
--     'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'DataGraphOpticalFlowImageFormatInfoARM'
--
-- == New Enums
--
-- -   'DataGraphOpticalFlowCreateFlagBitsARM'
--
-- -   'DataGraphOpticalFlowExecuteFlagBitsARM'
--
-- -   'DataGraphOpticalFlowGridSizeFlagBitsARM'
--
-- -   'DataGraphOpticalFlowImageUsageFlagBitsARM'
--
-- -   'DataGraphOpticalFlowPerformanceLevelARM'
--
-- -   'DataGraphPipelineNodeConnectionTypeARM'
--
-- -   'DataGraphPipelineNodeTypeARM'
--
-- == New Bitmasks
--
-- -   'DataGraphOpticalFlowCreateFlagsARM'
--
-- -   'DataGraphOpticalFlowExecuteFlagsARM'
--
-- -   'DataGraphOpticalFlowGridSizeFlagsARM'
--
-- -   'DataGraphOpticalFlowImageUsageFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION'
--
-- -   Extending 'DataGraphPipelineNodeConnectionTypeARM':
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM'
--
-- -   Extending 'DataGraphPipelineNodeTypeARM':
--
--     -   'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionBindPointARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_OPTICAL_FLOW_CACHE_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateFlagBitsARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_CREATE_OPTICAL_FLOW_CACHE_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_COST_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_IMAGE_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_VECTOR_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_OPTICAL_FLOW_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_DISPATCH_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_IMAGE_LAYOUT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CONNECTION_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_OPTICAL_FLOW_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_OPTICAL_FLOW_PROPERTIES_ARM'
--
-- == Version History
--
-- -   Revision 1, 2026-04-08 (Kevin Petit)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_optical_flow Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_optical_flow  ( getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM
                                                         , DataGraphPipelineResourceInfoImageLayoutARM(..)
                                                         , DataGraphPipelineSingleNodeConnectionARM(..)
                                                         , PhysicalDeviceDataGraphOpticalFlowFeaturesARM(..)
                                                         , QueueFamilyDataGraphOpticalFlowPropertiesARM(..)
                                                         , DataGraphOpticalFlowImageFormatInfoARM(..)
                                                         , DataGraphOpticalFlowImageFormatPropertiesARM(..)
                                                         , DataGraphPipelineSingleNodeCreateInfoARM(..)
                                                         , DataGraphPipelineOpticalFlowCreateInfoARM(..)
                                                         , DataGraphPipelineOpticalFlowDispatchInfoARM(..)
                                                         , DataGraphOpticalFlowGridSizeFlagsARM
                                                         , DataGraphOpticalFlowGridSizeFlagBitsARM( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_UNKNOWN_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_1X1_BIT_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_2X2_BIT_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_8X8_BIT_ARM
                                                                                                  , ..
                                                                                                  )
                                                         , DataGraphOpticalFlowImageUsageFlagsARM
                                                         , DataGraphOpticalFlowImageUsageFlagBitsARM( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_UNKNOWN_ARM
                                                                                                    , DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM
                                                                                                    , DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM
                                                                                                    , DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM
                                                                                                    , DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM
                                                                                                    , ..
                                                                                                    )
                                                         , DataGraphOpticalFlowPerformanceLevelARM( DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM
                                                                                                  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM
                                                                                                  , ..
                                                                                                  )
                                                         , DataGraphPipelineNodeConnectionTypeARM( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM
                                                                                                 , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM
                                                                                                 , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM
                                                                                                 , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM
                                                                                                 , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM
                                                                                                 , ..
                                                                                                 )
                                                         , DataGraphPipelineNodeTypeARM( DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM
                                                                                       , ..
                                                                                       )
                                                         , DataGraphOpticalFlowCreateFlagsARM
                                                         , DataGraphOpticalFlowCreateFlagBitsARM( DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM
                                                                                                , DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM
                                                                                                , DATA_GRAPH_OPTICAL_FLOW_CREATE_RESERVED_30_BIT_ARM
                                                                                                , ..
                                                                                                )
                                                         , DataGraphOpticalFlowExecuteFlagsARM
                                                         , DataGraphOpticalFlowExecuteFlagBitsARM( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_ARM
                                                                                                 , DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_UNCHANGED_BIT_ARM
                                                                                                 , DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_UNCHANGED_BIT_ARM
                                                                                                 , DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM
                                                                                                 , DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM
                                                                                                 , ..
                                                                                                 )
                                                         , ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION
                                                         , pattern ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION
                                                         , ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME
                                                         , pattern ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME
                                                         , PhysicalDeviceDataGraphProcessingEngineARM(..)
                                                         , PhysicalDeviceDataGraphOperationSupportARM(..)
                                                         , QueueFamilyDataGraphPropertiesARM(..)
                                                         , getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM
                                                         , DataGraphPipelineSessionCreateFlagBitsARM(..)
                                                         , DataGraphPipelineSessionCreateFlagsARM
                                                         , DataGraphPipelineSessionBindPointARM(..)
                                                         , PhysicalDeviceDataGraphProcessingEngineTypeARM(..)
                                                         , PhysicalDeviceDataGraphOperationTypeARM(..)
                                                         , MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                         , pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                         ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Extensions.VK_ARM_data_graph (QueueFamilyDataGraphPropertiesARM)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_DISPATCH_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_IMAGE_LAYOUT_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CONNECTION_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_OPTICAL_FLOW_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_OPTICAL_FLOW_PROPERTIES_ARM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_ARM_data_graph_instruction_set_tosa (getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM)
import Vulkan.Extensions.VK_ARM_data_graph (DataGraphPipelineSessionBindPointARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (DataGraphPipelineSessionCreateFlagBitsARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (DataGraphPipelineSessionCreateFlagsARM)
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationSupportARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationTypeARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphProcessingEngineARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphProcessingEngineTypeARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (QueueFamilyDataGraphPropertiesARM(..))
import Vulkan.Core10.APIConstants (pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> Ptr DataGraphOpticalFlowImageFormatInfoARM -> Ptr Word32 -> Ptr DataGraphOpticalFlowImageFormatPropertiesARM -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> Ptr DataGraphOpticalFlowImageFormatInfoARM -> Ptr Word32 -> Ptr DataGraphOpticalFlowImageFormatPropertiesARM -> IO Result

-- | vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM -
-- Query image formats for data graph optical flow
--
-- = Description
--
-- If @pImageFormatProperties@ is @NULL@, then the number of optical flow
-- properties supported for the given @physicalDevice@ is returned in
-- @pFormatCount@. Otherwise, @pFormatCount@ /must/ point to a variable set
-- by the user to the number of elements in the @pImageFormatProperties@
-- array, and on return the variable is overwritten with the number of
-- values actually written to @pImageFormatProperties@. If the value of
-- @pFormatCount@ is less than the number of optical flow properties
-- supported, at most @pFormatCount@ values will be written to
-- @pImageFormatProperties@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available values were returned. Before
-- creating an image to be used as a optical flow image, obtain the
-- supported image creation parameters by querying with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- using one of the reported formats and adding
-- 'DataGraphOpticalFlowImageFormatInfoARM' to the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'.
-- When querying the parameters with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- for images used for optical flow operations, the
-- 'DataGraphOpticalFlowImageFormatInfoARM'::@usage@ field should contain
-- one or more of the bits defined in
-- 'DataGraphOpticalFlowImageUsageFlagBitsARM'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-pQueueFamilyDataGraphProperties-09965#
--     @pQueueFamilyDataGraphProperties@ /must/ point to a structure whose
--     @operation@ member has its @name@ member equal to @OpticalFlow@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-pQueueFamilyDataGraphProperties-parameter#
--     @pQueueFamilyDataGraphProperties@ /must/ be a valid pointer to a
--     valid
--     'Vulkan.Extensions.VK_ARM_data_graph.QueueFamilyDataGraphPropertiesARM'
--     structure
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-pOpticalFlowImageFormatInfo-parameter#
--     @pOpticalFlowImageFormatInfo@ /must/ be a valid pointer to a valid
--     'DataGraphOpticalFlowImageFormatInfoARM' structure
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-pFormatCount-parameter#
--     @pFormatCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM-pImageFormatProperties-parameter#
--     If the value referenced by @pFormatCount@ is not @0@, and
--     @pImageFormatProperties@ is not @NULL@, @pImageFormatProperties@
--     /must/ be a valid pointer to an array of @pFormatCount@
--     'DataGraphOpticalFlowImageFormatPropertiesARM' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_B8G8R8A8_UNORM',
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM',
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8_UNORM',
-- 'Vulkan.Core10.Enums.Format.FORMAT_B8G8R8_UNORM',
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UNORM', and
-- 'Vulkan.Core10.Enums.Format.FORMAT_B10G11R11_UFLOAT_PACK32' are
-- initially supported for images with
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#opticalflow-usageARM optical flow usage>
-- 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM'.
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SFLOAT' is initially supported
-- for images with
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#opticalflow-usageARM optical flow usage>
-- 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM' and
-- 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM'.
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT' is initially supported for
-- images with
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#opticalflow-usageARM optical flow usage>
-- 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowImageFormatInfoARM',
-- 'DataGraphOpticalFlowImageFormatPropertiesARM',
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Extensions.VK_ARM_data_graph.QueueFamilyDataGraphPropertiesARM'
getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM :: forall io
                                                                 . (MonadIO io)
                                                                => -- | @physicalDevice@ is the physical device being queried.
                                                                   PhysicalDevice
                                                                -> -- | @queueFamilyIndex@ is the index of the queue family being queried.
                                                                   ("queueFamilyIndex" ::: Word32)
                                                                -> -- | @pQueueFamilyDataGraphProperties@ is a pointer to a
                                                                   -- 'Vulkan.Extensions.VK_ARM_data_graph.QueueFamilyDataGraphPropertiesARM'
                                                                   -- structure that selects the processing engine and operation set for which
                                                                   -- the properties are queried.
                                                                   QueueFamilyDataGraphPropertiesARM
                                                                -> -- | @pOpticalFlowImageFormatInfo@ is a pointer to a
                                                                   -- 'DataGraphOpticalFlowImageFormatInfoARM' structure specifying the
                                                                   -- optical flow usage for which information is returned.
                                                                   DataGraphOpticalFlowImageFormatInfoARM
                                                                -> io (Result, ("imageFormatProperties" ::: Vector DataGraphOpticalFlowImageFormatPropertiesARM))
getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM physicalDevice
                                                                  queueFamilyIndex
                                                                  queueFamilyDataGraphProperties
                                                                  opticalFlowImageFormatInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARMPtr = pVkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM' = mkVkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARMPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pQueueFamilyDataGraphProperties <- ContT $ withCStruct (queueFamilyDataGraphProperties)
  pOpticalFlowImageFormatInfo <- ContT $ withCStruct (opticalFlowImageFormatInfo)
  pPFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM" (vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
                                                                                                      physicalDevice'
                                                                                                      (queueFamilyIndex)
                                                                                                      pQueueFamilyDataGraphProperties
                                                                                                      pOpticalFlowImageFormatInfo
                                                                                                      (pPFormatCount)
                                                                                                      (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFormatCount <- lift $ peek @Word32 pPFormatCount
  pPImageFormatProperties <- ContT $ bracket (callocBytes @DataGraphOpticalFlowImageFormatPropertiesARM ((fromIntegral (pFormatCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPImageFormatProperties `advancePtrBytes` (i * 24) :: Ptr DataGraphOpticalFlowImageFormatPropertiesARM) . ($ ())) [0..(fromIntegral (pFormatCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM" (vkGetPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
                                                                                                       physicalDevice'
                                                                                                       (queueFamilyIndex)
                                                                                                       pQueueFamilyDataGraphProperties
                                                                                                       pOpticalFlowImageFormatInfo
                                                                                                       (pPFormatCount)
                                                                                                       ((pPImageFormatProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFormatCount' <- lift $ peek @Word32 pPFormatCount
  pImageFormatProperties' <- lift $ generateM (fromIntegral (pFormatCount')) (\i -> peekCStruct @DataGraphOpticalFlowImageFormatPropertiesARM (((pPImageFormatProperties) `advancePtrBytes` (24 * (i)) :: Ptr DataGraphOpticalFlowImageFormatPropertiesARM)))
  pure $ ((r'), pImageFormatProperties')


-- | VkDataGraphPipelineResourceInfoImageLayoutARM - Structure specifying
-- parameters of a graph pipeline image resource
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineResourceInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineResourceInfoImageLayoutARM = DataGraphPipelineResourceInfoImageLayoutARM
  { -- | @layout@ specifies the layout that the image subresource accessible from
    -- the view provided as a graph pipeline resource /must/ be in at the time
    -- where the graph pipeline being created is dispatched.
    --
    -- #VUID-VkDataGraphPipelineResourceInfoImageLayoutARM-layout-parameter#
    -- @layout@ /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
    -- value
    layout :: ImageLayout }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineResourceInfoImageLayoutARM)
#endif
deriving instance Show DataGraphPipelineResourceInfoImageLayoutARM

instance ToCStruct DataGraphPipelineResourceInfoImageLayoutARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineResourceInfoImageLayoutARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_IMAGE_LAYOUT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (layout)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_IMAGE_LAYOUT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct DataGraphPipelineResourceInfoImageLayoutARM where
  peekCStruct p = do
    layout <- peek @ImageLayout ((p `plusPtr` 16 :: Ptr ImageLayout))
    pure $ DataGraphPipelineResourceInfoImageLayoutARM
             layout

instance Storable DataGraphPipelineResourceInfoImageLayoutARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineResourceInfoImageLayoutARM where
  zero = DataGraphPipelineResourceInfoImageLayoutARM
           zero


-- | VkDataGraphPipelineSingleNodeConnectionARM - Structure describing a
-- single connection between a data graph node and the pipeline layout of a
-- graph pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphPipelineNodeConnectionTypeARM',
-- 'DataGraphPipelineSingleNodeCreateInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineSingleNodeConnectionARM = DataGraphPipelineSingleNodeConnectionARM
  { -- | @set@ is the descriptor set number of the graph pipeline layout resource
    -- to be connected to this connection point.
    set :: Word32
  , -- | @binding@ is the binding number of the graph pipeline layout resource to
    -- be connected to this connection point.
    binding :: Word32
  , -- | @connection@ is a 'DataGraphPipelineNodeConnectionTypeARM' specifying
    -- the connection point to link to a graph pipeline layout resource.
    --
    -- #VUID-VkDataGraphPipelineSingleNodeConnectionARM-connection-parameter#
    -- @connection@ /must/ be a valid 'DataGraphPipelineNodeConnectionTypeARM'
    -- value
    connection :: DataGraphPipelineNodeConnectionTypeARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSingleNodeConnectionARM)
#endif
deriving instance Show DataGraphPipelineSingleNodeConnectionARM

instance ToCStruct DataGraphPipelineSingleNodeConnectionARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSingleNodeConnectionARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CONNECTION_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (set)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineNodeConnectionTypeARM)) (connection)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CONNECTION_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineNodeConnectionTypeARM)) (zero)
    f

instance FromCStruct DataGraphPipelineSingleNodeConnectionARM where
  peekCStruct p = do
    set <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    binding <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    connection <- peek @DataGraphPipelineNodeConnectionTypeARM ((p `plusPtr` 24 :: Ptr DataGraphPipelineNodeConnectionTypeARM))
    pure $ DataGraphPipelineSingleNodeConnectionARM
             set binding connection

instance Storable DataGraphPipelineSingleNodeConnectionARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSingleNodeConnectionARM where
  zero = DataGraphPipelineSingleNodeConnectionARM
           zero
           zero
           zero


-- | VkPhysicalDeviceDataGraphOpticalFlowFeaturesARM - Structure describing
-- the data graph optical flow features supported by the implementation
--
-- = Members
--
-- This structure describes the following feature: * @sType@ is a
-- 'Vulkan.Core10.Enums.StructureType.StructureType' value identifying this
-- structure. * @pNext@ is @NULL@ or a pointer to a structure extending
-- this structure. * #features-dataGraphOpticalFlow# @dataGraphOpticalFlow@
-- indicates whether the implementation supports optical flow graph
-- pipelines.
--
-- If the 'PhysicalDeviceDataGraphOpticalFlowFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDataGraphOpticalFlowFeaturesARM', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDataGraphOpticalFlowFeaturesARM = PhysicalDeviceDataGraphOpticalFlowFeaturesARM
  { -- No documentation found for Nested "VkPhysicalDeviceDataGraphOpticalFlowFeaturesARM" "dataGraphOpticalFlow"
    dataGraphOpticalFlow :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphOpticalFlowFeaturesARM)
#endif
deriving instance Show PhysicalDeviceDataGraphOpticalFlowFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphOpticalFlowFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphOpticalFlowFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_OPTICAL_FLOW_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dataGraphOpticalFlow))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_OPTICAL_FLOW_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDataGraphOpticalFlowFeaturesARM where
  peekCStruct p = do
    dataGraphOpticalFlow <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDataGraphOpticalFlowFeaturesARM
             (bool32ToBool dataGraphOpticalFlow)

instance Storable PhysicalDeviceDataGraphOpticalFlowFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphOpticalFlowFeaturesARM where
  zero = PhysicalDeviceDataGraphOpticalFlowFeaturesARM
           zero


-- | VkQueueFamilyDataGraphOpticalFlowPropertiesARM - Structure describing
-- optical flow properties of a processing engine and operation set for a
-- specific queue family of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DataGraphOpticalFlowGridSizeFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyDataGraphOpticalFlowPropertiesARM = QueueFamilyDataGraphOpticalFlowPropertiesARM
  { -- | #limits-supportedOutputGridSizesARM# @supportedOutputGridSizes@ are the
    -- supported 'DataGraphOpticalFlowGridSizeFlagsARM' which /can/ be
    -- specified in
    -- 'DataGraphPipelineOpticalFlowCreateInfoARM'::@outputGridSize@.
    supportedOutputGridSizes :: DataGraphOpticalFlowGridSizeFlagsARM
  , -- | #limits-supportedHintGridSizesARM# @supportedHintGridSizes@ are the
    -- supported 'DataGraphOpticalFlowGridSizeFlagsARM' which /can/ be
    -- specified in
    -- 'DataGraphPipelineOpticalFlowCreateInfoARM'::@hintGridSize@.
    supportedHintGridSizes :: DataGraphOpticalFlowGridSizeFlagsARM
  , -- | #limits-hintSupportedARM# @hintSupported@ is a boolean describing
    -- whether using hint flow vector map is supported in an optical flow graph
    -- pipeline.
    hintSupported :: Bool
  , -- | #limits-costSupportedARM# @costSupported@ is a boolean describing
    -- whether cost map generation is supported in an optical flow graph
    -- pipeline.
    costSupported :: Bool
  , -- | #limits-minWidthARM# @minWidth@ is the minimum width in pixels for
    -- images used in an optical flow graph pipeline.
    minWidth :: Word32
  , -- | #limits-minHeightARM# @minHeight@ is the minimum height in pixels for
    -- images used in an optical flow graph pipeline.
    minHeight :: Word32
  , -- | #limits-maxWidthARM# @maxWidth@ is the maximum width in pixels for
    -- images used in an optical flow graph pipeline.
    maxWidth :: Word32
  , -- | #limits-maxHeightARM# @maxHeight@ is the maximum height in pixels for
    -- images used in an optical flow graph pipeline.
    maxHeight :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyDataGraphOpticalFlowPropertiesARM)
#endif
deriving instance Show QueueFamilyDataGraphOpticalFlowPropertiesARM

instance ToCStruct QueueFamilyDataGraphOpticalFlowPropertiesARM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyDataGraphOpticalFlowPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_OPTICAL_FLOW_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (supportedOutputGridSizes)
    poke ((p `plusPtr` 20 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (supportedHintGridSizes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (hintSupported))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (costSupported))
    poke ((p `plusPtr` 32 :: Ptr Word32)) (minWidth)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (minHeight)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxWidth)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxHeight)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_OPTICAL_FLOW_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (zero)
    poke ((p `plusPtr` 20 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueueFamilyDataGraphOpticalFlowPropertiesARM where
  peekCStruct p = do
    supportedOutputGridSizes <- peek @DataGraphOpticalFlowGridSizeFlagsARM ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM))
    supportedHintGridSizes <- peek @DataGraphOpticalFlowGridSizeFlagsARM ((p `plusPtr` 20 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM))
    hintSupported <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    costSupported <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    minWidth <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    minHeight <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxWidth <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxHeight <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ QueueFamilyDataGraphOpticalFlowPropertiesARM
             supportedOutputGridSizes
             supportedHintGridSizes
             (bool32ToBool hintSupported)
             (bool32ToBool costSupported)
             minWidth
             minHeight
             maxWidth
             maxHeight

instance Storable QueueFamilyDataGraphOpticalFlowPropertiesARM where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyDataGraphOpticalFlowPropertiesARM where
  zero = QueueFamilyDataGraphOpticalFlowPropertiesARM
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDataGraphOpticalFlowImageFormatInfoARM - Structure describing data
-- graph optical flow image format info
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Image.ImageCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowImageUsageFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
data DataGraphOpticalFlowImageFormatInfoARM = DataGraphOpticalFlowImageFormatInfoARM
  { -- | #opticalflow-usageARM# @usage@ is a bitmask of
    -- 'DataGraphOpticalFlowImageUsageFlagBitsARM' describing the intended
    -- optical flow usage of the image.
    --
    -- #VUID-VkDataGraphOpticalFlowImageFormatInfoARM-usage-parameter# @usage@
    -- /must/ be a valid combination of
    -- 'DataGraphOpticalFlowImageUsageFlagBitsARM' values
    --
    -- #VUID-VkDataGraphOpticalFlowImageFormatInfoARM-usage-requiredbitmask#
    -- @usage@ /must/ not be @0@
    usage :: DataGraphOpticalFlowImageUsageFlagsARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphOpticalFlowImageFormatInfoARM)
#endif
deriving instance Show DataGraphOpticalFlowImageFormatInfoARM

instance ToCStruct DataGraphOpticalFlowImageFormatInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphOpticalFlowImageFormatInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowImageUsageFlagsARM)) (usage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowImageUsageFlagsARM)) (zero)
    f

instance FromCStruct DataGraphOpticalFlowImageFormatInfoARM where
  peekCStruct p = do
    usage <- peek @DataGraphOpticalFlowImageUsageFlagsARM ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowImageUsageFlagsARM))
    pure $ DataGraphOpticalFlowImageFormatInfoARM
             usage

instance Storable DataGraphOpticalFlowImageFormatInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphOpticalFlowImageFormatInfoARM where
  zero = DataGraphOpticalFlowImageFormatInfoARM
           zero


-- | VkDataGraphOpticalFlowImageFormatPropertiesARM - Structure describing
-- properties of an optical flow image format
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
data DataGraphOpticalFlowImageFormatPropertiesARM = DataGraphOpticalFlowImageFormatPropertiesARM
  { -- | #opticalflow-formatARM# @format@ is a
    -- 'Vulkan.Core10.Enums.Format.Format' that specifies the format that /can/
    -- be used with the specified optical flow image usages.
    format :: Format }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphOpticalFlowImageFormatPropertiesARM)
#endif
deriving instance Show DataGraphOpticalFlowImageFormatPropertiesARM

instance ToCStruct DataGraphOpticalFlowImageFormatPropertiesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphOpticalFlowImageFormatPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    f

instance FromCStruct DataGraphOpticalFlowImageFormatPropertiesARM where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    pure $ DataGraphOpticalFlowImageFormatPropertiesARM
             format

instance Storable DataGraphOpticalFlowImageFormatPropertiesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphOpticalFlowImageFormatPropertiesARM where
  zero = DataGraphOpticalFlowImageFormatPropertiesARM
           zero


-- | VkDataGraphPipelineSingleNodeCreateInfoARM - Structure specifying
-- parameters of a newly-created single fixed-function node graph pipeline
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-nodeType-09963# If
--     @nodeType@ is 'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM', then
--     a 'DataGraphPipelineOpticalFlowCreateInfoARM' structure /must/ be
--     included in the @pNext@ chain of this structure
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-nodeType-09978# If
--     @nodeType@ is 'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM', then
--     one and only 'DataGraphPipelineSingleNodeConnectionARM' structure
--     /must/ be present in the @pConnections@ array for each of the
--     following values of its @connection@ member:
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM'
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-nodeType-09979# If
--     @nodeType@ is 'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM' and
--     'DataGraphPipelineOpticalFlowCreateInfoARM'::@hintGridSize@ is not
--     0, then one and only 'DataGraphPipelineSingleNodeConnectionARM'
--     structure whose @connection@ member is
--     'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM'
--     /must/ be present in the @pConnections@ array
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-nodeType-parameter#
--     @nodeType@ /must/ be a valid 'DataGraphPipelineNodeTypeARM' value
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-pConnections-parameter#
--     @pConnections@ /must/ be a valid pointer to an array of
--     @connectionCount@ valid 'DataGraphPipelineSingleNodeConnectionARM'
--     structures
--
-- -   #VUID-VkDataGraphPipelineSingleNodeCreateInfoARM-connectionCount-arraylength#
--     @connectionCount@ /must/ be greater than @0@
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphPipelineNodeTypeARM',
-- 'DataGraphPipelineSingleNodeConnectionARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineSingleNodeCreateInfoARM = DataGraphPipelineSingleNodeCreateInfoARM
  { -- | @nodeType@ is a 'DataGraphPipelineNodeTypeARM' describing the type of
    -- this node.
    nodeType :: DataGraphPipelineNodeTypeARM
  , -- | @pConnections@ is a pointer to an array of @connectionCount@
    -- 'DataGraphPipelineSingleNodeConnectionARM' structures.
    connections :: Vector DataGraphPipelineSingleNodeConnectionARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSingleNodeCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineSingleNodeCreateInfoARM

instance ToCStruct DataGraphPipelineSingleNodeCreateInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSingleNodeCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineNodeTypeARM)) (nodeType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (connections)) :: Word32))
    pPConnections' <- ContT $ allocaBytes @DataGraphPipelineSingleNodeConnectionARM ((Data.Vector.length (connections)) * 32)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPConnections' `plusPtr` (32 * (i)) :: Ptr DataGraphPipelineSingleNodeConnectionARM) (e)) (connections)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DataGraphPipelineSingleNodeConnectionARM))) (pPConnections')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineNodeTypeARM)) (zero)
    f

instance FromCStruct DataGraphPipelineSingleNodeCreateInfoARM where
  peekCStruct p = do
    nodeType <- peek @DataGraphPipelineNodeTypeARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineNodeTypeARM))
    connectionCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pConnections <- peek @(Ptr DataGraphPipelineSingleNodeConnectionARM) ((p `plusPtr` 24 :: Ptr (Ptr DataGraphPipelineSingleNodeConnectionARM)))
    pConnections' <- generateM (fromIntegral connectionCount) (\i -> peekCStruct @DataGraphPipelineSingleNodeConnectionARM ((pConnections `advancePtrBytes` (32 * (i)) :: Ptr DataGraphPipelineSingleNodeConnectionARM)))
    pure $ DataGraphPipelineSingleNodeCreateInfoARM
             nodeType pConnections'

instance Zero DataGraphPipelineSingleNodeCreateInfoARM where
  zero = DataGraphPipelineSingleNodeCreateInfoARM
           zero
           mempty


-- | VkDataGraphPipelineOpticalFlowCreateInfoARM - Structure specifying the
-- parameters of a newly-created optical flow graph pipeline
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-width-09966#
--     @width@ /must/ be greater than or equal to
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@minWidth@ and less
--     than or equal to
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@maxWidth@
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-height-09967#
--     @height@ /must/ be greater than or equal to
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@minHeight@ and less
--     than or equal to
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@maxHeight@
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-imageFormat-09968#
--     @imageFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
--     for 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM'
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-flowVectorFormat-09969#
--     @flowVectorFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
--     for 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM'
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-costFormat-09970#
--     @costFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
--     for 'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM' if
--     'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM' is set in
--     @flags@
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-outputGridSize-09971#
--     @outputGridSize@ /must/ be exactly one of the bits reported in
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@supportedOutputGridSizes@
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-hintGridSize-09972#
--     @hintGridSize@ /must/ be 0 or exactly one of the bits reported in
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@supportedHintGridSizes@
--     if 'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM' is set in
--     @flags@
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-hintGridSize-09973#
--     @hintGridSize@ /must/ be the same as @outputGridSize@ if
--     @hintGridSize@ is not 0
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-flags-09974#
--     'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM' /must/ not be
--     set in @flags@ if
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@hintSupported@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-flags-09975#
--     'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM' /must/ not be
--     set in @flags@ if
--     'QueueFamilyDataGraphOpticalFlowPropertiesARM'::@costSupported@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-imageFormat-parameter#
--     @imageFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-flowVectorFormat-parameter#
--     @flowVectorFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-costFormat-parameter#
--     If @costFormat@ is not @0@, @costFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-performanceLevel-parameter#
--     If @performanceLevel@ is not @0@, @performanceLevel@ /must/ be a
--     valid 'DataGraphOpticalFlowPerformanceLevelARM' value
--
-- -   #VUID-VkDataGraphPipelineOpticalFlowCreateInfoARM-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'DataGraphOpticalFlowCreateFlagBitsARM' values
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowCreateFlagsARM',
-- 'DataGraphOpticalFlowGridSizeFlagsARM',
-- 'DataGraphOpticalFlowPerformanceLevelARM',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineOpticalFlowCreateInfoARM = DataGraphPipelineOpticalFlowCreateInfoARM
  { -- | @width@ is the width in pixels of the input or reference image to be
    -- bound to this optical flow pipeline.
    width :: Word32
  , -- | @height@ is the height in pixels of the input or reference image to be
    -- bound to this optical flow pipeline.
    height :: Word32
  , -- | @imageFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the input
    -- and reference image to be bound to this optical flow pipeline.
    imageFormat :: Format
  , -- | @flowVectorFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the
    -- flow vector maps (output or hint) to be bound to this optical flow
    -- pipeline.
    flowVectorFormat :: Format
  , -- | @costFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the cost maps
    -- to be bound to this optical flow pipeline.
    costFormat :: Format
  , -- | @outputGridSize@ is exactly one bit of
    -- 'DataGraphOpticalFlowGridSizeFlagsARM' specifying the grid size of the
    -- output flow and cost maps to be bound to this optical flow pipeline. The
    -- size of the output flow and cost maps is a function of the input image
    -- dimensions and @outputGridSize@ and is calculated as follows:
    -- OutputWidth = ⌈ @width@ \/ OutputGridWidth ⌉
    -- OutputHeight = ⌈ @height@ \/ OutputGridHeight ⌉
    -- where OutputGridWidth and OutputGridHeight are determined by
    -- @outputGridSize@.
    outputGridSize :: DataGraphOpticalFlowGridSizeFlagsARM
  , -- | @hintGridSize@ is one exactly bit of
    -- 'DataGraphOpticalFlowGridSizeFlagsARM' specifying the grid size of the
    -- hint flow vector map to be bound to this optical flow pipeline. The size
    -- of the hint maps is a function of the input image dimensions and
    -- @hintGridSize@ and is calculated as follows:
    -- HintWidth = ⌈ @width@ \/ HintGridWidth ⌉
    -- HintHeight = ⌈ @height@ \/ HintGridHeight ⌉
    -- where HintGridWidth and HintGridHeight are determined by @hintGridSize@.
    hintGridSize :: DataGraphOpticalFlowGridSizeFlagsARM
  , -- | @performanceLevel@ is the 'DataGraphOpticalFlowPerformanceLevelARM' used
    -- for this optical flow pipeline.
    performanceLevel :: DataGraphOpticalFlowPerformanceLevelARM
  , -- | @flags@ are the 'DataGraphOpticalFlowCreateFlagsARM' used for this
    -- optical flow pipeline.
    flags :: DataGraphOpticalFlowCreateFlagsARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineOpticalFlowCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineOpticalFlowCreateInfoARM

instance ToCStruct DataGraphPipelineOpticalFlowCreateInfoARM where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineOpticalFlowCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 24 :: Ptr Format)) (imageFormat)
    poke ((p `plusPtr` 28 :: Ptr Format)) (flowVectorFormat)
    poke ((p `plusPtr` 32 :: Ptr Format)) (costFormat)
    poke ((p `plusPtr` 36 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (outputGridSize)
    poke ((p `plusPtr` 40 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (hintGridSize)
    poke ((p `plusPtr` 44 :: Ptr DataGraphOpticalFlowPerformanceLevelARM)) (performanceLevel)
    poke ((p `plusPtr` 48 :: Ptr DataGraphOpticalFlowCreateFlagsARM)) (flags)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM)) (zero)
    f

instance FromCStruct DataGraphPipelineOpticalFlowCreateInfoARM where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    imageFormat <- peek @Format ((p `plusPtr` 24 :: Ptr Format))
    flowVectorFormat <- peek @Format ((p `plusPtr` 28 :: Ptr Format))
    costFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    outputGridSize <- peek @DataGraphOpticalFlowGridSizeFlagsARM ((p `plusPtr` 36 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM))
    hintGridSize <- peek @DataGraphOpticalFlowGridSizeFlagsARM ((p `plusPtr` 40 :: Ptr DataGraphOpticalFlowGridSizeFlagsARM))
    performanceLevel <- peek @DataGraphOpticalFlowPerformanceLevelARM ((p `plusPtr` 44 :: Ptr DataGraphOpticalFlowPerformanceLevelARM))
    flags <- peek @DataGraphOpticalFlowCreateFlagsARM ((p `plusPtr` 48 :: Ptr DataGraphOpticalFlowCreateFlagsARM))
    pure $ DataGraphPipelineOpticalFlowCreateInfoARM
             width
             height
             imageFormat
             flowVectorFormat
             costFormat
             outputGridSize
             hintGridSize
             performanceLevel
             flags

instance Storable DataGraphPipelineOpticalFlowCreateInfoARM where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineOpticalFlowCreateInfoARM where
  zero = DataGraphPipelineOpticalFlowCreateInfoARM
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDataGraphPipelineOpticalFlowDispatchInfoARM - Structure specifying
-- parameters of a optical flow vector calculation
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineDispatchInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowExecuteFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineOpticalFlowDispatchInfoARM = DataGraphPipelineOpticalFlowDispatchInfoARM
  { -- | @flags@ are the 'DataGraphOpticalFlowExecuteFlagsARM' used for this
    -- command.
    --
    -- #VUID-VkDataGraphPipelineOpticalFlowDispatchInfoARM-flags-parameter#
    -- @flags@ /must/ be a valid combination of
    -- 'DataGraphOpticalFlowExecuteFlagBitsARM' values
    flags :: DataGraphOpticalFlowExecuteFlagsARM
  , -- | @meanFlowL1NormHint@ is an integer used to hint to the implementation
    -- that the mean L1 norm of flow vectors is expected to be centered around
    -- this value (in number of pixels of the input image). The implementation
    -- may use this value to influence how flow vectors are computed. Different
    -- values may result in different flow vectors and will affect the cost of
    -- computing the flow vectors. A value of 0 means that the application does
    -- not wish to provide a hint.
    --
    -- #VUID-VkDataGraphPipelineOpticalFlowDispatchInfoARM-meanFlowL1NormHint-09976#
    -- @meanFlowL1NormHint@, when different from 0, /must/ be less than or
    -- equal to the maximum of the width or height of the input image provided
    -- at pipeline creation time via
    -- 'DataGraphPipelineOpticalFlowCreateInfoARM'::@width@ or
    -- 'DataGraphPipelineOpticalFlowCreateInfoARM'::@height@, respectively
    meanFlowL1NormHint :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineOpticalFlowDispatchInfoARM)
#endif
deriving instance Show DataGraphPipelineOpticalFlowDispatchInfoARM

instance ToCStruct DataGraphPipelineOpticalFlowDispatchInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineOpticalFlowDispatchInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_DISPATCH_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowExecuteFlagsARM)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (meanFlowL1NormHint)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_DISPATCH_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DataGraphPipelineOpticalFlowDispatchInfoARM where
  peekCStruct p = do
    flags <- peek @DataGraphOpticalFlowExecuteFlagsARM ((p `plusPtr` 16 :: Ptr DataGraphOpticalFlowExecuteFlagsARM))
    meanFlowL1NormHint <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ DataGraphPipelineOpticalFlowDispatchInfoARM
             flags meanFlowL1NormHint

instance Storable DataGraphPipelineOpticalFlowDispatchInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineOpticalFlowDispatchInfoARM where
  zero = DataGraphPipelineOpticalFlowDispatchInfoARM
           zero
           zero


type DataGraphOpticalFlowGridSizeFlagsARM = DataGraphOpticalFlowGridSizeFlagBitsARM

-- | VkDataGraphOpticalFlowGridSizeFlagBitsARM - Bits specifying grid sizes
-- for optical flow operations
--
-- = Description
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_1X1_BIT_ARM' specifies that grid
--     is 1x1 pixel.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_2X2_BIT_ARM' specifies that grid
--     is 2x2 pixel.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_ARM' specifies that grid
--     is 4x4 pixel.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_8X8_BIT_ARM' specifies that grid
--     is 8x8 pixel.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowGridSizeFlagsARM'
newtype DataGraphOpticalFlowGridSizeFlagBitsARM = DataGraphOpticalFlowGridSizeFlagBitsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphOpticalFlowGridSizeFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_UNKNOWN_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_UNKNOWN_ARM = DataGraphOpticalFlowGridSizeFlagBitsARM 0x00000000

-- No documentation found for Nested "VkDataGraphOpticalFlowGridSizeFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_1X1_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_1X1_BIT_ARM = DataGraphOpticalFlowGridSizeFlagBitsARM 0x00000001

-- No documentation found for Nested "VkDataGraphOpticalFlowGridSizeFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_2X2_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_2X2_BIT_ARM = DataGraphOpticalFlowGridSizeFlagBitsARM 0x00000002

-- No documentation found for Nested "VkDataGraphOpticalFlowGridSizeFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_ARM = DataGraphOpticalFlowGridSizeFlagBitsARM 0x00000004

-- No documentation found for Nested "VkDataGraphOpticalFlowGridSizeFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_8X8_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_8X8_BIT_ARM = DataGraphOpticalFlowGridSizeFlagBitsARM 0x00000008

conNameDataGraphOpticalFlowGridSizeFlagBitsARM :: String
conNameDataGraphOpticalFlowGridSizeFlagBitsARM = "DataGraphOpticalFlowGridSizeFlagBitsARM"

enumPrefixDataGraphOpticalFlowGridSizeFlagBitsARM :: String
enumPrefixDataGraphOpticalFlowGridSizeFlagBitsARM = "DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_"

showTableDataGraphOpticalFlowGridSizeFlagBitsARM :: [(DataGraphOpticalFlowGridSizeFlagBitsARM, String)]
showTableDataGraphOpticalFlowGridSizeFlagBitsARM =
  [
    ( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_UNKNOWN_ARM
    , "UNKNOWN_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_1X1_BIT_ARM
    , "1X1_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_2X2_BIT_ARM
    , "2X2_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_ARM
    , "4X4_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_GRID_SIZE_8X8_BIT_ARM
    , "8X8_BIT_ARM"
    )
  ]

instance Show DataGraphOpticalFlowGridSizeFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphOpticalFlowGridSizeFlagBitsARM
      showTableDataGraphOpticalFlowGridSizeFlagBitsARM
      conNameDataGraphOpticalFlowGridSizeFlagBitsARM
      (\(DataGraphOpticalFlowGridSizeFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphOpticalFlowGridSizeFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphOpticalFlowGridSizeFlagBitsARM
      showTableDataGraphOpticalFlowGridSizeFlagBitsARM
      conNameDataGraphOpticalFlowGridSizeFlagBitsARM
      DataGraphOpticalFlowGridSizeFlagBitsARM

type DataGraphOpticalFlowImageUsageFlagsARM = DataGraphOpticalFlowImageUsageFlagBitsARM

-- | VkDataGraphOpticalFlowImageUsageFlagBitsARM - Bits specifying image
-- usage for optical flow operations
--
-- = Description
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM' specifies that
--     the image /can/ be used as input or reference image for an optical
--     flow operation.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM' specifies that
--     the image /can/ be used as output flow vector map for an optical
--     flow operation.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM' specifies that
--     the image /can/ be used as hint flow vector map for an optical flow
--     operation.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM' specifies that
--     the image /can/ be used as output cost map for an optical flow
--     operation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowImageUsageFlagsARM'
newtype DataGraphOpticalFlowImageUsageFlagBitsARM = DataGraphOpticalFlowImageUsageFlagBitsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphOpticalFlowImageUsageFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_UNKNOWN_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_UNKNOWN_ARM = DataGraphOpticalFlowImageUsageFlagBitsARM 0x00000000

-- No documentation found for Nested "VkDataGraphOpticalFlowImageUsageFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM = DataGraphOpticalFlowImageUsageFlagBitsARM 0x00000001

-- No documentation found for Nested "VkDataGraphOpticalFlowImageUsageFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM = DataGraphOpticalFlowImageUsageFlagBitsARM 0x00000002

-- No documentation found for Nested "VkDataGraphOpticalFlowImageUsageFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM = DataGraphOpticalFlowImageUsageFlagBitsARM 0x00000004

-- No documentation found for Nested "VkDataGraphOpticalFlowImageUsageFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM = DataGraphOpticalFlowImageUsageFlagBitsARM 0x00000008

conNameDataGraphOpticalFlowImageUsageFlagBitsARM :: String
conNameDataGraphOpticalFlowImageUsageFlagBitsARM = "DataGraphOpticalFlowImageUsageFlagBitsARM"

enumPrefixDataGraphOpticalFlowImageUsageFlagBitsARM :: String
enumPrefixDataGraphOpticalFlowImageUsageFlagBitsARM = "DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_"

showTableDataGraphOpticalFlowImageUsageFlagBitsARM :: [(DataGraphOpticalFlowImageUsageFlagBitsARM, String)]
showTableDataGraphOpticalFlowImageUsageFlagBitsARM =
  [
    ( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_UNKNOWN_ARM
    , "UNKNOWN_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_INPUT_BIT_ARM
    , "INPUT_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_OUTPUT_BIT_ARM
    , "OUTPUT_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_HINT_BIT_ARM
    , "HINT_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_IMAGE_USAGE_COST_BIT_ARM
    , "COST_BIT_ARM"
    )
  ]

instance Show DataGraphOpticalFlowImageUsageFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphOpticalFlowImageUsageFlagBitsARM
      showTableDataGraphOpticalFlowImageUsageFlagBitsARM
      conNameDataGraphOpticalFlowImageUsageFlagBitsARM
      (\(DataGraphOpticalFlowImageUsageFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphOpticalFlowImageUsageFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphOpticalFlowImageUsageFlagBitsARM
      showTableDataGraphOpticalFlowImageUsageFlagBitsARM
      conNameDataGraphOpticalFlowImageUsageFlagBitsARM
      DataGraphOpticalFlowImageUsageFlagBitsARM

-- | VkDataGraphOpticalFlowPerformanceLevelARM - Optical flow performance
-- level types
--
-- = Description
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM' is a level with
--     slower performance but higher quality.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM' is a level
--     with medium performance and medium quality.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM' is a preset
--     with higher performance but lower quality.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphPipelineOpticalFlowCreateInfoARM'
newtype DataGraphOpticalFlowPerformanceLevelARM = DataGraphOpticalFlowPerformanceLevelARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphOpticalFlowPerformanceLevelARM" "VK_DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_ARM = DataGraphOpticalFlowPerformanceLevelARM 0

-- No documentation found for Nested "VkDataGraphOpticalFlowPerformanceLevelARM" "VK_DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM = DataGraphOpticalFlowPerformanceLevelARM 1

-- No documentation found for Nested "VkDataGraphOpticalFlowPerformanceLevelARM" "VK_DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM = DataGraphOpticalFlowPerformanceLevelARM 2

-- No documentation found for Nested "VkDataGraphOpticalFlowPerformanceLevelARM" "VK_DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM = DataGraphOpticalFlowPerformanceLevelARM 3

{-# COMPLETE
  DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_ARM
  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM
  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM
  , DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM ::
    DataGraphOpticalFlowPerformanceLevelARM
  #-}

conNameDataGraphOpticalFlowPerformanceLevelARM :: String
conNameDataGraphOpticalFlowPerformanceLevelARM = "DataGraphOpticalFlowPerformanceLevelARM"

enumPrefixDataGraphOpticalFlowPerformanceLevelARM :: String
enumPrefixDataGraphOpticalFlowPerformanceLevelARM = "DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_"

showTableDataGraphOpticalFlowPerformanceLevelARM :: [(DataGraphOpticalFlowPerformanceLevelARM, String)]
showTableDataGraphOpticalFlowPerformanceLevelARM =
  [
    ( DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_ARM
    , "UNKNOWN_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_ARM
    , "SLOW_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_ARM
    , "MEDIUM_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_ARM
    , "FAST_ARM"
    )
  ]

instance Show DataGraphOpticalFlowPerformanceLevelARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphOpticalFlowPerformanceLevelARM
      showTableDataGraphOpticalFlowPerformanceLevelARM
      conNameDataGraphOpticalFlowPerformanceLevelARM
      (\(DataGraphOpticalFlowPerformanceLevelARM x) -> x)
      (showsPrec 11)

instance Read DataGraphOpticalFlowPerformanceLevelARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphOpticalFlowPerformanceLevelARM
      showTableDataGraphOpticalFlowPerformanceLevelARM
      conNameDataGraphOpticalFlowPerformanceLevelARM
      DataGraphOpticalFlowPerformanceLevelARM

-- | VkDataGraphPipelineNodeConnectionTypeARM - Connection points for a
-- fixed-function data graph node
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM'
--     specifies the connection point for the input image of an optical
--     flow node.
--
-- -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM'
--     specifies the connection point for the input reference image of an
--     optical flow node.
--
-- -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM'
--     specifies the connection point for the optional external hint flow
--     vector map of an optical flow node.
--
-- -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM'
--     specifies the connection point for the output flow vector map of an
--     optical flow node.
--
-- -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM'
--     specifies the connection point for the optional output cost map of
--     an optical flow node.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphPipelineSingleNodeConnectionARM'
newtype DataGraphPipelineNodeConnectionTypeARM = DataGraphPipelineNodeConnectionTypeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkDataGraphPipelineNodeConnectionTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM = DataGraphPipelineNodeConnectionTypeARM 1000631004

-- No documentation found for Nested "VkDataGraphPipelineNodeConnectionTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM = DataGraphPipelineNodeConnectionTypeARM 1000631003

-- No documentation found for Nested "VkDataGraphPipelineNodeConnectionTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM = DataGraphPipelineNodeConnectionTypeARM 1000631002

-- No documentation found for Nested "VkDataGraphPipelineNodeConnectionTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM = DataGraphPipelineNodeConnectionTypeARM 1000631001

-- No documentation found for Nested "VkDataGraphPipelineNodeConnectionTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM = DataGraphPipelineNodeConnectionTypeARM 1000631000

{-# COMPLETE
  DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM
  , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM
  , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM
  , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM
  , DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM ::
    DataGraphPipelineNodeConnectionTypeARM
  #-}

conNameDataGraphPipelineNodeConnectionTypeARM :: String
conNameDataGraphPipelineNodeConnectionTypeARM = "DataGraphPipelineNodeConnectionTypeARM"

enumPrefixDataGraphPipelineNodeConnectionTypeARM :: String
enumPrefixDataGraphPipelineNodeConnectionTypeARM = "DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_"

showTableDataGraphPipelineNodeConnectionTypeARM :: [(DataGraphPipelineNodeConnectionTypeARM, String)]
showTableDataGraphPipelineNodeConnectionTypeARM =
  [
    ( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM
    , "COST_ARM"
    )
  ,
    ( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM
    , "FLOW_VECTOR_ARM"
    )
  ,
    ( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM
    , "HINT_ARM"
    )
  ,
    ( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM
    , "REFERENCE_ARM"
    )
  ,
    ( DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM
    , "INPUT_ARM"
    )
  ]

instance Show DataGraphPipelineNodeConnectionTypeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineNodeConnectionTypeARM
      showTableDataGraphPipelineNodeConnectionTypeARM
      conNameDataGraphPipelineNodeConnectionTypeARM
      (\(DataGraphPipelineNodeConnectionTypeARM x) -> x)
      (showsPrec 11)

instance Read DataGraphPipelineNodeConnectionTypeARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineNodeConnectionTypeARM
      showTableDataGraphPipelineNodeConnectionTypeARM
      conNameDataGraphPipelineNodeConnectionTypeARM
      DataGraphPipelineNodeConnectionTypeARM

-- | VkDataGraphPipelineNodeTypeARM - Enumeration describing the type of a
-- data graph pipeline node
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM' corresponds to an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#graphs-operations-opticalflow optical flow node>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphPipelineSingleNodeCreateInfoARM'
newtype DataGraphPipelineNodeTypeARM = DataGraphPipelineNodeTypeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkDataGraphPipelineNodeTypeARM" "VK_DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM"
pattern DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM = DataGraphPipelineNodeTypeARM 1000631000

{-# COMPLETE DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM :: DataGraphPipelineNodeTypeARM #-}

conNameDataGraphPipelineNodeTypeARM :: String
conNameDataGraphPipelineNodeTypeARM = "DataGraphPipelineNodeTypeARM"

enumPrefixDataGraphPipelineNodeTypeARM :: String
enumPrefixDataGraphPipelineNodeTypeARM = "DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM"

showTableDataGraphPipelineNodeTypeARM :: [(DataGraphPipelineNodeTypeARM, String)]
showTableDataGraphPipelineNodeTypeARM =
  [
    ( DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM
    , ""
    )
  ]

instance Show DataGraphPipelineNodeTypeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineNodeTypeARM
      showTableDataGraphPipelineNodeTypeARM
      conNameDataGraphPipelineNodeTypeARM
      (\(DataGraphPipelineNodeTypeARM x) -> x)
      (showsPrec 11)

instance Read DataGraphPipelineNodeTypeARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineNodeTypeARM
      showTableDataGraphPipelineNodeTypeARM
      conNameDataGraphPipelineNodeTypeARM
      DataGraphPipelineNodeTypeARM

type DataGraphOpticalFlowCreateFlagsARM = DataGraphOpticalFlowCreateFlagBitsARM

-- | VkDataGraphOpticalFlowCreateFlagBitsARM - Bits specifying flags for
-- newly created optical flow data graph node
--
-- = Description
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM' specifies that
--     a 'Vulkan.Core10.Handles.ImageView' with external flow vector map
--     will be used as hints in performing the motion search and /must/ be
--     connected to
--     'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM'.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM' specifies that
--     the cost for the forward flow is generated in a
--     'Vulkan.Core10.Handles.ImageView' which /must/ be connected to
--     'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM'.
--     The cost is the confidence level of the flow vector for each grid in
--     the image. The cost implies how (in)accurate the flow vector is.
--     Higher cost value implies the flow vector to be less accurate and
--     vice-versa.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowCreateFlagsARM'
newtype DataGraphOpticalFlowCreateFlagBitsARM = DataGraphOpticalFlowCreateFlagBitsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphOpticalFlowCreateFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM = DataGraphOpticalFlowCreateFlagBitsARM 0x00000001

-- No documentation found for Nested "VkDataGraphOpticalFlowCreateFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM = DataGraphOpticalFlowCreateFlagBitsARM 0x00000002

-- No documentation found for Nested "VkDataGraphOpticalFlowCreateFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_CREATE_RESERVED_30_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_CREATE_RESERVED_30_BIT_ARM = DataGraphOpticalFlowCreateFlagBitsARM 0x40000000

conNameDataGraphOpticalFlowCreateFlagBitsARM :: String
conNameDataGraphOpticalFlowCreateFlagBitsARM = "DataGraphOpticalFlowCreateFlagBitsARM"

enumPrefixDataGraphOpticalFlowCreateFlagBitsARM :: String
enumPrefixDataGraphOpticalFlowCreateFlagBitsARM = "DATA_GRAPH_OPTICAL_FLOW_CREATE_"

showTableDataGraphOpticalFlowCreateFlagBitsARM :: [(DataGraphOpticalFlowCreateFlagBitsARM, String)]
showTableDataGraphOpticalFlowCreateFlagBitsARM =
  [
    ( DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_HINT_BIT_ARM
    , "ENABLE_HINT_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_CREATE_ENABLE_COST_BIT_ARM
    , "ENABLE_COST_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_CREATE_RESERVED_30_BIT_ARM
    , "RESERVED_30_BIT_ARM"
    )
  ]

instance Show DataGraphOpticalFlowCreateFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphOpticalFlowCreateFlagBitsARM
      showTableDataGraphOpticalFlowCreateFlagBitsARM
      conNameDataGraphOpticalFlowCreateFlagBitsARM
      (\(DataGraphOpticalFlowCreateFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphOpticalFlowCreateFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphOpticalFlowCreateFlagBitsARM
      showTableDataGraphOpticalFlowCreateFlagBitsARM
      conNameDataGraphOpticalFlowCreateFlagBitsARM
      DataGraphOpticalFlowCreateFlagBitsARM

type DataGraphOpticalFlowExecuteFlagsARM = DataGraphOpticalFlowExecuteFlagBitsARM

-- | VkDataGraphOpticalFlowExecuteFlagBitsARM - Bits specifying flags for a
-- optical flow vector calculation
--
-- = Description
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_ARM'
--     specifies that temporal hints from previously generated flow vector
--     map are not used. If temporal hints are enabled, the optical flow
--     vector map from previous
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' calls
--     in the same graph pipeline session /may/ be automatically used as
--     hints for the current
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' call,
--     to take advantage of temporal correlation in a video sequence.
--     Temporal hints should be disabled if there is a-priori knowledge of
--     no temporal correlation (e.g. a scene change, independent successive
--     image pairs).
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_UNCHANGED_BIT_ARM' specifies
--     that the contents of the input image are the same as in the
--     previously executed
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' call
--     in the same graph pipeline session.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_UNCHANGED_BIT_ARM'
--     specifies that the contents of the reference image are the same as
--     in the previously executed
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' call
--     in the same graph pipeline session.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM'
--     specifies that the contents of the input image are the same as the
--     contents of the reference image in the previously executed
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' call
--     in the same graph pipeline session.
--
-- -   'DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM'
--     specifies that the contents of the reference image are the same as
--     the contents of the input image in the previously executed
--     'Vulkan.Extensions.VK_ARM_data_graph.cmdDispatchDataGraphARM' call
--     in the same graph pipeline session.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_optical_flow VK_ARM_data_graph_optical_flow>,
-- 'DataGraphOpticalFlowExecuteFlagsARM'
newtype DataGraphOpticalFlowExecuteFlagBitsARM = DataGraphOpticalFlowExecuteFlagBitsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphOpticalFlowExecuteFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_ARM = DataGraphOpticalFlowExecuteFlagBitsARM 0x00000001

-- No documentation found for Nested "VkDataGraphOpticalFlowExecuteFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_UNCHANGED_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_UNCHANGED_BIT_ARM = DataGraphOpticalFlowExecuteFlagBitsARM 0x00000002

-- No documentation found for Nested "VkDataGraphOpticalFlowExecuteFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_UNCHANGED_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_UNCHANGED_BIT_ARM = DataGraphOpticalFlowExecuteFlagBitsARM 0x00000004

-- No documentation found for Nested "VkDataGraphOpticalFlowExecuteFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM = DataGraphOpticalFlowExecuteFlagBitsARM 0x00000008

-- No documentation found for Nested "VkDataGraphOpticalFlowExecuteFlagBitsARM" "VK_DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM"
pattern DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM = DataGraphOpticalFlowExecuteFlagBitsARM 0x00000010

conNameDataGraphOpticalFlowExecuteFlagBitsARM :: String
conNameDataGraphOpticalFlowExecuteFlagBitsARM = "DataGraphOpticalFlowExecuteFlagBitsARM"

enumPrefixDataGraphOpticalFlowExecuteFlagBitsARM :: String
enumPrefixDataGraphOpticalFlowExecuteFlagBitsARM = "DATA_GRAPH_OPTICAL_FLOW_EXECUTE_"

showTableDataGraphOpticalFlowExecuteFlagBitsARM :: [(DataGraphOpticalFlowExecuteFlagBitsARM, String)]
showTableDataGraphOpticalFlowExecuteFlagBitsARM =
  [
    ( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_ARM
    , "DISABLE_TEMPORAL_HINTS_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_UNCHANGED_BIT_ARM
    , "INPUT_UNCHANGED_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_UNCHANGED_BIT_ARM
    , "REFERENCE_UNCHANGED_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM
    , "INPUT_IS_PREVIOUS_REFERENCE_BIT_ARM"
    )
  ,
    ( DATA_GRAPH_OPTICAL_FLOW_EXECUTE_REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM
    , "REFERENCE_IS_PREVIOUS_INPUT_BIT_ARM"
    )
  ]

instance Show DataGraphOpticalFlowExecuteFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphOpticalFlowExecuteFlagBitsARM
      showTableDataGraphOpticalFlowExecuteFlagBitsARM
      conNameDataGraphOpticalFlowExecuteFlagBitsARM
      (\(DataGraphOpticalFlowExecuteFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphOpticalFlowExecuteFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphOpticalFlowExecuteFlagBitsARM
      showTableDataGraphOpticalFlowExecuteFlagBitsARM
      conNameDataGraphOpticalFlowExecuteFlagBitsARM
      DataGraphOpticalFlowExecuteFlagBitsARM

type ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION"
pattern ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION = 1


type ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME = "VK_ARM_data_graph_optical_flow"

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME"
pattern ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME = "VK_ARM_data_graph_optical_flow"

