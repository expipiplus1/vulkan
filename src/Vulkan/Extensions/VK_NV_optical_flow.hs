{-# language CPP #-}
-- | = Name
--
-- VK_NV_optical_flow - device extension
--
-- == VK_NV_optical_flow
--
-- [__Name String__]
--     @VK_NV_optical_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     465
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
--     -   Requires @VK_KHR_format_feature_flags2@ to be enabled for any
--         device-level functionality
--
--     -   Requires @VK_KHR_synchronization2@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_optical_flow] @crohde%0A*Here describe the issue or question you have about the VK_NV_optical_flow extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-09-26
--
-- [__Contributors__]
--
--     -   Carsten Rohde, NVIDIA
--
--     -   Vipul Parashar, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Optical flow are fundamental algorithms in computer vision (CV) area.
-- This extension allows applications to estimate 2D displacement of pixels
-- between two frames.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
--
-- == New Commands
--
-- -   'bindOpticalFlowSessionImageNV'
--
-- -   'cmdOpticalFlowExecuteNV'
--
-- -   'createOpticalFlowSessionNV'
--
-- -   'destroyOpticalFlowSessionNV'
--
-- -   'getPhysicalDeviceOpticalFlowImageFormatsNV'
--
-- == New Structures
--
-- -   'OpticalFlowExecuteInfoNV'
--
-- -   'OpticalFlowImageFormatPropertiesNV'
--
-- -   'OpticalFlowSessionCreateInfoNV'
--
-- -   Extending 'OpticalFlowSessionCreateInfoNV':
--
--     -   'OpticalFlowSessionCreatePrivateDataInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceOpticalFlowFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
--     'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'OpticalFlowImageFormatInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceOpticalFlowPropertiesNV'
--
-- == New Enums
--
-- -   'OpticalFlowExecuteFlagBitsNV'
--
-- -   'OpticalFlowGridSizeFlagBitsNV'
--
-- -   'OpticalFlowPerformanceLevelNV'
--
-- -   'OpticalFlowSessionBindingPointNV'
--
-- -   'OpticalFlowSessionCreateFlagBitsNV'
--
-- -   'OpticalFlowUsageFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'OpticalFlowExecuteFlagsNV'
--
-- -   'OpticalFlowGridSizeFlagsNV'
--
-- -   'OpticalFlowSessionCreateFlagsNV'
--
-- -   'OpticalFlowUsageFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_OPTICAL_FLOW_EXTENSION_NAME'
--
-- -   'NV_OPTICAL_FLOW_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_S10_5_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_OPTICAL_FLOW_SESSION_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_OPTICAL_FLOW_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV'
--
-- == Examples
--
-- > // Example querying available input formats
-- > VkOpticalFlowImageFormatInfoNV ofFormatInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV };
-- > ofFormatInfo.usage = VK_OPTICAL_FLOW_USAGE_INPUT_BIT_NV;
-- >
-- > uint32_t count = 0;
-- > vkGetPhysicalDeviceOpticalFlowImageFormatsNV(physicalDevice, &ofFormatInfo, &count, NULL);
-- > VkOpticalFlowImageFormatPropertiesNV* fmt = new VkOpticalFlowImageFormatPropertiesNV[count];
-- > memset(fmt, 0, count  * sizeof(VkOpticalFlowImageFormatPropertiesNV));
-- > for (uint32_t i = 0; i < count; i++) {
-- >     fmt[i].sType = VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV;
-- > }
-- > vkGetPhysicalDeviceOpticalFlowImageFormatsNV(physicalDevice, &ofFormatInfo, &count, fmt);
-- >
-- > // Pick one of the available formats
-- > VkFormat inputFormat = fmt[0].format;
-- >
-- > // Check feature support for optimal tiling
-- > VkFormatProperties3 formatProperties3 = { VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3 };
-- > VkFormatProperties2 formatProperties2 = { VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2, &formatProperties3 };
-- > vkGetPhysicalDeviceFormatProperties2(physicalDevice, inputFormat, &formatProperties2);
-- > if (!(formatProperties3.optimalTilingFeatures & VK_FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV)) {
-- >     return false;
-- > }
-- >
-- > // Check support for image creation parameters
-- > VkPhysicalDeviceImageFormatInfo2 imageFormatInfo2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2, &ofFormatInfo };
-- > imageFormatInfo2.format = inputFormat;
-- > imageFormatInfo2.type = VK_IMAGE_TYPE_2D;
-- > imageFormatInfo2.tiling = VK_IMAGE_TILING_OPTIMAL;
-- > imageFormatInfo2.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
-- >
-- > VkImageFormatProperties2 imageFormatProperties2 = { VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 };
-- > if (vkGetPhysicalDeviceImageFormatProperties2(physicalDevice, &imageFormatInfo2, &imageFormatProperties2) != VK_SUCCESS) {
-- >     return false;
-- > }
-- >
-- > VkImageCreateInfo imageCreateInfo = { VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO, &ofFormatInfo };
-- > imageCreateInfo.imageType = VK_IMAGE_TYPE_2D;
-- > imageCreateInfo.format = inputFormat;
-- > imageCreateInfo.extent = { width, height, (uint32_t)1};
-- > imageCreateInfo.mipLevels = 1;
-- > imageCreateInfo.arrayLayers = 1;
-- > imageCreateInfo.samples = VK_SAMPLE_COUNT_1_BIT;
-- > imageCreateInfo.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;;
-- > imageCreateInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
-- >
-- > vkCreateImage(device, &imageCreateInfo, NULL, &input);
-- > "allocate memory, bind image, create view"
-- >
-- > "do the same for reference and output"
-- >
-- > // Create optical flow session
-- > VkOpticalFlowSessionCreateInfoNV sessionCreateInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV };
-- > sessionCreateInfo.width = width;
-- > sessionCreateInfo.height = height;
-- > sessionCreateInfo.imageFormat = inputFormat;
-- > sessionCreateInfo.flowVectorFormat = outputFormat;
-- > sessionCreateInfo.outputGridSize = VK_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV;
-- > sessionCreateInfo.performanceLevel = VK_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV;
-- > VkOpticalFlowSessionNV session;
-- > vkCreateOpticalFlowSessionNV(device, &sessionCreateInfo, NULL, &session);
-- >
-- > "allocate command buffer"
-- >
-- > "transfer images to VK_PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV"
-- > "transfer input images to VK_ACCESS_2_OPTICAL_FLOW_READ_BIT_NV and output image to VK_ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV"
-- >
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV, inputView, VK_IMAGE_LAYOUT_GENERAL);
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV, refView, VK_IMAGE_LAYOUT_GENERAL);
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV, outputView, VK_IMAGE_LAYOUT_GENERAL);
-- >
-- > VkOpticalFlowExecuteInfoNV opticalFlowExecuteInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV };
-- > vkCmdOpticalFlowExecuteNV(cmd, session, &opticalFlowExecuteInfo);
-- >
-- > "submit command buffer"
--
-- == Version History
--
-- -   Revision 1, 2022-09-26 (Carsten Rohde)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'OpticalFlowExecuteFlagBitsNV', 'OpticalFlowExecuteFlagsNV',
-- 'OpticalFlowExecuteInfoNV', 'OpticalFlowGridSizeFlagBitsNV',
-- 'OpticalFlowGridSizeFlagsNV', 'OpticalFlowImageFormatInfoNV',
-- 'OpticalFlowImageFormatPropertiesNV', 'OpticalFlowPerformanceLevelNV',
-- 'OpticalFlowSessionBindingPointNV',
-- 'OpticalFlowSessionCreateFlagBitsNV', 'OpticalFlowSessionCreateFlagsNV',
-- 'OpticalFlowSessionCreateInfoNV',
-- 'OpticalFlowSessionCreatePrivateDataInfoNV',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV',
-- 'OpticalFlowUsageFlagBitsNV', 'OpticalFlowUsageFlagsNV',
-- 'PhysicalDeviceOpticalFlowFeaturesNV',
-- 'PhysicalDeviceOpticalFlowPropertiesNV',
-- 'bindOpticalFlowSessionImageNV', 'cmdOpticalFlowExecuteNV',
-- 'createOpticalFlowSessionNV', 'destroyOpticalFlowSessionNV',
-- 'getPhysicalDeviceOpticalFlowImageFormatsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_optical_flow Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_optical_flow  ( getPhysicalDeviceOpticalFlowImageFormatsNV
                                             , createOpticalFlowSessionNV
                                             , withOpticalFlowSessionNV
                                             , destroyOpticalFlowSessionNV
                                             , bindOpticalFlowSessionImageNV
                                             , cmdOpticalFlowExecuteNV
                                             , PhysicalDeviceOpticalFlowFeaturesNV(..)
                                             , PhysicalDeviceOpticalFlowPropertiesNV(..)
                                             , OpticalFlowImageFormatInfoNV(..)
                                             , OpticalFlowImageFormatPropertiesNV(..)
                                             , OpticalFlowSessionCreateInfoNV(..)
                                             , OpticalFlowSessionCreatePrivateDataInfoNV(..)
                                             , OpticalFlowExecuteInfoNV(..)
                                             , OpticalFlowGridSizeFlagsNV
                                             , OpticalFlowGridSizeFlagBitsNV( OPTICAL_FLOW_GRID_SIZE_UNKNOWN_NV
                                                                            , OPTICAL_FLOW_GRID_SIZE_1X1_BIT_NV
                                                                            , OPTICAL_FLOW_GRID_SIZE_2X2_BIT_NV
                                                                            , OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV
                                                                            , OPTICAL_FLOW_GRID_SIZE_8X8_BIT_NV
                                                                            , ..
                                                                            )
                                             , OpticalFlowUsageFlagsNV
                                             , OpticalFlowUsageFlagBitsNV( OPTICAL_FLOW_USAGE_UNKNOWN_NV
                                                                         , OPTICAL_FLOW_USAGE_INPUT_BIT_NV
                                                                         , OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV
                                                                         , OPTICAL_FLOW_USAGE_HINT_BIT_NV
                                                                         , OPTICAL_FLOW_USAGE_COST_BIT_NV
                                                                         , OPTICAL_FLOW_USAGE_GLOBAL_FLOW_BIT_NV
                                                                         , ..
                                                                         )
                                             , OpticalFlowPerformanceLevelNV( OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_NV
                                                                            , OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV
                                                                            , OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_NV
                                                                            , OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_NV
                                                                            , ..
                                                                            )
                                             , OpticalFlowSessionBindingPointNV( OPTICAL_FLOW_SESSION_BINDING_POINT_UNKNOWN_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_FLOW_VECTOR_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV
                                                                               , OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV
                                                                               , ..
                                                                               )
                                             , OpticalFlowSessionCreateFlagsNV
                                             , OpticalFlowSessionCreateFlagBitsNV( OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV
                                                                                 , OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV
                                                                                 , OPTICAL_FLOW_SESSION_CREATE_ENABLE_GLOBAL_FLOW_BIT_NV
                                                                                 , OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV
                                                                                 , OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV
                                                                                 , ..
                                                                                 )
                                             , OpticalFlowExecuteFlagsNV
                                             , OpticalFlowExecuteFlagBitsNV( OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_NV
                                                                           , ..
                                                                           )
                                             , NV_OPTICAL_FLOW_SPEC_VERSION
                                             , pattern NV_OPTICAL_FLOW_SPEC_VERSION
                                             , NV_OPTICAL_FLOW_EXTENSION_NAME
                                             , pattern NV_OPTICAL_FLOW_EXTENSION_NAME
                                             , OpticalFlowSessionNV(..)
                                             ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkBindOpticalFlowSessionImageNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdOpticalFlowExecuteNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateOpticalFlowSessionNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyOpticalFlowSessionNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceOpticalFlowImageFormatsNV))
import Vulkan.Extensions.Handles (OpticalFlowSessionNV)
import Vulkan.Extensions.Handles (OpticalFlowSessionNV(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (OpticalFlowSessionNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceOpticalFlowImageFormatsNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr OpticalFlowImageFormatInfoNV -> Ptr Word32 -> Ptr OpticalFlowImageFormatPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr OpticalFlowImageFormatInfoNV -> Ptr Word32 -> Ptr OpticalFlowImageFormatPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceOpticalFlowImageFormatsNV - Query image formats for
-- optical flow
--
-- = Description
--
-- If @pImageFormatProperties@ is @NULL@, then the number of optical flow
-- properties supported for the given @physicalDevice@ is returned in
-- @pFormatCount@. Otherwise, @pFormatCount@ must point to a variable set
-- by the user to the number of elements in the @pImageFormatProperties@
-- array, and on return the variable is overwritten with the number of
-- values actually written to @pImageFormatProperties@. If the value of
-- @pFormatCount@ is less than the number of optical flow properties
-- supported, at most @pFormatCount@ values will be written to
-- @pImageFormatProperties@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available values were returned. Before
-- creating an image to be used as a optical flow frame, obtain the
-- supported image creation parameters by querying with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- using one of the reported formats and adding
-- 'OpticalFlowImageFormatInfoNV' to the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'.
-- When querying the parameters with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- for images used for optical flow operations, the
-- 'OpticalFlowImageFormatInfoNV'::@usage@ field should contain one or more
-- of the bits defined in 'OpticalFlowUsageFlagBitsNV'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceOpticalFlowImageFormatsNV-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceOpticalFlowImageFormatsNV-pOpticalFlowImageFormatInfo-parameter#
--     @pOpticalFlowImageFormatInfo@ /must/ be a valid pointer to a valid
--     'OpticalFlowImageFormatInfoNV' structure
--
-- -   #VUID-vkGetPhysicalDeviceOpticalFlowImageFormatsNV-pFormatCount-parameter#
--     @pFormatCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceOpticalFlowImageFormatsNV-pImageFormatProperties-parameter#
--     If the value referenced by @pFormatCount@ is not @0@, and
--     @pImageFormatProperties@ is not @NULL@, @pImageFormatProperties@
--     /must/ be a valid pointer to an array of @pFormatCount@
--     'OpticalFlowImageFormatPropertiesNV' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- Note
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_B8G8R8A8_UNORM',
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UNORM' and
-- 'Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_420_UNORM' are
-- initially supported for images with
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-usage optical usage>
-- 'OPTICAL_FLOW_USAGE_INPUT_BIT_NV'.
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_R16G16_S10_5_NV' is initially
-- supported for images with
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-usage optical usage>
-- 'OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV', 'OPTICAL_FLOW_USAGE_HINT_BIT_NV' and
-- 'OPTICAL_FLOW_USAGE_GLOBAL_FLOW_BIT_NV'.
--
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT' and
-- 'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT' are initially supported for
-- images with
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-usage optical usage>
-- 'OPTICAL_FLOW_USAGE_COST_BIT_NV'. It is recommended to use
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT' because of the lower
-- bandwidth.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowImageFormatInfoNV', 'OpticalFlowImageFormatPropertiesNV',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceOpticalFlowImageFormatsNV :: forall io
                                            . (MonadIO io)
                                           => -- | @physicalDevice@ is the physical device being queried.
                                              PhysicalDevice
                                           -> -- | #opticalflow-getimageformat-pOpticalFlowImageFormatInfo#
                                              -- @pOpticalFlowImageFormatInfo@ is a pointer to a
                                              -- 'OpticalFlowImageFormatInfoNV' structure specifying the optical flow
                                              -- usage for which information is returned.
                                              OpticalFlowImageFormatInfoNV
                                           -> io (Result, ("imageFormatProperties" ::: Vector OpticalFlowImageFormatPropertiesNV))
getPhysicalDeviceOpticalFlowImageFormatsNV physicalDevice
                                             opticalFlowImageFormatInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceOpticalFlowImageFormatsNVPtr = pVkGetPhysicalDeviceOpticalFlowImageFormatsNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceOpticalFlowImageFormatsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceOpticalFlowImageFormatsNV is null" Nothing Nothing
  let vkGetPhysicalDeviceOpticalFlowImageFormatsNV' = mkVkGetPhysicalDeviceOpticalFlowImageFormatsNV vkGetPhysicalDeviceOpticalFlowImageFormatsNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pOpticalFlowImageFormatInfo <- ContT $ withCStruct (opticalFlowImageFormatInfo)
  pPFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceOpticalFlowImageFormatsNV" (vkGetPhysicalDeviceOpticalFlowImageFormatsNV'
                                                                                 physicalDevice'
                                                                                 pOpticalFlowImageFormatInfo
                                                                                 (pPFormatCount)
                                                                                 (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFormatCount <- lift $ peek @Word32 pPFormatCount
  pPImageFormatProperties <- ContT $ bracket (callocBytes @OpticalFlowImageFormatPropertiesNV ((fromIntegral (pFormatCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPImageFormatProperties `advancePtrBytes` (i * 24) :: Ptr OpticalFlowImageFormatPropertiesNV) . ($ ())) [0..(fromIntegral (pFormatCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceOpticalFlowImageFormatsNV" (vkGetPhysicalDeviceOpticalFlowImageFormatsNV'
                                                                                  physicalDevice'
                                                                                  pOpticalFlowImageFormatInfo
                                                                                  (pPFormatCount)
                                                                                  ((pPImageFormatProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFormatCount' <- lift $ peek @Word32 pPFormatCount
  pImageFormatProperties' <- lift $ generateM (fromIntegral (pFormatCount')) (\i -> peekCStruct @OpticalFlowImageFormatPropertiesNV (((pPImageFormatProperties) `advancePtrBytes` (24 * (i)) :: Ptr OpticalFlowImageFormatPropertiesNV)))
  pure $ ((r'), pImageFormatProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateOpticalFlowSessionNV
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct OpticalFlowSessionCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr OpticalFlowSessionNV -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct OpticalFlowSessionCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr OpticalFlowSessionNV -> IO Result

-- | vkCreateOpticalFlowSessionNV - Creates an optical flow session object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateOpticalFlowSessionNV-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateOpticalFlowSessionNV-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'OpticalFlowSessionCreateInfoNV' structure
--
-- -   #VUID-vkCreateOpticalFlowSessionNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateOpticalFlowSessionNV-pSession-parameter# @pSession@
--     /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.OpticalFlowSessionNV' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'OpticalFlowSessionCreateInfoNV',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
createOpticalFlowSessionNV :: forall a io
                            . ( Extendss OpticalFlowSessionCreateInfoNV a
                              , PokeChain a
                              , MonadIO io )
                           => -- | @device@ is the logical device that creates the optical flow session
                              -- object.
                              Device
                           -> -- | @pCreateInfo@ is a pointer to a 'OpticalFlowSessionCreateInfoNV'
                              -- structure containing parameters specifying the creation of the optical
                              -- flow session.
                              (OpticalFlowSessionCreateInfoNV a)
                           -> -- | @pAllocator@ controls host memory allocation as described in the
                              -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                              -- chapter.
                              ("allocator" ::: Maybe AllocationCallbacks)
                           -> io (OpticalFlowSessionNV)
createOpticalFlowSessionNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateOpticalFlowSessionNVPtr = pVkCreateOpticalFlowSessionNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateOpticalFlowSessionNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateOpticalFlowSessionNV is null" Nothing Nothing
  let vkCreateOpticalFlowSessionNV' = mkVkCreateOpticalFlowSessionNV vkCreateOpticalFlowSessionNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSession <- ContT $ bracket (callocBytes @OpticalFlowSessionNV 8) free
  r <- lift $ traceAroundEvent "vkCreateOpticalFlowSessionNV" (vkCreateOpticalFlowSessionNV'
                                                                 (deviceHandle (device))
                                                                 (forgetExtensions pCreateInfo)
                                                                 pAllocator
                                                                 (pPSession))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSession <- lift $ peek @OpticalFlowSessionNV pPSession
  pure $ (pSession)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createOpticalFlowSessionNV' and 'destroyOpticalFlowSessionNV'
--
-- To ensure that 'destroyOpticalFlowSessionNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withOpticalFlowSessionNV :: forall a io r . (Extendss OpticalFlowSessionCreateInfoNV a, PokeChain a, MonadIO io) => Device -> OpticalFlowSessionCreateInfoNV a -> Maybe AllocationCallbacks -> (io OpticalFlowSessionNV -> (OpticalFlowSessionNV -> io ()) -> r) -> r
withOpticalFlowSessionNV device pCreateInfo pAllocator b =
  b (createOpticalFlowSessionNV device pCreateInfo pAllocator)
    (\(o0) -> destroyOpticalFlowSessionNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyOpticalFlowSessionNV
  :: FunPtr (Ptr Device_T -> OpticalFlowSessionNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> OpticalFlowSessionNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyOpticalFlowSessionNV - Destroy optical flow session object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyOpticalFlowSessionNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyOpticalFlowSessionNV-session-parameter# @session@
--     /must/ be a valid 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
--     handle
--
-- -   #VUID-vkDestroyOpticalFlowSessionNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyOpticalFlowSessionNV-session-parent# @session@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
destroyOpticalFlowSessionNV :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the device that was used for the creation of the optical
                               -- flow session.
                               Device
                            -> -- | @session@ is the optical flow session to be destroyed.
                               OpticalFlowSessionNV
                            -> -- | @pAllocator@ controls host memory allocation as described in the
                               -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                               -- chapter.
                               ("allocator" ::: Maybe AllocationCallbacks)
                            -> io ()
destroyOpticalFlowSessionNV device session allocator = liftIO . evalContT $ do
  let vkDestroyOpticalFlowSessionNVPtr = pVkDestroyOpticalFlowSessionNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyOpticalFlowSessionNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyOpticalFlowSessionNV is null" Nothing Nothing
  let vkDestroyOpticalFlowSessionNV' = mkVkDestroyOpticalFlowSessionNV vkDestroyOpticalFlowSessionNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyOpticalFlowSessionNV" (vkDestroyOpticalFlowSessionNV'
                                                             (deviceHandle (device))
                                                             (session)
                                                             pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindOpticalFlowSessionImageNV
  :: FunPtr (Ptr Device_T -> OpticalFlowSessionNV -> OpticalFlowSessionBindingPointNV -> ImageView -> ImageLayout -> IO Result) -> Ptr Device_T -> OpticalFlowSessionNV -> OpticalFlowSessionBindingPointNV -> ImageView -> ImageLayout -> IO Result

-- | vkBindOpticalFlowSessionImageNV - Bind image to an optical flow session
--
-- = Parameters
--
-- -   @device@ is the device which owns the optical flow session object
--     @session@.
--
-- -   @session@ is the optical flow session object to which the image view
--     is to be bound.
--
-- -   @bindingPoint@ specifies the binding point
--     'OpticalFlowSessionBindingPointNV' to which the image view is bound.
--
-- -   @view@ is a 'Vulkan.Core10.Handles.ImageView' to be bound.
--
-- -   layout /must/ specify the layout that the image subresources
--     accessible from @view@ will be in at the time the optical flow
--     vectors are calculated with 'cmdOpticalFlowExecuteNV' on a
--     'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-session-parameter# @session@
--     /must/ be a valid 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
--     handle
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-bindingPoint-parameter#
--     @bindingPoint@ /must/ be a valid 'OpticalFlowSessionBindingPointNV'
--     value
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-view-parameter# If @view@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @view@ /must/ be a
--     valid 'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-session-parent# @session@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- -   #VUID-vkBindOpticalFlowSessionImageNV-view-parent# If @view@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView', 'OpticalFlowSessionBindingPointNV',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
bindOpticalFlowSessionImageNV :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkBindOpticalFlowSessionImageNV" "device"
                                 Device
                              -> -- No documentation found for Nested "vkBindOpticalFlowSessionImageNV" "session"
                                 OpticalFlowSessionNV
                              -> -- No documentation found for Nested "vkBindOpticalFlowSessionImageNV" "bindingPoint"
                                 OpticalFlowSessionBindingPointNV
                              -> -- No documentation found for Nested "vkBindOpticalFlowSessionImageNV" "view"
                                 ImageView
                              -> -- No documentation found for Nested "vkBindOpticalFlowSessionImageNV" "layout"
                                 ImageLayout
                              -> io ()
bindOpticalFlowSessionImageNV device
                                session
                                bindingPoint
                                view
                                layout = liftIO $ do
  let vkBindOpticalFlowSessionImageNVPtr = pVkBindOpticalFlowSessionImageNV (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkBindOpticalFlowSessionImageNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindOpticalFlowSessionImageNV is null" Nothing Nothing
  let vkBindOpticalFlowSessionImageNV' = mkVkBindOpticalFlowSessionImageNV vkBindOpticalFlowSessionImageNVPtr
  r <- traceAroundEvent "vkBindOpticalFlowSessionImageNV" (vkBindOpticalFlowSessionImageNV'
                                                             (deviceHandle (device))
                                                             (session)
                                                             (bindingPoint)
                                                             (view)
                                                             (layout))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdOpticalFlowExecuteNV
  :: FunPtr (Ptr CommandBuffer_T -> OpticalFlowSessionNV -> Ptr OpticalFlowExecuteInfoNV -> IO ()) -> Ptr CommandBuffer_T -> OpticalFlowSessionNV -> Ptr OpticalFlowExecuteInfoNV -> IO ()

-- | vkCmdOpticalFlowExecuteNV - Calculate optical flow vectors
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-session-parameter# @session@ /must/
--     be a valid 'Vulkan.Extensions.Handles.OpticalFlowSessionNV' handle
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-pExecuteInfo-parameter#
--     @pExecuteInfo@ /must/ be a valid pointer to a valid
--     'OpticalFlowExecuteInfoNV' structure
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support opticalflow operations
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdOpticalFlowExecuteNV-commonparent# Both of
--     @commandBuffer@, and @session@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Opticalflow                                                                                                           | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'OpticalFlowExecuteInfoNV',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
cmdOpticalFlowExecuteNV :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @session@ is the optical flow session object on which this command is
                           -- operating.
                           OpticalFlowSessionNV
                        -> -- | @pExecuteInfo@ Info is a pointer to a 'OpticalFlowExecuteInfoNV'.
                           OpticalFlowExecuteInfoNV
                        -> io ()
cmdOpticalFlowExecuteNV commandBuffer
                          session
                          executeInfo = liftIO . evalContT $ do
  let vkCmdOpticalFlowExecuteNVPtr = pVkCmdOpticalFlowExecuteNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdOpticalFlowExecuteNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdOpticalFlowExecuteNV is null" Nothing Nothing
  let vkCmdOpticalFlowExecuteNV' = mkVkCmdOpticalFlowExecuteNV vkCmdOpticalFlowExecuteNVPtr
  pExecuteInfo <- ContT $ withCStruct (executeInfo)
  lift $ traceAroundEvent "vkCmdOpticalFlowExecuteNV" (vkCmdOpticalFlowExecuteNV'
                                                         (commandBufferHandle (commandBuffer))
                                                         (session)
                                                         pExecuteInfo)
  pure $ ()


-- | VkPhysicalDeviceOpticalFlowFeaturesNV - Structure describing the optical
-- flow features supported by the implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceOpticalFlowFeaturesNV' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceOpticalFlowFeaturesNV' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpticalFlowFeaturesNV = PhysicalDeviceOpticalFlowFeaturesNV
  { -- | #features-opticalFlow# @opticalFlow@ indicates whether the
    -- implementation supports optical flow.
    opticalFlow :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpticalFlowFeaturesNV)
#endif
deriving instance Show PhysicalDeviceOpticalFlowFeaturesNV

instance ToCStruct PhysicalDeviceOpticalFlowFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpticalFlowFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (opticalFlow))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceOpticalFlowFeaturesNV where
  peekCStruct p = do
    opticalFlow <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceOpticalFlowFeaturesNV
             (bool32ToBool opticalFlow)

instance Storable PhysicalDeviceOpticalFlowFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpticalFlowFeaturesNV where
  zero = PhysicalDeviceOpticalFlowFeaturesNV
           zero


-- | VkPhysicalDeviceOpticalFlowPropertiesNV - Structure describing
-- properties supported by VK_NV_optical_flow
--
-- = Description
--
-- If the 'PhysicalDeviceOpticalFlowPropertiesNV' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'OpticalFlowGridSizeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpticalFlowPropertiesNV = PhysicalDeviceOpticalFlowPropertiesNV
  { -- | #limits-supportedOutputGridSizes# @supportedOutputGridSizes@ are the
    -- supported 'OpticalFlowGridSizeFlagsNV' which can be specified in
    -- 'OpticalFlowSessionCreateInfoNV'::@outputGridSize@.
    supportedOutputGridSizes :: OpticalFlowGridSizeFlagsNV
  , -- | #limits-supportedHintGridSizes# @supportedHintGridSizes@ are the
    -- supported 'OpticalFlowGridSizeFlagsNV' which can be specified in
    -- 'OpticalFlowSessionCreateInfoNV'::@hintGridSize@.
    supportedHintGridSizes :: OpticalFlowGridSizeFlagsNV
  , -- | #limits-hintSupported# @hintSupported@ is a boolean describing whether
    -- using hint flow vector map is supported in an optical flow session.
    hintSupported :: Bool
  , -- | #limits-costSupported# @costSupported@ is a boolean describing whether
    -- cost map generation is supported in an optical flow session.
    costSupported :: Bool
  , -- | #limits-bidirectionalFlowSupported# @bidirectionalFlowSupported@ is a
    -- boolean describing whether bi-directional flow generation is supported
    -- in an optical flow session.
    bidirectionalFlowSupported :: Bool
  , -- | #limits-globalFlowSupported# @globalFlowSupported@ is a boolean
    -- describing whether global flow vector map generation is supported in an
    -- optical flow session.
    globalFlowSupported :: Bool
  , -- | #limits-minWidth# @minWidth@ is the minimum width in pixels for images
    -- used in an optical flow session.
    minWidth :: Word32
  , -- | #limits-minHeight# @minHeight@ is the minimum height in pixels for
    -- images used in an optical flow session.
    minHeight :: Word32
  , -- | #limits-maxWidth# @maxWidth@ is the maximum width in pixels for images
    -- used in an optical flow session.
    maxWidth :: Word32
  , -- | #limits-maxHeight# @maxHeight@ is the maximum height in pixels for
    -- images used in an optical flow session.
    maxHeight :: Word32
  , -- | #limits-maxNumRegionsOfInterest# @maxNumRegionsOfInterest@ is the
    -- maximum number of regions of interest which can be used in an optical
    -- flow session. If this @maxNumRegionsOfInterest@ is 0, regions of
    -- interest are not supported in an optical flow session.
    maxNumRegionsOfInterest :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpticalFlowPropertiesNV)
#endif
deriving instance Show PhysicalDeviceOpticalFlowPropertiesNV

instance ToCStruct PhysicalDeviceOpticalFlowPropertiesNV where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpticalFlowPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpticalFlowGridSizeFlagsNV)) (supportedOutputGridSizes)
    poke ((p `plusPtr` 20 :: Ptr OpticalFlowGridSizeFlagsNV)) (supportedHintGridSizes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (hintSupported))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (costSupported))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (bidirectionalFlowSupported))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (globalFlowSupported))
    poke ((p `plusPtr` 40 :: Ptr Word32)) (minWidth)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (minHeight)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxWidth)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxHeight)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxNumRegionsOfInterest)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpticalFlowGridSizeFlagsNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr OpticalFlowGridSizeFlagsNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceOpticalFlowPropertiesNV where
  peekCStruct p = do
    supportedOutputGridSizes <- peek @OpticalFlowGridSizeFlagsNV ((p `plusPtr` 16 :: Ptr OpticalFlowGridSizeFlagsNV))
    supportedHintGridSizes <- peek @OpticalFlowGridSizeFlagsNV ((p `plusPtr` 20 :: Ptr OpticalFlowGridSizeFlagsNV))
    hintSupported <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    costSupported <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    bidirectionalFlowSupported <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    globalFlowSupported <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    minWidth <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    minHeight <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxWidth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxHeight <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    maxNumRegionsOfInterest <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceOpticalFlowPropertiesNV
             supportedOutputGridSizes
             supportedHintGridSizes
             (bool32ToBool hintSupported)
             (bool32ToBool costSupported)
             (bool32ToBool bidirectionalFlowSupported)
             (bool32ToBool globalFlowSupported)
             minWidth
             minHeight
             maxWidth
             maxHeight
             maxNumRegionsOfInterest

instance Storable PhysicalDeviceOpticalFlowPropertiesNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpticalFlowPropertiesNV where
  zero = PhysicalDeviceOpticalFlowPropertiesNV
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


-- | VkOpticalFlowImageFormatInfoNV - Structure describing optical flow image
-- format info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowUsageFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceOpticalFlowImageFormatsNV'
data OpticalFlowImageFormatInfoNV = OpticalFlowImageFormatInfoNV
  { -- | #opticalflow-usage# @usage@ is a bitmask of 'OpticalFlowUsageFlagBitsNV'
    -- describing the intended optical flow usage of the image.
    --
    -- #VUID-VkOpticalFlowImageFormatInfoNV-usage-parameter# @usage@ /must/ be
    -- a valid combination of 'OpticalFlowUsageFlagBitsNV' values
    --
    -- #VUID-VkOpticalFlowImageFormatInfoNV-usage-requiredbitmask# @usage@
    -- /must/ not be @0@
    usage :: OpticalFlowUsageFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpticalFlowImageFormatInfoNV)
#endif
deriving instance Show OpticalFlowImageFormatInfoNV

instance ToCStruct OpticalFlowImageFormatInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpticalFlowImageFormatInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpticalFlowUsageFlagsNV)) (usage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OpticalFlowUsageFlagsNV)) (zero)
    f

instance FromCStruct OpticalFlowImageFormatInfoNV where
  peekCStruct p = do
    usage <- peek @OpticalFlowUsageFlagsNV ((p `plusPtr` 16 :: Ptr OpticalFlowUsageFlagsNV))
    pure $ OpticalFlowImageFormatInfoNV
             usage

instance Storable OpticalFlowImageFormatInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero OpticalFlowImageFormatInfoNV where
  zero = OpticalFlowImageFormatInfoNV
           zero


-- | VkOpticalFlowImageFormatPropertiesNV - Structure describing properties
-- of an optical flow image format
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceOpticalFlowImageFormatsNV'
data OpticalFlowImageFormatPropertiesNV = OpticalFlowImageFormatPropertiesNV
  { -- | #opticalflow-format# @format@ is a 'Vulkan.Core10.Enums.Format.Format'
    -- that specifies the format that can be used with the specified optical
    -- flow image usages.
    format :: Format }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpticalFlowImageFormatPropertiesNV)
#endif
deriving instance Show OpticalFlowImageFormatPropertiesNV

instance ToCStruct OpticalFlowImageFormatPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpticalFlowImageFormatPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    f

instance FromCStruct OpticalFlowImageFormatPropertiesNV where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    pure $ OpticalFlowImageFormatPropertiesNV
             format

instance Storable OpticalFlowImageFormatPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero OpticalFlowImageFormatPropertiesNV where
  zero = OpticalFlowImageFormatPropertiesNV
           zero


-- | VkOpticalFlowSessionCreateInfoNV - Structure specifying parameters of a
-- newly created optical flow session
--
-- == Valid Usage
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-width-07581# @width@ /must/
--     be greater than or equal to
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@minWidth@ and less than or
--     equal to 'PhysicalDeviceOpticalFlowPropertiesNV'::@maxWidth@.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-height-07582# @height@ /must/
--     be greater than or equal to
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@minHeight@ and less than
--     or equal to 'PhysicalDeviceOpticalFlowPropertiesNV'::@maxHeight@.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-imageFormat-07583#
--     @imageFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceOpticalFlowImageFormatsNV' for
--     'OPTICAL_FLOW_USAGE_INPUT_BIT_NV'.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flowVectorFormat-07584#
--     @flowVectorFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceOpticalFlowImageFormatsNV' for
--     'OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV'.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-costFormat-07585#
--     @costFormat@ /must/ be one of the formats returned by
--     'getPhysicalDeviceOpticalFlowImageFormatsNV' for
--     'OPTICAL_FLOW_USAGE_COST_BIT_NV' if
--     'OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV' is set in @flags@.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-outputGridSize-07586#
--     @outputGridSize@ /must/ be exactly one of the bits reported in
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@supportedOutputGridSizes@.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-hintGridSize-07587#
--     @hintGridSize@ /must/ be exactly one of the bits reported in
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@supportedHintGridSizes@ if
--     'OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV' is set in @flags@.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-07588#
--     'OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV' /must/ not be set
--     in @flags@ if
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@hintSupported@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-07589#
--     'OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV' /must/ not be set
--     in @flags@ if
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@costSupported@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-07590#
--     'OPTICAL_FLOW_SESSION_CREATE_ENABLE_GLOBAL_FLOW_BIT_NV' /must/ not
--     be set in @flags@ if
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@globalFlowSupported@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-07591#
--     'OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV' /must/ not be set
--     in @flags@ if
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@maxNumRegionsOfInterest@
--     is 0.
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-07592#
--     'OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV' /must/ not be
--     set in @flags@ if
--     'PhysicalDeviceOpticalFlowPropertiesNV'::@bidirectionalFlowSupported@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV'
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-pNext-pNext# @pNext@ /must/
--     be @NULL@ or a pointer to a valid instance of
--     'OpticalFlowSessionCreatePrivateDataInfoNV'
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-imageFormat-parameter#
--     @imageFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flowVectorFormat-parameter#
--     @flowVectorFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-costFormat-parameter# If
--     @costFormat@ is not @0@, @costFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-outputGridSize-parameter#
--     @outputGridSize@ /must/ be a valid combination of
--     'OpticalFlowGridSizeFlagBitsNV' values
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-outputGridSize-requiredbitmask#
--     @outputGridSize@ /must/ not be @0@
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-hintGridSize-parameter#
--     @hintGridSize@ /must/ be a valid combination of
--     'OpticalFlowGridSizeFlagBitsNV' values
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-performanceLevel-parameter#
--     If @performanceLevel@ is not @0@, @performanceLevel@ /must/ be a
--     valid 'OpticalFlowPerformanceLevelNV' value
--
-- -   #VUID-VkOpticalFlowSessionCreateInfoNV-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'OpticalFlowSessionCreateFlagBitsNV' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.Enums.Format.Format', 'OpticalFlowGridSizeFlagsNV',
-- 'OpticalFlowPerformanceLevelNV', 'OpticalFlowSessionCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createOpticalFlowSessionNV'
data OpticalFlowSessionCreateInfoNV (es :: [Type]) = OpticalFlowSessionCreateInfoNV
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @width@ is the width in pixels of the input or reference frame to be
    -- bound to this optical flow session.
    width :: Word32
  , -- | @height@ is the height in pixels of the input or reference frame to be
    -- bound to this optical flow session.
    height :: Word32
  , -- | @imageFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the input
    -- and reference frame to be bound to this optical flow session.
    imageFormat :: Format
  , -- | @flowVectorFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the
    -- flow vector maps (output or hint) to be bound to this optical flow
    -- session.
    flowVectorFormat :: Format
  , -- | @costFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of the cost maps
    -- to be bound to this optical flow session.
    costFormat :: Format
  , -- | @outputGridSize@ is exactly one bit of 'OpticalFlowGridSizeFlagsNV'
    -- specifying the grid size of the output flow and cost maps to be bound to
    -- this optical flow session. The size of the output flow and cost maps is
    -- determined by 'OpticalFlowSessionCreateInfoNV'::@width@ and
    -- 'OpticalFlowSessionCreateInfoNV'::@height@ divided by
    -- 'OpticalFlowSessionCreateInfoNV'::@outputGridSize@.
    outputGridSize :: OpticalFlowGridSizeFlagsNV
  , -- | @hintGridSize@ is one exactly bit of 'OpticalFlowGridSizeFlagsNV'
    -- specifying the grid size of of the hint flow vector maps to be bound to
    -- this optical flow session. The size of the hint maps is determined by
    -- 'OpticalFlowSessionCreateInfoNV'::@width@ and
    -- 'OpticalFlowSessionCreateInfoNV'::@height@ divided by
    -- 'OpticalFlowSessionCreateInfoNV'::@hintGridSize@.
    hintGridSize :: OpticalFlowGridSizeFlagsNV
  , -- | @performanceLevel@ is the 'OpticalFlowPerformanceLevelNV' used for this
    -- optical flow session.
    performanceLevel :: OpticalFlowPerformanceLevelNV
  , -- | @flags@ are the 'OpticalFlowSessionCreateFlagsNV' used for this optical
    -- flow session.
    flags :: OpticalFlowSessionCreateFlagsNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpticalFlowSessionCreateInfoNV (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (OpticalFlowSessionCreateInfoNV es)

instance Extensible OpticalFlowSessionCreateInfoNV where
  extensibleTypeName = "OpticalFlowSessionCreateInfoNV"
  setNext OpticalFlowSessionCreateInfoNV{..} next' = OpticalFlowSessionCreateInfoNV{next = next', ..}
  getNext OpticalFlowSessionCreateInfoNV{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends OpticalFlowSessionCreateInfoNV e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @OpticalFlowSessionCreatePrivateDataInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss OpticalFlowSessionCreateInfoNV es
         , PokeChain es ) => ToCStruct (OpticalFlowSessionCreateInfoNV es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpticalFlowSessionCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (width)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (height)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (imageFormat)
    lift $ poke ((p `plusPtr` 28 :: Ptr Format)) (flowVectorFormat)
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (costFormat)
    lift $ poke ((p `plusPtr` 36 :: Ptr OpticalFlowGridSizeFlagsNV)) (outputGridSize)
    lift $ poke ((p `plusPtr` 40 :: Ptr OpticalFlowGridSizeFlagsNV)) (hintGridSize)
    lift $ poke ((p `plusPtr` 44 :: Ptr OpticalFlowPerformanceLevelNV)) (performanceLevel)
    lift $ poke ((p `plusPtr` 48 :: Ptr OpticalFlowSessionCreateFlagsNV)) (flags)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr OpticalFlowGridSizeFlagsNV)) (zero)
    lift $ f

instance ( Extendss OpticalFlowSessionCreateInfoNV es
         , PeekChain es ) => FromCStruct (OpticalFlowSessionCreateInfoNV es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    width <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    imageFormat <- peek @Format ((p `plusPtr` 24 :: Ptr Format))
    flowVectorFormat <- peek @Format ((p `plusPtr` 28 :: Ptr Format))
    costFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    outputGridSize <- peek @OpticalFlowGridSizeFlagsNV ((p `plusPtr` 36 :: Ptr OpticalFlowGridSizeFlagsNV))
    hintGridSize <- peek @OpticalFlowGridSizeFlagsNV ((p `plusPtr` 40 :: Ptr OpticalFlowGridSizeFlagsNV))
    performanceLevel <- peek @OpticalFlowPerformanceLevelNV ((p `plusPtr` 44 :: Ptr OpticalFlowPerformanceLevelNV))
    flags <- peek @OpticalFlowSessionCreateFlagsNV ((p `plusPtr` 48 :: Ptr OpticalFlowSessionCreateFlagsNV))
    pure $ OpticalFlowSessionCreateInfoNV
             next
             width
             height
             imageFormat
             flowVectorFormat
             costFormat
             outputGridSize
             hintGridSize
             performanceLevel
             flags

instance es ~ '[] => Zero (OpticalFlowSessionCreateInfoNV es) where
  zero = OpticalFlowSessionCreateInfoNV
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkOpticalFlowSessionCreatePrivateDataInfoNV - Structure for NV internal
-- use only
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data OpticalFlowSessionCreatePrivateDataInfoNV = OpticalFlowSessionCreatePrivateDataInfoNV
  { -- | @id@ is an identifier for data which is passed at a memory location
    -- specified in
    -- 'OpticalFlowSessionCreatePrivateDataInfoNV'::@pPrivateData@.
    id' :: Word32
  , -- | @size@ is is the size of data in bytes which is passed at a memory
    -- location specified in
    -- 'OpticalFlowSessionCreatePrivateDataInfoNV'::@pPrivateData@.
    size :: Word32
  , -- | @pPrivateData@ is a pointer to NV internal data.
    --
    -- #VUID-VkOpticalFlowSessionCreatePrivateDataInfoNV-pPrivateData-parameter#
    -- @pPrivateData@ /must/ be a pointer value
    privateData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpticalFlowSessionCreatePrivateDataInfoNV)
#endif
deriving instance Show OpticalFlowSessionCreatePrivateDataInfoNV

instance ToCStruct OpticalFlowSessionCreatePrivateDataInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpticalFlowSessionCreatePrivateDataInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (id')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (privateData)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct OpticalFlowSessionCreatePrivateDataInfoNV where
  peekCStruct p = do
    id' <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    size <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pPrivateData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ OpticalFlowSessionCreatePrivateDataInfoNV
             id' size pPrivateData

instance Storable OpticalFlowSessionCreatePrivateDataInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero OpticalFlowSessionCreatePrivateDataInfoNV where
  zero = OpticalFlowSessionCreatePrivateDataInfoNV
           zero
           zero
           zero


-- | VkOpticalFlowExecuteInfoNV - Structure specifying parameters of a
-- optical flow vector calculation
--
-- == Valid Usage
--
-- -   #VUID-VkOpticalFlowExecuteInfoNV-regionCount-07593# @regionCount@
--     /must/ be 0 if 'OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV'
--     was not set for 'Vulkan.Extensions.Handles.OpticalFlowSessionNV' on
--     which this command is operating.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkOpticalFlowExecuteInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV'
--
-- -   #VUID-VkOpticalFlowExecuteInfoNV-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkOpticalFlowExecuteInfoNV-flags-parameter# @flags@ /must/ be
--     a valid combination of 'OpticalFlowExecuteFlagBitsNV' values
--
-- -   #VUID-VkOpticalFlowExecuteInfoNV-pRegions-parameter# If
--     @regionCount@ is not @0@, @pRegions@ /must/ be a valid pointer to an
--     array of @regionCount@ 'Vulkan.Core10.FundamentalTypes.Rect2D'
--     structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowExecuteFlagsNV', 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdOpticalFlowExecuteNV'
data OpticalFlowExecuteInfoNV = OpticalFlowExecuteInfoNV
  { -- | @flags@ are the 'OpticalFlowExecuteFlagsNV' used for this command.
    flags :: OpticalFlowExecuteFlagsNV
  , -- | @pRegions@ is a pointer to @regionCount@
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' regions of interest.
    regions :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpticalFlowExecuteInfoNV)
#endif
deriving instance Show OpticalFlowExecuteInfoNV

instance ToCStruct OpticalFlowExecuteInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpticalFlowExecuteInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr OpticalFlowExecuteFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32))
    pPRegions' <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (regions)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (regions)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPRegions')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct OpticalFlowExecuteInfoNV where
  peekCStruct p = do
    flags <- peek @OpticalFlowExecuteFlagsNV ((p `plusPtr` 16 :: Ptr OpticalFlowExecuteFlagsNV))
    regionCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pRegions <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pRegions' <- generateM (fromIntegral regionCount) (\i -> peekCStruct @Rect2D ((pRegions `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ OpticalFlowExecuteInfoNV
             flags pRegions'

instance Zero OpticalFlowExecuteInfoNV where
  zero = OpticalFlowExecuteInfoNV
           zero
           mempty


type OpticalFlowGridSizeFlagsNV = OpticalFlowGridSizeFlagBitsNV

-- | VkOpticalFlowGridSizeFlagBitsNV - Bits specifying grid sizes for optical
-- flow operations
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowGridSizeFlagsNV'
newtype OpticalFlowGridSizeFlagBitsNV = OpticalFlowGridSizeFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkOpticalFlowGridSizeFlagBitsNV" "VK_OPTICAL_FLOW_GRID_SIZE_UNKNOWN_NV"
pattern OPTICAL_FLOW_GRID_SIZE_UNKNOWN_NV = OpticalFlowGridSizeFlagBitsNV 0x00000000

-- | 'OPTICAL_FLOW_GRID_SIZE_1X1_BIT_NV' specifies that grid is 1x1 pixel.
pattern OPTICAL_FLOW_GRID_SIZE_1X1_BIT_NV = OpticalFlowGridSizeFlagBitsNV 0x00000001

-- | 'OPTICAL_FLOW_GRID_SIZE_2X2_BIT_NV' specifies that grid is 2x2 pixel.
pattern OPTICAL_FLOW_GRID_SIZE_2X2_BIT_NV = OpticalFlowGridSizeFlagBitsNV 0x00000002

-- | 'OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV' specifies that grid is 4x4 pixel.
pattern OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV = OpticalFlowGridSizeFlagBitsNV 0x00000004

-- | 'OPTICAL_FLOW_GRID_SIZE_8X8_BIT_NV' specifies that grid is 8x8 pixel.
pattern OPTICAL_FLOW_GRID_SIZE_8X8_BIT_NV = OpticalFlowGridSizeFlagBitsNV 0x00000008

conNameOpticalFlowGridSizeFlagBitsNV :: String
conNameOpticalFlowGridSizeFlagBitsNV = "OpticalFlowGridSizeFlagBitsNV"

enumPrefixOpticalFlowGridSizeFlagBitsNV :: String
enumPrefixOpticalFlowGridSizeFlagBitsNV = "OPTICAL_FLOW_GRID_SIZE_"

showTableOpticalFlowGridSizeFlagBitsNV :: [(OpticalFlowGridSizeFlagBitsNV, String)]
showTableOpticalFlowGridSizeFlagBitsNV =
  [
    ( OPTICAL_FLOW_GRID_SIZE_UNKNOWN_NV
    , "UNKNOWN_NV"
    )
  ,
    ( OPTICAL_FLOW_GRID_SIZE_1X1_BIT_NV
    , "1X1_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_GRID_SIZE_2X2_BIT_NV
    , "2X2_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV
    , "4X4_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_GRID_SIZE_8X8_BIT_NV
    , "8X8_BIT_NV"
    )
  ]

instance Show OpticalFlowGridSizeFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowGridSizeFlagBitsNV
      showTableOpticalFlowGridSizeFlagBitsNV
      conNameOpticalFlowGridSizeFlagBitsNV
      (\(OpticalFlowGridSizeFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read OpticalFlowGridSizeFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowGridSizeFlagBitsNV
      showTableOpticalFlowGridSizeFlagBitsNV
      conNameOpticalFlowGridSizeFlagBitsNV
      OpticalFlowGridSizeFlagBitsNV

type OpticalFlowUsageFlagsNV = OpticalFlowUsageFlagBitsNV

-- | VkOpticalFlowUsageFlagBitsNV - Bits specifying usage for optical flow
-- operations
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowUsageFlagsNV'
newtype OpticalFlowUsageFlagBitsNV = OpticalFlowUsageFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkOpticalFlowUsageFlagBitsNV" "VK_OPTICAL_FLOW_USAGE_UNKNOWN_NV"
pattern OPTICAL_FLOW_USAGE_UNKNOWN_NV = OpticalFlowUsageFlagBitsNV 0x00000000

-- | 'OPTICAL_FLOW_USAGE_INPUT_BIT_NV' specifies that the image /can/ be used
-- as input or reference frame for an optical flow operation.
pattern OPTICAL_FLOW_USAGE_INPUT_BIT_NV = OpticalFlowUsageFlagBitsNV 0x00000001

-- | 'OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV' specifies that the image /can/ be
-- used as output flow vector map for an optical flow operation.
pattern OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV = OpticalFlowUsageFlagBitsNV 0x00000002

-- | 'OPTICAL_FLOW_USAGE_HINT_BIT_NV' specifies that the image /can/ be used
-- as hint flow vector map for an optical flow operation.
pattern OPTICAL_FLOW_USAGE_HINT_BIT_NV = OpticalFlowUsageFlagBitsNV 0x00000004

-- | 'OPTICAL_FLOW_USAGE_COST_BIT_NV' specifies that the image /can/ be used
-- as output cost map for an optical flow operation.
pattern OPTICAL_FLOW_USAGE_COST_BIT_NV = OpticalFlowUsageFlagBitsNV 0x00000008

-- | 'OPTICAL_FLOW_USAGE_GLOBAL_FLOW_BIT_NV' specifies that the image /can/
-- be used as global flow vector for an optical flow operation.
pattern OPTICAL_FLOW_USAGE_GLOBAL_FLOW_BIT_NV = OpticalFlowUsageFlagBitsNV 0x00000010

conNameOpticalFlowUsageFlagBitsNV :: String
conNameOpticalFlowUsageFlagBitsNV = "OpticalFlowUsageFlagBitsNV"

enumPrefixOpticalFlowUsageFlagBitsNV :: String
enumPrefixOpticalFlowUsageFlagBitsNV = "OPTICAL_FLOW_USAGE_"

showTableOpticalFlowUsageFlagBitsNV :: [(OpticalFlowUsageFlagBitsNV, String)]
showTableOpticalFlowUsageFlagBitsNV =
  [
    ( OPTICAL_FLOW_USAGE_UNKNOWN_NV
    , "UNKNOWN_NV"
    )
  ,
    ( OPTICAL_FLOW_USAGE_INPUT_BIT_NV
    , "INPUT_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_USAGE_OUTPUT_BIT_NV
    , "OUTPUT_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_USAGE_HINT_BIT_NV
    , "HINT_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_USAGE_COST_BIT_NV
    , "COST_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_USAGE_GLOBAL_FLOW_BIT_NV
    , "GLOBAL_FLOW_BIT_NV"
    )
  ]

instance Show OpticalFlowUsageFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowUsageFlagBitsNV
      showTableOpticalFlowUsageFlagBitsNV
      conNameOpticalFlowUsageFlagBitsNV
      (\(OpticalFlowUsageFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read OpticalFlowUsageFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowUsageFlagBitsNV
      showTableOpticalFlowUsageFlagBitsNV
      conNameOpticalFlowUsageFlagBitsNV
      OpticalFlowUsageFlagBitsNV

-- | VkOpticalFlowPerformanceLevelNV - Optical flow performance level types
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowSessionCreateInfoNV'
newtype OpticalFlowPerformanceLevelNV = OpticalFlowPerformanceLevelNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkOpticalFlowPerformanceLevelNV" "VK_OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_NV"
pattern OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_NV = OpticalFlowPerformanceLevelNV 0

-- | 'OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV' is a level with slower
-- performance but higher quality.
pattern OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV = OpticalFlowPerformanceLevelNV 1

-- | 'OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_NV' is a level with medium
-- performance and medium quality.
pattern OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_NV = OpticalFlowPerformanceLevelNV 2

-- | 'OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_NV' is a preset with higher
-- performance but lower quality.
pattern OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_NV = OpticalFlowPerformanceLevelNV 3

{-# COMPLETE
  OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_NV
  , OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV
  , OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_NV
  , OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_NV ::
    OpticalFlowPerformanceLevelNV
  #-}

conNameOpticalFlowPerformanceLevelNV :: String
conNameOpticalFlowPerformanceLevelNV = "OpticalFlowPerformanceLevelNV"

enumPrefixOpticalFlowPerformanceLevelNV :: String
enumPrefixOpticalFlowPerformanceLevelNV = "OPTICAL_FLOW_PERFORMANCE_LEVEL_"

showTableOpticalFlowPerformanceLevelNV :: [(OpticalFlowPerformanceLevelNV, String)]
showTableOpticalFlowPerformanceLevelNV =
  [
    ( OPTICAL_FLOW_PERFORMANCE_LEVEL_UNKNOWN_NV
    , "UNKNOWN_NV"
    )
  ,
    ( OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV
    , "SLOW_NV"
    )
  ,
    ( OPTICAL_FLOW_PERFORMANCE_LEVEL_MEDIUM_NV
    , "MEDIUM_NV"
    )
  ,
    ( OPTICAL_FLOW_PERFORMANCE_LEVEL_FAST_NV
    , "FAST_NV"
    )
  ]

instance Show OpticalFlowPerformanceLevelNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowPerformanceLevelNV
      showTableOpticalFlowPerformanceLevelNV
      conNameOpticalFlowPerformanceLevelNV
      (\(OpticalFlowPerformanceLevelNV x) -> x)
      (showsPrec 11)

instance Read OpticalFlowPerformanceLevelNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowPerformanceLevelNV
      showTableOpticalFlowPerformanceLevelNV
      conNameOpticalFlowPerformanceLevelNV
      OpticalFlowPerformanceLevelNV

-- | VkOpticalFlowSessionBindingPointNV - Binding points of an optical flow
-- session
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'bindOpticalFlowSessionImageNV'
newtype OpticalFlowSessionBindingPointNV = OpticalFlowSessionBindingPointNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkOpticalFlowSessionBindingPointNV" "VK_OPTICAL_FLOW_SESSION_BINDING_POINT_UNKNOWN_NV"
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_UNKNOWN_NV = OpticalFlowSessionBindingPointNV 0

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV' specifies the binding
-- point for the input frame.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV = OpticalFlowSessionBindingPointNV 1

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV' specifies the binding
-- point for the input reference frame.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV = OpticalFlowSessionBindingPointNV 2

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV' specifies the binding point
-- for the optional external hint flow vectors.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV = OpticalFlowSessionBindingPointNV 3

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV' specifies the
-- binding point for output flow vectors of default forward flow calcution.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV = OpticalFlowSessionBindingPointNV 4

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_FLOW_VECTOR_NV' specifies
-- the binding point for the optional output flow vector map of optional
-- backward flow calcution.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_FLOW_VECTOR_NV = OpticalFlowSessionBindingPointNV 5

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV' specifies the binding point
-- for the optional output cost map of default forward flow calcution.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV = OpticalFlowSessionBindingPointNV 6

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV' specifies the
-- binding point for the optional output cost map of optional backward flow
-- calcution.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV = OpticalFlowSessionBindingPointNV 7

-- | 'OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV' specifies the
-- binding point for the optional global flow value of default forward flow
-- calcution.
pattern OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV = OpticalFlowSessionBindingPointNV 8

{-# COMPLETE
  OPTICAL_FLOW_SESSION_BINDING_POINT_UNKNOWN_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_FLOW_VECTOR_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV
  , OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV ::
    OpticalFlowSessionBindingPointNV
  #-}

conNameOpticalFlowSessionBindingPointNV :: String
conNameOpticalFlowSessionBindingPointNV = "OpticalFlowSessionBindingPointNV"

enumPrefixOpticalFlowSessionBindingPointNV :: String
enumPrefixOpticalFlowSessionBindingPointNV = "OPTICAL_FLOW_SESSION_BINDING_POINT_"

showTableOpticalFlowSessionBindingPointNV :: [(OpticalFlowSessionBindingPointNV, String)]
showTableOpticalFlowSessionBindingPointNV =
  [
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_UNKNOWN_NV
    , "UNKNOWN_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV
    , "INPUT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV
    , "REFERENCE_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV
    , "HINT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV
    , "FLOW_VECTOR_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_FLOW_VECTOR_NV
    , "BACKWARD_FLOW_VECTOR_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV
    , "COST_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV
    , "BACKWARD_COST_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV
    , "GLOBAL_FLOW_NV"
    )
  ]

instance Show OpticalFlowSessionBindingPointNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowSessionBindingPointNV
      showTableOpticalFlowSessionBindingPointNV
      conNameOpticalFlowSessionBindingPointNV
      (\(OpticalFlowSessionBindingPointNV x) -> x)
      (showsPrec 11)

instance Read OpticalFlowSessionBindingPointNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowSessionBindingPointNV
      showTableOpticalFlowSessionBindingPointNV
      conNameOpticalFlowSessionBindingPointNV
      OpticalFlowSessionBindingPointNV

type OpticalFlowSessionCreateFlagsNV = OpticalFlowSessionCreateFlagBitsNV

-- | VkOpticalFlowSessionCreateFlagBitsNV - Bits specifying flags for optical
-- flow session
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowSessionCreateFlagsNV'
newtype OpticalFlowSessionCreateFlagBitsNV = OpticalFlowSessionCreateFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV' specifies that a
-- 'Vulkan.Core10.Handles.ImageView' with external flow vectors will be
-- used as hints in performing the motion search and /must/ be bound to
-- 'OPTICAL_FLOW_SESSION_BINDING_POINT_HINT_NV'.
pattern OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV = OpticalFlowSessionCreateFlagBitsNV 0x00000001

-- | 'OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV' specifies that the cost
-- for the forward flow is generated in a 'Vulkan.Core10.Handles.ImageView'
-- which /must/ be bound to 'OPTICAL_FLOW_SESSION_BINDING_POINT_COST_NV'.
-- Additionally, if 'OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV' is
-- also set, the cost for backward flow is generated in a
-- 'Vulkan.Core10.Handles.ImageView' which /must/ be bound to
-- 'OPTICAL_FLOW_SESSION_BINDING_POINT_BACKWARD_COST_NV'. The cost is the
-- confidence level of the flow vector for each grid in the frame. The Cost
-- implies how (in)accurate the flow vector is. Higher cost value implies
-- the flow vector to be less accurate and vice-versa.
pattern OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV = OpticalFlowSessionCreateFlagBitsNV 0x00000002

-- | 'OPTICAL_FLOW_SESSION_CREATE_ENABLE_GLOBAL_FLOW_BIT_NV' specifies that a
-- global flow vector is estimated from forward flow in a single pixel
-- 'Vulkan.Core10.Handles.ImageView' which /must/ be bound to
-- 'OPTICAL_FLOW_SESSION_BINDING_POINT_GLOBAL_FLOW_NV'.
pattern OPTICAL_FLOW_SESSION_CREATE_ENABLE_GLOBAL_FLOW_BIT_NV = OpticalFlowSessionCreateFlagBitsNV 0x00000004

-- | 'OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV' specifies that
-- regions of interest /can/ be specified in 'OpticalFlowExecuteInfoNV'.
pattern OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV = OpticalFlowSessionCreateFlagBitsNV 0x00000008

-- | 'OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV' specifies that
-- backward flow is generated in addition to forward flow which is always
-- generated.
pattern OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV = OpticalFlowSessionCreateFlagBitsNV 0x00000010

conNameOpticalFlowSessionCreateFlagBitsNV :: String
conNameOpticalFlowSessionCreateFlagBitsNV = "OpticalFlowSessionCreateFlagBitsNV"

enumPrefixOpticalFlowSessionCreateFlagBitsNV :: String
enumPrefixOpticalFlowSessionCreateFlagBitsNV = "OPTICAL_FLOW_SESSION_CREATE_"

showTableOpticalFlowSessionCreateFlagBitsNV :: [(OpticalFlowSessionCreateFlagBitsNV, String)]
showTableOpticalFlowSessionCreateFlagBitsNV =
  [
    ( OPTICAL_FLOW_SESSION_CREATE_ENABLE_HINT_BIT_NV
    , "ENABLE_HINT_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_CREATE_ENABLE_COST_BIT_NV
    , "ENABLE_COST_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_CREATE_ENABLE_GLOBAL_FLOW_BIT_NV
    , "ENABLE_GLOBAL_FLOW_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_CREATE_ALLOW_REGIONS_BIT_NV
    , "ALLOW_REGIONS_BIT_NV"
    )
  ,
    ( OPTICAL_FLOW_SESSION_CREATE_BOTH_DIRECTIONS_BIT_NV
    , "BOTH_DIRECTIONS_BIT_NV"
    )
  ]

instance Show OpticalFlowSessionCreateFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowSessionCreateFlagBitsNV
      showTableOpticalFlowSessionCreateFlagBitsNV
      conNameOpticalFlowSessionCreateFlagBitsNV
      (\(OpticalFlowSessionCreateFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read OpticalFlowSessionCreateFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowSessionCreateFlagBitsNV
      showTableOpticalFlowSessionCreateFlagBitsNV
      conNameOpticalFlowSessionCreateFlagBitsNV
      OpticalFlowSessionCreateFlagBitsNV

type OpticalFlowExecuteFlagsNV = OpticalFlowExecuteFlagBitsNV

-- | VkOpticalFlowExecuteFlagBitsNV - Bits specifying flags for a optical
-- flow vector calculation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_optical_flow VK_NV_optical_flow>,
-- 'OpticalFlowExecuteFlagsNV'
newtype OpticalFlowExecuteFlagBitsNV = OpticalFlowExecuteFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_NV' specifies that
-- temporal hints from previously generated flow vectors are not used. If
-- temporal hints are enabled, optical flow vectors from previous
-- 'cmdOpticalFlowExecuteNV' call are automatically used as hints for the
-- current 'cmdOpticalFlowExecuteNV' call, to take advantage of temporal
-- correlation in a video sequence. Temporal hints should be disabled if
-- there is a-priori knowledge of no temporal correlation (e.g. a scene
-- change, independent successive frame pairs).
pattern OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_NV = OpticalFlowExecuteFlagBitsNV 0x00000001

conNameOpticalFlowExecuteFlagBitsNV :: String
conNameOpticalFlowExecuteFlagBitsNV = "OpticalFlowExecuteFlagBitsNV"

enumPrefixOpticalFlowExecuteFlagBitsNV :: String
enumPrefixOpticalFlowExecuteFlagBitsNV = "OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_NV"

showTableOpticalFlowExecuteFlagBitsNV :: [(OpticalFlowExecuteFlagBitsNV, String)]
showTableOpticalFlowExecuteFlagBitsNV =
  [
    ( OPTICAL_FLOW_EXECUTE_DISABLE_TEMPORAL_HINTS_BIT_NV
    , ""
    )
  ]

instance Show OpticalFlowExecuteFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOpticalFlowExecuteFlagBitsNV
      showTableOpticalFlowExecuteFlagBitsNV
      conNameOpticalFlowExecuteFlagBitsNV
      (\(OpticalFlowExecuteFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read OpticalFlowExecuteFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixOpticalFlowExecuteFlagBitsNV
      showTableOpticalFlowExecuteFlagBitsNV
      conNameOpticalFlowExecuteFlagBitsNV
      OpticalFlowExecuteFlagBitsNV

type NV_OPTICAL_FLOW_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_OPTICAL_FLOW_SPEC_VERSION"
pattern NV_OPTICAL_FLOW_SPEC_VERSION :: forall a . Integral a => a
pattern NV_OPTICAL_FLOW_SPEC_VERSION = 1


type NV_OPTICAL_FLOW_EXTENSION_NAME = "VK_NV_optical_flow"

-- No documentation found for TopLevel "VK_NV_OPTICAL_FLOW_EXTENSION_NAME"
pattern NV_OPTICAL_FLOW_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_OPTICAL_FLOW_EXTENSION_NAME = "VK_NV_optical_flow"

