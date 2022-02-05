{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_executable_properties - device extension
--
-- == VK_KHR_pipeline_executable_properties
--
-- [__Name String__]
--     @VK_KHR_pipeline_executable_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     270
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jason Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_pipeline_executable_properties] @jekstrand%0A<<Here describe the issue or question you have about the VK_KHR_pipeline_executable_properties extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Romanick, Intel
--
--     -   Kenneth Graunke, Intel
--
--     -   Baldur Karlsson, Valve
--
--     -   Jesse Hall, Google
--
--     -   Jeff Bolz, Nvidia
--
--     -   Piers Daniel, Nvidia
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
--     -   Daniel Koch, Nvidia
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- When a pipeline is created, its state and shaders are compiled into zero
-- or more device-specific executables, which are used when executing
-- commands against that pipeline. This extension adds a mechanism to query
-- properties and statistics about the different executables produced by
-- the pipeline compilation process. This is intended to be used by
-- debugging and performance tools to allow them to provide more detailed
-- information to the user. Certain compile-time shader statistics provided
-- through this extension may be useful to developers for debugging or
-- performance analysis.
--
-- == New Commands
--
-- -   'getPipelineExecutableInternalRepresentationsKHR'
--
-- -   'getPipelineExecutablePropertiesKHR'
--
-- -   'getPipelineExecutableStatisticsKHR'
--
-- == New Structures
--
-- -   'PipelineExecutableInfoKHR'
--
-- -   'PipelineExecutableInternalRepresentationKHR'
--
-- -   'PipelineExecutablePropertiesKHR'
--
-- -   'PipelineExecutableStatisticKHR'
--
-- -   'PipelineInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR'
--
-- == New Unions
--
-- -   'PipelineExecutableStatisticValueKHR'
--
-- == New Enums
--
-- -   'PipelineExecutableStatisticFormatKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INFO_KHR'
--
-- == Issues
--
-- 1) What should we call the pieces of the pipeline which are produced by
-- the compilation process and about which you can query properties and
-- statistics?
--
-- __RESOLVED__: Call them “executables”. The name “binary” was used in
-- early drafts of the extension but it was determined that “pipeline
-- binary” could have a fairly broad meaning (such as a binary serialized
-- form of an entire pipeline) and was too big of a namespace for the very
-- specific needs of this extension.
--
-- == Version History
--
-- -   Revision 1, 2019-05-28 (Jason Ekstrand)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableInternalRepresentationKHR',
-- 'PipelineExecutablePropertiesKHR',
-- 'PipelineExecutableStatisticFormatKHR',
-- 'PipelineExecutableStatisticKHR', 'PipelineExecutableStatisticValueKHR',
-- 'PipelineInfoKHR', 'getPipelineExecutableInternalRepresentationsKHR',
-- 'getPipelineExecutablePropertiesKHR',
-- 'getPipelineExecutableStatisticsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_executable_properties  ( getPipelineExecutablePropertiesKHR
                                                                , getPipelineExecutableStatisticsKHR
                                                                , getPipelineExecutableInternalRepresentationsKHR
                                                                , PhysicalDevicePipelineExecutablePropertiesFeaturesKHR(..)
                                                                , PipelineInfoKHR(..)
                                                                , PipelineExecutablePropertiesKHR(..)
                                                                , PipelineExecutableInfoKHR(..)
                                                                , PipelineExecutableStatisticKHR(..)
                                                                , PipelineExecutableInternalRepresentationKHR(..)
                                                                , PipelineExecutableStatisticValueKHR(..)
                                                                , peekPipelineExecutableStatisticValueKHR
                                                                , PipelineExecutableStatisticFormatKHR( PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR
                                                                                                      , ..
                                                                                                      )
                                                                , KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION
                                                                , pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION
                                                                , KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
                                                                , pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
                                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CDouble)
import Foreign.C.Types (CDouble(..))
import Foreign.C.Types (CDouble(CDouble))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableInternalRepresentationsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutablePropertiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableStatisticsKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutablePropertiesKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result

-- | vkGetPipelineExecutablePropertiesKHR - Get the executables associated
-- with a pipeline
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of pipeline executables
-- associated with the pipeline is returned in @pExecutableCount@.
-- Otherwise, @pExecutableCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pExecutableCount@ is less than the number of
-- pipeline executables associated with the pipeline, at most
-- @pExecutableCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available properties were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-pipelineExecutableInfo-03270#
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-pipeline-03271#
--     @pipeline@ member of @pPipelineInfo@ /must/ have been created with
--     @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-pPipelineInfo-parameter#
--     @pPipelineInfo@ /must/ be a valid pointer to a valid
--     'PipelineInfoKHR' structure
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-pExecutableCount-parameter#
--     @pExecutableCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPipelineExecutablePropertiesKHR-pProperties-parameter# If
--     the value referenced by @pExecutableCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pExecutableCount@ 'PipelineExecutablePropertiesKHR'
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineExecutablePropertiesKHR',
-- 'PipelineInfoKHR'
getPipelineExecutablePropertiesKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @device@ is the device that created the pipeline.
                                      Device
                                   -> -- | @pPipelineInfo@ describes the pipeline being queried.
                                      PipelineInfoKHR
                                   -> io (Result, ("properties" ::: Vector PipelineExecutablePropertiesKHR))
getPipelineExecutablePropertiesKHR device pipelineInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutablePropertiesKHRPtr = pVkGetPipelineExecutablePropertiesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineExecutablePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutablePropertiesKHR is null" Nothing Nothing
  let vkGetPipelineExecutablePropertiesKHR' = mkVkGetPipelineExecutablePropertiesKHR vkGetPipelineExecutablePropertiesKHRPtr
  let device' = deviceHandle (device)
  pPipelineInfo <- ContT $ withCStruct (pipelineInfo)
  pPExecutableCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPipelineExecutablePropertiesKHR" (vkGetPipelineExecutablePropertiesKHR' device' pPipelineInfo (pPExecutableCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pExecutableCount <- lift $ peek @Word32 pPExecutableCount
  pPProperties <- ContT $ bracket (callocBytes @PipelineExecutablePropertiesKHR ((fromIntegral (pExecutableCount)) * 536)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 536) :: Ptr PipelineExecutablePropertiesKHR) . ($ ())) [0..(fromIntegral (pExecutableCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPipelineExecutablePropertiesKHR" (vkGetPipelineExecutablePropertiesKHR' device' pPipelineInfo (pPExecutableCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pExecutableCount' <- lift $ peek @Word32 pPExecutableCount
  pProperties' <- lift $ generateM (fromIntegral (pExecutableCount')) (\i -> peekCStruct @PipelineExecutablePropertiesKHR (((pPProperties) `advancePtrBytes` (536 * (i)) :: Ptr PipelineExecutablePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutableStatisticsKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableStatisticKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableStatisticKHR -> IO Result

-- | vkGetPipelineExecutableStatisticsKHR - Get compile time statistics
-- associated with a pipeline executable
--
-- = Description
--
-- If @pStatistics@ is @NULL@, then the number of statistics associated
-- with the pipeline executable is returned in @pStatisticCount@.
-- Otherwise, @pStatisticCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pStatistics@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pStatistics@. If @pStatisticCount@ is less than the number of
-- statistics associated with the pipeline executable, at most
-- @pStatisticCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available statistics were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pipelineExecutableInfo-03272#
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pipeline-03273#
--     @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     @device@
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pipeline-03274#
--     @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pExecutableInfo-parameter#
--     @pExecutableInfo@ /must/ be a valid pointer to a valid
--     'PipelineExecutableInfoKHR' structure
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pStatisticCount-parameter#
--     @pStatisticCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPipelineExecutableStatisticsKHR-pStatistics-parameter# If
--     the value referenced by @pStatisticCount@ is not @0@, and
--     @pStatistics@ is not @NULL@, @pStatistics@ /must/ be a valid pointer
--     to an array of @pStatisticCount@ 'PipelineExecutableStatisticKHR'
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableStatisticKHR'
getPipelineExecutableStatisticsKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @device@ is the device that created the pipeline.
                                      Device
                                   -> -- | @pExecutableInfo@ describes the pipeline executable being queried.
                                      PipelineExecutableInfoKHR
                                   -> io (Result, ("statistics" ::: Vector PipelineExecutableStatisticKHR))
getPipelineExecutableStatisticsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableStatisticsKHRPtr = pVkGetPipelineExecutableStatisticsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineExecutableStatisticsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutableStatisticsKHR is null" Nothing Nothing
  let vkGetPipelineExecutableStatisticsKHR' = mkVkGetPipelineExecutableStatisticsKHR vkGetPipelineExecutableStatisticsKHRPtr
  let device' = deviceHandle (device)
  pExecutableInfo <- ContT $ withCStruct (executableInfo)
  pPStatisticCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPipelineExecutableStatisticsKHR" (vkGetPipelineExecutableStatisticsKHR' device' pExecutableInfo (pPStatisticCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pStatisticCount <- lift $ peek @Word32 pPStatisticCount
  pPStatistics <- ContT $ bracket (callocBytes @PipelineExecutableStatisticKHR ((fromIntegral (pStatisticCount)) * 544)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPStatistics `advancePtrBytes` (i * 544) :: Ptr PipelineExecutableStatisticKHR) . ($ ())) [0..(fromIntegral (pStatisticCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPipelineExecutableStatisticsKHR" (vkGetPipelineExecutableStatisticsKHR' device' pExecutableInfo (pPStatisticCount) ((pPStatistics)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pStatisticCount' <- lift $ peek @Word32 pPStatisticCount
  pStatistics' <- lift $ generateM (fromIntegral (pStatisticCount')) (\i -> peekCStruct @PipelineExecutableStatisticKHR (((pPStatistics) `advancePtrBytes` (544 * (i)) :: Ptr PipelineExecutableStatisticKHR)))
  pure $ ((r'), pStatistics')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutableInternalRepresentationsKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableInternalRepresentationKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableInternalRepresentationKHR -> IO Result

-- | vkGetPipelineExecutableInternalRepresentationsKHR - Get internal
-- representations of the pipeline executable
--
-- = Description
--
-- If @pInternalRepresentations@ is @NULL@, then the number of internal
-- representations associated with the pipeline executable is returned in
-- @pInternalRepresentationCount@. Otherwise,
-- @pInternalRepresentationCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pInternalRepresentations@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pInternalRepresentations@. If
-- @pInternalRepresentationCount@ is less than the number of internal
-- representations associated with the pipeline executable, at most
-- @pInternalRepresentationCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available representations were returned.
--
-- While the details of the internal representations remain
-- implementation-dependent, the implementation /should/ order the internal
-- representations in the order in which they occur in the compiled
-- pipeline with the final shader assembly (if any) last.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pipelineExecutableInfo-03276#
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pipeline-03277#
--     @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     @device@
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pipeline-03278#
--     @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pExecutableInfo-parameter#
--     @pExecutableInfo@ /must/ be a valid pointer to a valid
--     'PipelineExecutableInfoKHR' structure
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pInternalRepresentationCount-parameter#
--     @pInternalRepresentationCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetPipelineExecutableInternalRepresentationsKHR-pInternalRepresentations-parameter#
--     If the value referenced by @pInternalRepresentationCount@ is not
--     @0@, and @pInternalRepresentations@ is not @NULL@,
--     @pInternalRepresentations@ /must/ be a valid pointer to an array of
--     @pInternalRepresentationCount@
--     'PipelineExecutableInternalRepresentationKHR' structures
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableInternalRepresentationKHR'
getPipelineExecutableInternalRepresentationsKHR :: forall io
                                                 . (MonadIO io)
                                                => -- | @device@ is the device that created the pipeline.
                                                   Device
                                                -> -- | @pExecutableInfo@ describes the pipeline executable being queried.
                                                   PipelineExecutableInfoKHR
                                                -> io (Result, ("internalRepresentations" ::: Vector PipelineExecutableInternalRepresentationKHR))
getPipelineExecutableInternalRepresentationsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableInternalRepresentationsKHRPtr = pVkGetPipelineExecutableInternalRepresentationsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineExecutableInternalRepresentationsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutableInternalRepresentationsKHR is null" Nothing Nothing
  let vkGetPipelineExecutableInternalRepresentationsKHR' = mkVkGetPipelineExecutableInternalRepresentationsKHR vkGetPipelineExecutableInternalRepresentationsKHRPtr
  let device' = deviceHandle (device)
  pExecutableInfo <- ContT $ withCStruct (executableInfo)
  pPInternalRepresentationCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPipelineExecutableInternalRepresentationsKHR" (vkGetPipelineExecutableInternalRepresentationsKHR' device' pExecutableInfo (pPInternalRepresentationCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInternalRepresentationCount <- lift $ peek @Word32 pPInternalRepresentationCount
  pPInternalRepresentations <- ContT $ bracket (callocBytes @PipelineExecutableInternalRepresentationKHR ((fromIntegral (pInternalRepresentationCount)) * 552)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPInternalRepresentations `advancePtrBytes` (i * 552) :: Ptr PipelineExecutableInternalRepresentationKHR) . ($ ())) [0..(fromIntegral (pInternalRepresentationCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPipelineExecutableInternalRepresentationsKHR" (vkGetPipelineExecutableInternalRepresentationsKHR' device' pExecutableInfo (pPInternalRepresentationCount) ((pPInternalRepresentations)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pInternalRepresentationCount' <- lift $ peek @Word32 pPInternalRepresentationCount
  pInternalRepresentations' <- lift $ generateM (fromIntegral (pInternalRepresentationCount')) (\i -> peekCStruct @PipelineExecutableInternalRepresentationKHR (((pPInternalRepresentations) `advancePtrBytes` (552 * (i)) :: Ptr PipelineExecutableInternalRepresentationKHR)))
  pure $ ((r'), pInternalRepresentations')


-- | VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR - Structure
-- describing whether pipeline executable properties are available
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
  { -- | #features-pipelineExecutableInfo# @pipelineExecutableInfo@ indicates
    -- that the implementation supports reporting properties and statistics
    -- about the pipeline executables associated with a compiled pipeline.
    pipelineExecutableInfo :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance ToCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineExecutablePropertiesFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineExecutableInfo))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  peekCStruct p = do
    pipelineExecutableInfo <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
             (bool32ToBool pipelineExecutableInfo)

instance Storable PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  zero = PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
           zero


-- | VkPipelineInfoKHR - Structure describing a pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutablePropertiesKHR'
data PipelineInfoKHR = PipelineInfoKHR
  { -- | @pipeline@ is a 'Vulkan.Core10.Handles.Pipeline' handle.
    --
    -- #VUID-VkPipelineInfoKHR-pipeline-parameter# @pipeline@ /must/ be a valid
    -- 'Vulkan.Core10.Handles.Pipeline' handle
    pipeline :: Pipeline }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineInfoKHR)
#endif
deriving instance Show PipelineInfoKHR

instance ToCStruct PipelineInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct PipelineInfoKHR where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    pure $ PipelineInfoKHR
             pipeline

instance Storable PipelineInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineInfoKHR where
  zero = PipelineInfoKHR
           zero


-- | VkPipelineExecutablePropertiesKHR - Structure describing a pipeline
-- executable
--
-- = Description
--
-- Not all implementations have a 1:1 mapping between shader stages and
-- pipeline executables and some implementations /may/ reduce a given
-- shader stage to fixed function hardware programming such that no
-- pipeline executable is available. No guarantees are provided about the
-- mapping between shader stages and pipeline executables and @stages@
-- /should/ be considered a best effort hint. Because the application
-- /cannot/ rely on the @stages@ field to provide an exact description,
-- @name@ and @description@ provide a human readable name and description
-- which more accurately describes the given pipeline executable.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutablePropertiesKHR'
data PipelineExecutablePropertiesKHR = PipelineExecutablePropertiesKHR
  { -- | @stages@ is a bitmask of zero or more
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' indicating
    -- which shader stages (if any) were principally used as inputs to compile
    -- this pipeline executable.
    stages :: ShaderStageFlags
  , -- | @name@ is an array of 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    -- @char@ containing a null-terminated UTF-8 string which is a short human
    -- readable name for this pipeline executable.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description for
    -- this pipeline executable.
    description :: ByteString
  , -- | @subgroupSize@ is the subgroup size with which this pipeline executable
    -- is dispatched.
    subgroupSize :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutablePropertiesKHR)
#endif
deriving instance Show PipelineExecutablePropertiesKHR

instance ToCStruct PipelineExecutablePropertiesKHR where
  withCStruct x f = allocaBytes 536 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutablePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stages)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (subgroupSize)
    f
  cStructSize = 536
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineExecutablePropertiesKHR where
  peekCStruct p = do
    stages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    subgroupSize <- peek @Word32 ((p `plusPtr` 532 :: Ptr Word32))
    pure $ PipelineExecutablePropertiesKHR
             stages name description subgroupSize

instance Storable PipelineExecutablePropertiesKHR where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutablePropertiesKHR where
  zero = PipelineExecutablePropertiesKHR
           zero
           mempty
           mempty
           zero


-- | VkPipelineExecutableInfoKHR - Structure describing a pipeline executable
-- to query for associated statistics or internal representations
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableInternalRepresentationsKHR',
-- 'getPipelineExecutableStatisticsKHR'
data PipelineExecutableInfoKHR = PipelineExecutableInfoKHR
  { -- | @pipeline@ is the pipeline to query.
    --
    -- #VUID-VkPipelineExecutableInfoKHR-pipeline-parameter# @pipeline@ /must/
    -- be a valid 'Vulkan.Core10.Handles.Pipeline' handle
    pipeline :: Pipeline
  , -- | @executableIndex@ is the index of the pipeline executable to query in
    -- the array of executable properties returned by
    -- 'getPipelineExecutablePropertiesKHR'.
    --
    -- #VUID-VkPipelineExecutableInfoKHR-executableIndex-03275#
    -- @executableIndex@ /must/ be less than the number of pipeline executables
    -- associated with @pipeline@ as returned in the @pExecutableCount@
    -- parameter of 'getPipelineExecutablePropertiesKHR'
    executableIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableInfoKHR)
#endif
deriving instance Show PipelineExecutableInfoKHR

instance ToCStruct PipelineExecutableInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (executableIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineExecutableInfoKHR where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    executableIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PipelineExecutableInfoKHR
             pipeline executableIndex

instance Storable PipelineExecutableInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutableInfoKHR where
  zero = PipelineExecutableInfoKHR
           zero
           zero


-- | VkPipelineExecutableStatisticKHR - Structure describing a compile-time
-- pipeline executable statistic
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'PipelineExecutableStatisticFormatKHR',
-- 'PipelineExecutableStatisticValueKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableStatisticsKHR'
data PipelineExecutableStatisticKHR = PipelineExecutableStatisticKHR
  { -- | @name@ is an array of 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    -- @char@ containing a null-terminated UTF-8 string which is a short human
    -- readable name for this statistic.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description for
    -- this statistic.
    description :: ByteString
  , -- | @format@ is a 'PipelineExecutableStatisticFormatKHR' value specifying
    -- the format of the data found in @value@.
    format :: PipelineExecutableStatisticFormatKHR
  , -- | @value@ is the value of this statistic.
    value :: PipelineExecutableStatisticValueKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableStatisticKHR)
#endif
deriving instance Show PipelineExecutableStatisticKHR

instance ToCStruct PipelineExecutableStatisticKHR where
  withCStruct x f = allocaBytes 544 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableStatisticKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (format)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (value) . ($ ())
    lift $ f
  cStructSize = 544
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (zero) . ($ ())
    lift $ f

instance FromCStruct PipelineExecutableStatisticKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    format <- peek @PipelineExecutableStatisticFormatKHR ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR))
    value <- peekPipelineExecutableStatisticValueKHR format ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR))
    pure $ PipelineExecutableStatisticKHR
             name description format value

instance Zero PipelineExecutableStatisticKHR where
  zero = PipelineExecutableStatisticKHR
           mempty
           mempty
           zero
           zero


-- | VkPipelineExecutableInternalRepresentationKHR - Structure describing the
-- textual form of a pipeline executable internal representation
--
-- = Description
--
-- If @pData@ is @NULL@, then the size, in bytes, of the internal
-- representation data is returned in @dataSize@. Otherwise, @dataSize@
-- must be the size of the buffer, in bytes, pointed to by @pData@ and on
-- return @dataSize@ is overwritten with the number of bytes of data
-- actually written to @pData@ including any trailing null character. If
-- @dataSize@ is less than the size, in bytes, of the internal
-- representation’s data, at most @dataSize@ bytes of data will be written
-- to @pData@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not
-- all the available representation was returned.
--
-- If @isText@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and @pData@ is not
-- @NULL@ and @dataSize@ is not zero, the last byte written to @pData@ will
-- be a null character.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableInternalRepresentationsKHR'
data PipelineExecutableInternalRepresentationKHR = PipelineExecutableInternalRepresentationKHR
  { -- | @name@ is an array of 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    -- @char@ containing a null-terminated UTF-8 string which is a short human
    -- readable name for this internal representation.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description for
    -- this internal representation.
    description :: ByteString
  , -- | @isText@ specifies whether the returned data is text or opaque data. If
    -- @isText@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then the data returned
    -- in @pData@ is text and is guaranteed to be a null-terminated UTF-8
    -- string.
    isText :: Bool
  , -- | @dataSize@ is an integer related to the size, in bytes, of the internal
    -- representation’s data, as described below.
    dataSize :: Word64
  , -- | @pData@ is either @NULL@ or a pointer to a block of data into which the
    -- implementation will write the internal representation.
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableInternalRepresentationKHR)
#endif
deriving instance Show PipelineExecutableInternalRepresentationKHR

instance ToCStruct PipelineExecutableInternalRepresentationKHR where
  withCStruct x f = allocaBytes 552 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableInternalRepresentationKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (isText))
    poke ((p `plusPtr` 536 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 544 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 552
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 536 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct PipelineExecutableInternalRepresentationKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    isText <- peek @Bool32 ((p `plusPtr` 528 :: Ptr Bool32))
    dataSize <- peek @CSize ((p `plusPtr` 536 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 544 :: Ptr (Ptr ())))
    pure $ PipelineExecutableInternalRepresentationKHR
             name description (bool32ToBool isText) (coerce @CSize @Word64 dataSize) pData

instance Storable PipelineExecutableInternalRepresentationKHR where
  sizeOf ~_ = 552
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutableInternalRepresentationKHR where
  zero = PipelineExecutableInternalRepresentationKHR
           mempty
           mempty
           zero
           zero
           zero


data PipelineExecutableStatisticValueKHR
  = B32 Bool
  | I64 Int64
  | U64 Word64
  | F64 Double
  deriving (Show)

instance ToCStruct PipelineExecutableStatisticValueKHR where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr PipelineExecutableStatisticValueKHR -> PipelineExecutableStatisticValueKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    B32 v -> lift $ poke (castPtr @_ @Bool32 p) (boolToBool32 (v))
    I64 v -> lift $ poke (castPtr @_ @Int64 p) (v)
    U64 v -> lift $ poke (castPtr @_ @Word64 p) (v)
    F64 v -> lift $ poke (castPtr @_ @CDouble p) (CDouble (v))
  pokeZeroCStruct :: Ptr PipelineExecutableStatisticValueKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero PipelineExecutableStatisticValueKHR where
  zero = I64 zero

peekPipelineExecutableStatisticValueKHR :: PipelineExecutableStatisticFormatKHR -> Ptr PipelineExecutableStatisticValueKHR -> IO PipelineExecutableStatisticValueKHR
peekPipelineExecutableStatisticValueKHR tag p = case tag of
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR -> B32 <$> (do
    b32 <- peek @Bool32 (castPtr @_ @Bool32 p)
    pure $ bool32ToBool b32)
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR -> I64 <$> (peek @Int64 (castPtr @_ @Int64 p))
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR -> U64 <$> (peek @Word64 (castPtr @_ @Word64 p))
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR -> F64 <$> (do
    f64 <- peek @CDouble (castPtr @_ @CDouble p)
    pure $ coerce @CDouble @Double f64)


-- | VkPipelineExecutableStatisticFormatKHR - Enum describing a pipeline
-- executable statistic
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>,
-- 'PipelineExecutableStatisticKHR'
newtype PipelineExecutableStatisticFormatKHR = PipelineExecutableStatisticFormatKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR' specifies that the
-- statistic is returned as a 32-bit boolean value which /must/ be either
-- 'Vulkan.Core10.FundamentalTypes.TRUE' or
-- 'Vulkan.Core10.FundamentalTypes.FALSE' and /should/ be read from the
-- @b32@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR  = PipelineExecutableStatisticFormatKHR 0
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR' specifies that the
-- statistic is returned as a signed 64-bit integer and /should/ be read
-- from the @i64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR   = PipelineExecutableStatisticFormatKHR 1
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR' specifies that the
-- statistic is returned as an unsigned 64-bit integer and /should/ be read
-- from the @u64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR  = PipelineExecutableStatisticFormatKHR 2
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR' specifies that the
-- statistic is returned as a 64-bit floating-point value and /should/ be
-- read from the @f64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR = PipelineExecutableStatisticFormatKHR 3
{-# complete PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR :: PipelineExecutableStatisticFormatKHR #-}

conNamePipelineExecutableStatisticFormatKHR :: String
conNamePipelineExecutableStatisticFormatKHR = "PipelineExecutableStatisticFormatKHR"

enumPrefixPipelineExecutableStatisticFormatKHR :: String
enumPrefixPipelineExecutableStatisticFormatKHR = "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_"

showTablePipelineExecutableStatisticFormatKHR :: [(PipelineExecutableStatisticFormatKHR, String)]
showTablePipelineExecutableStatisticFormatKHR =
  [ (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR , "BOOL32_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR  , "INT64_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR , "UINT64_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR, "FLOAT64_KHR")
  ]

instance Show PipelineExecutableStatisticFormatKHR where
  showsPrec = enumShowsPrec enumPrefixPipelineExecutableStatisticFormatKHR
                            showTablePipelineExecutableStatisticFormatKHR
                            conNamePipelineExecutableStatisticFormatKHR
                            (\(PipelineExecutableStatisticFormatKHR x) -> x)
                            (showsPrec 11)

instance Read PipelineExecutableStatisticFormatKHR where
  readPrec = enumReadPrec enumPrefixPipelineExecutableStatisticFormatKHR
                          showTablePipelineExecutableStatisticFormatKHR
                          conNamePipelineExecutableStatisticFormatKHR
                          PipelineExecutableStatisticFormatKHR


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

