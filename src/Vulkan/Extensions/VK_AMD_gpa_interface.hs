{-# language CPP #-}
-- | = Name
--
-- VK_AMD_gpa_interface - device extension
--
-- = VK_AMD_gpa_interface
--
-- [__Name String__]
--     @VK_AMD_gpa_interface@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     134
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_gpa_interface] @stu-s%0A*Here describe the issue or question you have about the VK_AMD_gpa_interface extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_gpa_interface.adoc VK_AMD_gpa_interface>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Noah Fredriks, AMD
--
--     -   Peter Lohrmann, AMD
--
--     -   Maciej Dziuban, AMD
--
-- == Description
--
-- This extension adds GPU Performance API (GPA) interface support for
-- accessing GPU global performance counters, streaming performance
-- monitors (SPM), and thread traces (SQTT), on AMD Radeon™ GPUs.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.GpaSessionAMD'
--
-- == New Commands
--
-- -   'cmdBeginGpaSampleAMD'
--
-- -   'cmdBeginGpaSessionAMD'
--
-- -   'cmdCopyGpaSessionResultsAMD'
--
-- -   'cmdEndGpaSampleAMD'
--
-- -   'cmdEndGpaSessionAMD'
--
-- -   'createGpaSessionAMD'
--
-- -   'destroyGpaSessionAMD'
--
-- -   'getGpaDeviceClockInfoAMD'
--
-- -   'getGpaSessionResultsAMD'
--
-- -   'getGpaSessionStatusAMD'
--
-- -   'resetGpaSessionAMD'
--
-- -   'setGpaDeviceClockModeAMD'
--
-- == New Structures
--
-- -   'GpaDeviceClockModeInfoAMD'
--
-- -   'GpaDeviceGetClockInfoAMD'
--
-- -   'GpaPerfBlockPropertiesAMD'
--
-- -   'GpaPerfCounterAMD'
--
-- -   'GpaSampleBeginInfoAMD'
--
-- -   'GpaSessionCreateInfoAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGpaFeaturesAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceGpaProperties2AMD'
--
--     -   'PhysicalDeviceGpaPropertiesAMD'
--
-- == New Enums
--
-- -   'GpaDeviceClockModeAMD'
--
-- -   'GpaPerfBlockAMD'
--
-- -   'GpaSampleTypeAMD'
--
-- -   'GpaSqShaderStageFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'GpaPerfBlockPropertiesFlagsAMD'
--
-- -   'GpaSqShaderStageFlagsAMD'
--
-- -   'PhysicalDeviceGpaPropertiesFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_GPA_INTERFACE_EXTENSION_NAME'
--
-- -   'AMD_GPA_INTERFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_GPA_SESSION_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_DEVICE_CLOCK_MODE_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_DEVICE_GET_CLOCK_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_FEATURES_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_2_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2026-05-01 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_gpa_interface Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_gpa_interface  ( createGpaSessionAMD
                                               , withGpaSessionAMD
                                               , destroyGpaSessionAMD
                                               , setGpaDeviceClockModeAMD
                                               , getGpaDeviceClockInfoAMD
                                               , cmdBeginGpaSessionAMD
                                               , cmdEndGpaSessionAMD
                                               , cmdBeginGpaSampleAMD
                                               , cmdEndGpaSampleAMD
                                               , getGpaSessionStatusAMD
                                               , getGpaSessionResultsAMD
                                               , resetGpaSessionAMD
                                               , cmdCopyGpaSessionResultsAMD
                                               , pattern GPA_PERF_BLOCK_GE1_AMD
                                               , pattern GPA_PERF_BLOCK_RLCLOCAL_AMD
                                               , GpaPerfBlockPropertiesAMD(..)
                                               , PhysicalDeviceGpaFeaturesAMD(..)
                                               , PhysicalDeviceGpaPropertiesAMD(..)
                                               , PhysicalDeviceGpaProperties2AMD(..)
                                               , GpaPerfCounterAMD(..)
                                               , GpaSampleBeginInfoAMD(..)
                                               , GpaDeviceClockModeInfoAMD(..)
                                               , GpaDeviceGetClockInfoAMD(..)
                                               , GpaSessionCreateInfoAMD(..)
                                               , GpaPerfBlockPropertiesFlagsAMD(..)
                                               , PhysicalDeviceGpaPropertiesFlagsAMD(..)
                                               , GpaSqShaderStageFlagsAMD
                                               , GpaSqShaderStageFlagBitsAMD( GPA_SQ_SHADER_STAGE_PS_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_VS_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_GS_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_ES_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_HS_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_LS_BIT_AMD
                                                                            , GPA_SQ_SHADER_STAGE_CS_BIT_AMD
                                                                            , ..
                                                                            )
                                               , GpaPerfBlockAMD( GPA_PERF_BLOCK_CPF_AMD
                                                                , GPA_PERF_BLOCK_IA_AMD
                                                                , GPA_PERF_BLOCK_VGT_AMD
                                                                , GPA_PERF_BLOCK_PA_AMD
                                                                , GPA_PERF_BLOCK_SC_AMD
                                                                , GPA_PERF_BLOCK_SPI_AMD
                                                                , GPA_PERF_BLOCK_SQ_AMD
                                                                , GPA_PERF_BLOCK_SX_AMD
                                                                , GPA_PERF_BLOCK_TA_AMD
                                                                , GPA_PERF_BLOCK_TD_AMD
                                                                , GPA_PERF_BLOCK_TCP_AMD
                                                                , GPA_PERF_BLOCK_TCC_AMD
                                                                , GPA_PERF_BLOCK_TCA_AMD
                                                                , GPA_PERF_BLOCK_DB_AMD
                                                                , GPA_PERF_BLOCK_CB_AMD
                                                                , GPA_PERF_BLOCK_GDS_AMD
                                                                , GPA_PERF_BLOCK_SRBM_AMD
                                                                , GPA_PERF_BLOCK_GRBM_AMD
                                                                , GPA_PERF_BLOCK_GRBM_SE_AMD
                                                                , GPA_PERF_BLOCK_RLC_AMD
                                                                , GPA_PERF_BLOCK_DMA_AMD
                                                                , GPA_PERF_BLOCK_MC_AMD
                                                                , GPA_PERF_BLOCK_CPG_AMD
                                                                , GPA_PERF_BLOCK_CPC_AMD
                                                                , GPA_PERF_BLOCK_WD_AMD
                                                                , GPA_PERF_BLOCK_TCS_AMD
                                                                , GPA_PERF_BLOCK_ATC_AMD
                                                                , GPA_PERF_BLOCK_ATC_L2_AMD
                                                                , GPA_PERF_BLOCK_MC_VM_L2_AMD
                                                                , GPA_PERF_BLOCK_EA_AMD
                                                                , GPA_PERF_BLOCK_RPB_AMD
                                                                , GPA_PERF_BLOCK_RMI_AMD
                                                                , GPA_PERF_BLOCK_UMCCH_AMD
                                                                , GPA_PERF_BLOCK_GE_AMD
                                                                , GPA_PERF_BLOCK_GL1A_AMD
                                                                , GPA_PERF_BLOCK_GL1C_AMD
                                                                , GPA_PERF_BLOCK_GL1CG_AMD
                                                                , GPA_PERF_BLOCK_GL2A_AMD
                                                                , GPA_PERF_BLOCK_GL2C_AMD
                                                                , GPA_PERF_BLOCK_CHA_AMD
                                                                , GPA_PERF_BLOCK_CHC_AMD
                                                                , GPA_PERF_BLOCK_CHCG_AMD
                                                                , GPA_PERF_BLOCK_GUS_AMD
                                                                , GPA_PERF_BLOCK_GCR_AMD
                                                                , GPA_PERF_BLOCK_PH_AMD
                                                                , GPA_PERF_BLOCK_UTCL1_AMD
                                                                , GPA_PERF_BLOCK_GE_DIST_AMD
                                                                , GPA_PERF_BLOCK_GE_SE_AMD
                                                                , GPA_PERF_BLOCK_DF_MALL_AMD
                                                                , GPA_PERF_BLOCK_SQ_WGP_AMD
                                                                , GPA_PERF_BLOCK_PC_AMD
                                                                , GPA_PERF_BLOCK_GL1XA_AMD
                                                                , GPA_PERF_BLOCK_GL1XC_AMD
                                                                , GPA_PERF_BLOCK_WGS_AMD
                                                                , GPA_PERF_BLOCK_EACPWD_AMD
                                                                , GPA_PERF_BLOCK_EASE_AMD
                                                                , GPA_PERF_BLOCK_RLCUSER_AMD
                                                                , ..
                                                                )
                                               , GpaSampleTypeAMD( GPA_SAMPLE_TYPE_CUMULATIVE_AMD
                                                                 , GPA_SAMPLE_TYPE_TRACE_AMD
                                                                 , GPA_SAMPLE_TYPE_TIMING_AMD
                                                                 , ..
                                                                 )
                                               , GpaDeviceClockModeAMD( GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD
                                                                      , GPA_DEVICE_CLOCK_MODE_QUERY_AMD
                                                                      , GPA_DEVICE_CLOCK_MODE_PROFILING_AMD
                                                                      , GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD
                                                                      , GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD
                                                                      , GPA_DEVICE_CLOCK_MODE_PEAK_AMD
                                                                      , ..
                                                                      )
                                               , AMD_GPA_INTERFACE_SPEC_VERSION
                                               , pattern AMD_GPA_INTERFACE_SPEC_VERSION
                                               , AMD_GPA_INTERFACE_EXTENSION_NAME
                                               , pattern AMD_GPA_INTERFACE_EXTENSION_NAME
                                               , GpaSessionAMD(..)
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
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
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
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginGpaSampleAMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginGpaSessionAMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyGpaSessionResultsAMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndGpaSampleAMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndGpaSessionAMD))
import Vulkan.Dynamic (DeviceCmds(pVkCreateGpaSessionAMD))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyGpaSessionAMD))
import Vulkan.Dynamic (DeviceCmds(pVkGetGpaDeviceClockInfoAMD))
import Vulkan.Dynamic (DeviceCmds(pVkGetGpaSessionResultsAMD))
import Vulkan.Dynamic (DeviceCmds(pVkGetGpaSessionStatusAMD))
import Vulkan.Dynamic (DeviceCmds(pVkResetGpaSessionAMD))
import Vulkan.Dynamic (DeviceCmds(pVkSetGpaDeviceClockModeAMD))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Extensions.Handles (GpaSessionAMD)
import Vulkan.Extensions.Handles (GpaSessionAMD(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GPA_DEVICE_CLOCK_MODE_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GPA_DEVICE_GET_CLOCK_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_FEATURES_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_2_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_AMD))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (GpaSessionAMD(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateGpaSessionAMD
  :: FunPtr (Ptr Device_T -> Ptr GpaSessionCreateInfoAMD -> Ptr AllocationCallbacks -> Ptr GpaSessionAMD -> IO Result) -> Ptr Device_T -> Ptr GpaSessionCreateInfoAMD -> Ptr AllocationCallbacks -> Ptr GpaSessionAMD -> IO Result

-- | vkCreateGpaSessionAMD - Create a new GPA session object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateGpaSessionAMD-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateGpaSessionAMD-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'GpaSessionCreateInfoAMD'
--     structure
--
-- -   #VUID-vkCreateGpaSessionAMD-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateGpaSessionAMD-pGpaSession-parameter# @pGpaSession@
--     /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCreateGpaSessionAMD-device-queuecount# The device /must/
--     have been created with at least @1@ queue
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD', 'GpaSessionCreateInfoAMD'
createGpaSessionAMD :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that creates the GPA session object.
                       Device
                    -> -- | @pCreateInfo@ is a pointer to a 'GpaSessionCreateInfoAMD' structure
                       -- containing information about how the GPA session object is to be
                       -- created.
                       GpaSessionCreateInfoAMD
                    -> -- | @pAllocator@ controls host memory allocation as described in the
                       -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                       -- chapter.
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (GpaSessionAMD)
createGpaSessionAMD device createInfo allocator = liftIO . evalContT $ do
  let vkCreateGpaSessionAMDPtr = pVkCreateGpaSessionAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateGpaSessionAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateGpaSessionAMD is null" Nothing Nothing
  let vkCreateGpaSessionAMD' = mkVkCreateGpaSessionAMD vkCreateGpaSessionAMDPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPGpaSession <- ContT $ bracket (callocBytes @GpaSessionAMD 8) free
  r <- lift $ traceAroundEvent "vkCreateGpaSessionAMD" (vkCreateGpaSessionAMD'
                                                          (deviceHandle (device))
                                                          pCreateInfo
                                                          pAllocator
                                                          (pPGpaSession))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pGpaSession <- lift $ peek @GpaSessionAMD pPGpaSession
  pure $ (pGpaSession)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createGpaSessionAMD' and 'destroyGpaSessionAMD'
--
-- To ensure that 'destroyGpaSessionAMD' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withGpaSessionAMD :: forall io r . MonadIO io => Device -> GpaSessionCreateInfoAMD -> Maybe AllocationCallbacks -> (io GpaSessionAMD -> (GpaSessionAMD -> io ()) -> r) -> r
withGpaSessionAMD device pCreateInfo pAllocator b =
  b (createGpaSessionAMD device pCreateInfo pAllocator)
    (\(o0) -> destroyGpaSessionAMD device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyGpaSessionAMD
  :: FunPtr (Ptr Device_T -> GpaSessionAMD -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> GpaSessionAMD -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyGpaSessionAMD - Destroy a GPA session object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyGpaSessionAMD-gpaSession-12408# All submitted
--     commands that refer to @gpaSession@ /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyGpaSessionAMD-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyGpaSessionAMD-gpaSession-parameter# If @gpaSession@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @gpaSession@ /must/
--     be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkDestroyGpaSessionAMD-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyGpaSessionAMD-gpaSession-parent# If @gpaSession@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @gpaSession@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
destroyGpaSessionAMD :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that destroys the GPA session.
                        Device
                     -> -- | @gpaSession@ is the handle of the GPA session to destroy.
                        GpaSessionAMD
                     -> -- | @pAllocator@ controls host memory allocation as described in the
                        -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                        -- chapter.
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io ()
destroyGpaSessionAMD device gpaSession allocator = liftIO . evalContT $ do
  let vkDestroyGpaSessionAMDPtr = pVkDestroyGpaSessionAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyGpaSessionAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyGpaSessionAMD is null" Nothing Nothing
  let vkDestroyGpaSessionAMD' = mkVkDestroyGpaSessionAMD vkDestroyGpaSessionAMDPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyGpaSessionAMD" (vkDestroyGpaSessionAMD'
                                                      (deviceHandle (device))
                                                      (gpaSession)
                                                      pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetGpaDeviceClockModeAMD
  :: FunPtr (Ptr Device_T -> Ptr GpaDeviceClockModeInfoAMD -> IO Result) -> Ptr Device_T -> Ptr GpaDeviceClockModeInfoAMD -> IO Result

-- | vkSetGpaDeviceClockModeAMD - Setting a device clock
--
-- == Valid Usage
--
-- -   #VUID-vkSetGpaDeviceClockModeAMD-clockModes-12415# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-clockModes ::clockModes>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetGpaDeviceClockModeAMD-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetGpaDeviceClockModeAMD-pInfo-parameter# @pInfo@ /must/ be
--     a valid pointer to a 'GpaDeviceClockModeInfoAMD' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.Device', 'GpaDeviceClockModeInfoAMD'
setGpaDeviceClockModeAMD :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that sets the clocks.
                            Device
                         -> io (GpaDeviceClockModeInfoAMD)
setGpaDeviceClockModeAMD device = liftIO . evalContT $ do
  let vkSetGpaDeviceClockModeAMDPtr = pVkSetGpaDeviceClockModeAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetGpaDeviceClockModeAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetGpaDeviceClockModeAMD is null" Nothing Nothing
  let vkSetGpaDeviceClockModeAMD' = mkVkSetGpaDeviceClockModeAMD vkSetGpaDeviceClockModeAMDPtr
  pPInfo <- ContT (withZeroCStruct @GpaDeviceClockModeInfoAMD)
  r <- lift $ traceAroundEvent "vkSetGpaDeviceClockModeAMD" (vkSetGpaDeviceClockModeAMD'
                                                               (deviceHandle (device))
                                                               (pPInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfo <- lift $ peekCStruct @GpaDeviceClockModeInfoAMD pPInfo
  pure $ (pInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGpaDeviceClockInfoAMD
  :: FunPtr (Ptr Device_T -> Ptr GpaDeviceGetClockInfoAMD -> IO Result) -> Ptr Device_T -> Ptr GpaDeviceGetClockInfoAMD -> IO Result

-- | vkGetGpaDeviceClockInfoAMD - Getting device clocks and ratios
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.Device', 'GpaDeviceGetClockInfoAMD'
getGpaDeviceClockInfoAMD :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that sets the clocks.
                            --
                            -- #VUID-vkGetGpaDeviceClockInfoAMD-device-parameter# @device@ /must/ be a
                            -- valid 'Vulkan.Core10.Handles.Device' handle
                            Device
                         -> io (GpaDeviceGetClockInfoAMD)
getGpaDeviceClockInfoAMD device = liftIO . evalContT $ do
  let vkGetGpaDeviceClockInfoAMDPtr = pVkGetGpaDeviceClockInfoAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetGpaDeviceClockInfoAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGpaDeviceClockInfoAMD is null" Nothing Nothing
  let vkGetGpaDeviceClockInfoAMD' = mkVkGetGpaDeviceClockInfoAMD vkGetGpaDeviceClockInfoAMDPtr
  pPInfo <- ContT (withZeroCStruct @GpaDeviceGetClockInfoAMD)
  r <- lift $ traceAroundEvent "vkGetGpaDeviceClockInfoAMD" (vkGetGpaDeviceClockInfoAMD'
                                                               (deviceHandle (device))
                                                               (pPInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfo <- lift $ peekCStruct @GpaDeviceGetClockInfoAMD pPInfo
  pure $ (pInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginGpaSessionAMD
  :: FunPtr (Ptr CommandBuffer_T -> GpaSessionAMD -> IO Result) -> Ptr CommandBuffer_T -> GpaSessionAMD -> IO Result

-- | vkCmdBeginGpaSessionAMD - Begin a GPA session
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-gpaSession-12409# If @gpaSession@ has
--     been used previously to begin and end a session,
--     'resetGpaSessionAMD' /must/ have first been called
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-commandBuffer-12410# If another GPA
--     session has been started with 'cmdBeginGpaSessionAMD' in
--     @commandBuffer@, it /must/ have been ended using
--     'cmdEndGpaSessionAMD' before this call
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-gpaSession-parameter# @gpaSession@
--     /must/ be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginGpaSessionAMD-commonparent# Both of @commandBuffer@,
--     and @gpaSession@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBeginGpaSessionAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
cmdBeginGpaSessionAMD :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @gpaSession@ is the handle of the GPA session to begin.
                         GpaSessionAMD
                      -> io ()
cmdBeginGpaSessionAMD commandBuffer gpaSession = liftIO $ do
  let vkCmdBeginGpaSessionAMDPtr = pVkCmdBeginGpaSessionAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBeginGpaSessionAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginGpaSessionAMD is null" Nothing Nothing
  let vkCmdBeginGpaSessionAMD' = mkVkCmdBeginGpaSessionAMD vkCmdBeginGpaSessionAMDPtr
  r <- traceAroundEvent "vkCmdBeginGpaSessionAMD" (vkCmdBeginGpaSessionAMD'
                                                     (commandBufferHandle (commandBuffer))
                                                     (gpaSession))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndGpaSessionAMD
  :: FunPtr (Ptr CommandBuffer_T -> GpaSessionAMD -> IO Result) -> Ptr CommandBuffer_T -> GpaSessionAMD -> IO Result

-- | vkCmdEndGpaSessionAMD - End a GPA session
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndGpaSessionAMD-gpaSession-12411# @gpaSession@ /must/
--     have previously begun using 'cmdBeginGpaSessionAMD'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndGpaSessionAMD-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndGpaSessionAMD-gpaSession-parameter# @gpaSession@
--     /must/ be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCmdEndGpaSessionAMD-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndGpaSessionAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdEndGpaSessionAMD-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdEndGpaSessionAMD-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdEndGpaSessionAMD-commonparent# Both of @commandBuffer@,
--     and @gpaSession@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdEndGpaSessionAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
cmdEndGpaSessionAMD :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @gpaSession@ is the handle of the GPA session to end.
                       GpaSessionAMD
                    -> io ()
cmdEndGpaSessionAMD commandBuffer gpaSession = liftIO $ do
  let vkCmdEndGpaSessionAMDPtr = pVkCmdEndGpaSessionAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndGpaSessionAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndGpaSessionAMD is null" Nothing Nothing
  let vkCmdEndGpaSessionAMD' = mkVkCmdEndGpaSessionAMD vkCmdEndGpaSessionAMDPtr
  r <- traceAroundEvent "vkCmdEndGpaSessionAMD" (vkCmdEndGpaSessionAMD'
                                                   (commandBufferHandle (commandBuffer))
                                                   (gpaSession))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginGpaSampleAMD
  :: FunPtr (Ptr CommandBuffer_T -> GpaSessionAMD -> Ptr GpaSampleBeginInfoAMD -> Ptr Word32 -> IO Result) -> Ptr CommandBuffer_T -> GpaSessionAMD -> Ptr GpaSampleBeginInfoAMD -> Ptr Word32 -> IO Result

-- | vkCmdBeginGpaSampleAMD - Beginning a sample
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-gpaSession-parameter# @gpaSession@
--     /must/ be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-pGpaSampleBeginInfo-parameter#
--     @pGpaSampleBeginInfo@ /must/ be a valid pointer to a valid
--     'GpaSampleBeginInfoAMD' structure
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-pSampleID-parameter# @pSampleID@ /must/
--     be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginGpaSampleAMD-commonparent# Both of @commandBuffer@,
--     and @gpaSession@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBeginGpaSampleAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GpaSampleBeginInfoAMD',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
cmdBeginGpaSampleAMD :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @gpaSession@ is the handle of the GPA session to record the sample.
                        GpaSessionAMD
                     -> -- | @pGpaSampleBeginInfo@ is a pointer to a 'GpaSampleBeginInfoAMD'
                        -- structure describing the sample parameters.
                        GpaSampleBeginInfoAMD
                     -> io (("sampleID" ::: Word32))
cmdBeginGpaSampleAMD commandBuffer
                       gpaSession
                       gpaSampleBeginInfo = liftIO . evalContT $ do
  let vkCmdBeginGpaSampleAMDPtr = pVkCmdBeginGpaSampleAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginGpaSampleAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginGpaSampleAMD is null" Nothing Nothing
  let vkCmdBeginGpaSampleAMD' = mkVkCmdBeginGpaSampleAMD vkCmdBeginGpaSampleAMDPtr
  pGpaSampleBeginInfo <- ContT $ withCStruct (gpaSampleBeginInfo)
  pPSampleID <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkCmdBeginGpaSampleAMD" (vkCmdBeginGpaSampleAMD'
                                                           (commandBufferHandle (commandBuffer))
                                                           (gpaSession)
                                                           pGpaSampleBeginInfo
                                                           (pPSampleID))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSampleID <- lift $ peek @Word32 pPSampleID
  pure $ (pSampleID)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndGpaSampleAMD
  :: FunPtr (Ptr CommandBuffer_T -> GpaSessionAMD -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> GpaSessionAMD -> Word32 -> IO ()

-- | vkCmdEndGpaSampleAMD - Ending a sample
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndGpaSampleAMD-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndGpaSampleAMD-gpaSession-parameter# @gpaSession@ /must/
--     be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCmdEndGpaSampleAMD-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndGpaSampleAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdEndGpaSampleAMD-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdEndGpaSampleAMD-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdEndGpaSampleAMD-commonparent# Both of @commandBuffer@,
--     and @gpaSession@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdEndGpaSampleAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
cmdEndGpaSampleAMD :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @gpaSession@ is the handle of the GPA session that is recording the
                      -- sample.
                      GpaSessionAMD
                   -> -- | @sampleID@ is a unique sample ID returned by a previous call to
                      -- 'cmdBeginGpaSampleAMD'.
                      ("sampleID" ::: Word32)
                   -> io ()
cmdEndGpaSampleAMD commandBuffer gpaSession sampleID = liftIO $ do
  let vkCmdEndGpaSampleAMDPtr = pVkCmdEndGpaSampleAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndGpaSampleAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndGpaSampleAMD is null" Nothing Nothing
  let vkCmdEndGpaSampleAMD' = mkVkCmdEndGpaSampleAMD vkCmdEndGpaSampleAMDPtr
  traceAroundEvent "vkCmdEndGpaSampleAMD" (vkCmdEndGpaSampleAMD'
                                             (commandBufferHandle (commandBuffer))
                                             (gpaSession)
                                             (sampleID))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGpaSessionStatusAMD
  :: FunPtr (Ptr Device_T -> GpaSessionAMD -> IO Result) -> Ptr Device_T -> GpaSessionAMD -> IO Result

-- | vkGetGpaSessionStatusAMD - Getting the status of a GPA session
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
getGpaSessionStatusAMD :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that sets the clocks.
                          --
                          -- #VUID-vkGetGpaSessionStatusAMD-device-parameter# @device@ /must/ be a
                          -- valid 'Vulkan.Core10.Handles.Device' handle
                          Device
                       -> -- | @gpaSession@ is the session whose status is queried.
                          --
                          -- #VUID-vkGetGpaSessionStatusAMD-gpaSession-parameter# @gpaSession@ /must/
                          -- be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
                          --
                          -- #VUID-vkGetGpaSessionStatusAMD-gpaSession-parent# @gpaSession@ /must/
                          -- have been created, allocated, or retrieved from @device@
                          GpaSessionAMD
                       -> io ()
getGpaSessionStatusAMD device gpaSession = liftIO $ do
  let vkGetGpaSessionStatusAMDPtr = pVkGetGpaSessionStatusAMD (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetGpaSessionStatusAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGpaSessionStatusAMD is null" Nothing Nothing
  let vkGetGpaSessionStatusAMD' = mkVkGetGpaSessionStatusAMD vkGetGpaSessionStatusAMDPtr
  r <- traceAroundEvent "vkGetGpaSessionStatusAMD" (vkGetGpaSessionStatusAMD'
                                                      (deviceHandle (device))
                                                      (gpaSession))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGpaSessionResultsAMD
  :: FunPtr (Ptr Device_T -> GpaSessionAMD -> Word32 -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> GpaSessionAMD -> Word32 -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetGpaSessionResultsAMD - Getting the status of a GPA session
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetGpaSessionResultsAMD-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetGpaSessionResultsAMD-gpaSession-parameter# @gpaSession@
--     /must/ be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkGetGpaSessionResultsAMD-pSizeInBytes-parameter#
--     @pSizeInBytes@ /must/ be a valid pointer to a @size_t@ value
--
-- -   #VUID-vkGetGpaSessionResultsAMD-pData-parameter# If @pData@ is not
--     @NULL@, @pData@ /must/ be a valid pointer to an array of
--     @pSizeInBytes@ bytes
--
-- -   #VUID-vkGetGpaSessionResultsAMD-pSizeInBytes-arraylength# If @pData@
--     is not @NULL@, the value referenced by @pSizeInBytes@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkGetGpaSessionResultsAMD-gpaSession-parent# @gpaSession@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
getGpaSessionResultsAMD :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that sets the clocks.
                           Device
                        -> -- | @gpaSession@ is the session whose results are queried.
                           GpaSessionAMD
                        -> -- | @sampleID@ is the sample ID, returned by 'cmdBeginGpaSampleAMD', whose
                           -- results are to be queried.
                           ("sampleID" ::: Word32)
                        -> -- | @pData@ is either @NULL@ or a pointer to an array of @pSizeInBytes@
                           -- bytes where the results will be written.
                           ("data" ::: Ptr ())
                        -> io (("sizeInBytes" ::: Word64))
getGpaSessionResultsAMD device
                          gpaSession
                          sampleID
                          data' = liftIO . evalContT $ do
  let vkGetGpaSessionResultsAMDPtr = pVkGetGpaSessionResultsAMD (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetGpaSessionResultsAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGpaSessionResultsAMD is null" Nothing Nothing
  let vkGetGpaSessionResultsAMD' = mkVkGetGpaSessionResultsAMD vkGetGpaSessionResultsAMDPtr
  pPSizeInBytes <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetGpaSessionResultsAMD" (vkGetGpaSessionResultsAMD'
                                                              (deviceHandle (device))
                                                              (gpaSession)
                                                              (sampleID)
                                                              (pPSizeInBytes)
                                                              (data'))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSizeInBytes <- lift $ peek @CSize pPSizeInBytes
  pure $ ((coerce @CSize @Word64 pSizeInBytes))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetGpaSessionAMD
  :: FunPtr (Ptr Device_T -> GpaSessionAMD -> IO Result) -> Ptr Device_T -> GpaSessionAMD -> IO Result

-- | vkResetGpaSessionAMD - Reset a GPA session
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
resetGpaSessionAMD :: forall io
                    . (MonadIO io)
                   => -- | #VUID-vkResetGpaSessionAMD-device-parameter# @device@ /must/ be a valid
                      -- 'Vulkan.Core10.Handles.Device' handle
                      Device
                   -> -- | @gpaSession@ is the handle of the GPA session to reset.
                      --
                      -- #VUID-vkResetGpaSessionAMD-gpaSession-parameter# @gpaSession@ /must/ be
                      -- a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
                      --
                      -- #VUID-vkResetGpaSessionAMD-gpaSession-parent# @gpaSession@ /must/ have
                      -- been created, allocated, or retrieved from @device@
                      GpaSessionAMD
                   -> io ()
resetGpaSessionAMD device gpaSession = liftIO $ do
  let vkResetGpaSessionAMDPtr = pVkResetGpaSessionAMD (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkResetGpaSessionAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetGpaSessionAMD is null" Nothing Nothing
  let vkResetGpaSessionAMD' = mkVkResetGpaSessionAMD vkResetGpaSessionAMDPtr
  r <- traceAroundEvent "vkResetGpaSessionAMD" (vkResetGpaSessionAMD'
                                                  (deviceHandle (device))
                                                  (gpaSession))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyGpaSessionResultsAMD
  :: FunPtr (Ptr CommandBuffer_T -> GpaSessionAMD -> IO ()) -> Ptr CommandBuffer_T -> GpaSessionAMD -> IO ()

-- | vkCmdCopyGpaSessionResultsAMD - Copying GPA session results
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-gpaSession-parameter#
--     @gpaSession@ /must/ be a valid
--     'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-suspended# This command /must/
--     not be called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdCopyGpaSessionResultsAMD-commonparent# Both of
--     @commandBuffer@, and @gpaSession@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdCopyGpaSessionResultsAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.GpaSessionAMD'
cmdCopyGpaSessionResultsAMD :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @gpaSession@ is the handle of the GPA session that is the destination of
                               -- the copy.
                               GpaSessionAMD
                            -> io ()
cmdCopyGpaSessionResultsAMD commandBuffer gpaSession = liftIO $ do
  let vkCmdCopyGpaSessionResultsAMDPtr = pVkCmdCopyGpaSessionResultsAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdCopyGpaSessionResultsAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyGpaSessionResultsAMD is null" Nothing Nothing
  let vkCmdCopyGpaSessionResultsAMD' = mkVkCmdCopyGpaSessionResultsAMD vkCmdCopyGpaSessionResultsAMDPtr
  traceAroundEvent "vkCmdCopyGpaSessionResultsAMD" (vkCmdCopyGpaSessionResultsAMD'
                                                      (commandBufferHandle (commandBuffer))
                                                      (gpaSession))
  pure $ ()


-- No documentation found for TopLevel "VK_GPA_PERF_BLOCK_GE1_AMD"
pattern GPA_PERF_BLOCK_GE1_AMD = GPA_PERF_BLOCK_GE_AMD


-- No documentation found for TopLevel "VK_GPA_PERF_BLOCK_RLCLOCAL_AMD"
pattern GPA_PERF_BLOCK_RLCLOCAL_AMD = GPA_PERF_BLOCK_RLCUSER_AMD


-- | VkGpaPerfBlockPropertiesAMD - Structure describing GPU performance API
-- block properties for a physical device
--
-- = Members
--
-- The members of the 'GpaPerfBlockPropertiesAMD' structure describe the
-- following:
--
-- = Description
--
-- If the 'GpaPerfBlockPropertiesAMD' structure is included in the @pNext@
-- chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaPerfBlockAMD', 'GpaPerfBlockPropertiesFlagsAMD',
-- 'PhysicalDeviceGpaPropertiesAMD'
data GpaPerfBlockPropertiesAMD = GpaPerfBlockPropertiesAMD
  { -- | @blockType@ is a 'GpaPerfBlockAMD' specifying the performance block
    -- type.
    --
    -- #VUID-VkGpaPerfBlockPropertiesAMD-blockType-parameter# @blockType@
    -- /must/ be a valid 'GpaPerfBlockAMD' value
    blockType :: GpaPerfBlockAMD
  , -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkGpaPerfBlockPropertiesAMD-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: GpaPerfBlockPropertiesFlagsAMD
  , -- | @instanceCount@ is the number of instances of this block that are
    -- available in the device.
    instanceCount :: Word32
  , -- | @maxEventID@ is the maximum event ID for this block.
    maxEventID :: Word32
  , -- | @maxGlobalOnlyCounters@ is the number of counters available only for
    -- global counters.
    maxGlobalOnlyCounters :: Word32
  , -- | @maxGlobalSharedCounters@ is the total counters available including
    -- state shared between global and streaming performance monitor counters.
    maxGlobalSharedCounters :: Word32
  , -- | @maxStreamingCounters@ is the maximum number of counters available for
    -- streaming only.
    maxStreamingCounters :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaPerfBlockPropertiesAMD)
#endif
deriving instance Show GpaPerfBlockPropertiesAMD

instance ToCStruct GpaPerfBlockPropertiesAMD where
  withCStruct x f = allocaBytes 28 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaPerfBlockPropertiesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD)) (blockType)
    poke ((p `plusPtr` 4 :: Ptr GpaPerfBlockPropertiesFlagsAMD)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (maxEventID)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxGlobalOnlyCounters)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxGlobalSharedCounters)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxStreamingCounters)
    f
  cStructSize = 28
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD)) (zero)
    poke ((p `plusPtr` 4 :: Ptr GpaPerfBlockPropertiesFlagsAMD)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct GpaPerfBlockPropertiesAMD where
  peekCStruct p = do
    blockType <- peek @GpaPerfBlockAMD ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD))
    flags <- peek @GpaPerfBlockPropertiesFlagsAMD ((p `plusPtr` 4 :: Ptr GpaPerfBlockPropertiesFlagsAMD))
    instanceCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    maxEventID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    maxGlobalOnlyCounters <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxGlobalSharedCounters <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxStreamingCounters <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ GpaPerfBlockPropertiesAMD
             blockType
             flags
             instanceCount
             maxEventID
             maxGlobalOnlyCounters
             maxGlobalSharedCounters
             maxStreamingCounters

instance Storable GpaPerfBlockPropertiesAMD where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GpaPerfBlockPropertiesAMD where
  zero = GpaPerfBlockPropertiesAMD
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceGpaFeaturesAMD - Structure describing support for GPU
-- performance API
--
-- = Description
--
-- If the 'PhysicalDeviceGpaFeaturesAMD' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceGpaFeaturesAMD', it /must/ add an instance of the
-- structure, with the desired feature members set to
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGpaFeaturesAMD = PhysicalDeviceGpaFeaturesAMD
  { -- | #features-perfCounters# @perfCounters@ specifies whether performance
    -- counters are supported.
    perfCounters :: Bool
  , -- | #features-streamingPerfCounters# @streamingPerfCounters@ specifies
    -- whether streaming performance counters are supported.
    streamingPerfCounters :: Bool
  , -- | #features-sqThreadTracing# @sqThreadTracing@ specifies whether thread
    -- tracing is supported.
    sqThreadTracing :: Bool
  , -- | #features-clockModes# @clockModes@ specifies whether setting clock modes
    -- is supported.
    clockModes :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGpaFeaturesAMD)
#endif
deriving instance Show PhysicalDeviceGpaFeaturesAMD

instance ToCStruct PhysicalDeviceGpaFeaturesAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGpaFeaturesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perfCounters))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (streamingPerfCounters))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (sqThreadTracing))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (clockModes))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGpaFeaturesAMD where
  peekCStruct p = do
    perfCounters <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    streamingPerfCounters <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    sqThreadTracing <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    clockModes <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceGpaFeaturesAMD
             (bool32ToBool perfCounters)
             (bool32ToBool streamingPerfCounters)
             (bool32ToBool sqThreadTracing)
             (bool32ToBool clockModes)

instance Storable PhysicalDeviceGpaFeaturesAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGpaFeaturesAMD where
  zero = PhysicalDeviceGpaFeaturesAMD
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceGpaPropertiesAMD - Structure describing GPU performance
-- API properties for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceGpaPropertiesAMD' structure describe
-- the following:
--
-- = Description
--
-- If the 'PhysicalDeviceGpaPropertiesAMD' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'GpaPerfBlockPropertiesAMD', 'PhysicalDeviceGpaPropertiesFlagsAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGpaPropertiesAMD = PhysicalDeviceGpaPropertiesAMD
  { -- | @flags@ is reserved for future use.
    flags :: PhysicalDeviceGpaPropertiesFlagsAMD
  , -- | @maxSqttSeBufferSize@ is the SQTT buffer size per engine.
    maxSqttSeBufferSize :: DeviceSize
  , -- | @shaderEngineCount@ is the number of shader engines.
    shaderEngineCount :: Word32
  , -- | @perfBlockCount@ is the number of entries in @pPerfBlocks@.
    perfBlockCount :: Word32
  , -- | @pPerfBlocks@ is a pointer to an array of 'GpaPerfBlockPropertiesAMD'
    -- structures containing the available performance blocks.
    perfBlocks :: Ptr GpaPerfBlockPropertiesAMD
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGpaPropertiesAMD)
#endif
deriving instance Show PhysicalDeviceGpaPropertiesAMD

instance ToCStruct PhysicalDeviceGpaPropertiesAMD where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGpaPropertiesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceGpaPropertiesFlagsAMD)) (flags)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxSqttSeBufferSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (shaderEngineCount)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (perfBlockCount)
    poke ((p `plusPtr` 40 :: Ptr (Ptr GpaPerfBlockPropertiesAMD))) (perfBlocks)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceGpaPropertiesFlagsAMD)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr GpaPerfBlockPropertiesAMD))) (zero)
    f

instance FromCStruct PhysicalDeviceGpaPropertiesAMD where
  peekCStruct p = do
    flags <- peek @PhysicalDeviceGpaPropertiesFlagsAMD ((p `plusPtr` 16 :: Ptr PhysicalDeviceGpaPropertiesFlagsAMD))
    maxSqttSeBufferSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    shaderEngineCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    perfBlockCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pPerfBlocks <- peek @(Ptr GpaPerfBlockPropertiesAMD) ((p `plusPtr` 40 :: Ptr (Ptr GpaPerfBlockPropertiesAMD)))
    pure $ PhysicalDeviceGpaPropertiesAMD
             flags
             maxSqttSeBufferSize
             shaderEngineCount
             perfBlockCount
             pPerfBlocks

instance Storable PhysicalDeviceGpaPropertiesAMD where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGpaPropertiesAMD where
  zero = PhysicalDeviceGpaPropertiesAMD
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceGpaProperties2AMD - Structure describing additional GPU
-- performance API properties for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceGpaProperties2AMD' structure describe
-- the following:
--
-- = Description
--
-- If the 'PhysicalDeviceGpaProperties2AMD' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGpaProperties2AMD = PhysicalDeviceGpaProperties2AMD
  { -- | @revisionId@ is the GPU product identifier that may be used to classify
    -- its GPA behavior.
    revisionId :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGpaProperties2AMD)
#endif
deriving instance Show PhysicalDeviceGpaProperties2AMD

instance ToCStruct PhysicalDeviceGpaProperties2AMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGpaProperties2AMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_2_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (revisionId)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_2_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceGpaProperties2AMD where
  peekCStruct p = do
    revisionId <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceGpaProperties2AMD
             revisionId

instance Storable PhysicalDeviceGpaProperties2AMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGpaProperties2AMD where
  zero = PhysicalDeviceGpaProperties2AMD
           zero


-- | VkGpaPerfCounterAMD - Structure specifying parameters of a GPA sample
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaPerfBlockAMD', 'GpaSampleBeginInfoAMD'
data GpaPerfCounterAMD = GpaPerfCounterAMD
  { -- | @blockType@ is a 'GpaPerfBlockAMD' value specifying the GPU block type
    -- to sample.
    --
    -- #VUID-VkGpaPerfCounterAMD-blockType-parameter# @blockType@ /must/ be a
    -- valid 'GpaPerfBlockAMD' value
    blockType :: GpaPerfBlockAMD
  , -- | @blockInstance@ is a value specifying which instance of the GPU block to
    -- sample.
    blockInstance :: Word32
  , -- | @eventID@ is a value specifying the hardware-specific identifier of the
    -- performance counter to sample.
    eventID :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaPerfCounterAMD)
#endif
deriving instance Show GpaPerfCounterAMD

instance ToCStruct GpaPerfCounterAMD where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaPerfCounterAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD)) (blockType)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (blockInstance)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (eventID)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct GpaPerfCounterAMD where
  peekCStruct p = do
    blockType <- peek @GpaPerfBlockAMD ((p `plusPtr` 0 :: Ptr GpaPerfBlockAMD))
    blockInstance <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    eventID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ GpaPerfCounterAMD
             blockType blockInstance eventID

instance Storable GpaPerfCounterAMD where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GpaPerfCounterAMD where
  zero = GpaPerfCounterAMD
           zero
           zero
           zero


-- | VkGpaSampleBeginInfoAMD - Structure specifying parameters of a GPA
-- sample
--
-- == Valid Usage
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sampleType-12412# If @sampleType@ is
--     'GPA_SAMPLE_TYPE_CUMULATIVE_AMD', the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-perfCounters ::perfCounters>
--     feature /must/ be enabled
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sampleType-12413# If @sampleType@ is
--     'GPA_SAMPLE_TYPE_TRACE_AMD', at least one of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-streamingPerfCounters ::streamingPerfCounters>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-sqThreadTracing ::sqThreadTracing>
--     features /must/ be enabled
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sqThreadTraceEnable-12414# If
--     @sqThreadTraceEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-sqThreadTracing ::sqThreadTracing>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD'
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sampleType-parameter# @sampleType@
--     /must/ be a valid 'GpaSampleTypeAMD' value
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sqShaderMask-parameter# @sqShaderMask@
--     /must/ be a valid combination of 'GpaSqShaderStageFlagBitsAMD'
--     values
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-sqShaderMask-requiredbitmask#
--     @sqShaderMask@ /must/ not be @0@
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-pPerfCounters-parameter#
--     @pPerfCounters@ /must/ be a valid pointer to an array of
--     @perfCounterCount@ valid 'GpaPerfCounterAMD' structures
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-timingPreSample-parameter#
--     @timingPreSample@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-timingPreSample-requiredbitmask#
--     @timingPreSample@ /must/ not be @0@
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-timingPostSample-parameter#
--     @timingPostSample@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-timingPostSample-requiredbitmask#
--     @timingPostSample@ /must/ not be @0@
--
-- -   #VUID-VkGpaSampleBeginInfoAMD-perfCounterCount-arraylength#
--     @perfCounterCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'GpaPerfCounterAMD',
-- 'GpaSampleTypeAMD', 'GpaSqShaderStageFlagsAMD',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginGpaSampleAMD'
data GpaSampleBeginInfoAMD = GpaSampleBeginInfoAMD
  { -- | @sampleType@ is a 'GpaSampleTypeAMD' value specifying the type of
    -- sample.
    sampleType :: GpaSampleTypeAMD
  , -- | @sampleInternalOperations@ is a boolean indicating whether internal
    -- driver operations are included in the results.
    sampleInternalOperations :: Bool
  , -- | @cacheFlushOnCounterCollection@ is a boolean indicating whether the
    -- driver should insert cache flush and invalidate events before and after
    -- every sample.
    cacheFlushOnCounterCollection :: Bool
  , -- | @sqShaderMaskEnable@ is a boolean indicating whether @sqShaderMask@
    -- specifies the shader stages to sample. If
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', all shader stages are sampled.
    sqShaderMaskEnable :: Bool
  , -- | @sqShaderMask@ is a bitmask of 'GpaSqShaderStageFlagBitsAMD' values
    -- specifying which shader stages to sample. Shader stage bits that are not
    -- relevant to the specific device are ignored.
    sqShaderMask :: GpaSqShaderStageFlagsAMD
  , -- | @pPerfCounters@ is a pointer to an array of 'GpaPerfCounterAMD'
    -- structures specifying the counters to be sampled. If @sampleType@ is
    -- 'GPA_SAMPLE_TYPE_CUMULATIVE_AMD' @pPerfCounters@ specifies the counters
    -- that are sampled at the beginning and at end of the sample period. If
    -- @sampleType@ is 'GPA_SAMPLE_TYPE_TRACE_AMD' then the SPM data will be
    -- added to the samples RGP data blob.
    perfCounters :: Vector GpaPerfCounterAMD
  , -- | @streamingPerfTraceSampleInterval@ is a value specifying the period for
    -- SPM samples in cycles, and is ignored if @sampleType@ is not
    -- 'GPA_SAMPLE_TYPE_TRACE_AMD'.
    streamingPerfTraceSampleInterval :: Word32
  , -- | @perfCounterDeviceMemoryLimit@ is a value specifying the maximum amount
    -- of GPU memory that this sample can allocate for SPM data. If
    -- @sampleType@ is not 'GPA_SAMPLE_TYPE_TRACE_AMD' this value is ignored.
    perfCounterDeviceMemoryLimit :: DeviceSize
  , -- | @sqThreadTraceEnable@ is a boolean specifying whether SQTT data should
    -- be included. If @sampleType@ is not 'GPA_SAMPLE_TYPE_TRACE_AMD' this
    -- value is ignored.
    sqThreadTraceEnable :: Bool
  , -- | @sqThreadTraceSuppressInstructionTokens@ is a boolean specifying whether
    -- instruction-level SQTT tokens should be captured. If
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the amount of sample data is
    -- significantly reduced. If @sampleType@ is not
    -- 'GPA_SAMPLE_TYPE_TRACE_AMD' this value is ignored.
    sqThreadTraceSuppressInstructionTokens :: Bool
  , -- | @sqThreadTraceDeviceMemoryLimit@ is a value specifying the maximum
    -- amount of GPU memory in bytes that this sample can allocate for the SQTT
    -- buffer. If @0@, the maximum size to prevent dropping tokens towards the
    -- end of the sample is allocated. If @sampleType@ is not
    -- 'GPA_SAMPLE_TYPE_TRACE_AMD' this value is ignored.
    sqThreadTraceDeviceMemoryLimit :: DeviceSize
  , -- | @timingPreSample@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages specifying where the begin timestamp should take place.
    -- If @sampleType@ is not 'GPA_SAMPLE_TYPE_TIMING_AMD' this value is
    -- ignored.
    timingPreSample :: PipelineStageFlags
  , -- | @timingPostSample@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages specifying where the end timestamp should take place. If
    -- @sampleType@ is not 'GPA_SAMPLE_TYPE_TIMING_AMD' this value is ignored.
    timingPostSample :: PipelineStageFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaSampleBeginInfoAMD)
#endif
deriving instance Show GpaSampleBeginInfoAMD

instance ToCStruct GpaSampleBeginInfoAMD where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaSampleBeginInfoAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GpaSampleTypeAMD)) (sampleType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sampleInternalOperations))
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (cacheFlushOnCounterCollection))
    lift $ poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (sqShaderMaskEnable))
    lift $ poke ((p `plusPtr` 32 :: Ptr GpaSqShaderStageFlagsAMD)) (sqShaderMask)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (perfCounters)) :: Word32))
    pPPerfCounters' <- ContT $ allocaBytes @GpaPerfCounterAMD ((Data.Vector.length (perfCounters)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPerfCounters' `plusPtr` (12 * (i)) :: Ptr GpaPerfCounterAMD) (e)) (perfCounters)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr GpaPerfCounterAMD))) (pPPerfCounters')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (streamingPerfTraceSampleInterval)
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (perfCounterDeviceMemoryLimit)
    lift $ poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (sqThreadTraceEnable))
    lift $ poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (sqThreadTraceSuppressInstructionTokens))
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (sqThreadTraceDeviceMemoryLimit)
    lift $ poke ((p `plusPtr` 80 :: Ptr PipelineStageFlags)) (timingPreSample)
    lift $ poke ((p `plusPtr` 84 :: Ptr PipelineStageFlags)) (timingPostSample)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GpaSampleTypeAMD)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr GpaSqShaderStageFlagsAMD)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 80 :: Ptr PipelineStageFlags)) (zero)
    poke ((p `plusPtr` 84 :: Ptr PipelineStageFlags)) (zero)
    f

instance FromCStruct GpaSampleBeginInfoAMD where
  peekCStruct p = do
    sampleType <- peek @GpaSampleTypeAMD ((p `plusPtr` 16 :: Ptr GpaSampleTypeAMD))
    sampleInternalOperations <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    cacheFlushOnCounterCollection <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    sqShaderMaskEnable <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    sqShaderMask <- peek @GpaSqShaderStageFlagsAMD ((p `plusPtr` 32 :: Ptr GpaSqShaderStageFlagsAMD))
    perfCounterCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pPerfCounters <- peek @(Ptr GpaPerfCounterAMD) ((p `plusPtr` 40 :: Ptr (Ptr GpaPerfCounterAMD)))
    pPerfCounters' <- generateM (fromIntegral perfCounterCount) (\i -> peekCStruct @GpaPerfCounterAMD ((pPerfCounters `advancePtrBytes` (12 * (i)) :: Ptr GpaPerfCounterAMD)))
    streamingPerfTraceSampleInterval <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    perfCounterDeviceMemoryLimit <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    sqThreadTraceEnable <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    sqThreadTraceSuppressInstructionTokens <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    sqThreadTraceDeviceMemoryLimit <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    timingPreSample <- peek @PipelineStageFlags ((p `plusPtr` 80 :: Ptr PipelineStageFlags))
    timingPostSample <- peek @PipelineStageFlags ((p `plusPtr` 84 :: Ptr PipelineStageFlags))
    pure $ GpaSampleBeginInfoAMD
             sampleType
             (bool32ToBool sampleInternalOperations)
             (bool32ToBool cacheFlushOnCounterCollection)
             (bool32ToBool sqShaderMaskEnable)
             sqShaderMask
             pPerfCounters'
             streamingPerfTraceSampleInterval
             perfCounterDeviceMemoryLimit
             (bool32ToBool sqThreadTraceEnable)
             (bool32ToBool sqThreadTraceSuppressInstructionTokens)
             sqThreadTraceDeviceMemoryLimit
             timingPreSample
             timingPostSample

instance Zero GpaSampleBeginInfoAMD where
  zero = GpaSampleBeginInfoAMD
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGpaDeviceClockModeInfoAMD - Structure containing returned clock ratios
-- or clock mode to set
--
-- = Description
--
-- If @clockMode@ is 'GPA_DEVICE_CLOCK_MODE_QUERY_AMD',
-- @memoryClockRatioToPeak@ and @engineClockRatioToPeak@ are filled with
-- the ratios of their current values to their maximums respectively,
-- otherwise they are left unchanged.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaDeviceClockModeAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setGpaDeviceClockModeAMD'
data GpaDeviceClockModeInfoAMD = GpaDeviceClockModeInfoAMD
  { -- | @clockMode@ is a enum:VkGpaDeviceClockModeAMD value specify which clock
    -- mode to set, or whether to query the current clocks.
    --
    -- #VUID-VkGpaDeviceClockModeInfoAMD-clockMode-parameter# @clockMode@
    -- /must/ be a valid 'GpaDeviceClockModeAMD' value
    clockMode :: GpaDeviceClockModeAMD
  , -- | @memoryClockRatioToPeak@ is the returned ratio of the current memory
    -- clock to the maximum memory clock, if @clockMode@ is
    -- 'GPA_DEVICE_CLOCK_MODE_QUERY_AMD'.
    memoryClockRatioToPeak :: Float
  , -- | @engineClockRatioToPeak@ is the returned ratio of the current engine
    -- clock to the maximum engine clock, if @clockMode@ is
    -- 'GPA_DEVICE_CLOCK_MODE_QUERY_AMD'.
    engineClockRatioToPeak :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaDeviceClockModeInfoAMD)
#endif
deriving instance Show GpaDeviceClockModeInfoAMD

instance ToCStruct GpaDeviceClockModeInfoAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaDeviceClockModeInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_DEVICE_CLOCK_MODE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GpaDeviceClockModeAMD)) (clockMode)
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (memoryClockRatioToPeak))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (engineClockRatioToPeak))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_DEVICE_CLOCK_MODE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GpaDeviceClockModeAMD)) (zero)
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct GpaDeviceClockModeInfoAMD where
  peekCStruct p = do
    clockMode <- peek @GpaDeviceClockModeAMD ((p `plusPtr` 16 :: Ptr GpaDeviceClockModeAMD))
    memoryClockRatioToPeak <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    engineClockRatioToPeak <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    pure $ GpaDeviceClockModeInfoAMD
             clockMode
             (coerce @CFloat @Float memoryClockRatioToPeak)
             (coerce @CFloat @Float engineClockRatioToPeak)

instance Storable GpaDeviceClockModeInfoAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GpaDeviceClockModeInfoAMD where
  zero = GpaDeviceClockModeInfoAMD
           zero
           zero
           zero


-- | VkGpaDeviceGetClockInfoAMD - Structure containing returned clock ratios
-- or clock mode to set
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getGpaDeviceClockInfoAMD'
data GpaDeviceGetClockInfoAMD = GpaDeviceGetClockInfoAMD
  { -- | @memoryClockRatioToPeak@ is the returned ratio of the current memory
    -- clock to the maximum memory clock.
    memoryClockRatioToPeak :: Float
  , -- | @engineClockRatioToPeak@ is the returned ratio of the current engine
    -- clock to the maximum engine clock.
    engineClockRatioToPeak :: Float
  , -- | @memoryClockFrequency@ is the current memory clock frequency in MHz.
    memoryClockFrequency :: Word32
  , -- | @engineClockFrequency@ is the current engine clock frequency in MHz.
    engineClockFrequency :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaDeviceGetClockInfoAMD)
#endif
deriving instance Show GpaDeviceGetClockInfoAMD

instance ToCStruct GpaDeviceGetClockInfoAMD where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaDeviceGetClockInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_DEVICE_GET_CLOCK_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (memoryClockRatioToPeak))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (engineClockRatioToPeak))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryClockFrequency)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (engineClockFrequency)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_DEVICE_GET_CLOCK_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct GpaDeviceGetClockInfoAMD where
  peekCStruct p = do
    memoryClockRatioToPeak <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    engineClockRatioToPeak <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    memoryClockFrequency <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    engineClockFrequency <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ GpaDeviceGetClockInfoAMD
             (coerce @CFloat @Float memoryClockRatioToPeak)
             (coerce @CFloat @Float engineClockRatioToPeak)
             memoryClockFrequency
             engineClockFrequency

instance Storable GpaDeviceGetClockInfoAMD where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GpaDeviceGetClockInfoAMD where
  zero = GpaDeviceGetClockInfoAMD
           zero
           zero
           zero
           zero


-- | VkGpaSessionCreateInfoAMD - Structure specifying parameters of a newly
-- created GPA session
--
-- = Description
--
-- When sampling counters inside secondary command buffers, repeated
-- invocations of the same secondary command buffer causes the previous
-- results to be overwritten. To avoid this a GPA session object’s memory
-- layout /can/ be cloned for use with multiple secondary invocations by
-- specifying its handle in @secondaryCopySource@, allowing the new GPA
-- session object to be the target of a copy using
-- 'cmdCopyGpaSessionResultsAMD'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGpaSessionCreateInfoAMD-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD'
--
-- -   #VUID-VkGpaSessionCreateInfoAMD-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkGpaSessionCreateInfoAMD-secondaryCopySource-parameter# If
--     @secondaryCopySource@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @secondaryCopySource@
--     /must/ be a valid 'Vulkan.Extensions.Handles.GpaSessionAMD' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Extensions.Handles.GpaSessionAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createGpaSessionAMD'
data GpaSessionCreateInfoAMD = GpaSessionCreateInfoAMD
  { -- | @secondaryCopySource@ is a 'Vulkan.Extensions.Handles.GpaSessionAMD'
    -- handle whose results /can/ be copied into this session.
    secondaryCopySource :: GpaSessionAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GpaSessionCreateInfoAMD)
#endif
deriving instance Show GpaSessionCreateInfoAMD

instance ToCStruct GpaSessionCreateInfoAMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GpaSessionCreateInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GpaSessionAMD)) (secondaryCopySource)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct GpaSessionCreateInfoAMD where
  peekCStruct p = do
    secondaryCopySource <- peek @GpaSessionAMD ((p `plusPtr` 16 :: Ptr GpaSessionAMD))
    pure $ GpaSessionCreateInfoAMD
             secondaryCopySource

instance Storable GpaSessionCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GpaSessionCreateInfoAMD where
  zero = GpaSessionCreateInfoAMD
           zero


-- | VkGpaPerfBlockPropertiesFlagsAMD - Reserved for future use
--
-- = Description
--
-- 'GpaPerfBlockPropertiesFlagsAMD' is a bitmask type for setting GPA
-- performance block property flags, but is currently reserved for future
-- use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.FundamentalTypes.Flags', 'GpaPerfBlockPropertiesAMD'
newtype GpaPerfBlockPropertiesFlagsAMD = GpaPerfBlockPropertiesFlagsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameGpaPerfBlockPropertiesFlagsAMD :: String
conNameGpaPerfBlockPropertiesFlagsAMD = "GpaPerfBlockPropertiesFlagsAMD"

enumPrefixGpaPerfBlockPropertiesFlagsAMD :: String
enumPrefixGpaPerfBlockPropertiesFlagsAMD = ""

showTableGpaPerfBlockPropertiesFlagsAMD :: [(GpaPerfBlockPropertiesFlagsAMD, String)]
showTableGpaPerfBlockPropertiesFlagsAMD = []

instance Show GpaPerfBlockPropertiesFlagsAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixGpaPerfBlockPropertiesFlagsAMD
      showTableGpaPerfBlockPropertiesFlagsAMD
      conNameGpaPerfBlockPropertiesFlagsAMD
      (\(GpaPerfBlockPropertiesFlagsAMD x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read GpaPerfBlockPropertiesFlagsAMD where
  readPrec =
    enumReadPrec
      enumPrefixGpaPerfBlockPropertiesFlagsAMD
      showTableGpaPerfBlockPropertiesFlagsAMD
      conNameGpaPerfBlockPropertiesFlagsAMD
      GpaPerfBlockPropertiesFlagsAMD

-- | VkPhysicalDeviceGpaPropertiesFlagsAMD - Reserved for future use
--
-- = Description
--
-- 'PhysicalDeviceGpaPropertiesFlagsAMD' is a bitmask type for setting
-- physical device GPA property flags, but is currently reserved for future
-- use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'Vulkan.Core10.FundamentalTypes.Flags', 'PhysicalDeviceGpaPropertiesAMD'
newtype PhysicalDeviceGpaPropertiesFlagsAMD = PhysicalDeviceGpaPropertiesFlagsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePhysicalDeviceGpaPropertiesFlagsAMD :: String
conNamePhysicalDeviceGpaPropertiesFlagsAMD = "PhysicalDeviceGpaPropertiesFlagsAMD"

enumPrefixPhysicalDeviceGpaPropertiesFlagsAMD :: String
enumPrefixPhysicalDeviceGpaPropertiesFlagsAMD = ""

showTablePhysicalDeviceGpaPropertiesFlagsAMD :: [(PhysicalDeviceGpaPropertiesFlagsAMD, String)]
showTablePhysicalDeviceGpaPropertiesFlagsAMD = []

instance Show PhysicalDeviceGpaPropertiesFlagsAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceGpaPropertiesFlagsAMD
      showTablePhysicalDeviceGpaPropertiesFlagsAMD
      conNamePhysicalDeviceGpaPropertiesFlagsAMD
      (\(PhysicalDeviceGpaPropertiesFlagsAMD x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PhysicalDeviceGpaPropertiesFlagsAMD where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceGpaPropertiesFlagsAMD
      showTablePhysicalDeviceGpaPropertiesFlagsAMD
      conNamePhysicalDeviceGpaPropertiesFlagsAMD
      PhysicalDeviceGpaPropertiesFlagsAMD

type GpaSqShaderStageFlagsAMD = GpaSqShaderStageFlagBitsAMD

-- | VkGpaSqShaderStageFlagBitsAMD - Bitmask specifying GPU shader stage to
-- sample
--
-- = Description
--
-- -   'GPA_SQ_SHADER_STAGE_PS_BIT_AMD' specifies the pixel shader stage.
--
-- -   'GPA_SQ_SHADER_STAGE_VS_BIT_AMD' specifies the vertex shader stage.
--
-- -   'GPA_SQ_SHADER_STAGE_GS_BIT_AMD' specifies the geometry shader
--     stage.
--
-- -   'GPA_SQ_SHADER_STAGE_ES_BIT_AMD' specifies the export shader stage.
--
-- -   'GPA_SQ_SHADER_STAGE_HS_BIT_AMD' specifies the hull shader stage.
--
-- -   'GPA_SQ_SHADER_STAGE_LS_BIT_AMD' specifies the local shader stage.
--
-- -   'GPA_SQ_SHADER_STAGE_CS_BIT_AMD' specifies the compute shader stage.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaSqShaderStageFlagsAMD'
newtype GpaSqShaderStageFlagBitsAMD = GpaSqShaderStageFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_PS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_PS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000001

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_VS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_VS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000002

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_GS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_GS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000004

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_ES_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_ES_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000008

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_HS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_HS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000010

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_LS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_LS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000020

-- No documentation found for Nested "VkGpaSqShaderStageFlagBitsAMD" "VK_GPA_SQ_SHADER_STAGE_CS_BIT_AMD"
pattern GPA_SQ_SHADER_STAGE_CS_BIT_AMD = GpaSqShaderStageFlagBitsAMD 0x00000040

conNameGpaSqShaderStageFlagBitsAMD :: String
conNameGpaSqShaderStageFlagBitsAMD = "GpaSqShaderStageFlagBitsAMD"

enumPrefixGpaSqShaderStageFlagBitsAMD :: String
enumPrefixGpaSqShaderStageFlagBitsAMD = "GPA_SQ_SHADER_STAGE_"

showTableGpaSqShaderStageFlagBitsAMD :: [(GpaSqShaderStageFlagBitsAMD, String)]
showTableGpaSqShaderStageFlagBitsAMD =
  [
    ( GPA_SQ_SHADER_STAGE_PS_BIT_AMD
    , "PS_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_VS_BIT_AMD
    , "VS_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_GS_BIT_AMD
    , "GS_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_ES_BIT_AMD
    , "ES_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_HS_BIT_AMD
    , "HS_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_LS_BIT_AMD
    , "LS_BIT_AMD"
    )
  ,
    ( GPA_SQ_SHADER_STAGE_CS_BIT_AMD
    , "CS_BIT_AMD"
    )
  ]

instance Show GpaSqShaderStageFlagBitsAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixGpaSqShaderStageFlagBitsAMD
      showTableGpaSqShaderStageFlagBitsAMD
      conNameGpaSqShaderStageFlagBitsAMD
      (\(GpaSqShaderStageFlagBitsAMD x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read GpaSqShaderStageFlagBitsAMD where
  readPrec =
    enumReadPrec
      enumPrefixGpaSqShaderStageFlagBitsAMD
      showTableGpaSqShaderStageFlagBitsAMD
      conNameGpaSqShaderStageFlagBitsAMD
      GpaSqShaderStageFlagBitsAMD

-- | VkGpaPerfBlockAMD - Enum providing performance counter types
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaPerfBlockPropertiesAMD', 'GpaPerfCounterAMD'
newtype GpaPerfBlockAMD = GpaPerfBlockAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CPF_AMD"
pattern GPA_PERF_BLOCK_CPF_AMD = GpaPerfBlockAMD 0

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_IA_AMD"
pattern GPA_PERF_BLOCK_IA_AMD = GpaPerfBlockAMD 1

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_VGT_AMD"
pattern GPA_PERF_BLOCK_VGT_AMD = GpaPerfBlockAMD 2

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_PA_AMD"
pattern GPA_PERF_BLOCK_PA_AMD = GpaPerfBlockAMD 3

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SC_AMD"
pattern GPA_PERF_BLOCK_SC_AMD = GpaPerfBlockAMD 4

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SPI_AMD"
pattern GPA_PERF_BLOCK_SPI_AMD = GpaPerfBlockAMD 5

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SQ_AMD"
pattern GPA_PERF_BLOCK_SQ_AMD = GpaPerfBlockAMD 6

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SX_AMD"
pattern GPA_PERF_BLOCK_SX_AMD = GpaPerfBlockAMD 7

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TA_AMD"
pattern GPA_PERF_BLOCK_TA_AMD = GpaPerfBlockAMD 8

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TD_AMD"
pattern GPA_PERF_BLOCK_TD_AMD = GpaPerfBlockAMD 9

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TCP_AMD"
pattern GPA_PERF_BLOCK_TCP_AMD = GpaPerfBlockAMD 10

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TCC_AMD"
pattern GPA_PERF_BLOCK_TCC_AMD = GpaPerfBlockAMD 11

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TCA_AMD"
pattern GPA_PERF_BLOCK_TCA_AMD = GpaPerfBlockAMD 12

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_DB_AMD"
pattern GPA_PERF_BLOCK_DB_AMD = GpaPerfBlockAMD 13

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CB_AMD"
pattern GPA_PERF_BLOCK_CB_AMD = GpaPerfBlockAMD 14

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GDS_AMD"
pattern GPA_PERF_BLOCK_GDS_AMD = GpaPerfBlockAMD 15

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SRBM_AMD"
pattern GPA_PERF_BLOCK_SRBM_AMD = GpaPerfBlockAMD 16

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GRBM_AMD"
pattern GPA_PERF_BLOCK_GRBM_AMD = GpaPerfBlockAMD 17

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GRBM_SE_AMD"
pattern GPA_PERF_BLOCK_GRBM_SE_AMD = GpaPerfBlockAMD 18

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_RLC_AMD"
pattern GPA_PERF_BLOCK_RLC_AMD = GpaPerfBlockAMD 19

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_DMA_AMD"
pattern GPA_PERF_BLOCK_DMA_AMD = GpaPerfBlockAMD 20

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_MC_AMD"
pattern GPA_PERF_BLOCK_MC_AMD = GpaPerfBlockAMD 21

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CPG_AMD"
pattern GPA_PERF_BLOCK_CPG_AMD = GpaPerfBlockAMD 22

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CPC_AMD"
pattern GPA_PERF_BLOCK_CPC_AMD = GpaPerfBlockAMD 23

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_WD_AMD"
pattern GPA_PERF_BLOCK_WD_AMD = GpaPerfBlockAMD 24

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_TCS_AMD"
pattern GPA_PERF_BLOCK_TCS_AMD = GpaPerfBlockAMD 25

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_ATC_AMD"
pattern GPA_PERF_BLOCK_ATC_AMD = GpaPerfBlockAMD 26

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_ATC_L2_AMD"
pattern GPA_PERF_BLOCK_ATC_L2_AMD = GpaPerfBlockAMD 27

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_MC_VM_L2_AMD"
pattern GPA_PERF_BLOCK_MC_VM_L2_AMD = GpaPerfBlockAMD 28

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_EA_AMD"
pattern GPA_PERF_BLOCK_EA_AMD = GpaPerfBlockAMD 29

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_RPB_AMD"
pattern GPA_PERF_BLOCK_RPB_AMD = GpaPerfBlockAMD 30

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_RMI_AMD"
pattern GPA_PERF_BLOCK_RMI_AMD = GpaPerfBlockAMD 31

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_UMCCH_AMD"
pattern GPA_PERF_BLOCK_UMCCH_AMD = GpaPerfBlockAMD 32

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GE_AMD"
pattern GPA_PERF_BLOCK_GE_AMD = GpaPerfBlockAMD 33

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL1A_AMD"
pattern GPA_PERF_BLOCK_GL1A_AMD = GpaPerfBlockAMD 34

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL1C_AMD"
pattern GPA_PERF_BLOCK_GL1C_AMD = GpaPerfBlockAMD 35

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL1CG_AMD"
pattern GPA_PERF_BLOCK_GL1CG_AMD = GpaPerfBlockAMD 36

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL2A_AMD"
pattern GPA_PERF_BLOCK_GL2A_AMD = GpaPerfBlockAMD 37

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL2C_AMD"
pattern GPA_PERF_BLOCK_GL2C_AMD = GpaPerfBlockAMD 38

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CHA_AMD"
pattern GPA_PERF_BLOCK_CHA_AMD = GpaPerfBlockAMD 39

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CHC_AMD"
pattern GPA_PERF_BLOCK_CHC_AMD = GpaPerfBlockAMD 40

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_CHCG_AMD"
pattern GPA_PERF_BLOCK_CHCG_AMD = GpaPerfBlockAMD 41

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GUS_AMD"
pattern GPA_PERF_BLOCK_GUS_AMD = GpaPerfBlockAMD 42

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GCR_AMD"
pattern GPA_PERF_BLOCK_GCR_AMD = GpaPerfBlockAMD 43

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_PH_AMD"
pattern GPA_PERF_BLOCK_PH_AMD = GpaPerfBlockAMD 44

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_UTCL1_AMD"
pattern GPA_PERF_BLOCK_UTCL1_AMD = GpaPerfBlockAMD 45

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GE_DIST_AMD"
pattern GPA_PERF_BLOCK_GE_DIST_AMD = GpaPerfBlockAMD 46

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GE_SE_AMD"
pattern GPA_PERF_BLOCK_GE_SE_AMD = GpaPerfBlockAMD 47

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_DF_MALL_AMD"
pattern GPA_PERF_BLOCK_DF_MALL_AMD = GpaPerfBlockAMD 48

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_SQ_WGP_AMD"
pattern GPA_PERF_BLOCK_SQ_WGP_AMD = GpaPerfBlockAMD 49

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_PC_AMD"
pattern GPA_PERF_BLOCK_PC_AMD = GpaPerfBlockAMD 50

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL1XA_AMD"
pattern GPA_PERF_BLOCK_GL1XA_AMD = GpaPerfBlockAMD 51

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_GL1XC_AMD"
pattern GPA_PERF_BLOCK_GL1XC_AMD = GpaPerfBlockAMD 52

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_WGS_AMD"
pattern GPA_PERF_BLOCK_WGS_AMD = GpaPerfBlockAMD 53

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_EACPWD_AMD"
pattern GPA_PERF_BLOCK_EACPWD_AMD = GpaPerfBlockAMD 54

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_EASE_AMD"
pattern GPA_PERF_BLOCK_EASE_AMD = GpaPerfBlockAMD 55

-- No documentation found for Nested "VkGpaPerfBlockAMD" "VK_GPA_PERF_BLOCK_RLCUSER_AMD"
pattern GPA_PERF_BLOCK_RLCUSER_AMD = GpaPerfBlockAMD 56

{-# COMPLETE
  GPA_PERF_BLOCK_CPF_AMD
  , GPA_PERF_BLOCK_IA_AMD
  , GPA_PERF_BLOCK_VGT_AMD
  , GPA_PERF_BLOCK_PA_AMD
  , GPA_PERF_BLOCK_SC_AMD
  , GPA_PERF_BLOCK_SPI_AMD
  , GPA_PERF_BLOCK_SQ_AMD
  , GPA_PERF_BLOCK_SX_AMD
  , GPA_PERF_BLOCK_TA_AMD
  , GPA_PERF_BLOCK_TD_AMD
  , GPA_PERF_BLOCK_TCP_AMD
  , GPA_PERF_BLOCK_TCC_AMD
  , GPA_PERF_BLOCK_TCA_AMD
  , GPA_PERF_BLOCK_DB_AMD
  , GPA_PERF_BLOCK_CB_AMD
  , GPA_PERF_BLOCK_GDS_AMD
  , GPA_PERF_BLOCK_SRBM_AMD
  , GPA_PERF_BLOCK_GRBM_AMD
  , GPA_PERF_BLOCK_GRBM_SE_AMD
  , GPA_PERF_BLOCK_RLC_AMD
  , GPA_PERF_BLOCK_DMA_AMD
  , GPA_PERF_BLOCK_MC_AMD
  , GPA_PERF_BLOCK_CPG_AMD
  , GPA_PERF_BLOCK_CPC_AMD
  , GPA_PERF_BLOCK_WD_AMD
  , GPA_PERF_BLOCK_TCS_AMD
  , GPA_PERF_BLOCK_ATC_AMD
  , GPA_PERF_BLOCK_ATC_L2_AMD
  , GPA_PERF_BLOCK_MC_VM_L2_AMD
  , GPA_PERF_BLOCK_EA_AMD
  , GPA_PERF_BLOCK_RPB_AMD
  , GPA_PERF_BLOCK_RMI_AMD
  , GPA_PERF_BLOCK_UMCCH_AMD
  , GPA_PERF_BLOCK_GE_AMD
  , GPA_PERF_BLOCK_GL1A_AMD
  , GPA_PERF_BLOCK_GL1C_AMD
  , GPA_PERF_BLOCK_GL1CG_AMD
  , GPA_PERF_BLOCK_GL2A_AMD
  , GPA_PERF_BLOCK_GL2C_AMD
  , GPA_PERF_BLOCK_CHA_AMD
  , GPA_PERF_BLOCK_CHC_AMD
  , GPA_PERF_BLOCK_CHCG_AMD
  , GPA_PERF_BLOCK_GUS_AMD
  , GPA_PERF_BLOCK_GCR_AMD
  , GPA_PERF_BLOCK_PH_AMD
  , GPA_PERF_BLOCK_UTCL1_AMD
  , GPA_PERF_BLOCK_GE_DIST_AMD
  , GPA_PERF_BLOCK_GE_SE_AMD
  , GPA_PERF_BLOCK_DF_MALL_AMD
  , GPA_PERF_BLOCK_SQ_WGP_AMD
  , GPA_PERF_BLOCK_PC_AMD
  , GPA_PERF_BLOCK_GL1XA_AMD
  , GPA_PERF_BLOCK_GL1XC_AMD
  , GPA_PERF_BLOCK_WGS_AMD
  , GPA_PERF_BLOCK_EACPWD_AMD
  , GPA_PERF_BLOCK_EASE_AMD
  , GPA_PERF_BLOCK_RLCUSER_AMD ::
    GpaPerfBlockAMD
  #-}

conNameGpaPerfBlockAMD :: String
conNameGpaPerfBlockAMD = "GpaPerfBlockAMD"

enumPrefixGpaPerfBlockAMD :: String
enumPrefixGpaPerfBlockAMD = "GPA_PERF_BLOCK_"

showTableGpaPerfBlockAMD :: [(GpaPerfBlockAMD, String)]
showTableGpaPerfBlockAMD =
  [ (GPA_PERF_BLOCK_CPF_AMD, "CPF_AMD")
  , (GPA_PERF_BLOCK_IA_AMD, "IA_AMD")
  , (GPA_PERF_BLOCK_VGT_AMD, "VGT_AMD")
  , (GPA_PERF_BLOCK_PA_AMD, "PA_AMD")
  , (GPA_PERF_BLOCK_SC_AMD, "SC_AMD")
  , (GPA_PERF_BLOCK_SPI_AMD, "SPI_AMD")
  , (GPA_PERF_BLOCK_SQ_AMD, "SQ_AMD")
  , (GPA_PERF_BLOCK_SX_AMD, "SX_AMD")
  , (GPA_PERF_BLOCK_TA_AMD, "TA_AMD")
  , (GPA_PERF_BLOCK_TD_AMD, "TD_AMD")
  , (GPA_PERF_BLOCK_TCP_AMD, "TCP_AMD")
  , (GPA_PERF_BLOCK_TCC_AMD, "TCC_AMD")
  , (GPA_PERF_BLOCK_TCA_AMD, "TCA_AMD")
  , (GPA_PERF_BLOCK_DB_AMD, "DB_AMD")
  , (GPA_PERF_BLOCK_CB_AMD, "CB_AMD")
  , (GPA_PERF_BLOCK_GDS_AMD, "GDS_AMD")
  , (GPA_PERF_BLOCK_SRBM_AMD, "SRBM_AMD")
  , (GPA_PERF_BLOCK_GRBM_AMD, "GRBM_AMD")
  , (GPA_PERF_BLOCK_GRBM_SE_AMD, "GRBM_SE_AMD")
  , (GPA_PERF_BLOCK_RLC_AMD, "RLC_AMD")
  , (GPA_PERF_BLOCK_DMA_AMD, "DMA_AMD")
  , (GPA_PERF_BLOCK_MC_AMD, "MC_AMD")
  , (GPA_PERF_BLOCK_CPG_AMD, "CPG_AMD")
  , (GPA_PERF_BLOCK_CPC_AMD, "CPC_AMD")
  , (GPA_PERF_BLOCK_WD_AMD, "WD_AMD")
  , (GPA_PERF_BLOCK_TCS_AMD, "TCS_AMD")
  , (GPA_PERF_BLOCK_ATC_AMD, "ATC_AMD")
  , (GPA_PERF_BLOCK_ATC_L2_AMD, "ATC_L2_AMD")
  , (GPA_PERF_BLOCK_MC_VM_L2_AMD, "MC_VM_L2_AMD")
  , (GPA_PERF_BLOCK_EA_AMD, "EA_AMD")
  , (GPA_PERF_BLOCK_RPB_AMD, "RPB_AMD")
  , (GPA_PERF_BLOCK_RMI_AMD, "RMI_AMD")
  , (GPA_PERF_BLOCK_UMCCH_AMD, "UMCCH_AMD")
  , (GPA_PERF_BLOCK_GE_AMD, "GE_AMD")
  , (GPA_PERF_BLOCK_GL1A_AMD, "GL1A_AMD")
  , (GPA_PERF_BLOCK_GL1C_AMD, "GL1C_AMD")
  , (GPA_PERF_BLOCK_GL1CG_AMD, "GL1CG_AMD")
  , (GPA_PERF_BLOCK_GL2A_AMD, "GL2A_AMD")
  , (GPA_PERF_BLOCK_GL2C_AMD, "GL2C_AMD")
  , (GPA_PERF_BLOCK_CHA_AMD, "CHA_AMD")
  , (GPA_PERF_BLOCK_CHC_AMD, "CHC_AMD")
  , (GPA_PERF_BLOCK_CHCG_AMD, "CHCG_AMD")
  , (GPA_PERF_BLOCK_GUS_AMD, "GUS_AMD")
  , (GPA_PERF_BLOCK_GCR_AMD, "GCR_AMD")
  , (GPA_PERF_BLOCK_PH_AMD, "PH_AMD")
  , (GPA_PERF_BLOCK_UTCL1_AMD, "UTCL1_AMD")
  , (GPA_PERF_BLOCK_GE_DIST_AMD, "GE_DIST_AMD")
  , (GPA_PERF_BLOCK_GE_SE_AMD, "GE_SE_AMD")
  , (GPA_PERF_BLOCK_DF_MALL_AMD, "DF_MALL_AMD")
  , (GPA_PERF_BLOCK_SQ_WGP_AMD, "SQ_WGP_AMD")
  , (GPA_PERF_BLOCK_PC_AMD, "PC_AMD")
  , (GPA_PERF_BLOCK_GL1XA_AMD, "GL1XA_AMD")
  , (GPA_PERF_BLOCK_GL1XC_AMD, "GL1XC_AMD")
  , (GPA_PERF_BLOCK_WGS_AMD, "WGS_AMD")
  , (GPA_PERF_BLOCK_EACPWD_AMD, "EACPWD_AMD")
  , (GPA_PERF_BLOCK_EASE_AMD, "EASE_AMD")
  , (GPA_PERF_BLOCK_RLCUSER_AMD, "RLCUSER_AMD")
  ]

instance Show GpaPerfBlockAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixGpaPerfBlockAMD
      showTableGpaPerfBlockAMD
      conNameGpaPerfBlockAMD
      (\(GpaPerfBlockAMD x) -> x)
      (showsPrec 11)

instance Read GpaPerfBlockAMD where
  readPrec =
    enumReadPrec
      enumPrefixGpaPerfBlockAMD
      showTableGpaPerfBlockAMD
      conNameGpaPerfBlockAMD
      GpaPerfBlockAMD

-- | VkGpaSampleTypeAMD - Enum providing the sample type
--
-- = Description
--
-- -   'GPA_SAMPLE_TYPE_CUMULATIVE_AMD' specifies that one 64-bit result
--     will be returned per global performance counter, representing the
--     cumulative delta for that counter over the sample period. Cumulative
--     samples /must/ begin and end in the same command buffer.
--
-- -   'GPA_SAMPLE_TYPE_TRACE_AMD' specifies that a buffer will be filled
--     with SQTT results data in RGP file format, and\/or streaming
--     performance monitor data. Trace samples /may/ span multiple command
--     buffers.
--
-- -   'GPA_SAMPLE_TYPE_TIMING_AMD' specifies that two 64-bit results will
--     be recorded to gather timestamp data.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaSampleBeginInfoAMD'
newtype GpaSampleTypeAMD = GpaSampleTypeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkGpaSampleTypeAMD" "VK_GPA_SAMPLE_TYPE_CUMULATIVE_AMD"
pattern GPA_SAMPLE_TYPE_CUMULATIVE_AMD = GpaSampleTypeAMD 0

-- No documentation found for Nested "VkGpaSampleTypeAMD" "VK_GPA_SAMPLE_TYPE_TRACE_AMD"
pattern GPA_SAMPLE_TYPE_TRACE_AMD = GpaSampleTypeAMD 1

-- No documentation found for Nested "VkGpaSampleTypeAMD" "VK_GPA_SAMPLE_TYPE_TIMING_AMD"
pattern GPA_SAMPLE_TYPE_TIMING_AMD = GpaSampleTypeAMD 2

{-# COMPLETE
  GPA_SAMPLE_TYPE_CUMULATIVE_AMD
  , GPA_SAMPLE_TYPE_TRACE_AMD
  , GPA_SAMPLE_TYPE_TIMING_AMD ::
    GpaSampleTypeAMD
  #-}

conNameGpaSampleTypeAMD :: String
conNameGpaSampleTypeAMD = "GpaSampleTypeAMD"

enumPrefixGpaSampleTypeAMD :: String
enumPrefixGpaSampleTypeAMD = "GPA_SAMPLE_TYPE_"

showTableGpaSampleTypeAMD :: [(GpaSampleTypeAMD, String)]
showTableGpaSampleTypeAMD =
  [ (GPA_SAMPLE_TYPE_CUMULATIVE_AMD, "CUMULATIVE_AMD")
  , (GPA_SAMPLE_TYPE_TRACE_AMD, "TRACE_AMD")
  , (GPA_SAMPLE_TYPE_TIMING_AMD, "TIMING_AMD")
  ]

instance Show GpaSampleTypeAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixGpaSampleTypeAMD
      showTableGpaSampleTypeAMD
      conNameGpaSampleTypeAMD
      (\(GpaSampleTypeAMD x) -> x)
      (showsPrec 11)

instance Read GpaSampleTypeAMD where
  readPrec =
    enumReadPrec
      enumPrefixGpaSampleTypeAMD
      showTableGpaSampleTypeAMD
      conNameGpaSampleTypeAMD
      GpaSampleTypeAMD

-- | VkGpaDeviceClockModeAMD - Enum providing the clock mode or query
--
-- = Description
--
-- -   'GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD' specifies that device clocks and
--     other power settings are restored to their default values.
--
-- -   'GPA_DEVICE_CLOCK_MODE_QUERY_AMD' specifies that the current clock
--     values should be queried, with no new values set.
--
-- -   'GPA_DEVICE_CLOCK_MODE_PROFILING_AMD' specifies that clocks are set
--     to a constant amount which is known to be power and thermal
--     sustainable. The engine\/memory clock ratio will be kept the same as
--     much as possible.
--
-- -   'GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD' specifies that the memory
--     clock is set to the lowest available level and the engine clock is
--     set to a thermal and power sustainable level.
--
-- -   'GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD' specifies that the engine
--     clock is set to the lowest available level and the memory clock is
--     set to a thermal and power sustainable level.
--
-- -   'GPA_DEVICE_CLOCK_MODE_PEAK_AMD' specifies that the clocks set to
--     maximum when possible and fans set to maximum. Under power and
--     thermal constraints device will clock down.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpa_interface VK_AMD_gpa_interface>,
-- 'GpaDeviceClockModeInfoAMD'
newtype GpaDeviceClockModeAMD = GpaDeviceClockModeAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD"
pattern GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD = GpaDeviceClockModeAMD 0

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_QUERY_AMD"
pattern GPA_DEVICE_CLOCK_MODE_QUERY_AMD = GpaDeviceClockModeAMD 1

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_PROFILING_AMD"
pattern GPA_DEVICE_CLOCK_MODE_PROFILING_AMD = GpaDeviceClockModeAMD 2

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD"
pattern GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD = GpaDeviceClockModeAMD 3

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD"
pattern GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD = GpaDeviceClockModeAMD 4

-- No documentation found for Nested "VkGpaDeviceClockModeAMD" "VK_GPA_DEVICE_CLOCK_MODE_PEAK_AMD"
pattern GPA_DEVICE_CLOCK_MODE_PEAK_AMD = GpaDeviceClockModeAMD 5

{-# COMPLETE
  GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD
  , GPA_DEVICE_CLOCK_MODE_QUERY_AMD
  , GPA_DEVICE_CLOCK_MODE_PROFILING_AMD
  , GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD
  , GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD
  , GPA_DEVICE_CLOCK_MODE_PEAK_AMD ::
    GpaDeviceClockModeAMD
  #-}

conNameGpaDeviceClockModeAMD :: String
conNameGpaDeviceClockModeAMD = "GpaDeviceClockModeAMD"

enumPrefixGpaDeviceClockModeAMD :: String
enumPrefixGpaDeviceClockModeAMD = "GPA_DEVICE_CLOCK_MODE_"

showTableGpaDeviceClockModeAMD :: [(GpaDeviceClockModeAMD, String)]
showTableGpaDeviceClockModeAMD =
  [
    ( GPA_DEVICE_CLOCK_MODE_DEFAULT_AMD
    , "DEFAULT_AMD"
    )
  ,
    ( GPA_DEVICE_CLOCK_MODE_QUERY_AMD
    , "QUERY_AMD"
    )
  ,
    ( GPA_DEVICE_CLOCK_MODE_PROFILING_AMD
    , "PROFILING_AMD"
    )
  ,
    ( GPA_DEVICE_CLOCK_MODE_MIN_MEMORY_AMD
    , "MIN_MEMORY_AMD"
    )
  ,
    ( GPA_DEVICE_CLOCK_MODE_MIN_ENGINE_AMD
    , "MIN_ENGINE_AMD"
    )
  ,
    ( GPA_DEVICE_CLOCK_MODE_PEAK_AMD
    , "PEAK_AMD"
    )
  ]

instance Show GpaDeviceClockModeAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixGpaDeviceClockModeAMD
      showTableGpaDeviceClockModeAMD
      conNameGpaDeviceClockModeAMD
      (\(GpaDeviceClockModeAMD x) -> x)
      (showsPrec 11)

instance Read GpaDeviceClockModeAMD where
  readPrec =
    enumReadPrec
      enumPrefixGpaDeviceClockModeAMD
      showTableGpaDeviceClockModeAMD
      conNameGpaDeviceClockModeAMD
      GpaDeviceClockModeAMD

type AMD_GPA_INTERFACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_GPA_INTERFACE_SPEC_VERSION"
pattern AMD_GPA_INTERFACE_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_GPA_INTERFACE_SPEC_VERSION = 1


type AMD_GPA_INTERFACE_EXTENSION_NAME = "VK_AMD_gpa_interface"

-- No documentation found for TopLevel "VK_AMD_GPA_INTERFACE_EXTENSION_NAME"
pattern AMD_GPA_INTERFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_GPA_INTERFACE_EXTENSION_NAME = "VK_AMD_gpa_interface"

