{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_instrumentation - device extension
--
-- = VK_ARM_shader_instrumentation
--
-- [__Name String__]
--     @VK_ARM_shader_instrumentation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     608
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
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_shader_object
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_instrumentation] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_shader_instrumentation extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_shader_instrumentation.adoc VK_ARM_shader_instrumentation>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Embla Flatlandsmo, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Mikel Garai, Arm Ltd.
--
--     -   Peter Harris, Arm Ltd.
--
--     -   Ting Wei, Arm Ltd.
--
--     -   Torbjörn Nilsson, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to instrument shaders and capture
-- performance metrics per shader type from commands executed by a queue.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
--
-- == New Commands
--
-- -   'clearShaderInstrumentationMetricsARM'
--
-- -   'cmdBeginShaderInstrumentationARM'
--
-- -   'cmdEndShaderInstrumentationARM'
--
-- -   'createShaderInstrumentationARM'
--
-- -   'destroyShaderInstrumentationARM'
--
-- -   'enumeratePhysicalDeviceShaderInstrumentationMetricsARM'
--
-- -   'getShaderInstrumentationValuesARM'
--
-- == New Structures
--
-- -   'ShaderInstrumentationCreateInfoARM'
--
-- -   'ShaderInstrumentationMetricDataHeaderARM'
--
-- -   'ShaderInstrumentationMetricDescriptionARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderInstrumentationFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderInstrumentationPropertiesARM'
--
-- == New Bitmasks
--
-- -   'ShaderInstrumentationValuesFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME'
--
-- -   'ARM_SHADER_INSTRUMENTATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_SHADER_INSTRUMENTATION_ARM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INSTRUMENT_SHADERS_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_INSTRUMENTATION_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_INSTRUMENTATION_METRIC_DESCRIPTION_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INSTRUMENT_SHADER_BIT_ARM'
--
-- == Version History
--
-- -   Revision 1, 2026-02-26 (Embla Flatlandsmo, Jan-Harald Fredriksen)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_shader_instrumentation Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_instrumentation  ( enumeratePhysicalDeviceShaderInstrumentationMetricsARM
                                                        , createShaderInstrumentationARM
                                                        , withShaderInstrumentationARM
                                                        , destroyShaderInstrumentationARM
                                                        , cmdBeginShaderInstrumentationARM
                                                        , cmdEndShaderInstrumentationARM
                                                        , getShaderInstrumentationValuesARM
                                                        , clearShaderInstrumentationMetricsARM
                                                        , PhysicalDeviceShaderInstrumentationFeaturesARM(..)
                                                        , PhysicalDeviceShaderInstrumentationPropertiesARM(..)
                                                        , ShaderInstrumentationCreateInfoARM(..)
                                                        , ShaderInstrumentationMetricDescriptionARM(..)
                                                        , ShaderInstrumentationMetricDataHeaderARM(..)
                                                        , ShaderInstrumentationValuesFlagsARM(..)
                                                        , ARM_SHADER_INSTRUMENTATION_SPEC_VERSION
                                                        , pattern ARM_SHADER_INSTRUMENTATION_SPEC_VERSION
                                                        , ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME
                                                        , pattern ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME
                                                        , ShaderInstrumentationARM(..)
                                                        , ShaderCreateFlagBitsEXT(..)
                                                        , ShaderCreateFlagsEXT
                                                        ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
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
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkClearShaderInstrumentationMetricsARM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginShaderInstrumentationARM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndShaderInstrumentationARM))
import Vulkan.Dynamic (DeviceCmds(pVkCreateShaderInstrumentationARM))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyShaderInstrumentationARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetShaderInstrumentationValuesARM))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (ShaderInstrumentationARM)
import Vulkan.Extensions.Handles (ShaderInstrumentationARM(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_INSTRUMENTATION_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_INSTRUMENTATION_METRIC_DESCRIPTION_ARM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
import Vulkan.Extensions.Handles (ShaderInstrumentationARM(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr ShaderInstrumentationMetricDescriptionARM -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr ShaderInstrumentationMetricDescriptionARM -> IO Result

-- | vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM - Returns
-- properties describing what shader instrumentation metrics are supported
--
-- = Description
--
-- If @pDescriptions@ is @NULL@, then the number of shader instrumentation
-- metrics available is returned in @pDescriptionCount@. Otherwise,
-- @pDescriptionCount@ /must/ point to a variable set by the application to
-- the number of elements in the @pDescriptions@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pDescriptions@. If @pDescriptionCount@ is less than the number
-- shader instrumentation metrics available, at most @pDescriptionCount@
-- structures will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available shader instrumentation metrics were
-- returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM-pDescriptionCount-parameter#
--     @pDescriptionCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM-pDescriptions-parameter#
--     If the value referenced by @pDescriptionCount@ is not @0@, and
--     @pDescriptions@ is not @NULL@, @pDescriptions@ /must/ be a valid
--     pointer to an array of @pDescriptionCount@
--     'ShaderInstrumentationMetricDescriptionARM' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'ShaderInstrumentationMetricDescriptionARM'
enumeratePhysicalDeviceShaderInstrumentationMetricsARM :: forall io
                                                        . (MonadIO io)
                                                       => -- | @physicalDevice@ is the physical device.
                                                          PhysicalDevice
                                                       -> io (Result, ("descriptions" ::: Vector ShaderInstrumentationMetricDescriptionARM))
enumeratePhysicalDeviceShaderInstrumentationMetricsARM physicalDevice = liftIO . evalContT $ do
  let vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARMPtr = pVkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM is null" Nothing Nothing
  let vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM' = mkVkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARMPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPDescriptionCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM" (vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM'
                                                                                             physicalDevice'
                                                                                             (pPDescriptionCount)
                                                                                             (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptionCount <- lift $ peek @Word32 pPDescriptionCount
  pPDescriptions <- ContT $ bracket (callocBytes @ShaderInstrumentationMetricDescriptionARM ((fromIntegral (pDescriptionCount)) * 528)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPDescriptions `advancePtrBytes` (i * 528) :: Ptr ShaderInstrumentationMetricDescriptionARM) . ($ ())) [0..(fromIntegral (pDescriptionCount)) - 1]
  r' <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM" (vkEnumeratePhysicalDeviceShaderInstrumentationMetricsARM'
                                                                                              physicalDevice'
                                                                                              (pPDescriptionCount)
                                                                                              ((pPDescriptions)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDescriptionCount' <- lift $ peek @Word32 pPDescriptionCount
  pDescriptions' <- lift $ generateM (fromIntegral (pDescriptionCount')) (\i -> peekCStruct @ShaderInstrumentationMetricDescriptionARM (((pPDescriptions) `advancePtrBytes` (528 * (i)) :: Ptr ShaderInstrumentationMetricDescriptionARM)))
  pure $ ((r'), pDescriptions')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateShaderInstrumentationARM
  :: FunPtr (Ptr Device_T -> Ptr ShaderInstrumentationCreateInfoARM -> Ptr AllocationCallbacks -> Ptr ShaderInstrumentationARM -> IO Result) -> Ptr Device_T -> Ptr ShaderInstrumentationCreateInfoARM -> Ptr AllocationCallbacks -> Ptr ShaderInstrumentationARM -> IO Result

-- | vkCreateShaderInstrumentationARM - Create a new shader instrumentation
-- object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateShaderInstrumentationARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateShaderInstrumentationARM-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ShaderInstrumentationCreateInfoARM' structure
--
-- -   #VUID-vkCreateShaderInstrumentationARM-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateShaderInstrumentationARM-pInstrumentation-parameter#
--     @pInstrumentation@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.ShaderInstrumentationARM' handle
--
-- -   #VUID-vkCreateShaderInstrumentationARM-device-queuecount# The device
--     /must/ have been created with at least @1@ queue
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM',
-- 'ShaderInstrumentationCreateInfoARM'
createShaderInstrumentationARM :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the shader instrumentation
                                  -- object.
                                  Device
                               -> -- | @pCreateInfo@ is a pointer to a 'ShaderInstrumentationCreateInfoARM'
                                  -- structure containing information about how the shader instrumentation
                                  -- object is to be created.
                                  ShaderInstrumentationCreateInfoARM
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (ShaderInstrumentationARM)
createShaderInstrumentationARM device
                                 createInfo
                                 allocator = liftIO . evalContT $ do
  let vkCreateShaderInstrumentationARMPtr = pVkCreateShaderInstrumentationARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateShaderInstrumentationARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateShaderInstrumentationARM is null" Nothing Nothing
  let vkCreateShaderInstrumentationARM' = mkVkCreateShaderInstrumentationARM vkCreateShaderInstrumentationARMPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPInstrumentation <- ContT $ bracket (callocBytes @ShaderInstrumentationARM 8) free
  r <- lift $ traceAroundEvent "vkCreateShaderInstrumentationARM" (vkCreateShaderInstrumentationARM'
                                                                     (deviceHandle (device))
                                                                     pCreateInfo
                                                                     pAllocator
                                                                     (pPInstrumentation))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInstrumentation <- lift $ peek @ShaderInstrumentationARM pPInstrumentation
  pure $ (pInstrumentation)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createShaderInstrumentationARM' and 'destroyShaderInstrumentationARM'
--
-- To ensure that 'destroyShaderInstrumentationARM' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withShaderInstrumentationARM :: forall io r . MonadIO io => Device -> ShaderInstrumentationCreateInfoARM -> Maybe AllocationCallbacks -> (io ShaderInstrumentationARM -> (ShaderInstrumentationARM -> io ()) -> r) -> r
withShaderInstrumentationARM device pCreateInfo pAllocator b =
  b (createShaderInstrumentationARM device pCreateInfo pAllocator)
    (\(o0) -> destroyShaderInstrumentationARM device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyShaderInstrumentationARM
  :: FunPtr (Ptr Device_T -> ShaderInstrumentationARM -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ShaderInstrumentationARM -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyShaderInstrumentationARM - Destroy a shader instrumentation
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyShaderInstrumentationARM-instrumentation-12374# All
--     submitted commands that refer to @instrumentation@ /must/ have
--     completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyShaderInstrumentationARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyShaderInstrumentationARM-instrumentation-parameter#
--     If @instrumentation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @instrumentation@ /must/
--     be a valid 'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
--     handle
--
-- -   #VUID-vkDestroyShaderInstrumentationARM-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyShaderInstrumentationARM-instrumentation-parent# If
--     @instrumentation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @instrumentation@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
destroyShaderInstrumentationARM :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the logical device that destroys the shader instrumentation.
                                   Device
                                -> -- | @instrumentation@ is the handle of the shader instrumentation to
                                   -- destroy.
                                   ShaderInstrumentationARM
                                -> -- | @pAllocator@ controls host memory allocation as described in the
                                   -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyShaderInstrumentationARM device
                                  instrumentation
                                  allocator = liftIO . evalContT $ do
  let vkDestroyShaderInstrumentationARMPtr = pVkDestroyShaderInstrumentationARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyShaderInstrumentationARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyShaderInstrumentationARM is null" Nothing Nothing
  let vkDestroyShaderInstrumentationARM' = mkVkDestroyShaderInstrumentationARM vkDestroyShaderInstrumentationARMPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyShaderInstrumentationARM" (vkDestroyShaderInstrumentationARM'
                                                                 (deviceHandle (device))
                                                                 (instrumentation)
                                                                 pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginShaderInstrumentationARM
  :: FunPtr (Ptr CommandBuffer_T -> ShaderInstrumentationARM -> IO ()) -> Ptr CommandBuffer_T -> ShaderInstrumentationARM -> IO ()

-- | vkCmdBeginShaderInstrumentationARM - Begin shader instrumentation
--
-- = Description
--
-- After beginning shader instrumentation, shader instrumentation is
-- considered /active/ within the command buffer it was called in until
-- shader instrumentation is ended.
--
-- The shader instrumentation object has an implicit result index where the
-- per-shader metrics will be written. The result index is set to 0 when
-- the object is created by calling 'createShaderInstrumentationARM', and
-- incremented by @1@ for each draw, dispatch, and ray tracing command
-- recorded while the shader instrumentation object is active.
--
-- The result index is also incremented by @1@ when
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdExecuteGeneratedCommandsEXT'
-- is recorded.
--
-- While shader instrumentation is active, instrumented shaders write to
-- the instrumentation object. These writes /must/ be synchronized using
-- the instrumented shader’s stage with access mask
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT'. If no
-- instrumentation object is bound, writes are discarded.
--
-- If a command buffer is submitted multiple times, the shader instrumented
-- metrics for all submissions will be aggregated in the instrumentation
-- object, unless the metrics are
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-instrumentation-clear cleared>
-- between submissions.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commandBuffer-12375# This
--     command /must/ not be recorded while shader instrumentation is
--     active within @commandBuffer@
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commandBuffer-12376#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-instrumentation-parameter#
--     @instrumentation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.ShaderInstrumentationARM' handle
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdBeginShaderInstrumentationARM-commonparent# Both of
--     @commandBuffer@, and @instrumentation@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to @instrumentation@ /must/ be externally synchronized
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_DATA_GRAPH_BIT_ARM                                                                                           | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBeginShaderInstrumentationARM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
cmdBeginShaderInstrumentationARM :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer into which this command will be
                                    -- recorded.
                                    CommandBuffer
                                 -> -- | @instrumentation@ is the handle of the shader instrumentation object
                                    -- that will capture the metrics.
                                    ShaderInstrumentationARM
                                 -> io ()
cmdBeginShaderInstrumentationARM commandBuffer instrumentation = liftIO $ do
  let vkCmdBeginShaderInstrumentationARMPtr = pVkCmdBeginShaderInstrumentationARM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBeginShaderInstrumentationARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginShaderInstrumentationARM is null" Nothing Nothing
  let vkCmdBeginShaderInstrumentationARM' = mkVkCmdBeginShaderInstrumentationARM vkCmdBeginShaderInstrumentationARMPtr
  traceAroundEvent "vkCmdBeginShaderInstrumentationARM" (vkCmdBeginShaderInstrumentationARM'
                                                           (commandBufferHandle (commandBuffer))
                                                           (instrumentation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndShaderInstrumentationARM
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndShaderInstrumentationARM - End shader instrumentation
--
-- = Description
--
-- Once recorded, shader instrumentation is no longer considered /active/
-- within the command buffer.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-commandBuffer-12377# Shader
--     instrumentation /must/ be active within @commandBuffer@
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-commandBuffer-12378#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdEndShaderInstrumentationARM-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_DATA_GRAPH_BIT_ARM                                                                                           | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdEndShaderInstrumentationARM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndShaderInstrumentationARM :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which this command will be
                                  -- recorded.
                                  CommandBuffer
                               -> io ()
cmdEndShaderInstrumentationARM commandBuffer = liftIO $ do
  let vkCmdEndShaderInstrumentationARMPtr = pVkCmdEndShaderInstrumentationARM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdEndShaderInstrumentationARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndShaderInstrumentationARM is null" Nothing Nothing
  let vkCmdEndShaderInstrumentationARM' = mkVkCmdEndShaderInstrumentationARM vkCmdEndShaderInstrumentationARMPtr
  traceAroundEvent "vkCmdEndShaderInstrumentationARM" (vkCmdEndShaderInstrumentationARM'
                                                         (commandBufferHandle (commandBuffer)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderInstrumentationValuesARM
  :: FunPtr (Ptr Device_T -> ShaderInstrumentationARM -> Ptr Word32 -> Ptr () -> ShaderInstrumentationValuesFlagsARM -> IO Result) -> Ptr Device_T -> ShaderInstrumentationARM -> Ptr Word32 -> Ptr () -> ShaderInstrumentationValuesFlagsARM -> IO Result

-- | vkGetShaderInstrumentationValuesARM - Retrieve shader instrumentation
-- data
--
-- = Description
--
-- If @pMetricValues@ is @NULL@, then the number of metric blocks available
-- is returned in @pMetricBlockCount@. Otherwise, @pMetricBlockCount@
-- /must/ point to a variable set by the application to the number of
-- elements in the @pMetricValues@ array, and on return the variable is
-- overwritten with the number of metric blocks actually written to
-- @pMetricValues@. If @pMetricBlockCount@ is less than the number of
-- metric blocks available, at most @pMetricBlockCount@ elements will be
-- written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not
-- all the available metric blocks were returned.
--
-- Metrics are written to @pMetricValues@ as a tightly packed array of
-- metric blocks, where each block consists of a
-- 'ShaderInstrumentationMetricDataHeaderARM' header followed by
-- 'PhysicalDeviceShaderInstrumentationPropertiesARM'::@numMetrics@
-- unsigned 64-bit values. The order of the metrics matches the order in
-- which they are enumerated by
-- 'enumeratePhysicalDeviceShaderInstrumentationMetricsARM'.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM',
-- 'ShaderInstrumentationValuesFlagsARM'
getShaderInstrumentationValuesARM :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that was used to capture shader
                                     -- instrumentation data.
                                     --
                                     -- #VUID-vkGetShaderInstrumentationValuesARM-device-parameter# @device@
                                     -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                     Device
                                  -> -- | @instrumentation@ is the shader instrumentation object to retrieve
                                     -- values from
                                     --
                                     -- #VUID-vkGetShaderInstrumentationValuesARM-instrumentation-parameter#
                                     -- @instrumentation@ /must/ be a valid
                                     -- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM' handle
                                     --
                                     -- #VUID-vkGetShaderInstrumentationValuesARM-instrumentation-parent#
                                     -- @instrumentation@ /must/ have been created, allocated, or retrieved from
                                     -- @device@
                                     ShaderInstrumentationARM
                                  -> -- | @pMetricValues@ is either @NULL@ or a pointer to an
                                     -- application-allocated buffer where the results will be written.
                                     --
                                     -- #VUID-vkGetShaderInstrumentationValuesARM-pMetricValues-parameter#
                                     -- @pMetricValues@ /must/ be a pointer value
                                     ("metricValues" ::: Ptr ())
                                  -> -- | @flags@ is reserved for future use.
                                     --
                                     -- #VUID-vkGetShaderInstrumentationValuesARM-flags-zerobitmask# @flags@
                                     -- /must/ be @0@
                                     ShaderInstrumentationValuesFlagsARM
                                  -> io (Result, ("metricBlockCount" ::: Word32))
getShaderInstrumentationValuesARM device
                                    instrumentation
                                    metricValues
                                    flags = liftIO . evalContT $ do
  let vkGetShaderInstrumentationValuesARMPtr = pVkGetShaderInstrumentationValuesARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetShaderInstrumentationValuesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderInstrumentationValuesARM is null" Nothing Nothing
  let vkGetShaderInstrumentationValuesARM' = mkVkGetShaderInstrumentationValuesARM vkGetShaderInstrumentationValuesARMPtr
  pPMetricBlockCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetShaderInstrumentationValuesARM" (vkGetShaderInstrumentationValuesARM'
                                                                        (deviceHandle (device))
                                                                        (instrumentation)
                                                                        (pPMetricBlockCount)
                                                                        (metricValues)
                                                                        (flags))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMetricBlockCount <- lift $ peek @Word32 pPMetricBlockCount
  pure $ (r, pMetricBlockCount)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkClearShaderInstrumentationMetricsARM
  :: FunPtr (Ptr Device_T -> ShaderInstrumentationARM -> IO ()) -> Ptr Device_T -> ShaderInstrumentationARM -> IO ()

-- | vkClearShaderInstrumentationMetricsARM - Clear shader instrumentation
-- metrics to zero
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
clearShaderInstrumentationMetricsARM :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device that owns the shader instrumentation
                                        -- object.
                                        --
                                        -- #VUID-vkClearShaderInstrumentationMetricsARM-device-parameter# @device@
                                        -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                        Device
                                     -> -- | @instrumentation@ is the shader instrumentation object to clear.
                                        --
                                        -- #VUID-vkClearShaderInstrumentationMetricsARM-instrumentation-parameter#
                                        -- @instrumentation@ /must/ be a valid
                                        -- 'Vulkan.Extensions.Handles.ShaderInstrumentationARM' handle
                                        --
                                        -- #VUID-vkClearShaderInstrumentationMetricsARM-instrumentation-parent#
                                        -- @instrumentation@ /must/ have been created, allocated, or retrieved from
                                        -- @device@
                                        ShaderInstrumentationARM
                                     -> io ()
clearShaderInstrumentationMetricsARM device instrumentation = liftIO $ do
  let vkClearShaderInstrumentationMetricsARMPtr = pVkClearShaderInstrumentationMetricsARM (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkClearShaderInstrumentationMetricsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkClearShaderInstrumentationMetricsARM is null" Nothing Nothing
  let vkClearShaderInstrumentationMetricsARM' = mkVkClearShaderInstrumentationMetricsARM vkClearShaderInstrumentationMetricsARMPtr
  traceAroundEvent "vkClearShaderInstrumentationMetricsARM" (vkClearShaderInstrumentationMetricsARM'
                                                               (deviceHandle (device))
                                                               (instrumentation))
  pure $ ()


-- | VkPhysicalDeviceShaderInstrumentationFeaturesARM - Structure describing
-- support for shader instrumentation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderInstrumentationFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderInstrumentationFeaturesARM', it /must/ add an
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderInstrumentationFeaturesARM = PhysicalDeviceShaderInstrumentationFeaturesARM
  { -- | #features-shaderInstrumentation# @shaderInstrumentation@ specifies
    -- whether shader instrumentation is supported.
    shaderInstrumentation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderInstrumentationFeaturesARM)
#endif
deriving instance Show PhysicalDeviceShaderInstrumentationFeaturesARM

instance ToCStruct PhysicalDeviceShaderInstrumentationFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderInstrumentationFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderInstrumentation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderInstrumentationFeaturesARM where
  peekCStruct p = do
    shaderInstrumentation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderInstrumentationFeaturesARM
             (bool32ToBool shaderInstrumentation)

instance Storable PhysicalDeviceShaderInstrumentationFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderInstrumentationFeaturesARM where
  zero = PhysicalDeviceShaderInstrumentationFeaturesARM
           zero


-- | VkPhysicalDeviceShaderInstrumentationPropertiesARM - Structure
-- describing shader instrumentation properties for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderInstrumentationPropertiesARM'
-- structure describe the following:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderInstrumentationPropertiesARM' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderInstrumentationPropertiesARM = PhysicalDeviceShaderInstrumentationPropertiesARM
  { -- | @numMetrics@ is the number of shader instrumentation metrics supported.
    numMetrics :: Word32
  , -- | @perBasicBlockGranularity@ is a boolean value indicating whether shader
    -- instrumentation metrics are returned per basic block. If this is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', then all metrics for the shader
    -- are reported as basic block index @0@.
    perBasicBlockGranularity :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderInstrumentationPropertiesARM)
#endif
deriving instance Show PhysicalDeviceShaderInstrumentationPropertiesARM

instance ToCStruct PhysicalDeviceShaderInstrumentationPropertiesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderInstrumentationPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (numMetrics)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (perBasicBlockGranularity))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderInstrumentationPropertiesARM where
  peekCStruct p = do
    numMetrics <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    perBasicBlockGranularity <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderInstrumentationPropertiesARM
             numMetrics (bool32ToBool perBasicBlockGranularity)

instance Storable PhysicalDeviceShaderInstrumentationPropertiesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderInstrumentationPropertiesARM where
  zero = PhysicalDeviceShaderInstrumentationPropertiesARM
           zero
           zero


-- | VkShaderInstrumentationCreateInfoARM - Structure specifying parameters
-- of a newly created shader instrumentation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createShaderInstrumentationARM'
data ShaderInstrumentationCreateInfoARM = ShaderInstrumentationCreateInfoARM
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderInstrumentationCreateInfoARM)
#endif
deriving instance Show ShaderInstrumentationCreateInfoARM

instance ToCStruct ShaderInstrumentationCreateInfoARM where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderInstrumentationCreateInfoARM f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_INSTRUMENTATION_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_INSTRUMENTATION_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ShaderInstrumentationCreateInfoARM where
  peekCStruct _ = pure $ ShaderInstrumentationCreateInfoARM
                           

instance Storable ShaderInstrumentationCreateInfoARM where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderInstrumentationCreateInfoARM where
  zero = ShaderInstrumentationCreateInfoARM
           


-- | VkShaderInstrumentationMetricDescriptionARM - Structure specifying
-- shader instrumentation metric properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceShaderInstrumentationMetricsARM'
data ShaderInstrumentationMetricDescriptionARM = ShaderInstrumentationMetricDescriptionARM
  { -- | @name@ is an array of 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    -- @char@ containing a null-terminated UTF-8 string which is a short human
    -- readable name for this shader instrumentation metric.
    --
    -- #VUID-VkShaderInstrumentationMetricDescriptionARM-name-parameter# @name@
    -- /must/ be a null-terminated UTF-8 string whose length is less than or
    -- equal to 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description for
    -- this shader instrumentation metric.
    --
    -- #VUID-VkShaderInstrumentationMetricDescriptionARM-description-parameter#
    -- @description@ /must/ be a null-terminated UTF-8 string whose length is
    -- less than or equal to 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE'
    description :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderInstrumentationMetricDescriptionARM)
#endif
deriving instance Show ShaderInstrumentationMetricDescriptionARM

instance ToCStruct ShaderInstrumentationMetricDescriptionARM where
  withCStruct x f = allocaBytes 528 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderInstrumentationMetricDescriptionARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_INSTRUMENTATION_METRIC_DESCRIPTION_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 528
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_INSTRUMENTATION_METRIC_DESCRIPTION_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct ShaderInstrumentationMetricDescriptionARM where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pure $ ShaderInstrumentationMetricDescriptionARM
             name description

instance Storable ShaderInstrumentationMetricDescriptionARM where
  sizeOf ~_ = 528
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderInstrumentationMetricDescriptionARM where
  zero = ShaderInstrumentationMetricDescriptionARM
           mempty
           mempty


-- | VkShaderInstrumentationMetricDataHeaderARM - Structure describing the
-- header of a metric block
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
data ShaderInstrumentationMetricDataHeaderARM = ShaderInstrumentationMetricDataHeaderARM
  { -- | @resultIndex@ is the result index of the metric block, as captured when
    -- the command was recorded.
    resultIndex :: Word32
  , -- | @resultSubIndex@ is a secondary index with the result index, explained
    -- further below.
    resultSubIndex :: Word32
  , -- | @stages@ is a bitfield of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' describing
    -- the shader stages that the metric block is for.
    --
    -- #VUID-VkShaderInstrumentationMetricDataHeaderARM-stages-parameter#
    -- @stages@ /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
    --
    -- #VUID-VkShaderInstrumentationMetricDataHeaderARM-stages-requiredbitmask#
    -- @stages@ /must/ not be @0@
    stages :: ShaderStageFlags
  , -- | @basicBlockIndex@ is the index of the basic block within the shader that
    -- the metric block is for.
    basicBlockIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderInstrumentationMetricDataHeaderARM)
#endif
deriving instance Show ShaderInstrumentationMetricDataHeaderARM

instance ToCStruct ShaderInstrumentationMetricDataHeaderARM where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderInstrumentationMetricDataHeaderARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (resultIndex)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (resultSubIndex)
    poke ((p `plusPtr` 8 :: Ptr ShaderStageFlags)) (stages)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (basicBlockIndex)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct ShaderInstrumentationMetricDataHeaderARM where
  peekCStruct p = do
    resultIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    resultSubIndex <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    stages <- peek @ShaderStageFlags ((p `plusPtr` 8 :: Ptr ShaderStageFlags))
    basicBlockIndex <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ ShaderInstrumentationMetricDataHeaderARM
             resultIndex resultSubIndex stages basicBlockIndex

instance Storable ShaderInstrumentationMetricDataHeaderARM where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderInstrumentationMetricDataHeaderARM where
  zero = ShaderInstrumentationMetricDataHeaderARM
           zero
           zero
           zero
           zero


-- | VkShaderInstrumentationValuesFlagsARM - Reserved for future use
--
-- = Description
--
-- 'ShaderInstrumentationValuesFlagsARM' is a bitmask type for parameters
-- to the retrieval, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_instrumentation VK_ARM_shader_instrumentation>,
-- 'Vulkan.Core10.FundamentalTypes.Flags',
-- 'getShaderInstrumentationValuesARM'
newtype ShaderInstrumentationValuesFlagsARM = ShaderInstrumentationValuesFlagsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameShaderInstrumentationValuesFlagsARM :: String
conNameShaderInstrumentationValuesFlagsARM = "ShaderInstrumentationValuesFlagsARM"

enumPrefixShaderInstrumentationValuesFlagsARM :: String
enumPrefixShaderInstrumentationValuesFlagsARM = ""

showTableShaderInstrumentationValuesFlagsARM :: [(ShaderInstrumentationValuesFlagsARM, String)]
showTableShaderInstrumentationValuesFlagsARM = []

instance Show ShaderInstrumentationValuesFlagsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixShaderInstrumentationValuesFlagsARM
      showTableShaderInstrumentationValuesFlagsARM
      conNameShaderInstrumentationValuesFlagsARM
      (\(ShaderInstrumentationValuesFlagsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ShaderInstrumentationValuesFlagsARM where
  readPrec =
    enumReadPrec
      enumPrefixShaderInstrumentationValuesFlagsARM
      showTableShaderInstrumentationValuesFlagsARM
      conNameShaderInstrumentationValuesFlagsARM
      ShaderInstrumentationValuesFlagsARM

type ARM_SHADER_INSTRUMENTATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_SHADER_INSTRUMENTATION_SPEC_VERSION"
pattern ARM_SHADER_INSTRUMENTATION_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_SHADER_INSTRUMENTATION_SPEC_VERSION = 1


type ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME = "VK_ARM_shader_instrumentation"

-- No documentation found for TopLevel "VK_ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME"
pattern ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME = "VK_ARM_shader_instrumentation"

