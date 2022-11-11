{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_creation_cache_control - device extension
--
-- == VK_EXT_pipeline_creation_cache_control
--
-- [__Name String__]
--     @VK_EXT_pipeline_creation_cache_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     298
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Gregory Grebe
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_creation_cache_control] @grgrebe_amd%0A*Here describe the issue or question you have about the VK_EXT_pipeline_creation_cache_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-03-23
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Gregory Grebe, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Mitch Singer, AMD
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Jeff Bolz, NVIDIA Corporation
--
--     -   Daniel Koch, NVIDIA Corporation
--
--     -   Dan Ginsburg, Valve Corporation
--
--     -   Jeff Leger, QUALCOMM
--
--     -   Michal Pietrasiuk, Intel
--
--     -   Jan-Harald Fredriksen, Arm Limited
--
-- == Description
--
-- This extension adds flags to @Vk*PipelineCreateInfo@ and
-- 'Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo' structures with
-- the aim of improving the predictability of pipeline creation cost. The
-- goal is to provide information about potentially expensive hazards
-- within the client driver during pipeline creation to the application
-- before carrying them out rather than after.
--
-- == Background
--
-- Pipeline creation is a costly operation, and the explicit nature of the
-- Vulkan design means that cost is not hidden from the developer.
-- Applications are also expected to schedule, prioritize, and load balance
-- all calls for pipeline creation. It is strongly advised that
-- applications create pipelines sufficiently ahead of their usage. Failure
-- to do so will result in an unresponsive application, intermittent
-- stuttering, or other poor user experiences. Proper usage of pipeline
-- caches and\/or derivative pipelines help mitigate this but is not
-- assured to eliminate disruption in all cases. In the event that an
-- ahead-of-time creation is not possible, considerations should be taken
-- to ensure that the current execution context is suitable for the
-- workload of pipeline creation including possible shader compilation.
--
-- Applications making API calls to create a pipeline must be prepared for
-- any of the following to occur:
--
-- -   OS\/kernel calls to be made by the ICD
--
-- -   Internal memory allocation not tracked by the @pAllocator@ passed to
--     @vkCreate*Pipelines@
--
-- -   Internal thread synchronization or yielding of the current thread’s
--     core
--
-- -   Extremely long (multi-millisecond+), blocking, compilation times
--
-- -   Arbitrary call stacks depths and stack memory usage
--
-- The job or task based game engines that are being developed to take
-- advantage of explicit graphics APIs like Vulkan may behave exceptionally
-- poorly if any of the above scenarios occur. However, most game engines
-- are already built to “stream” in assets dynamically as the user plays
-- the game. By adding control by way of
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags', we can
-- require an ICD to report back a failure in critical execution paths
-- rather than forcing an unexpected wait.
--
-- Applications can prevent unexpected compilation by setting
-- 'PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT' on
-- @Vk*PipelineCreateInfo@::@flags@. When set, an ICD must not attempt
-- pipeline or shader compilation to create the pipeline object. In such a
-- case, if the implementation fails to create a pipeline without
-- compilation, the implementation /must/ return the result
-- 'PIPELINE_COMPILE_REQUIRED_EXT' and return
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' for the pipeline.
--
-- By default @vkCreate*Pipelines@ calls must attempt to create all
-- pipelines before returning. Setting
-- 'PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT' on
-- @Vk*PipelineCreateInfo@::@flags@ can be used as an escape hatch for
-- batched pipeline creates.
--
-- Hidden locks also add to the unpredictability of the cost of pipeline
-- creation. The most common case of locks inside the @vkCreate*Pipelines@
-- is internal synchronization of the 'Vulkan.Core10.Handles.PipelineCache'
-- object. 'PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT' can be
-- set when calling 'Vulkan.Core10.PipelineCache.createPipelineCache' to
-- state the cache is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>.
--
-- The hope is that armed with this information application and engine
-- developers can leverage existing asset streaming systems to recover from
-- \"just-in-time\" pipeline creation stalls.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineCreationCacheControlFeaturesEXT'
--
-- == New Enums
--
-- -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits':
--
--     -   'PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
--     -   'PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_PIPELINE_COMPILE_REQUIRED_EXT'
--
--     -   'PIPELINE_COMPILE_REQUIRED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2019-11-01 (Gregory Grebe)
--
--     -   Initial revision
--
-- -   Revision 2, 2020-02-24 (Gregory Grebe)
--
--     -   Initial public revision
--
-- -   Revision 3, 2020-03-23 (Tobias Hector)
--
--     -   Changed 'PIPELINE_COMPILE_REQUIRED_EXT' to a success code,
--         adding an alias for the original
--         'ERROR_PIPELINE_COMPILE_REQUIRED_EXT'. Also updated the xml to
--         include these codes as return values.
--
-- == See Also
--
-- 'PhysicalDevicePipelineCreationCacheControlFeaturesEXT',
-- 'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT
                                                                 , pattern PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT
                                                                 , pattern PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT
                                                                 , pattern PIPELINE_COMPILE_REQUIRED_EXT
                                                                 , pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT
                                                                 , pattern PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT
                                                                 , PhysicalDevicePipelineCreationCacheControlFeaturesEXT
                                                                 , EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION
                                                                 , pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION
                                                                 , EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
                                                                 , pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
                                                                 ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeatures)
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlags)
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlagBits(PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT))
import Vulkan.Core10.Enums.Result (Result(PIPELINE_COMPILE_REQUIRED))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT"
pattern PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT = PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT"
pattern PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT = PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT


-- No documentation found for TopLevel "VK_PIPELINE_COMPILE_REQUIRED_EXT"
pattern PIPELINE_COMPILE_REQUIRED_EXT = PIPELINE_COMPILE_REQUIRED


-- No documentation found for TopLevel "VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT"
pattern ERROR_PIPELINE_COMPILE_REQUIRED_EXT = PIPELINE_COMPILE_REQUIRED


-- No documentation found for TopLevel "VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT"
pattern PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT = PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT


-- No documentation found for TopLevel "VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT"
type PhysicalDevicePipelineCreationCacheControlFeaturesEXT = PhysicalDevicePipelineCreationCacheControlFeatures


type EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3


type EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME = "VK_EXT_pipeline_creation_cache_control"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME = "VK_EXT_pipeline_creation_cache_control"

