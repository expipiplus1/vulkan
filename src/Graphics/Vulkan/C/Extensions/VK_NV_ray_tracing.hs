{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsTypeNV(..)
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
  , VkAccelerationStructureNV
  , VkAccelerationStructureTypeNV(..)
  , pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
  , pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkBuildAccelerationStructureFlagBitsNV(..)
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
  , VkBuildAccelerationStructureFlagsNV
  , VkCopyAccelerationStructureModeNV(..)
  , pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
  , pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
  , VkGeometryAABBNV(..)
  , VkGeometryDataNV(..)
  , VkGeometryFlagBitsNV(..)
  , pattern VK_GEOMETRY_OPAQUE_BIT_NV
  , pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
  , VkGeometryFlagsNV
  , VkGeometryInstanceFlagBitsNV(..)
  , pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
  , VkGeometryInstanceFlagsNV
  , VkGeometryNV(..)
  , VkGeometryTrianglesNV(..)
  , VkGeometryTypeNV(..)
  , pattern VK_GEOMETRY_TYPE_TRIANGLES_NV
  , pattern VK_GEOMETRY_TYPE_AABBS_NV
  , VkPhysicalDeviceRayTracingPropertiesNV(..)
  , VkRayTracingPipelineCreateInfoNV(..)
  , VkRayTracingShaderGroupCreateInfoNV(..)
  , VkRayTracingShaderGroupTypeNV(..)
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
  , VkWriteDescriptorSetAccelerationStructureNV(..)
  , FN_vkBindAccelerationStructureMemoryNV
  , PFN_vkBindAccelerationStructureMemoryNV
  , vkBindAccelerationStructureMemoryNV
  , FN_vkCmdBuildAccelerationStructureNV
  , PFN_vkCmdBuildAccelerationStructureNV
  , vkCmdBuildAccelerationStructureNV
  , FN_vkCmdCopyAccelerationStructureNV
  , PFN_vkCmdCopyAccelerationStructureNV
  , vkCmdCopyAccelerationStructureNV
  , FN_vkCmdTraceRaysNV
  , PFN_vkCmdTraceRaysNV
  , vkCmdTraceRaysNV
  , FN_vkCmdWriteAccelerationStructuresPropertiesNV
  , PFN_vkCmdWriteAccelerationStructuresPropertiesNV
  , vkCmdWriteAccelerationStructuresPropertiesNV
  , FN_vkCompileDeferredNV
  , PFN_vkCompileDeferredNV
  , vkCompileDeferredNV
  , FN_vkCreateAccelerationStructureNV
  , PFN_vkCreateAccelerationStructureNV
  , vkCreateAccelerationStructureNV
  , FN_vkCreateRayTracingPipelinesNV
  , PFN_vkCreateRayTracingPipelinesNV
  , vkCreateRayTracingPipelinesNV
  , FN_vkDestroyAccelerationStructureNV
  , PFN_vkDestroyAccelerationStructureNV
  , vkDestroyAccelerationStructureNV
  , FN_vkGetAccelerationStructureHandleNV
  , PFN_vkGetAccelerationStructureHandleNV
  , vkGetAccelerationStructureHandleNV
  , FN_vkGetAccelerationStructureMemoryRequirementsNV
  , PFN_vkGetAccelerationStructureMemoryRequirementsNV
  , vkGetAccelerationStructureMemoryRequirementsNV
  , FN_vkGetRayTracingShaderGroupHandlesNV
  , PFN_vkGetRayTracingShaderGroupHandlesNV
  , vkGetRayTracingShaderGroupHandlesNV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  , pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_INDEX_TYPE_NONE_NV
  , pattern VK_NV_RAY_TRACING_EXTENSION_NAME
  , pattern VK_NV_RAY_TRACING_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV
  , pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  , pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  , pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_CALLABLE_BIT_NV
  , pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern VK_SHADER_STAGE_MISS_BIT_NV
  , pattern VK_SHADER_STAGE_RAYGEN_BIT_NV
  , pattern VK_SHADER_UNUSED_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  , VkMemoryRequirements2KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkShaderStageFlagBits(..)
  , VkPipeline
  , VkPipelineCreateFlags
  , VkPipelineLayout
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryType(..)
  , VkQueryPool
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkMemoryRequirements2KHR
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkAccelerationStructureCreateInfoNV - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- == Valid Usage
--
-- -   If @compactedSize@ is not @0@ then both @info.geometryCount@ and
--     @info.instanceCount@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @info@ /must/ be a valid 'VkAccelerationStructureInfoNV' structure
--
-- = See Also
--
-- 'VkAccelerationStructureInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateAccelerationStructureNV'
data VkAccelerationStructureCreateInfoNV = VkAccelerationStructureCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @compactedSize@ is the size from the result of
  -- 'vkCmdWriteAccelerationStructuresPropertiesNV' if this acceleration
  -- structure is going to be the target of a compacting copy.
  vkCompactedSize :: VkDeviceSize
  , -- | @info@ is the 'VkAccelerationStructureInfoNV' structure that specifies
  -- further parameters of the created acceleration structure.
  vkInfo :: VkAccelerationStructureInfoNV
  }
  deriving (Eq, Show)

instance Storable VkAccelerationStructureCreateInfoNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkAccelerationStructureCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAccelerationStructureCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAccelerationStructureCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkCompactedSize (poked :: VkAccelerationStructureCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkInfo (poked :: VkAccelerationStructureCreateInfoNV))

instance Zero VkAccelerationStructureCreateInfoNV where
  zero = VkAccelerationStructureCreateInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
                                             zero
                                             zero
                                             zero

-- | VkAccelerationStructureInfoNV - Structure specifying the parameters of
-- acceleration structure object
--
-- = Description
--
-- 'VkAccelerationStructureInfoNV' contains information that is used both
-- for acceleration structure creation with
-- 'vkCreateAccelerationStructureNV' and in combination with the actual
-- geometric data to build the acceleration structure with
-- 'vkCmdBuildAccelerationStructureNV'.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @instanceCount@ /must/ be less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxInstanceCount@
--
-- -   The total number of triangles in all geometries /must/ be less than
--     or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxTriangleCount@
--
-- -   If @type@ is 'VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' then
--     @geometryCount@ /must/ be @0@
--
-- -   If @type@ is 'VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then
--     @instanceCount@ /must/ be @0@
--
-- -   If @flags@ has the
--     'VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' bit set,
--     then it /must/ not have the
--     'VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' bit set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'VkAccelerationStructureTypeNV' value
--
-- -   @flags@ /must/ be a valid combination of
--     'VkBuildAccelerationStructureFlagBitsNV' values
--
-- -   If @geometryCount@ is not @0@, @pGeometries@ /must/ be a valid
--     pointer to an array of @geometryCount@ valid 'VkGeometryNV'
--     structures
--
-- = See Also
--
-- 'VkAccelerationStructureCreateInfoNV', 'VkAccelerationStructureTypeNV',
-- 'VkBuildAccelerationStructureFlagsNV', 'VkGeometryNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCmdBuildAccelerationStructureNV'
data VkAccelerationStructureInfoNV = VkAccelerationStructureInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @type@ is a 'VkAccelerationStructureTypeNV' value specifying the type of
  -- acceleration structure that will be created.
  vkType :: VkAccelerationStructureTypeNV
  , -- | @flags@ is a bitmask of 'VkBuildAccelerationStructureFlagBitsNV'
  -- specifying additional parameters of the acceleration structure.
  vkFlags :: VkBuildAccelerationStructureFlagsNV
  , -- | @instanceCount@ specifies the number of instances that will be in the
  -- new acceleration structure.
  vkInstanceCount :: Word32
  , -- | @geometryCount@ specifies the number of geometries that will be in the
  -- new acceleration structure.
  vkGeometryCount :: Word32
  , -- | @pGeometries@ is an array of 'VkGeometryNV' structures, which contain
  -- the scene data being passed into the acceleration structure.
  vkPGeometries :: Ptr VkGeometryNV
  }
  deriving (Eq, Show)

instance Storable VkAccelerationStructureInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkAccelerationStructureInfoNV <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 28)
                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 16) (vkType (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 20) (vkFlags (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 24) (vkInstanceCount (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 28) (vkGeometryCount (poked :: VkAccelerationStructureInfoNV))
                *> poke (ptr `plusPtr` 32) (vkPGeometries (poked :: VkAccelerationStructureInfoNV))

instance Zero VkAccelerationStructureInfoNV where
  zero = VkAccelerationStructureInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

-- | VkAccelerationStructureMemoryRequirementsInfoNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkAccelerationStructureMemoryRequirementsTypeNV',
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetAccelerationStructureMemoryRequirementsNV'
data VkAccelerationStructureMemoryRequirementsInfoNV = VkAccelerationStructureMemoryRequirementsInfoNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @type@ /must/ be a valid
  -- 'VkAccelerationStructureMemoryRequirementsTypeNV' value
  vkType :: VkAccelerationStructureMemoryRequirementsTypeNV
  , -- | @accelerationStructure@ /must/ be a valid 'VkAccelerationStructureNV'
  -- handle
  vkAccelerationStructure :: VkAccelerationStructureNV
  }
  deriving (Eq, Show)

instance Storable VkAccelerationStructureMemoryRequirementsInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkAccelerationStructureMemoryRequirementsInfoNV <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAccelerationStructureMemoryRequirementsInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAccelerationStructureMemoryRequirementsInfoNV))
                *> poke (ptr `plusPtr` 16) (vkType (poked :: VkAccelerationStructureMemoryRequirementsInfoNV))
                *> poke (ptr `plusPtr` 24) (vkAccelerationStructure (poked :: VkAccelerationStructureMemoryRequirementsInfoNV))

instance Zero VkAccelerationStructureMemoryRequirementsInfoNV where
  zero = VkAccelerationStructureMemoryRequirementsInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
                                                         zero
                                                         zero
                                                         zero

-- ** VkAccelerationStructureMemoryRequirementsTypeNV

-- | VkAccelerationStructureMemoryRequirementsTypeNV - Acceleration structure
-- memory requirement type
--
-- = See Also
--
-- 'VkAccelerationStructureMemoryRequirementsInfoNV'
newtype VkAccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkAccelerationStructureMemoryRequirementsTypeNV where
  showsPrec _ VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = showString "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
  showsPrec _ VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = showString "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
  showsPrec _ VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = showString "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
  showsPrec p (VkAccelerationStructureMemoryRequirementsTypeNV x) = showParen (p >= 11) (showString "VkAccelerationStructureMemoryRequirementsTypeNV " . showsPrec 11 x)

instance Read VkAccelerationStructureMemoryRequirementsTypeNV where
  readPrec = parens ( choose [ ("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV",         pure VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV)
                             , ("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV",  pure VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV)
                             , ("VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV", pure VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccelerationStructureMemoryRequirementsTypeNV")
                        v <- step readPrec
                        pure (VkAccelerationStructureMemoryRequirementsTypeNV v)
                        )
                    )

-- | 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' requests
-- the memory requirement for the 'VkAccelerationStructureNV' backing
-- store.
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = VkAccelerationStructureMemoryRequirementsTypeNV 0

-- | 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
-- requests the memory requirement for scratch space during the initial
-- build.
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = VkAccelerationStructureMemoryRequirementsTypeNV 1

-- | 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
-- requests the memory requirement for scratch space during an update.
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = VkAccelerationStructureMemoryRequirementsTypeNV 2

-- | Dummy data to tag the 'Ptr' with
data VkAccelerationStructureNV_T
-- | VkAccelerationStructureNV - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- 'VkAccelerationStructureMemoryRequirementsInfoNV',
-- 'VkBindAccelerationStructureMemoryInfoNV',
-- 'VkWriteDescriptorSetAccelerationStructureNV',
-- 'vkCmdBuildAccelerationStructureNV', 'vkCmdCopyAccelerationStructureNV',
-- 'vkCmdWriteAccelerationStructuresPropertiesNV',
-- 'vkCreateAccelerationStructureNV', 'vkDestroyAccelerationStructureNV',
-- 'vkGetAccelerationStructureHandleNV'
type VkAccelerationStructureNV = Ptr VkAccelerationStructureNV_T

-- ** VkAccelerationStructureTypeNV

-- | VkAccelerationStructureTypeNV - Type of acceleration structure
--
-- = See Also
--
-- 'VkAccelerationStructureInfoNV'
newtype VkAccelerationStructureTypeNV = VkAccelerationStructureTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkAccelerationStructureTypeNV where
  showsPrec _ VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = showString "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
  showsPrec _ VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = showString "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
  showsPrec p (VkAccelerationStructureTypeNV x) = showParen (p >= 11) (showString "VkAccelerationStructureTypeNV " . showsPrec 11 x)

instance Read VkAccelerationStructureTypeNV where
  readPrec = parens ( choose [ ("VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV",    pure VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV)
                             , ("VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV", pure VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccelerationStructureTypeNV")
                        v <- step readPrec
                        pure (VkAccelerationStructureTypeNV v)
                        )
                    )

-- | 'VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' is a top-level
-- acceleration structure containing instance data referring to
-- bottom-level level acceleration structures.
pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV :: VkAccelerationStructureTypeNV
pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = VkAccelerationStructureTypeNV 0

-- | 'VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' is a bottom-level
-- acceleration structure containing the AABBs or geometry to be
-- intersected.
pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: VkAccelerationStructureTypeNV
pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = VkAccelerationStructureTypeNV 1

-- | VkBindAccelerationStructureMemoryInfoNV - Structure specifying
-- acceleration structure memory binding
--
-- == Valid Usage
--
-- -   @accelerationStructure@ /must/ not already be backed by a memory
--     object
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'vkGetAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'vkGetAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'vkGetAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--     /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @accelerationStructure@ /must/ be a valid
--     'VkAccelerationStructureNV' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   Both of @accelerationStructure@, and @memory@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkBindAccelerationStructureMemoryNV'
data VkBindAccelerationStructureMemoryInfoNV = VkBindAccelerationStructureMemoryInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @accelerationStructure@ is the acceleration structure to be attached to
  -- memory.
  vkAccelerationStructure :: VkAccelerationStructureNV
  , -- | @memory@ is a 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
  -- describing the device memory to attach.
  vkMemory :: VkDeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of memory that is to be
  -- bound to the acceleration structure. The number of bytes returned in the
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
  -- member in @memory@, starting from @memoryOffset@ bytes, will be bound to
  -- the specified acceleration structure.
  vkMemoryOffset :: VkDeviceSize
  , -- | @deviceIndexCount@ is the number of elements in @pDeviceIndices@.
  vkDeviceIndexCount :: Word32
  , -- | @pDeviceIndices@ is a pointer to an array of device indices.
  vkPDeviceIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkBindAccelerationStructureMemoryInfoNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBindAccelerationStructureMemoryInfoNV <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 40)
                                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 16) (vkAccelerationStructure (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 40) (vkDeviceIndexCount (poked :: VkBindAccelerationStructureMemoryInfoNV))
                *> poke (ptr `plusPtr` 48) (vkPDeviceIndices (poked :: VkBindAccelerationStructureMemoryInfoNV))

instance Zero VkBindAccelerationStructureMemoryInfoNV where
  zero = VkBindAccelerationStructureMemoryInfoNV VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
                                                 zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero

-- ** VkBuildAccelerationStructureFlagBitsNV

-- | VkBuildAccelerationStructureFlagBitsNV - Bitmask specifying additional
-- parameters for acceleration structure builds
--
-- = Description
--
-- __Note__
--
-- 'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' and
-- 'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' /may/ take
-- more time and memory than a normal build, and so /should/ only be used
-- when those features are used.
--
-- = See Also
--
-- 'VkBuildAccelerationStructureFlagsNV'
newtype VkBuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkBuildAccelerationStructureFlagBitsNV where
  showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = showString "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
  showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = showString "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
  showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = showString "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
  showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = showString "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
  showsPrec _ VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = showString "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
  showsPrec p (VkBuildAccelerationStructureFlagBitsNV x) = showParen (p >= 11) (showString "VkBuildAccelerationStructureFlagBitsNV " . showsPrec 11 x)

instance Read VkBuildAccelerationStructureFlagBitsNV where
  readPrec = parens ( choose [ ("VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV",      pure VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV)
                             , ("VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV",  pure VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV)
                             , ("VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV", pure VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV)
                             , ("VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV", pure VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV)
                             , ("VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV",        pure VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBuildAccelerationStructureFlagBitsNV")
                        v <- step readPrec
                        pure (VkBuildAccelerationStructureFlagBitsNV v)
                        )
                    )

-- | 'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' indicates that the
-- specified acceleration structure /can/ be updated with @update@ of
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' in
-- 'vkCmdBuildAccelerationStructureNV'.
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000001

-- | 'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' indicates that
-- the specified acceleration structure /can/ act as the source for
-- 'vkCmdCopyAccelerationStructureNV' with @mode@ of
-- 'VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' to produce a compacted
-- acceleration structure.
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000002

-- | 'VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' indicates
-- that the given acceleration structure build /should/ prioritize trace
-- performance over build time.
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000004

-- | 'VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' indicates
-- that the given acceleration structure build /should/ prioritize build
-- time over trace performance.
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000008

-- | 'VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV' indicates that this
-- acceleration structure /should/ minimize the size of the scratch memory
-- and the final result build, potentially at the expense of build time or
-- trace performance.
pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000010

-- | VkBuildAccelerationStructureFlagsNV - Bitmask of
-- VkBuildAccelerationStructureFlagBitsNV
--
-- = Description
--
-- 'VkBuildAccelerationStructureFlagsNV' is a bitmask type for setting a
-- mask of zero or more 'VkBuildAccelerationStructureFlagBitsNV'.
--
-- = See Also
--
-- 'VkAccelerationStructureInfoNV',
-- 'VkBuildAccelerationStructureFlagBitsNV'
type VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagBitsNV

-- ** VkCopyAccelerationStructureModeNV

-- | VkCopyAccelerationStructureModeNV - Acceleration structure copy mode
--
-- = See Also
--
-- 'vkCmdCopyAccelerationStructureNV'
newtype VkCopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkCopyAccelerationStructureModeNV where
  showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
  showsPrec _ VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = showString "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
  showsPrec p (VkCopyAccelerationStructureModeNV x) = showParen (p >= 11) (showString "VkCopyAccelerationStructureModeNV " . showsPrec 11 x)

instance Read VkCopyAccelerationStructureModeNV where
  readPrec = parens ( choose [ ("VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV",   pure VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV)
                             , ("VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV", pure VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCopyAccelerationStructureModeNV")
                        v <- step readPrec
                        pure (VkCopyAccelerationStructureModeNV v)
                        )
                    )

-- | 'VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV' creates a direct copy of
-- the acceleration structure specified in @src@ into the one specified by
-- @dst@. The @dst@ acceleration structure /must/ have been created with
-- the same parameters as @src@.
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV :: VkCopyAccelerationStructureModeNV
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = VkCopyAccelerationStructureModeNV 0

-- | 'VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' creates a more compact
-- version of an acceleration structure @src@ into @dst@. The acceleration
-- structure @dst@ /must/ have been created with a @compactedSize@
-- corresponding to the one returned by
-- 'vkCmdWriteAccelerationStructuresPropertiesNV' after the build of the
-- acceleration structure specified by @src@.
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: VkCopyAccelerationStructureModeNV
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = VkCopyAccelerationStructureModeNV 1

-- | VkGeometryAABBNV - Structure specifying axis-aligned bounding box
-- geometry in a bottom-level acceleration structure
--
-- = Description
--
-- The AABB data in memory is six 32-bit floats consisting of the minimum
-- x, y, and z values followed by the maximum x, y, and z values.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @aabbData@
--
-- -   @offset@ /must/ be a multiple of @8@
--
-- -   @stride@ /must/ be a multiple of @8@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @aabbData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @aabbData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkGeometryDataNV', 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkGeometryAABBNV = VkGeometryAABBNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @aabbData@ is the buffer containing axis-aligned bounding box data.
  vkAabbData :: VkBuffer
  , -- | @numAABBs@ is the number of AABBs in this geometry.
  vkNumAABBs :: Word32
  , -- | @stride@ is the stride in bytes between AABBs in @aabbData@.
  vkStride :: Word32
  , -- | @offset@ is the offset in bytes of the first AABB in @aabbData@.
  vkOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkGeometryAABBNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkGeometryAABBNV <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 28)
                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGeometryAABBNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGeometryAABBNV))
                *> poke (ptr `plusPtr` 16) (vkAabbData (poked :: VkGeometryAABBNV))
                *> poke (ptr `plusPtr` 24) (vkNumAABBs (poked :: VkGeometryAABBNV))
                *> poke (ptr `plusPtr` 28) (vkStride (poked :: VkGeometryAABBNV))
                *> poke (ptr `plusPtr` 32) (vkOffset (poked :: VkGeometryAABBNV))

instance Zero VkGeometryAABBNV where
  zero = VkGeometryAABBNV VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV
                          zero
                          zero
                          zero
                          zero
                          zero

-- | VkGeometryDataNV - Structure specifying geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkGeometryAABBNV', 'VkGeometryNV', 'VkGeometryTrianglesNV'
data VkGeometryDataNV = VkGeometryDataNV
  { -- | @triangles@ /must/ be a valid 'VkGeometryTrianglesNV' structure
  vkTriangles :: VkGeometryTrianglesNV
  , -- | @aabbs@ /must/ be a valid 'VkGeometryAABBNV' structure
  vkAabbs :: VkGeometryAABBNV
  }
  deriving (Eq, Show)

instance Storable VkGeometryDataNV where
  sizeOf ~_ = 136
  alignment ~_ = 8
  peek ptr = VkGeometryDataNV <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 96)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTriangles (poked :: VkGeometryDataNV))
                *> poke (ptr `plusPtr` 96) (vkAabbs (poked :: VkGeometryDataNV))

instance Zero VkGeometryDataNV where
  zero = VkGeometryDataNV zero
                          zero

-- ** VkGeometryFlagBitsNV

-- | VkGeometryFlagBitsNV - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- 'VkGeometryFlagsNV'
newtype VkGeometryFlagBitsNV = VkGeometryFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkGeometryFlagBitsNV where
  showsPrec _ VK_GEOMETRY_OPAQUE_BIT_NV = showString "VK_GEOMETRY_OPAQUE_BIT_NV"
  showsPrec _ VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = showString "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
  showsPrec p (VkGeometryFlagBitsNV x) = showParen (p >= 11) (showString "VkGeometryFlagBitsNV " . showsPrec 11 x)

instance Read VkGeometryFlagBitsNV where
  readPrec = parens ( choose [ ("VK_GEOMETRY_OPAQUE_BIT_NV",                          pure VK_GEOMETRY_OPAQUE_BIT_NV)
                             , ("VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV", pure VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkGeometryFlagBitsNV")
                        v <- step readPrec
                        pure (VkGeometryFlagBitsNV v)
                        )
                    )

-- | 'VK_GEOMETRY_OPAQUE_BIT_NV' indicates that this geometry does not invoke
-- the any-hit shaders even if present in a hit group.
pattern VK_GEOMETRY_OPAQUE_BIT_NV :: VkGeometryFlagBitsNV
pattern VK_GEOMETRY_OPAQUE_BIT_NV = VkGeometryFlagBitsNV 0x00000001

-- | 'VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV' indicates that the
-- implementation /must/ only call the any-hit shader a single time for
-- each primitive in this geometry. If this bit is absent an implementation
-- /may/ invoke the any-hit shader more than once for this geometry.
pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: VkGeometryFlagBitsNV
pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = VkGeometryFlagBitsNV 0x00000002

-- | VkGeometryFlagsNV - Bitmask of VkGeometryFlagBitsNV
--
-- = Description
--
-- 'VkGeometryFlagsNV' is a bitmask type for setting a mask of zero or more
-- 'VkGeometryFlagBitsNV'.
--
-- = See Also
--
-- 'VkGeometryFlagBitsNV', 'VkGeometryNV'
type VkGeometryFlagsNV = VkGeometryFlagBitsNV

-- ** VkGeometryInstanceFlagBitsNV

-- | VkGeometryInstanceFlagBitsNV - Instance flag bits
--
-- = Description
--
-- 'VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV' and
-- 'VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV' /must/ not be used in the
-- same flag.
--
-- = See Also
--
-- 'VkGeometryInstanceFlagsNV'
newtype VkGeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkGeometryInstanceFlagBitsNV where
  showsPrec _ VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = showString "VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
  showsPrec _ VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = showString "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
  showsPrec _ VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = showString "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
  showsPrec _ VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = showString "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
  showsPrec p (VkGeometryInstanceFlagBitsNV x) = showParen (p >= 11) (showString "VkGeometryInstanceFlagBitsNV " . showsPrec 11 x)

instance Read VkGeometryInstanceFlagBitsNV where
  readPrec = parens ( choose [ ("VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV",           pure VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV)
                             , ("VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV", pure VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV)
                             , ("VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV",                    pure VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV)
                             , ("VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV",                 pure VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkGeometryInstanceFlagBitsNV")
                        v <- step readPrec
                        pure (VkGeometryInstanceFlagBitsNV v)
                        )
                    )

-- | 'VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV' disables face
-- culling for this instance.
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000001

-- | 'VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV' indicates
-- that the front face of the triangle for culling purposes is the face
-- that is counter clockwise in object space relative to the ray origin.
-- Because the facing is determined in object space, an instance transform
-- matrix does not change the winding, but a geometry transform does.
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000002

-- | 'VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV' causes this instance to act
-- as though 'VK_GEOMETRY_OPAQUE_BIT_NV' were specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- ray flag @gl_RayFlagsNoOpaqueNV@.
pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000004

-- | 'VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV' causes this instance to
-- act as though 'VK_GEOMETRY_OPAQUE_BIT_NV' were not specified on all
-- geometries referenced by this instance. This behavior /can/ be
-- overridden by the ray flag @gl_RayFlagsOpaqueNV@.
pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000008

-- | VkGeometryInstanceFlagsNV - Bitmask of VkGeometryInstanceFlagBitsNV
--
-- = Description
--
-- 'VkGeometryInstanceFlagsNV' is a bitmask type for setting a mask of zero
-- or more 'VkGeometryInstanceFlagBitsNV'.
--
-- = See Also
--
-- 'VkGeometryInstanceFlagBitsNV'
type VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagBitsNV

-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkAccelerationStructureInfoNV', 'VkGeometryDataNV',
-- 'VkGeometryFlagsNV', 'VkGeometryTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkGeometryNV = VkGeometryNV
  { -- | @sType@ /must/ be 'VK_STRUCTURE_TYPE_GEOMETRY_NV'
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @geometryType@ /must/ be a valid 'VkGeometryTypeNV' value
  vkGeometryType :: VkGeometryTypeNV
  , -- | @geometry@ /must/ be a valid 'VkGeometryDataNV' structure
  vkGeometry :: VkGeometryDataNV
  , -- | @flags@ /must/ be a valid combination of 'VkGeometryFlagBitsNV' values
  vkFlags :: VkGeometryFlagsNV
  }
  deriving (Eq, Show)

instance Storable VkGeometryNV where
  sizeOf ~_ = 168
  alignment ~_ = 8
  peek ptr = VkGeometryNV <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 24)
                          <*> peek (ptr `plusPtr` 160)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGeometryNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGeometryNV))
                *> poke (ptr `plusPtr` 16) (vkGeometryType (poked :: VkGeometryNV))
                *> poke (ptr `plusPtr` 24) (vkGeometry (poked :: VkGeometryNV))
                *> poke (ptr `plusPtr` 160) (vkFlags (poked :: VkGeometryNV))

instance Zero VkGeometryNV where
  zero = VkGeometryNV VK_STRUCTURE_TYPE_GEOMETRY_NV
                      zero
                      zero
                      zero
                      zero

-- | VkGeometryTrianglesNV - Structure specifying a triangle geometry in a
-- bottom-level acceleration structure
--
-- = Description
--
-- If @indexType@ is 'VK_INDEX_TYPE_NONE_NV', then this structure describes
-- a set of triangles determined by @vertexCount@. Otherwise, this
-- structure describes a set of indexed triangles determined by
-- @indexCount@.
--
-- == Valid Usage
--
-- -   @vertexOffset@ /must/ be less than the size of @vertexData@
--
-- -   @vertexOffset@ /must/ be a multiple of the component size of
--     @vertexFormat@
--
-- -   @vertexFormat@ /must/ be one of
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SNORM', or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SNORM'
--
-- -   @indexOffset@ /must/ be less than the size of @indexData@
--
-- -   @indexOffset@ /must/ be a multiple of the element size of
--     @indexType@
--
-- -   @indexType@ /must/ be
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT16',
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32',
--     or 'VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' if @indexType@
--     is 'VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle if
--     @indexType@ is not 'VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexCount@ /must/ be @0@ if @indexType@ is 'VK_INDEX_TYPE_NONE_NV'
--
-- -   @transformOffset@ /must/ be less than the size of @transformData@
--
-- -   @transformOffset@ /must/ be a multiple of @16@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @vertexData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @vertexData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @vertexFormat@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' value
--
-- -   If @indexData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @indexData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @indexType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' value
--
-- -   If @transformData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @transformData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   Each of @indexData@, @transformData@, and @vertexData@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat', 'VkGeometryDataNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkGeometryTrianglesNV = VkGeometryTrianglesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @vertexData@ is the buffer containing vertex data for this geometry.
  vkVertexData :: VkBuffer
  , -- | @vertexOffset@ is the offset in bytes within @vertexData@ containing
  -- vertex data for this geometry.
  vkVertexOffset :: VkDeviceSize
  , -- | @vertexCount@ is the number of valid vertices.
  vkVertexCount :: Word32
  , -- | @vertexStride@ is the stride in bytes between each vertex.
  vkVertexStride :: VkDeviceSize
  , -- | @vertexFormat@ is the format of each vertex element.
  vkVertexFormat :: VkFormat
  , -- | @indexData@ is the buffer containing index data for this geometry.
  vkIndexData :: VkBuffer
  , -- | @indexOffset@ is the offset in bytes within @indexData@ containing index
  -- data for this geometry.
  vkIndexOffset :: VkDeviceSize
  , -- | @indexCount@ is the number of indices to include in this geometry.
  vkIndexCount :: Word32
  , -- | @indexType@ is the format of each index.
  vkIndexType :: VkIndexType
  , -- | @transformData@ is a buffer containing optional reference to an array of
  -- 32-bit floats representing a 3x4 row major affine transformation matrix
  -- for this geometry.
  vkTransformData :: VkBuffer
  , -- | @transformOffset@ is the offset in bytes in @transformData@ of the
  -- transform information described above.
  vkTransformOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkGeometryTrianglesNV where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkGeometryTrianglesNV <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 56)
                                   <*> peek (ptr `plusPtr` 64)
                                   <*> peek (ptr `plusPtr` 72)
                                   <*> peek (ptr `plusPtr` 76)
                                   <*> peek (ptr `plusPtr` 80)
                                   <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 16) (vkVertexData (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 24) (vkVertexOffset (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 32) (vkVertexCount (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 40) (vkVertexStride (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 48) (vkVertexFormat (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 56) (vkIndexData (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 64) (vkIndexOffset (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 72) (vkIndexCount (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 76) (vkIndexType (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 80) (vkTransformData (poked :: VkGeometryTrianglesNV))
                *> poke (ptr `plusPtr` 88) (vkTransformOffset (poked :: VkGeometryTrianglesNV))

instance Zero VkGeometryTrianglesNV where
  zero = VkGeometryTrianglesNV VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
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

-- ** VkGeometryTypeNV

-- | VkGeometryTypeNV - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- 'VkGeometryNV'
newtype VkGeometryTypeNV = VkGeometryTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkGeometryTypeNV where
  showsPrec _ VK_GEOMETRY_TYPE_TRIANGLES_NV = showString "VK_GEOMETRY_TYPE_TRIANGLES_NV"
  showsPrec _ VK_GEOMETRY_TYPE_AABBS_NV = showString "VK_GEOMETRY_TYPE_AABBS_NV"
  showsPrec p (VkGeometryTypeNV x) = showParen (p >= 11) (showString "VkGeometryTypeNV " . showsPrec 11 x)

instance Read VkGeometryTypeNV where
  readPrec = parens ( choose [ ("VK_GEOMETRY_TYPE_TRIANGLES_NV", pure VK_GEOMETRY_TYPE_TRIANGLES_NV)
                             , ("VK_GEOMETRY_TYPE_AABBS_NV",     pure VK_GEOMETRY_TYPE_AABBS_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkGeometryTypeNV")
                        v <- step readPrec
                        pure (VkGeometryTypeNV v)
                        )
                    )

-- | 'VK_GEOMETRY_TYPE_TRIANGLES_NV' indicates that the @triangles@ of
-- 'VkGeometryDataNV' contains valid data.
pattern VK_GEOMETRY_TYPE_TRIANGLES_NV :: VkGeometryTypeNV
pattern VK_GEOMETRY_TYPE_TRIANGLES_NV = VkGeometryTypeNV 0

-- | 'VK_GEOMETRY_TYPE_AABBS_NV' indicates that the @aabbs@ of
-- 'VkGeometryDataNV' contains valid data.
pattern VK_GEOMETRY_TYPE_AABBS_NV :: VkGeometryTypeNV
pattern VK_GEOMETRY_TYPE_AABBS_NV = VkGeometryTypeNV 1

-- | VkPhysicalDeviceRayTracingPropertiesNV - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the 'VkPhysicalDeviceRayTracingPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceRayTracingPropertiesNV = VkPhysicalDeviceRayTracingPropertiesNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @shaderGroupHandleSize@ size in bytes of the shader header.
  vkShaderGroupHandleSize :: Word32
  , -- | @maxRecursionDepth@ is the maximum number of levels of recursion allowed
  -- in a trace command.
  vkMaxRecursionDepth :: Word32
  , -- | @maxShaderGroupStride@ is the maximum stride in bytes allowed between
  -- shader groups in the SBT.
  vkMaxShaderGroupStride :: Word32
  , -- | @shaderGroupBaseAlignment@ is the required alignment in bytes for the
  -- base of the SBTs.
  vkShaderGroupBaseAlignment :: Word32
  , -- | @maxGeometryCount@ is the maximum number of geometries in the bottom
  -- level acceleration structure.
  vkMaxGeometryCount :: Word64
  , -- | @maxInstanceCount@ is the maximum number of instances in the top level
  -- acceleration structure.
  vkMaxInstanceCount :: Word64
  , -- | @maxTriangleCount@ is the maximum number of triangles in all geometries
  -- in the bottom level acceleration structure.
  vkMaxTriangleCount :: Word64
  , -- | @maxDescriptorSetAccelerationStructures@ is the maximum number of
  -- acceleration structure descriptors that are allowed in a descriptor set.
  vkMaxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceRayTracingPropertiesNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceRayTracingPropertiesNV <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkShaderGroupHandleSize (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 20) (vkMaxRecursionDepth (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 24) (vkMaxShaderGroupStride (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 28) (vkShaderGroupBaseAlignment (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 32) (vkMaxGeometryCount (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkMaxInstanceCount (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 48) (vkMaxTriangleCount (poked :: VkPhysicalDeviceRayTracingPropertiesNV))
                *> poke (ptr `plusPtr` 56) (vkMaxDescriptorSetAccelerationStructures (poked :: VkPhysicalDeviceRayTracingPropertiesNV))

instance Zero VkPhysicalDeviceRayTracingPropertiesNV where
  zero = VkPhysicalDeviceRayTracingPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero

-- | VkRayTracingPipelineCreateInfoNV - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be @-1@
--
-- -   The @stage@ member of one element of @pStages@ /must/ be
--     'VK_SHADER_STAGE_RAYGEN_BIT_NV'
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @maxRecursionDepth@ /must/ be less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxRecursionDepth@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlagBits' values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'
--     structures
--
-- -   @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid 'VkRayTracingShaderGroupCreateInfoNV' structures
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   @groupCount@ /must/ be greater than @0@
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'VkRayTracingShaderGroupCreateInfoNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateRayTracingPipelinesNV'
data VkRayTracingPipelineCreateInfoNV = VkRayTracingPipelineCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlagBits' specifying
  -- how the pipeline will be generated.
  vkFlags :: VkPipelineCreateFlags
  , -- | @stageCount@ is the number of entries in the @pStages@ array.
  vkStageCount :: Word32
  , -- | @pStages@ is an array of size @stageCount@ structures of type
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'
  -- describing the set of the shader stages to be included in the ray
  -- tracing pipeline.
  vkPStages :: Ptr VkPipelineShaderStageCreateInfo
  , -- | @groupCount@ is the number of entries in the @pGroups@ array.
  vkGroupCount :: Word32
  , -- | @pGroups@ is an array of size @groupCount@ structures of type
  -- 'VkRayTracingShaderGroupCreateInfoNV' describing the set of the shader
  -- stages to be included in each shader group in the ray tracing pipeline.
  vkPGroups :: Ptr VkRayTracingShaderGroupCreateInfoNV
  , -- | @maxRecursionDepth@ is the maximum recursion that will be called from
  -- this pipeline.
  vkMaxRecursionDepth :: Word32
  , -- | @layout@ is the description of binding locations used by both the
  -- pipeline and descriptor sets used with the pipeline.
  vkLayout :: VkPipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from.
  vkBasePipelineHandle :: VkPipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
  -- as a pipeline to derive from.
  vkBasePipelineIndex :: Int32
  }
  deriving (Eq, Show)

instance Storable VkRayTracingPipelineCreateInfoNV where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkRayTracingPipelineCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
                                              <*> peek (ptr `plusPtr` 40)
                                              <*> peek (ptr `plusPtr` 48)
                                              <*> peek (ptr `plusPtr` 56)
                                              <*> peek (ptr `plusPtr` 64)
                                              <*> peek (ptr `plusPtr` 72)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkStageCount (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPStages (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 32) (vkGroupCount (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 40) (vkPGroups (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 48) (vkMaxRecursionDepth (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 56) (vkLayout (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 64) (vkBasePipelineHandle (poked :: VkRayTracingPipelineCreateInfoNV))
                *> poke (ptr `plusPtr` 72) (vkBasePipelineIndex (poked :: VkRayTracingPipelineCreateInfoNV))

instance Zero VkRayTracingPipelineCreateInfoNV where
  zero = VkRayTracingPipelineCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
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

-- | VkRayTracingShaderGroupCreateInfoNV - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   If @type@ is 'VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @generalShader@ /must/ be a valid index into @pStages@ referring to
--     a shader of 'VK_SHADER_STAGE_RAYGEN_BIT_NV',
--     'VK_SHADER_STAGE_MISS_BIT_NV', or 'VK_SHADER_STAGE_CALLABLE_BIT_NV'
--
-- -   If @type@ is 'VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @closestHitShader@, @anyHitShader@, and @intersectionShader@ /must/
--     be 'VK_SHADER_UNUSED_NV'
--
-- -   If @type@ is
--     'VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' then
--     @intersectionShader@ /must/ be a valid index into @pStages@
--     referring to a shader of 'VK_SHADER_STAGE_INTERSECTION_BIT_NV'
--
-- -   If @type@ is
--     'VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' then
--     @intersectionShader@ /must/ be 'VK_SHADER_UNUSED_NV'
--
-- -   @closestHitShader@ /must/ be either 'VK_SHADER_UNUSED_NV' or a valid
--     index into @pStages@ referring to a shader of
--     'VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
-- -   @anyHitShader@ /must/ be either 'VK_SHADER_UNUSED_NV' or a valid
--     index into @pStages@ referring to a shader of
--     'VK_SHADER_STAGE_ANY_HIT_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'VkRayTracingShaderGroupTypeNV' value
--
-- = See Also
--
-- 'VkRayTracingPipelineCreateInfoNV', 'VkRayTracingShaderGroupTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkRayTracingShaderGroupCreateInfoNV = VkRayTracingShaderGroupCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @type@ is the type of hit group specified in this structure.
  vkType :: VkRayTracingShaderGroupTypeNV
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
  -- shader from 'VkRayTracingPipelineCreateInfoNV'::@pStages@ in the group
  -- if the shader group has @type@ of
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' and 'VK_SHADER_UNUSED_NV'
  -- otherwise.
  vkGeneralShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
  -- 'VkRayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
  -- group has @type@ of
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
  -- 'VK_SHADER_UNUSED_NV' otherwise.
  vkClosestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
  -- 'VkRayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
  -- group has @type@ of
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
  -- 'VK_SHADER_UNUSED_NV' otherwise.
  vkAnyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
  -- 'VkRayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
  -- group has @type@ of
  -- 'VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
  -- 'VK_SHADER_UNUSED_NV' otherwise.
  vkIntersectionShader :: Word32
  }
  deriving (Eq, Show)

instance Storable VkRayTracingShaderGroupCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkRayTracingShaderGroupCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkType (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkGeneralShader (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkClosestHitShader (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 28) (vkAnyHitShader (poked :: VkRayTracingShaderGroupCreateInfoNV))
                *> poke (ptr `plusPtr` 32) (vkIntersectionShader (poked :: VkRayTracingShaderGroupCreateInfoNV))

instance Zero VkRayTracingShaderGroupCreateInfoNV where
  zero = VkRayTracingShaderGroupCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
                                             zero
                                             zero
                                             zero
                                             zero
                                             zero
                                             zero

-- ** VkRayTracingShaderGroupTypeNV

-- | VkRayTracingShaderGroupTypeNV - Shader group types
--
-- = Description
--
-- __Note__
--
-- For current group types, the hit group type could be inferred from the
-- presence or absence of the intersection shader, but we provide the type
-- explicitly for future hit groups that do not have that property.
--
-- = See Also
--
-- 'VkRayTracingShaderGroupCreateInfoNV'
newtype VkRayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkRayTracingShaderGroupTypeNV where
  showsPrec _ VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = showString "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
  showsPrec _ VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = showString "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
  showsPrec _ VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = showString "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
  showsPrec p (VkRayTracingShaderGroupTypeNV x) = showParen (p >= 11) (showString "VkRayTracingShaderGroupTypeNV " . showsPrec 11 x)

instance Read VkRayTracingShaderGroupTypeNV where
  readPrec = parens ( choose [ ("VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV",              pure VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV)
                             , ("VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV",  pure VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV)
                             , ("VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV", pure VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkRayTracingShaderGroupTypeNV")
                        v <- step readPrec
                        pure (VkRayTracingShaderGroupTypeNV v)
                        )
                    )

-- | 'VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' indicates a shader group
-- with a single 'VK_SHADER_STAGE_RAYGEN_BIT_NV',
-- 'VK_SHADER_STAGE_MISS_BIT_NV', or 'VK_SHADER_STAGE_CALLABLE_BIT_NV'
-- shader in it.
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = VkRayTracingShaderGroupTypeNV 0

-- | 'VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' specifies a
-- shader group that only hits triangles and /must/ not contain an
-- intersection shader, only closest hit and any-hit.
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = VkRayTracingShaderGroupTypeNV 1

-- | 'VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' specifies a
-- shader group that only intersects with custom geometry and /must/
-- contain an intersection shader and /may/ contain closest hit and any-hit
-- shaders.
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = VkRayTracingShaderGroupTypeNV 2

-- | VkWriteDescriptorSetAccelerationStructureNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkWriteDescriptorSetAccelerationStructureNV = VkWriteDescriptorSetAccelerationStructureNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV'
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @accelerationStructureCount@ /must/ be greater than @0@
  vkAccelerationStructureCount :: Word32
  , -- | @pAccelerationStructures@ /must/ be a valid pointer to an array of
  -- @accelerationStructureCount@ valid 'VkAccelerationStructureNV' handles
  vkPAccelerationStructures :: Ptr VkAccelerationStructureNV
  }
  deriving (Eq, Show)

instance Storable VkWriteDescriptorSetAccelerationStructureNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkWriteDescriptorSetAccelerationStructureNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWriteDescriptorSetAccelerationStructureNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWriteDescriptorSetAccelerationStructureNV))
                *> poke (ptr `plusPtr` 16) (vkAccelerationStructureCount (poked :: VkWriteDescriptorSetAccelerationStructureNV))
                *> poke (ptr `plusPtr` 24) (vkPAccelerationStructures (poked :: VkWriteDescriptorSetAccelerationStructureNV))

instance Zero VkWriteDescriptorSetAccelerationStructureNV where
  zero = VkWriteDescriptorSetAccelerationStructureNV VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
                                                     zero
                                                     zero
                                                     zero

-- | vkBindAccelerationStructureMemoryNV - Bind acceleration structure memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the acceleration structures
--     and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'VkBindAccelerationStructureMemoryInfoNV', describing images and
--     memory to bind.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkBindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindAccelerationStructureMemoryNV" vkBindAccelerationStructureMemoryNV :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult
#else
vkBindAccelerationStructureMemoryNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult
vkBindAccelerationStructureMemoryNV deviceCmds = mkVkBindAccelerationStructureMemoryNV (pVkBindAccelerationStructureMemoryNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindAccelerationStructureMemoryNV
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult)
#endif

type FN_vkBindAccelerationStructureMemoryNV = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult
type PFN_vkBindAccelerationStructureMemoryNV = FunPtr FN_vkBindAccelerationStructureMemoryNV

-- | vkCmdBuildAccelerationStructureNV - Build an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ contains the shared information for the acceleration
--     structure’s structure.
--
-- -   @instanceData@ is the buffer containing instance data that will be
--     used to build the acceleration structure as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#acceleration-structure-instance Accelerator structure instances.>
--     This parameter /must/ be @NULL@ for bottom level acceleration
--     structures.
--
-- -   @instanceOffset@ is the offset in bytes (relative to the start of
--     @instanceData@) at which the instance data is located.
--
-- -   @update@ specifies whether to update the @dst@ acceleration
--     structure with the data in @src@.
--
-- -   @dst@ points to the target acceleration structure for the build.
--
-- -   @src@ points to an existing acceleration structure that is to be
--     used to update the @dst@ acceleration structure.
--
-- -   @scratch@ is the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that will be
--     used as scratch memory for the build.
--
-- -   @scratchOffset@ is the offset in bytes relative to the start of
--     @scratch@ that will be used as a scratch memory.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @dst@ /must/ have been created with compatible
--     'VkAccelerationStructureInfoNV' where
--     'VkAccelerationStructureInfoNV'::@type@ and
--     'VkAccelerationStructureInfoNV'::@flags@ are identical,
--     'VkAccelerationStructureInfoNV'::@instanceCount@ and
--     'VkAccelerationStructureInfoNV'::@geometryCount@ for @dst@ are
--     greater than or equal to the build size and each geometry in
--     'VkAccelerationStructureInfoNV'::@pGeometries@ for @dst@ has greater
--     than or equal to the number of vertices, indices, and AABBs.
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @src@ /must/
--     not be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @src@ /must/
--     have been built before with
--     'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' set in
--     'VkAccelerationStructureInfoNV'::@flags@
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', The @size@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'vkGetAccelerationStructureMemoryRequirementsNV' with
--     'VkAccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'VkAccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', The @size@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'vkGetAccelerationStructureMemoryRequirementsNV' with
--     'VkAccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'VkAccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'VkAccelerationStructureInfoNV' structure
--
-- -   If @instanceData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @instanceData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @dst@ /must/ be a valid 'VkAccelerationStructureNV' handle
--
-- -   If @src@ is not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @src@ /must/ be a valid 'VkAccelerationStructureNV' handle
--
-- -   @scratch@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @commandBuffer@, @dst@, @instanceData@, @scratch@, and @src@
--     that are valid handles /must/ have been created, allocated, or
--     retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkAccelerationStructureInfoNV', 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBuildAccelerationStructureNV" vkCmdBuildAccelerationStructureNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()
#else
vkCmdBuildAccelerationStructureNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()
vkCmdBuildAccelerationStructureNV deviceCmds = mkVkCmdBuildAccelerationStructureNV (pVkCmdBuildAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ())
#endif

type FN_vkCmdBuildAccelerationStructureNV = ("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()
type PFN_vkCmdBuildAccelerationStructureNV = FunPtr FN_vkCmdBuildAccelerationStructureNV

-- | vkCmdCopyAccelerationStructureNV - Copy an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @dst@ points to the target acceleration structure for the copy.
--
-- -   @src@ points to the source acceleration structure for the copy.
--
-- -   @mode@ is a 'VkCopyAccelerationStructureModeNV' value that specifies
--     additional operations to perform during the copy.
--
-- == Valid Usage
--
-- -   @mode@ /must/ be 'VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' or
--     'VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV'
--
-- -   @src@ /must/ have been built with
--     'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' if @mode@
--     is 'VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @dst@ /must/ be a valid 'VkAccelerationStructureNV' handle
--
-- -   @src@ /must/ be a valid 'VkAccelerationStructureNV' handle
--
-- -   @mode@ /must/ be a valid 'VkCopyAccelerationStructureModeNV' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @commandBuffer@, @dst@, and @src@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'VkCopyAccelerationStructureModeNV'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyAccelerationStructureNV" vkCmdCopyAccelerationStructureNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()
#else
vkCmdCopyAccelerationStructureNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()
vkCmdCopyAccelerationStructureNV deviceCmds = mkVkCmdCopyAccelerationStructureNV (pVkCmdCopyAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ())
#endif

type FN_vkCmdCopyAccelerationStructureNV = ("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()
type PFN_vkCmdCopyAccelerationStructureNV = FunPtr FN_vkCmdCopyAccelerationStructureNV

-- | vkCmdTraceRaysNV - Initialize a ray tracing dispatch
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @raygenShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the ray generation shader stage.
--
-- -   @raygenShaderBindingOffset@ is the offset in bytes (relative to
--     @raygenShaderBindingTableBuffer@) of the ray generation shader being
--     used for the trace.
--
-- -   @missShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the miss shader stage.
--
-- -   @missShaderBindingOffset@ is the offset in bytes (relative to
--     @missShaderBindingTableBuffer@) of the miss shader being used for
--     the trace.
--
-- -   @missShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @missShaderBindingTableBuffer@.
--
-- -   @hitShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the hit shader stages.
--
-- -   @hitShaderBindingOffset@ is the offset in bytes (relative to
--     @hitShaderBindingTableBuffer@) of the hit shader group being used
--     for the trace.
--
-- -   @hitShaderBindingStride@ is the size in bytes of each shader binding
--     table record in @hitShaderBindingTableBuffer@.
--
-- -   @callableShaderBindingTableBuffer@ is the buffer object that holds
--     the shader binding table data for the callable shader stage.
--
-- -   @callableShaderBindingOffset@ is the offset in bytes (relative to
--     @callableShaderBindingTableBuffer@) of the callable shader being
--     used for the trace.
--
-- -   @callableShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @callableShaderBindingTableBuffer@.
--
-- -   @width@ is the width of the ray trace query dimensions.
--
-- -   @height@ is height of the ray trace query dimensions.
--
-- -   @depth@ is depth of the ray trace query dimensions.
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be a less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be a less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be a less than or equal to
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @depth@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @raygenShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @missShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @missShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @hitShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @hitShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @callableShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @callableShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @callableShaderBindingTableBuffer@, @commandBuffer@,
--     @hitShaderBindingTableBuffer@, @missShaderBindingTableBuffer@, and
--     @raygenShaderBindingTableBuffer@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdTraceRaysNV" vkCmdTraceRaysNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()
#else
vkCmdTraceRaysNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()
vkCmdTraceRaysNV deviceCmds = mkVkCmdTraceRaysNV (pVkCmdTraceRaysNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
#endif

type FN_vkCmdTraceRaysNV = ("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()
type PFN_vkCmdTraceRaysNV = FunPtr FN_vkCmdTraceRaysNV

-- | vkCmdWriteAccelerationStructuresPropertiesNV - Write acceleration
-- structure result parameters to query results.
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @accelerationStructureCount@ is the count of acceleration structures
--     for which to query the property.
--
-- -   @pAccelerationStructures@ points to an array of existing previously
--     built acceleration structures.
--
-- -   @queryType@ is a 'Graphics.Vulkan.C.Core10.Query.VkQueryType' value
--     specifying the type of queries managed by the pool.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @firstQuery@ is the first query index within the query pool that
--     will contain the @accelerationStructureCount@ number of results.
--
-- == Valid Usage
--
-- -   @queryType@ /must/ be
--     'VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   @queryPool@ /must/ have been created with a @queryType@ matching
--     @queryType@
--
-- -   The queries identified by @queryPool@ and @firstQuery@ /must/ be
--     /unavailable/
--
-- -   All acceleration structures in @accelerationStructures@ /must/ have
--     been built with
--     'VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' if
--     @queryType@ is
--     'VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid 'VkAccelerationStructureNV'
--     handles
--
-- -   @queryType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryType' value
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @queryPool@, and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryType'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteAccelerationStructuresPropertiesNV" vkCmdWriteAccelerationStructuresPropertiesNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()
#else
vkCmdWriteAccelerationStructuresPropertiesNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()
vkCmdWriteAccelerationStructuresPropertiesNV deviceCmds = mkVkCmdWriteAccelerationStructuresPropertiesNV (pVkCmdWriteAccelerationStructuresPropertiesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ())
#endif

type FN_vkCmdWriteAccelerationStructuresPropertiesNV = ("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()
type PFN_vkCmdWriteAccelerationStructuresPropertiesNV = FunPtr FN_vkCmdWriteAccelerationStructuresPropertiesNV

-- | vkCompileDeferredNV - Deferred compilation of shaders
--
-- = Parameters
--
-- -   @device@ is the logical device that contains the ray tracing
--     pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object that contains the
--     shaders.
--
-- -   @shader@ is the index of the shader to compile.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCompileDeferredNV" vkCompileDeferredNV :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult
#else
vkCompileDeferredNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult
vkCompileDeferredNV deviceCmds = mkVkCompileDeferredNV (pVkCompileDeferredNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult)
#endif

type FN_vkCompileDeferredNV = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult
type PFN_vkCompileDeferredNV = FunPtr FN_vkCompileDeferredNV

-- | vkCreateAccelerationStructureNV - Create a new acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkAccelerationStructureCreateInfoNV' structure containing
--     parameters affecting creation of the acceleration structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pAccelerationStructure@ points to a 'VkAccelerationStructureNV'
--     handle in which the resulting acceleration structure object is
--     returned.
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape” as specified by the
-- information in 'VkAccelerationStructureInfoNV' and @compactedSize@ in
-- @pCreateInfo@. Populating the data in the object after allocating and
-- binding memory is done with 'vkCmdBuildAccelerationStructureNV' and
-- 'vkCmdCopyAccelerationStructureNV'.
--
-- Acceleration structure creation uses the count and type information from
-- the geometries, but does not use the data references in the structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkAccelerationStructureCreateInfoNV' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pAccelerationStructure@ /must/ be a valid pointer to a
--     'VkAccelerationStructureNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'VkAccelerationStructureCreateInfoNV', 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateAccelerationStructureNV" vkCreateAccelerationStructureNV :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult
#else
vkCreateAccelerationStructureNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult
vkCreateAccelerationStructureNV deviceCmds = mkVkCreateAccelerationStructureNV (pVkCreateAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureNV
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult)
#endif

type FN_vkCreateAccelerationStructureNV = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult
type PFN_vkCreateAccelerationStructureNV = FunPtr FN_vkCreateAccelerationStructureNV

-- | vkCreateRayTracingPipelinesNV - Creates a new ray tracing pipeline
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the ray tracing
--     pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled, or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of 'VkRayTracingPipelineCreateInfoNV'
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting ray
--     tracing pipeline objects are returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @pipelineCache@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'VkRayTracingPipelineCreateInfoNV'
--     structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--     handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader.VK_ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache',
-- 'VkRayTracingPipelineCreateInfoNV'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateRayTracingPipelinesNV" vkCreateRayTracingPipelinesNV :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
#else
vkCreateRayTracingPipelinesNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
vkCreateRayTracingPipelinesNV deviceCmds = mkVkCreateRayTracingPipelinesNV (pVkCreateRayTracingPipelinesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
#endif

type FN_vkCreateRayTracingPipelinesNV = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateRayTracingPipelinesNV = FunPtr FN_vkCreateRayTracingPipelinesNV

-- | vkDestroyAccelerationStructureNV - Destroy an acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer.
--
-- -   @accelerationStructure@ is the acceleration structure to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @accelerationStructure@ /must/
--     have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @accelerationStructure@ was created, a compatible
--     set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @accelerationStructure@ /must/ be a valid
--     'VkAccelerationStructureNV' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @accelerationStructure@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyAccelerationStructureNV" vkDestroyAccelerationStructureNV :: ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyAccelerationStructureNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyAccelerationStructureNV deviceCmds = mkVkDestroyAccelerationStructureNV (pVkDestroyAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureNV
  :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyAccelerationStructureNV = ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyAccelerationStructureNV = FunPtr FN_vkDestroyAccelerationStructureNV

-- | vkGetAccelerationStructureHandleNV - Get opaque acceleration structure
-- handle
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the acceleration
--     structures.
--
-- -   @accelerationStructure@ is the acceleration structure.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetAccelerationStructureHandleNV" vkGetAccelerationStructureHandleNV :: ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
#else
vkGetAccelerationStructureHandleNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
vkGetAccelerationStructureHandleNV deviceCmds = mkVkGetAccelerationStructureHandleNV (pVkGetAccelerationStructureHandleNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureHandleNV
  :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
#endif

type FN_vkGetAccelerationStructureHandleNV = ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetAccelerationStructureHandleNV = FunPtr FN_vkGetAccelerationStructureHandleNV

-- | vkGetAccelerationStructureMemoryRequirementsNV - Get acceleration
-- structure memory requirements
--
-- = Parameters
--
-- -   @device@ is the logical device on which the acceleration structure
--     was created.
--
-- -   @pInfo@ specifies the acceleration structure to get memory
--     requirements for.
--
-- -   @pMemoryRequirements@ returns the requested acceleration structure
--     memory requirements.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkAccelerationStructureMemoryRequirementsInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2KHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetAccelerationStructureMemoryRequirementsNV" vkGetAccelerationStructureMemoryRequirementsNV :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()
#else
vkGetAccelerationStructureMemoryRequirementsNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()
vkGetAccelerationStructureMemoryRequirementsNV deviceCmds = mkVkGetAccelerationStructureMemoryRequirementsNV (pVkGetAccelerationStructureMemoryRequirementsNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ())
#endif

type FN_vkGetAccelerationStructureMemoryRequirementsNV = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()
type PFN_vkGetAccelerationStructureMemoryRequirementsNV = FunPtr FN_vkGetAccelerationStructureMemoryRequirementsNV

-- | vkGetRayTracingShaderGroupHandlesNV - Query ray tracing pipeline shader
-- group handles
--
-- = Parameters
--
-- -   @device@ is the logical device that contains the ray tracing
--     pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object that contains the
--     shaders.
--
-- -   @firstGroup@ is the index of the first group to retrieve a handle
--     for from the 'VkRayTracingShaderGroupCreateInfoNV'::@pGroups@ array.
--
-- -   @groupCount@ is the number of shader handles to retrieve.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Valid Usage
--
-- -   The sum of @firstGroup@ and @groupCount@ /must/ be less than the
--     number of shader groups in @pipeline@.
--
-- -   @dataSize@ /must/ be at least
--     'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@ ×
--     @groupCount@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pipeline@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRayTracingShaderGroupHandlesNV" vkGetRayTracingShaderGroupHandlesNV :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
#else
vkGetRayTracingShaderGroupHandlesNV :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
vkGetRayTracingShaderGroupHandlesNV deviceCmds = mkVkGetRayTracingShaderGroupHandlesNV (pVkGetRayTracingShaderGroupHandlesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupHandlesNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
#endif

type FN_vkGetRayTracingShaderGroupHandlesNV = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetRayTracingShaderGroupHandlesNV = FunPtr FN_vkGetRayTracingShaderGroupHandlesNV

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV :: VkAccessFlagBits
pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = VkAccessFlagBits 0x00200000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV :: VkAccessFlagBits
pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = VkAccessFlagBits 0x00400000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RAY_TRACING_BIT_NV"
pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV = VkBufferUsageFlagBits 0x00000400

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT = VkDebugReportObjectTypeEXT 1000165000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = VkDescriptorType 1000165000

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_NONE_NV"
pattern VK_INDEX_TYPE_NONE_NV :: VkIndexType
pattern VK_INDEX_TYPE_NONE_NV = VkIndexType 1000165000

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern VK_NV_RAY_TRACING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern VK_NV_RAY_TRACING_SPEC_VERSION :: Integral a => a
pattern VK_NV_RAY_TRACING_SPEC_VERSION = 3

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV :: VkObjectType
pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV = VkObjectType 1000165000

-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV = VkPipelineBindPoint 1000165000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV = VkPipelineCreateFlagBits 0x00000020

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = VkPipelineStageFlagBits 0x02000000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = VkPipelineStageFlagBits 0x00200000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV :: VkQueryType
pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = VkQueryType 1000165000

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ANY_HIT_BIT_NV"
pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV = VkShaderStageFlagBits 0x00000200

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_CALLABLE_BIT_NV"
pattern VK_SHADER_STAGE_CALLABLE_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_CALLABLE_BIT_NV = VkShaderStageFlagBits 0x00002000

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV"
pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV = VkShaderStageFlagBits 0x00000400

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_INTERSECTION_BIT_NV"
pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV = VkShaderStageFlagBits 0x00001000

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_MISS_BIT_NV"
pattern VK_SHADER_STAGE_MISS_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_MISS_BIT_NV = VkShaderStageFlagBits 0x00000800

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_RAYGEN_BIT_NV"
pattern VK_SHADER_STAGE_RAYGEN_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_RAYGEN_BIT_NV = VkShaderStageFlagBits 0x00000100

-- No documentation found for Nested "Word32" "VK_SHADER_UNUSED_NV"
pattern VK_SHADER_UNUSED_NV :: Word32
pattern VK_SHADER_UNUSED_NV = 0xffffffff

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV = VkStructureType 1000165001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV"
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV = VkStructureType 1000165012

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV"
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV = VkStructureType 1000165008

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV"
pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV = VkStructureType 1000165006

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV"
pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV = VkStructureType 1000165005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_NV"
pattern VK_STRUCTURE_TYPE_GEOMETRY_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_GEOMETRY_NV = VkStructureType 1000165003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV"
pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV = VkStructureType 1000165004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV = VkStructureType 1000165009

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV = VkStructureType 1000165000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV = VkStructureType 1000165011

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV"
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV = VkStructureType 1000165007
