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
-- Unresolved directive in VkAccelerationStructureCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkAccelerationStructureCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in VkAccelerationStructureInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkAccelerationStructureInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- = Description
--
-- Unresolved directive in
-- VkAccelerationStructureMemoryRequirementsInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkAccelerationStructureMemoryRequirementsInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkAccelerationStructureMemoryRequirementsInfoNV = VkAccelerationStructureMemoryRequirementsInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @type@ selects the type of memory requirement being queried.
  -- 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' returns
  -- the memory requirements for the object itself.
  -- 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
  -- returns the memory requirements for the scratch memory when doing a
  -- build.
  -- 'VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
  -- returns the memory requirements for the scratch memory when doing an
  -- update.
  vkType :: VkAccelerationStructureMemoryRequirementsTypeNV
  , -- | @accelerationStructure@ is the acceleration structure to be queried for
  -- memory requirements.
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
-- No cross-references are available
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
-- No cross-references are available
type VkAccelerationStructureNV = Ptr VkAccelerationStructureNV_T

-- ** VkAccelerationStructureTypeNV

-- | VkAccelerationStructureTypeNV - Type of acceleration structure
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in VkBindAccelerationStructureMemoryInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkBindAccelerationStructureMemoryInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
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
-- No cross-references are available
type VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagBitsNV

-- ** VkCopyAccelerationStructureModeNV

-- | VkCopyAccelerationStructureModeNV - Acceleration structure copy mode
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in VkGeometryAABBNV.txt -
-- include::{generated}\/validity\/structs\/VkGeometryAABBNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkGeometryAABBNV = VkGeometryAABBNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @aabbData@ is the buffer containing axis-aligned bounding box data.
  vkAabbData :: VkBuffer
  , -- | @numAABBs@ is the number of AABBs in this geometry.
  vkNumAABBs :: Word32
  , -- | @stride@ /must/ be a multiple of @8@
  vkStride :: Word32
  , -- | @offset@ /must/ be a multiple of @8@
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
-- = Description
--
-- Unresolved directive in VkGeometryDataNV.txt -
-- include::{generated}\/validity\/structs\/VkGeometryDataNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkGeometryDataNV = VkGeometryDataNV
  { -- | @triangles@ contains triangle data if 'VkGeometryNV'::@geometryType@ is
  -- 'VK_GEOMETRY_TYPE_TRIANGLES_NV'.
  vkTriangles :: VkGeometryTrianglesNV
  , -- | @aabbs@ contains axis-aligned bounding box data if
  -- 'VkGeometryNV'::@geometryType@ is 'VK_GEOMETRY_TYPE_AABBS_NV'.
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
-- No cross-references are available
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
-- No cross-references are available
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
-- No cross-references are available
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
-- No cross-references are available
type VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagBitsNV

-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- = Description
--
-- Unresolved directive in VkGeometryNV.txt -
-- include::{generated}\/validity\/structs\/VkGeometryNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkGeometryNV = VkGeometryNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @geometryType@ describes which type of geometry this 'VkGeometryNV'
  -- refers to.
  vkGeometryType :: VkGeometryTypeNV
  , -- | @geometry@ contains the geometry data as described in
  -- 'VkGeometryDataNV'.
  vkGeometry :: VkGeometryDataNV
  , -- | @flags@ has flags describing options for this geometry.
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
-- Unresolved directive in VkGeometryTrianglesNV.txt -
-- include::{generated}\/validity\/structs\/VkGeometryTrianglesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkGeometryTrianglesNV = VkGeometryTrianglesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @vertexData@ is the buffer containing vertex data for this geometry.
  vkVertexData :: VkBuffer
  , -- | @vertexOffset@ /must/ be a multiple of the component size of
  -- @vertexFormat@
  vkVertexOffset :: VkDeviceSize
  , -- | @vertexCount@ is the number of valid vertices.
  vkVertexCount :: Word32
  , -- | @vertexStride@ is the stride in bytes between each vertex.
  vkVertexStride :: VkDeviceSize
  , -- | @vertexFormat@ /must/ be one of
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_SFLOAT',
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_SFLOAT',
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SFLOAT',
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SFLOAT',
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SNORM', or
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SNORM'
  vkVertexFormat :: VkFormat
  , -- | @indexData@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle if
  -- @indexType@ is not 'VK_INDEX_TYPE_NONE_NV'
  vkIndexData :: VkBuffer
  , -- | @indexOffset@ /must/ be a multiple of the element size of @indexType@
  vkIndexOffset :: VkDeviceSize
  , -- | @indexCount@ /must/ be @0@ if @indexType@ is 'VK_INDEX_TYPE_NONE_NV'
  vkIndexCount :: Word32
  , -- | @indexType@ /must/ be
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT16',
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32',
  -- or 'VK_INDEX_TYPE_NONE_NV'
  vkIndexType :: VkIndexType
  , -- | @transformData@ is a buffer containing optional reference to an array of
  -- 32-bit floats representing a 3x4 row major affine transformation matrix
  -- for this geometry.
  vkTransformData :: VkBuffer
  , -- | @transformOffset@ /must/ be a multiple of @16@
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
-- No cross-references are available
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
-- Unresolved directive in VkPhysicalDeviceRayTracingPropertiesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceRayTracingPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceRayTracingPropertiesNV = VkPhysicalDeviceRayTracingPropertiesNV
  { -- | @sType@ is the type of this structure.
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
-- Unresolved directive in VkRayTracingPipelineCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkRayTracingPipelineCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in VkRayTracingShaderGroupCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkRayTracingShaderGroupCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
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
-- == Valid Usage
--
-- Unresolved directive in VkWriteDescriptorSetAccelerationStructureNV.txt
-- -
-- include::{generated}\/validity\/structs\/VkWriteDescriptorSetAccelerationStructureNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkWriteDescriptorSetAccelerationStructureNV = VkWriteDescriptorSetAccelerationStructureNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @accelerationStructureCount@ /must/ be equal to @descriptorCount@ in the
  -- extended structure
  vkAccelerationStructureCount :: Word32
  , -- | @pAccelerationStructures@ are the acceleration structures to update.
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
-- = Description
--
-- Unresolved directive in vkBindAccelerationStructureMemoryNV.txt -
-- include::{generated}\/validity\/protos\/vkBindAccelerationStructureMemoryNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCmdBuildAccelerationStructureNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdBuildAccelerationStructureNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCmdCopyAccelerationStructureNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdCopyAccelerationStructureNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCmdTraceRaysNV.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_common.txt[]
-- Unresolved directive in vkCmdTraceRaysNV.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_nonindirect_common.txt[]
-- * @raygenShaderBindingOffset@ /must/ be less than the size of
-- @raygenShaderBindingTableBuffer@ * @raygenShaderBindingOffset@ /must/ be
-- a multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@ *
-- @missShaderBindingOffset@ /must/ be less than the size of
-- @missShaderBindingTableBuffer@ * @missShaderBindingOffset@ /must/ be a
-- multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@ *
-- @hitShaderBindingOffset@ /must/ be less than the size of
-- @hitShaderBindingTableBuffer@ * @hitShaderBindingOffset@ /must/ be a
-- multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@ *
-- @callableShaderBindingOffset@ /must/ be less than the size of
-- @callableShaderBindingTableBuffer@ * @callableShaderBindingOffset@
-- /must/ be a multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@ *
-- @missShaderBindingStride@ /must/ be a multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@ *
-- @hitShaderBindingStride@ /must/ be a multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@ *
-- @callableShaderBindingStride@ /must/ be a multiple of
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@ *
-- @missShaderBindingStride@ /must/ be a less than or equal to
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@ *
-- @hitShaderBindingStride@ /must/ be a less than or equal to
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@ *
-- @callableShaderBindingStride@ /must/ be a less than or equal to
-- 'VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@ *
-- @width@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
-- * @height@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
-- * @depth@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- Unresolved directive in vkCmdTraceRaysNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdTraceRaysNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCmdWriteAccelerationStructuresPropertiesNV.txt
-- -
-- include::{generated}\/validity\/protos\/vkCmdWriteAccelerationStructuresPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- == Valid Usage
--
-- Unresolved directive in vkCompileDeferredNV.txt -
-- include::{generated}\/validity\/protos\/vkCompileDeferredNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCreateAccelerationStructureNV.txt -
-- include::{generated}\/validity\/protos\/vkCreateAccelerationStructureNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkCreateRayTracingPipelinesNV.txt -
-- include::{generated}\/validity\/protos\/vkCreateRayTracingPipelinesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkDestroyAccelerationStructureNV.txt -
-- include::{generated}\/validity\/protos\/vkDestroyAccelerationStructureNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- == Valid Usage
--
-- Unresolved directive in vkGetAccelerationStructureHandleNV.txt -
-- include::{generated}\/validity\/protos\/vkGetAccelerationStructureHandleNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- = Description
--
-- Unresolved directive in
-- vkGetAccelerationStructureMemoryRequirementsNV.txt -
-- include::{generated}\/validity\/protos\/vkGetAccelerationStructureMemoryRequirementsNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetRayTracingShaderGroupHandlesNV.txt -
-- include::{generated}\/validity\/protos\/vkGetRayTracingShaderGroupHandlesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | 'VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV' specifies read access to
-- an acceleration structure as part of a trace or build command.
pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV :: VkAccessFlagBits
pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = VkAccessFlagBits 0x00200000

-- | 'VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV' specifies write access
-- to an acceleration structure as part of a build command.
pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV :: VkAccessFlagBits
pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = VkAccessFlagBits 0x00400000

-- | 'VK_BUFFER_USAGE_RAY_TRACING_BIT_NV' specifies that the buffer is
-- suitable for use in 'vkCmdTraceRaysNV' and
-- 'vkCmdBuildAccelerationStructureNV'.
pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV = VkBufferUsageFlagBits 0x00000400

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT = VkDebugReportObjectTypeEXT 1000165000

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = VkDescriptorType 1000165000

-- | 'VK_INDEX_TYPE_NONE_NV' specifies that no indices are provided.
pattern VK_INDEX_TYPE_NONE_NV :: VkIndexType
pattern VK_INDEX_TYPE_NONE_NV = VkIndexType 1000165000

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern VK_NV_RAY_TRACING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern VK_NV_RAY_TRACING_SPEC_VERSION :: Integral a => a
pattern VK_NV_RAY_TRACING_SPEC_VERSION = 3

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV :: VkObjectType
pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV = VkObjectType 1000165000

-- | 'VK_PIPELINE_BIND_POINT_RAY_TRACING_NV' specifies binding as a ray
-- tracing pipeline.
pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV = VkPipelineBindPoint 1000165000

-- | 'VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV' specifies that a pipeline is
-- created with all shaders in the deferred state. Before using the
-- pipeline the application /must/ call 'vkCompileDeferredNV' exactly once
-- on each shader in the pipeline before using the pipeline.
pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV = VkPipelineCreateFlagBits 0x00000020

-- | 'VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV' specifies the
-- execution of 'vkCmdBuildAccelerationStructureNV',
-- 'vkCmdCopyAccelerationStructureNV', and
-- 'vkCmdWriteAccelerationStructuresPropertiesNV'.
pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = VkPipelineStageFlagBits 0x02000000

-- | 'VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV' specifies the execution of
-- the ray tracing shader stages.
pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = VkPipelineStageFlagBits 0x00200000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV :: VkQueryType
pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = VkQueryType 1000165000

-- | 'VK_SHADER_STAGE_ANY_HIT_BIT_NV' specifies the any-hit stage.
pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV = VkShaderStageFlagBits 0x00000200

-- | 'VK_SHADER_STAGE_CALLABLE_BIT_NV' specifies the callable stage.
pattern VK_SHADER_STAGE_CALLABLE_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_CALLABLE_BIT_NV = VkShaderStageFlagBits 0x00002000

-- | 'VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV' specifies the closest hit stage.
pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV = VkShaderStageFlagBits 0x00000400

-- | 'VK_SHADER_STAGE_INTERSECTION_BIT_NV' specifies the intersection stage.
pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV = VkShaderStageFlagBits 0x00001000

-- | 'VK_SHADER_STAGE_MISS_BIT_NV' specifies the miss stage.
pattern VK_SHADER_STAGE_MISS_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_MISS_BIT_NV = VkShaderStageFlagBits 0x00000800

-- | 'VK_SHADER_STAGE_RAYGEN_BIT_NV' specifies the ray generation stage.
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
