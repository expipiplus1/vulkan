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


-- No documentation found for TopLevel "VkAccelerationStructureCreateInfoNV"
data VkAccelerationStructureCreateInfoNV = VkAccelerationStructureCreateInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "compactedSize"
  vkCompactedSize :: VkDeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "info"
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

-- No documentation found for TopLevel "VkAccelerationStructureInfoNV"
data VkAccelerationStructureInfoNV = VkAccelerationStructureInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "type"
  vkType :: VkAccelerationStructureTypeNV
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "flags"
  vkFlags :: VkBuildAccelerationStructureFlagsNV
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "instanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "geometryCount"
  vkGeometryCount :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "pGeometries"
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

-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsInfoNV"
data VkAccelerationStructureMemoryRequirementsInfoNV = VkAccelerationStructureMemoryRequirementsInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "type"
  vkType :: VkAccelerationStructureMemoryRequirementsTypeNV
  , -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "accelerationStructure"
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

-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsTypeNV"
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

-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = VkAccelerationStructureMemoryRequirementsTypeNV 0

-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = VkAccelerationStructureMemoryRequirementsTypeNV 1

-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: VkAccelerationStructureMemoryRequirementsTypeNV
pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = VkAccelerationStructureMemoryRequirementsTypeNV 2

-- | Dummy data to tag the 'Ptr' with
data VkAccelerationStructureNV_T
-- No documentation found for TopLevel "VkAccelerationStructureNV"
type VkAccelerationStructureNV = Ptr VkAccelerationStructureNV_T

-- ** VkAccelerationStructureTypeNV

-- No documentation found for TopLevel "VkAccelerationStructureTypeNV"
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

-- No documentation found for Nested "VkAccelerationStructureTypeNV" "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV :: VkAccelerationStructureTypeNV
pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = VkAccelerationStructureTypeNV 0

-- No documentation found for Nested "VkAccelerationStructureTypeNV" "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: VkAccelerationStructureTypeNV
pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = VkAccelerationStructureTypeNV 1

-- No documentation found for TopLevel "VkBindAccelerationStructureMemoryInfoNV"
data VkBindAccelerationStructureMemoryInfoNV = VkBindAccelerationStructureMemoryInfoNV
  { -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "accelerationStructure"
  vkAccelerationStructure :: VkAccelerationStructureNV
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "memoryOffset"
  vkMemoryOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "deviceIndexCount"
  vkDeviceIndexCount :: Word32
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "pDeviceIndices"
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

-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagBitsNV"
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

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsNV" "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000001

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsNV" "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000002

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsNV" "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000004

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsNV" "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000008

-- No documentation found for Nested "VkBuildAccelerationStructureFlagBitsNV" "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: VkBuildAccelerationStructureFlagBitsNV
pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = VkBuildAccelerationStructureFlagBitsNV 0x00000010

-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagsNV"
type VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagBitsNV

-- ** VkCopyAccelerationStructureModeNV

-- No documentation found for TopLevel "VkCopyAccelerationStructureModeNV"
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

-- No documentation found for Nested "VkCopyAccelerationStructureModeNV" "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV :: VkCopyAccelerationStructureModeNV
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = VkCopyAccelerationStructureModeNV 0

-- No documentation found for Nested "VkCopyAccelerationStructureModeNV" "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: VkCopyAccelerationStructureModeNV
pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = VkCopyAccelerationStructureModeNV 1

-- No documentation found for TopLevel "VkGeometryAABBNV"
data VkGeometryAABBNV = VkGeometryAABBNV
  { -- No documentation found for Nested "VkGeometryAABBNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkGeometryAABBNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkGeometryAABBNV" "aabbData"
  vkAabbData :: VkBuffer
  , -- No documentation found for Nested "VkGeometryAABBNV" "numAABBs"
  vkNumAABBs :: Word32
  , -- No documentation found for Nested "VkGeometryAABBNV" "stride"
  vkStride :: Word32
  , -- No documentation found for Nested "VkGeometryAABBNV" "offset"
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

-- No documentation found for TopLevel "VkGeometryDataNV"
data VkGeometryDataNV = VkGeometryDataNV
  { -- No documentation found for Nested "VkGeometryDataNV" "triangles"
  vkTriangles :: VkGeometryTrianglesNV
  , -- No documentation found for Nested "VkGeometryDataNV" "aabbs"
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

-- No documentation found for TopLevel "VkGeometryFlagBitsNV"
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

-- No documentation found for Nested "VkGeometryFlagBitsNV" "VK_GEOMETRY_OPAQUE_BIT_NV"
pattern VK_GEOMETRY_OPAQUE_BIT_NV :: VkGeometryFlagBitsNV
pattern VK_GEOMETRY_OPAQUE_BIT_NV = VkGeometryFlagBitsNV 0x00000001

-- No documentation found for Nested "VkGeometryFlagBitsNV" "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: VkGeometryFlagBitsNV
pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = VkGeometryFlagBitsNV 0x00000002

-- No documentation found for TopLevel "VkGeometryFlagsNV"
type VkGeometryFlagsNV = VkGeometryFlagBitsNV

-- ** VkGeometryInstanceFlagBitsNV

-- No documentation found for TopLevel "VkGeometryInstanceFlagBitsNV"
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

-- No documentation found for Nested "VkGeometryInstanceFlagBitsNV" "VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000001

-- No documentation found for Nested "VkGeometryInstanceFlagBitsNV" "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000002

-- No documentation found for Nested "VkGeometryInstanceFlagBitsNV" "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000004

-- No documentation found for Nested "VkGeometryInstanceFlagBitsNV" "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: VkGeometryInstanceFlagBitsNV
pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = VkGeometryInstanceFlagBitsNV 0x00000008

-- No documentation found for TopLevel "VkGeometryInstanceFlagsNV"
type VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagBitsNV

-- No documentation found for TopLevel "VkGeometryNV"
data VkGeometryNV = VkGeometryNV
  { -- No documentation found for Nested "VkGeometryNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkGeometryNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkGeometryNV" "geometryType"
  vkGeometryType :: VkGeometryTypeNV
  , -- No documentation found for Nested "VkGeometryNV" "geometry"
  vkGeometry :: VkGeometryDataNV
  , -- No documentation found for Nested "VkGeometryNV" "flags"
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

-- No documentation found for TopLevel "VkGeometryTrianglesNV"
data VkGeometryTrianglesNV = VkGeometryTrianglesNV
  { -- No documentation found for Nested "VkGeometryTrianglesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexData"
  vkVertexData :: VkBuffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexOffset"
  vkVertexOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexCount"
  vkVertexCount :: Word32
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexStride"
  vkVertexStride :: VkDeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexFormat"
  vkVertexFormat :: VkFormat
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexData"
  vkIndexData :: VkBuffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexOffset"
  vkIndexOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexCount"
  vkIndexCount :: Word32
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexType"
  vkIndexType :: VkIndexType
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "transformData"
  vkTransformData :: VkBuffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "transformOffset"
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

-- No documentation found for TopLevel "VkGeometryTypeNV"
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

-- No documentation found for Nested "VkGeometryTypeNV" "VK_GEOMETRY_TYPE_TRIANGLES_NV"
pattern VK_GEOMETRY_TYPE_TRIANGLES_NV :: VkGeometryTypeNV
pattern VK_GEOMETRY_TYPE_TRIANGLES_NV = VkGeometryTypeNV 0

-- No documentation found for Nested "VkGeometryTypeNV" "VK_GEOMETRY_TYPE_AABBS_NV"
pattern VK_GEOMETRY_TYPE_AABBS_NV :: VkGeometryTypeNV
pattern VK_GEOMETRY_TYPE_AABBS_NV = VkGeometryTypeNV 1

-- No documentation found for TopLevel "VkPhysicalDeviceRayTracingPropertiesNV"
data VkPhysicalDeviceRayTracingPropertiesNV = VkPhysicalDeviceRayTracingPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "shaderGroupHandleSize"
  vkShaderGroupHandleSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxRecursionDepth"
  vkMaxRecursionDepth :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxShaderGroupStride"
  vkMaxShaderGroupStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "shaderGroupBaseAlignment"
  vkShaderGroupBaseAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxGeometryCount"
  vkMaxGeometryCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxInstanceCount"
  vkMaxInstanceCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxTriangleCount"
  vkMaxTriangleCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxDescriptorSetAccelerationStructures"
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

-- No documentation found for TopLevel "VkRayTracingPipelineCreateInfoNV"
data VkRayTracingPipelineCreateInfoNV = VkRayTracingPipelineCreateInfoNV
  { -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "flags"
  vkFlags :: VkPipelineCreateFlags
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "stageCount"
  vkStageCount :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pStages"
  vkPStages :: Ptr VkPipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "groupCount"
  vkGroupCount :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pGroups"
  vkPGroups :: Ptr VkRayTracingShaderGroupCreateInfoNV
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "maxRecursionDepth"
  vkMaxRecursionDepth :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "layout"
  vkLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "basePipelineHandle"
  vkBasePipelineHandle :: VkPipeline
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "basePipelineIndex"
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

-- No documentation found for TopLevel "VkRayTracingShaderGroupCreateInfoNV"
data VkRayTracingShaderGroupCreateInfoNV = VkRayTracingShaderGroupCreateInfoNV
  { -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "type"
  vkType :: VkRayTracingShaderGroupTypeNV
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "generalShader"
  vkGeneralShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "closestHitShader"
  vkClosestHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "anyHitShader"
  vkAnyHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "intersectionShader"
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

-- No documentation found for TopLevel "VkRayTracingShaderGroupTypeNV"
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

-- No documentation found for Nested "VkRayTracingShaderGroupTypeNV" "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = VkRayTracingShaderGroupTypeNV 0

-- No documentation found for Nested "VkRayTracingShaderGroupTypeNV" "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = VkRayTracingShaderGroupTypeNV 1

-- No documentation found for Nested "VkRayTracingShaderGroupTypeNV" "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: VkRayTracingShaderGroupTypeNV
pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = VkRayTracingShaderGroupTypeNV 2

-- No documentation found for TopLevel "VkWriteDescriptorSetAccelerationStructureNV"
data VkWriteDescriptorSetAccelerationStructureNV = VkWriteDescriptorSetAccelerationStructureNV
  { -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureNV" "accelerationStructureCount"
  vkAccelerationStructureCount :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureNV" "pAccelerationStructures"
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

-- No documentation found for TopLevel "vkBindAccelerationStructureMemoryNV"
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

-- No documentation found for TopLevel "vkCmdBuildAccelerationStructureNV"
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

-- No documentation found for TopLevel "vkCmdCopyAccelerationStructureNV"
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

-- No documentation found for TopLevel "vkCmdTraceRaysNV"
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

-- No documentation found for TopLevel "vkCmdWriteAccelerationStructuresPropertiesNV"
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

-- No documentation found for TopLevel "vkCompileDeferredNV"
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

-- No documentation found for TopLevel "vkCreateAccelerationStructureNV"
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

-- No documentation found for TopLevel "vkCreateRayTracingPipelinesNV"
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

-- No documentation found for TopLevel "vkDestroyAccelerationStructureNV"
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

-- No documentation found for TopLevel "vkGetAccelerationStructureHandleNV"
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

-- No documentation found for TopLevel "vkGetAccelerationStructureMemoryRequirementsNV"
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

-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesNV"
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
