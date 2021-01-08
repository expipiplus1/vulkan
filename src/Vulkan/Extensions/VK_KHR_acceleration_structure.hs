{-# language CPP #-}
-- | = Name
--
-- VK_KHR_acceleration_structure - device extension
--
-- == VK_KHR_acceleration_structure
--
-- [__Name String__]
--     @VK_KHR_acceleration_structure@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     151
--
-- [__Revision__]
--     11
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
--     -   Requires @VK_EXT_descriptor_indexing@
--
--     -   Requires @VK_KHR_buffer_device_address@
--
--     -   Requires @VK_KHR_deferred_host_operations@
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_acceleration_structure:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-11-12
--
-- [__Contributors__]
--
--     -   Matthäus Chajdas, AMD
--
--     -   Greg Grebe, AMD
--
--     -   Nicolai Hähnle, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Dave Oldcorn, AMD
--
--     -   Skyler Saleh, AMD
--
--     -   Mathieu Robart, Arm
--
--     -   Marius Bjorge, Arm
--
--     -   Tom Olson, Arm
--
--     -   Sebastian Tafuri, EA
--
--     -   Henrik Rydgard, Embark
--
--     -   Juan Cañada, Epic Games
--
--     -   Patrick Kelly, Epic Games
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Michael Doggett, Facebook\/Oculus
--
--     -   Ricardo Garcia, Igalia
--
--     -   Andrew Garrard, Imagination
--
--     -   Don Scorgie, Imagination
--
--     -   Dae Kim, Imagination
--
--     -   Joshua Barczak, Intel
--
--     -   Slawek Grajewski, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pascal Gautron, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Robert Stepinski, NVIDIA
--
--     -   Martin Stich, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Jon Leech, Khronos
--
--     -   Jeroen van Schijndel, OTOY
--
--     -   Juul Joosten, OTOY
--
--     -   Alex Bourd, Qualcomm
--
--     -   Roman Larionov, Qualcomm
--
--     -   David McAllister, Qualcomm
--
--     -   Lewis Gordon, Samsung
--
--     -   Ralph Potter, Samsung
--
--     -   Jasper Bekkers, Traverse Research
--
--     -   Jesse Barker, Unity
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- In order to be efficient, rendering techniques such as ray tracing need
-- a quick way to identify which primitives may be intersected by a ray
-- traversing the geometries. Acceleration structures are the most common
-- way to represent the geometry spatially sorted, in order to quickly
-- identify such potential intersections.
--
-- This extension adds new functionalities:
--
-- -   Acceleration structure objects and build commands
--
-- -   Structures to describe geometry inputs to acceleration structure
--     builds
--
-- -   Acceleration structure copy commands
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.AccelerationStructureKHR'
--
-- == New Commands
--
-- -   'buildAccelerationStructuresKHR'
--
-- -   'cmdBuildAccelerationStructuresIndirectKHR'
--
-- -   'cmdBuildAccelerationStructuresKHR'
--
-- -   'cmdCopyAccelerationStructureKHR'
--
-- -   'cmdCopyAccelerationStructureToMemoryKHR'
--
-- -   'cmdCopyMemoryToAccelerationStructureKHR'
--
-- -   'cmdWriteAccelerationStructuresPropertiesKHR'
--
-- -   'copyAccelerationStructureKHR'
--
-- -   'copyAccelerationStructureToMemoryKHR'
--
-- -   'copyMemoryToAccelerationStructureKHR'
--
-- -   'createAccelerationStructureKHR'
--
-- -   'destroyAccelerationStructureKHR'
--
-- -   'getAccelerationStructureBuildSizesKHR'
--
-- -   'getAccelerationStructureDeviceAddressKHR'
--
-- -   'getDeviceAccelerationStructureCompatibilityKHR'
--
-- -   'writeAccelerationStructuresPropertiesKHR'
--
-- == New Structures
--
-- -   'AabbPositionsKHR'
--
-- -   'AccelerationStructureBuildGeometryInfoKHR'
--
-- -   'AccelerationStructureBuildRangeInfoKHR'
--
-- -   'AccelerationStructureBuildSizesInfoKHR'
--
-- -   'AccelerationStructureCreateInfoKHR'
--
-- -   'AccelerationStructureDeviceAddressInfoKHR'
--
-- -   'AccelerationStructureGeometryAabbsDataKHR'
--
-- -   'AccelerationStructureGeometryInstancesDataKHR'
--
-- -   'AccelerationStructureGeometryKHR'
--
-- -   'AccelerationStructureGeometryTrianglesDataKHR'
--
-- -   'AccelerationStructureInstanceKHR'
--
-- -   'AccelerationStructureVersionInfoKHR'
--
-- -   'CopyAccelerationStructureInfoKHR'
--
-- -   'CopyAccelerationStructureToMemoryInfoKHR'
--
-- -   'CopyMemoryToAccelerationStructureInfoKHR'
--
-- -   'TransformMatrixKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAccelerationStructureFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceAccelerationStructurePropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetAccelerationStructureKHR'
--
-- == New Unions
--
-- -   'AccelerationStructureGeometryDataKHR'
--
-- -   'DeviceOrHostAddressConstKHR'
--
-- -   'DeviceOrHostAddressKHR'
--
-- == New Enums
--
-- -   'AccelerationStructureBuildTypeKHR'
--
-- -   'AccelerationStructureCompatibilityKHR'
--
-- -   'AccelerationStructureCreateFlagBitsKHR'
--
-- -   'AccelerationStructureTypeKHR'
--
-- -   'BuildAccelerationStructureFlagBitsKHR'
--
-- -   'BuildAccelerationStructureModeKHR'
--
-- -   'CopyAccelerationStructureModeKHR'
--
-- -   'GeometryFlagBitsKHR'
--
-- -   'GeometryInstanceFlagBitsKHR'
--
-- -   'GeometryTypeKHR'
--
-- == New Bitmasks
--
-- -   'AccelerationStructureCreateFlagsKHR'
--
-- -   'BuildAccelerationStructureFlagsKHR'
--
-- -   'GeometryFlagsKHR'
--
-- -   'GeometryInstanceFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME'
--
-- -   'KHR_ACCELERATION_STRUCTURE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.IndexType.IndexType':
--
--     -   'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR'
--
-- == Issues
--
-- (1) How does this extension differ from VK_NV_ray_tracing?
--
-- __DISCUSSION__:
--
-- The following is a summary of the main functional differences between
-- VK_KHR_acceleration_structure and VK_NV_ray_tracing:
--
-- -   added acceleration structure serialization \/ deserialization
--     ('COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR',
--     'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR',
--     'cmdCopyAccelerationStructureToMemoryKHR',
--     'cmdCopyMemoryToAccelerationStructureKHR')
--
-- -   document
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive primitives and instances>
--
-- -   added 'PhysicalDeviceAccelerationStructureFeaturesKHR' structure
--
-- -   added indirect and batched acceleration structure builds
--     ('cmdBuildAccelerationStructuresIndirectKHR')
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#host-acceleration-structure host acceleration structure>
--     commands
--
-- -   reworked geometry structures so they could be better shared between
--     device, host, and indirect builds
--
-- -   explicitly made 'Vulkan.Extensions.Handles.AccelerationStructureKHR'
--     use device addresses
--
-- -   added acceleration structure compatibility check function
--     ('getDeviceAccelerationStructureCompatibilityKHR')
--
-- -   add parameter for requesting memory requirements for host and\/or
--     device build
--
-- -   added format feature for acceleration structure build vertex formats
--     ('Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR')
--
-- (2) Can you give a more detailed comparision of differences and
-- similarities between VK_NV_ray_tracing and
-- VK_KHR_acceleration_structure?
--
-- __DISCUSSION__:
--
-- The following is a more detailed comparision of which commands,
-- structures, and enums are aliased, changed, or removed.
--
-- -   Aliased functionality — enums, structures, and commands that are
--     considered equivalent:
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTypeNV' ↔
--         'GeometryTypeKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureTypeNV'
--         ↔ 'AccelerationStructureTypeKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.CopyAccelerationStructureModeNV'
--         ↔ 'CopyAccelerationStructureModeKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryFlagsNV' ↔
--         'GeometryFlagsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryFlagBitsNV' ↔
--         'GeometryFlagBitsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryInstanceFlagsNV' ↔
--         'GeometryInstanceFlagsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryInstanceFlagBitsNV'
--         ↔ 'GeometryInstanceFlagBitsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.BuildAccelerationStructureFlagsNV'
--         ↔ 'BuildAccelerationStructureFlagsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.BuildAccelerationStructureFlagBitsNV'
--         ↔ 'BuildAccelerationStructureFlagBitsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.TransformMatrixNV' ↔
--         'TransformMatrixKHR' (added to VK_NV_ray_tracing for descriptive
--         purposes)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AabbPositionsNV' ↔
--         'AabbPositionsKHR' (added to VK_NV_ray_tracing for descriptive
--         purposes)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureInstanceNV'
--         ↔ 'AccelerationStructureInstanceKHR' (added to VK_NV_ray_tracing
--         for descriptive purposes)
--
-- -   Changed enums, structures, and commands:
--
--     -   renamed
--         'Vulkan.Extensions.VK_NV_ray_tracing.GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV'
--         → 'GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR' in
--         'GeometryInstanceFlagBitsKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV' →
--         'AccelerationStructureGeometryTrianglesDataKHR' (device or host
--         address instead of buffer+offset)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV' →
--         'AccelerationStructureGeometryAabbsDataKHR' (device or host
--         address instead of buffer+offset)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryDataNV' →
--         'AccelerationStructureGeometryDataKHR' (union of
--         triangle\/aabbs\/instances)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.GeometryNV' →
--         'AccelerationStructureGeometryKHR' (changed type of geometry)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV'
--         → 'AccelerationStructureCreateInfoKHR' (reshuffle geometry
--         layout\/info)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV'
--         → 'PhysicalDeviceAccelerationStructurePropertiesKHR' (for
--         acceleration structure properties, renamed @maxTriangleCount@ to
--         @maxPrimitiveCount@, added per stage and update after bind
--         limits) and
--         'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'
--         (for ray tracing pipeline properties)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsInfoNV'
--         (deleted - replaced by allocating on top of
--         'Vulkan.Core10.Handles.Buffer')
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV'
--         → 'WriteDescriptorSetAccelerationStructureKHR' (different
--         acceleration structure type)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV'
--         → 'createAccelerationStructureKHR' (device address, different
--         geometry layout\/info)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureMemoryRequirementsNV'
--         (deleted - replaced by allocating on top of
--         'Vulkan.Core10.Handles.Buffer')
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV'
--         → 'cmdBuildAccelerationStructuresKHR' (params moved to structs,
--         layout differences)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV'
--         → 'cmdCopyAccelerationStructureKHR' (params to struct,
--         extendable)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--         → 'getAccelerationStructureDeviceAddressKHR' (device address
--         instead of handle)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsTypeNV'
--         → size queries for scratch space moved to
--         'getAccelerationStructureBuildSizesKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV'
--         → 'destroyAccelerationStructureKHR' (different acceleration
--         structure types)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'
--         → 'cmdWriteAccelerationStructuresPropertiesKHR' (different
--         acceleration structure types)
--
-- -   Added enums, structures and commands:
--
--     -   'GEOMETRY_TYPE_INSTANCES_KHR' to 'GeometryTypeKHR' enum
--
--     -   'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR',
--         'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR' to
--         'CopyAccelerationStructureModeKHR' enum
--
--     -   'PhysicalDeviceAccelerationStructureFeaturesKHR' structure
--
--     -   'AccelerationStructureBuildTypeKHR' enum
--
--     -   'BuildAccelerationStructureModeKHR' enum
--
--     -   'DeviceOrHostAddressKHR' and 'DeviceOrHostAddressConstKHR'
--         unions
--
--     -   'AccelerationStructureBuildRangeInfoKHR' struct
--
--     -   'AccelerationStructureGeometryInstancesDataKHR' struct
--
--     -   'AccelerationStructureDeviceAddressInfoKHR' struct
--
--     -   'AccelerationStructureVersionInfoKHR' struct
--
--     -   'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.StridedDeviceAddressRegionKHR'
--         struct
--
--     -   'CopyAccelerationStructureToMemoryInfoKHR' struct
--
--     -   'CopyMemoryToAccelerationStructureInfoKHR' struct
--
--     -   'CopyAccelerationStructureInfoKHR' struct
--
--     -   'buildAccelerationStructuresKHR' command (host build)
--
--     -   'copyAccelerationStructureKHR' command (host copy)
--
--     -   'copyAccelerationStructureToMemoryKHR' (host serialize)
--
--     -   'copyMemoryToAccelerationStructureKHR' (host deserialize)
--
--     -   'writeAccelerationStructuresPropertiesKHR' (host properties)
--
--     -   'cmdCopyAccelerationStructureToMemoryKHR' (device serialize)
--
--     -   'cmdCopyMemoryToAccelerationStructureKHR' (device deserialize)
--
--     -   'getDeviceAccelerationStructureCompatibilityKHR' (serialization)
--
-- (3) What are the changes between the public provisional
-- (VK_KHR_ray_tracing v8) release and the internal provisional
-- (VK_KHR_ray_tracing v9) release?
--
-- -   added @geometryFlags@ to
--     @VkAccelerationStructureCreateGeometryTypeInfoKHR@ (later reworked
--     to obsolete this)
--
-- -   added @minAccelerationStructureScratchOffsetAlignment@ property to
--     VkPhysicalDeviceRayTracingPropertiesKHR
--
-- -   fix naming and return enum from
--     'getDeviceAccelerationStructureCompatibilityKHR'
--
--     -   renamed @VkAccelerationStructureVersionKHR@ to
--         'AccelerationStructureVersionInfoKHR'
--
--     -   renamed @VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR@
--         to
--         'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR'
--
--     -   removed @VK_ERROR_INCOMPATIBLE_VERSION_KHR@
--
--     -   added 'AccelerationStructureCompatibilityKHR' enum
--
--     -   remove return value from
--         'getDeviceAccelerationStructureCompatibilityKHR' and added
--         return enum parameter
--
-- -   Require Vulkan 1.1
--
-- -   added creation time capture and replay flags
--
--     -   added 'AccelerationStructureCreateFlagBitsKHR' and
--         'AccelerationStructureCreateFlagsKHR'
--
--     -   renamed the @flags@ member of
--         'AccelerationStructureCreateInfoKHR' to @buildFlags@ (later
--         removed) and added the @createFlags@ member
--
-- -   change 'cmdBuildAccelerationStructuresIndirectKHR' to use buffer
--     device address for indirect parameter
--
-- -   make
--     <VK_KHR_deferred_host_operations.html VK_KHR_deferred_host_operations>
--     an interaction instead of a required extension (later went back on
--     this)
--
-- -   renamed @VkAccelerationStructureBuildOffsetInfoKHR@ to
--     'AccelerationStructureBuildRangeInfoKHR'
--
--     -   renamed the @ppOffsetInfos@ parameter of
--         'cmdBuildAccelerationStructuresKHR' to @ppBuildRangeInfos@
--
-- -   Re-unify geometry description between build and create
--
--     -   remove @VkAccelerationStructureCreateGeometryTypeInfoKHR@ and
--         @VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR@
--
--     -   added @VkAccelerationStructureCreateSizeInfoKHR@ structure
--         (later removed)
--
--     -   change type of the @pGeometryInfos@ member of
--         'AccelerationStructureCreateInfoKHR' from
--         @VkAccelerationStructureCreateGeometryTypeInfoKHR@ to
--         'AccelerationStructureGeometryKHR' (later removed)
--
--     -   added @pCreateSizeInfos@ member to
--         'AccelerationStructureCreateInfoKHR' (later removed)
--
-- -   Fix ppGeometries ambiguity, add pGeometries
--
--     -   remove @geometryArrayOfPointers@ member of
--         VkAccelerationStructureBuildGeometryInfoKHR
--
--     -   disambiguate two meanings of @ppGeometries@ by explicitly adding
--         @pGeometries@ to the 'AccelerationStructureBuildGeometryInfoKHR'
--         structure and require one of them be @NULL@
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     support for acceleration structures
--
-- -   changed the @update@ member of
--     'AccelerationStructureBuildGeometryInfoKHR' from a bool to the
--     @mode@ 'BuildAccelerationStructureModeKHR' enum which allows future
--     extensibility in update types
--
-- -   Clarify deferred host ops for pipeline creation
--
--     -   'Vulkan.Extensions.Handles.DeferredOperationKHR' is now a
--         top-level parameter for 'buildAccelerationStructuresKHR',
--         'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
--         'copyAccelerationStructureToMemoryKHR',
--         'copyAccelerationStructureKHR', and
--         'copyMemoryToAccelerationStructureKHR'
--
--     -   removed @VkDeferredOperationInfoKHR@ structure
--
--     -   change deferred host creation\/return parameter behavior such
--         that the implementation can modify such parameters until the
--         deferred host operation completes
--
--     -   <VK_KHR_deferred_host_operations.html VK_KHR_deferred_host_operations>
--         is required again
--
-- -   Change acceleration structure build to always be sized
--
--     -   de-alias
--         'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsTypeNV'
--         and @VkAccelerationStructureMemoryRequirementsTypeKHR@ and
--         remove @VkAccelerationStructureMemoryRequirementsTypeKHR@
--
--     -   add 'getAccelerationStructureBuildSizesKHR' command and
--         'AccelerationStructureBuildSizesInfoKHR' structure and
--         'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR'
--         enum to query sizes for acceleration structures and scratch
--         storage
--
--     -   move size queries for scratch space to
--         'getAccelerationStructureBuildSizesKHR'
--
--     -   remove @compactedSize@, @buildFlags@, @maxGeometryCount@,
--         @pGeometryInfos@, @pCreateSizeInfos@ members of
--         'AccelerationStructureCreateInfoKHR' and add the @size@ member
--
--     -   add @maxVertex@ member to
--         'AccelerationStructureGeometryTrianglesDataKHR' structure
--
--     -   remove @VkAccelerationStructureCreateSizeInfoKHR@ structure
--
-- (4) What are the changes between the internal provisional
-- (VK_KHR_ray_tracing v9) release and the final
-- (VK_KHR_acceleration_structure v11) release?
--
-- -   refactor VK_KHR_ray_tracing into 3 extensions, enabling
--     implementation flexibility and decoupling ray query support from ray
--     pipelines:
--
--     -   <VK_KHR_acceleration_structure.html VK_KHR_acceleration_structure>
--         (for acceleration structure operations)
--
--     -   <VK_KHR_ray_tracing_pipeline.html VK_KHR_ray_tracing_pipeline>
--         (for ray tracing pipeline and shader stages)
--
--     -   <VK_KHR_ray_query.html VK_KHR_ray_query> (for ray queries in
--         existing shader stages)
--
-- -   clarify buffer usage flags for ray tracing
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--         is left alone in <VK_NV_ray_tracing.html VK_NV_ray_tracing>
--         (required on @scratch@ and @instanceData@)
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--         is added as an alias of
--         'Vulkan.Extensions.VK_NV_ray_tracing.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--         in
--         <VK_KHR_ray_tracing_pipeline.html VK_KHR_ray_tracing_pipeline>
--         and is required on shader binding table buffers
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--         is added in
--         <VK_KHR_acceleration_structure.html VK_KHR_acceleration_structure>
--         for all vertex, index, transform, aabb, and instance buffer data
--         referenced by device build commands
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--         is used for @scratchData@
--
-- -   add max primitive counts (@ppMaxPrimitiveCounts@) to
--     'cmdBuildAccelerationStructuresIndirectKHR'
--
-- -   Allocate acceleration structures from @VkBuffers@ and add a mode to
--     constrain the device address
--
--     -   de-alias
--         'Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
--         'Vulkan.Extensions.VK_NV_ray_tracing.bindAccelerationStructureMemoryNV',
--         and remove @VkBindAccelerationStructureMemoryInfoKHR@,
--         @VkAccelerationStructureMemoryRequirementsInfoKHR@, and
--         @vkGetAccelerationStructureMemoryRequirementsKHR@
--
--     -   acceleration structures now take a
--         'Vulkan.Core10.Handles.Buffer' and offset at creation time for
--         memory placement
--
--     -   add a new
--         'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR'
--         buffer usage such buffers
--
--     -   add a new 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR' acceleration
--         structure type for layering
--
-- -   move 'GEOMETRY_TYPE_INSTANCES_KHR' to main enum instead of being
--     added via extension
--
-- -   make build commands more consistent - all now build multiple
--     acceleration structures and are named plurally
--     ('cmdBuildAccelerationStructuresIndirectKHR',
--     'cmdBuildAccelerationStructuresKHR',
--     'buildAccelerationStructuresKHR')
--
-- -   add interactions with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     for acceleration structures, including a new feature
--     (@descriptorBindingAccelerationStructureUpdateAfterBind@) and 3 new
--     properties (@maxPerStageDescriptorAccelerationStructures@,
--     @maxPerStageDescriptorUpdateAfterBindAccelerationStructures@,
--     @maxDescriptorSetUpdateAfterBindAccelerationStructures@)
--
-- -   extension is no longer provisional
--
-- -   define synchronization requirements for builds, traces, and copies
--
-- -   define synchronization requirements for AS build inputs and indirect
--     build buffer
--
-- (5) What is 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR' for?
--
-- RESOLVED: It is primarily intended for API layering. In DXR, the
-- acceleration structure is basically just a buffer in a special layout,
-- and you don’t know at creation time whether it will be used as a top or
-- bottom level acceleration structure. We thus added a generic
-- acceleration structure type whose type is unknown at creation time, but
-- is specified at build type instead. Applications which are written
-- directly for Vulkan should not use it.
--
-- == Version History
--
-- -   Revision 1, 2019-12-05 (Members of the Vulkan Ray Tracing TSG)
--
--     -   Internal revisions (forked from VK_NV_ray_tracing)
--
-- -   Revision 2, 2019-12-20 (Daniel Koch, Eric Werness)
--
--     -   Add const version of DeviceOrHostAddress (!3515)
--
--     -   Add VU to clarify that only handles in the current pipeline are
--         valid (!3518)
--
--     -   Restore some missing VUs and add in-place update language
--         (#1902, !3522)
--
--     -   rename VkAccelerationStructureInstanceKHR member from
--         accelerationStructure to accelerationStructureReference to
--         better match its type (!3523)
--
--     -   Allow VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS for pipeline
--         creation if shader group handles cannot be re-used. (!3523)
--
--     -   update documentation for the
--         VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS error code and add
--         missing documentation for new return codes from
--         VK_KHR_deferred_host_operations (!3523)
--
--     -   list new query types for VK_KHR_ray_tracing (!3523)
--
--     -   Fix VU statements for VkAccelerationStructureGeometryKHR
--         referring to correct union members and update to use more
--         current wording (!3523)
--
-- -   Revision 3, 2020-01-10 (Daniel Koch, Jon Leech, Christoph Kubisch)
--
--     -   Fix \'instance of\' and \'that\/which contains\/defines\' markup
--         issues (!3528)
--
--     -   factor out VK_KHR_pipeline_library as stand-alone extension
--         (!3540)
--
--     -   Resolve Vulkan-hpp issues (!3543)
--
--     -   add missing require for VkGeometryInstanceFlagsKHR
--
--     -   de-alias VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
--         since the KHR structure is no longer equivalent
--
--     -   add len to pDataSize attribute for
--         vkWriteAccelerationStructuresPropertiesKHR
--
-- -   Revision 4, 2020-01-23 (Daniel Koch, Eric Werness)
--
--     -   Improve vkWriteAccelerationStructuresPropertiesKHR, add return
--         value and VUs (#1947)
--
--     -   Clarify language to allow multiple raygen shaders (#1959)
--
--     -   Various editorial feedback (!3556)
--
--     -   Add language to help deal with looped self-intersecting fans
--         (#1901)
--
--     -   Change vkCmdTraceRays{Indirect}KHR args to pointers (!3559)
--
--     -   Add scratch address validation language (#1941, !3551)
--
--     -   Fix definition and add hierarchy information for shader call
--         scope (#1977, !3571)
--
-- -   Revision 5, 2020-02-04 (Eric Werness, Jeff Bolz, Daniel Koch)
--
--     -   remove vestigial accelerationStructureUUID (!3582)
--
--     -   update definition of repack instructions and improve memory
--         model interactions (#1910, #1913, !3584)
--
--     -   Fix wrong sType for VkPhysicalDeviceRayTracingFeaturesKHR
--         (#1988)
--
--     -   Use provisional SPIR-V capabilities (#1987)
--
--     -   require rayTraversalPrimitiveCulling if rayQuery is supported
--         (#1927)
--
--     -   Miss shaders do not have object parameters (!3592)
--
--     -   Fix missing required types in XML (!3592)
--
--     -   clarify matching conditions for update (!3592)
--
--     -   add goal that host and device builds be similar (!3592)
--
--     -   clarify that @maxPrimitiveCount@ limit should apply to triangles
--         and AABBs (!3592)
--
--     -   Require alignment for instance arrayOfPointers (!3592)
--
--     -   Zero is a valid value for instance flags (!3592)
--
--     -   Add some alignment VUs that got lost in refactoring (!3592)
--
--     -   Recommend TMin epsilon rather than culling (!3592)
--
--     -   Get angle from dot product not cross product (!3592)
--
--     -   Clarify that AH can access the payload and attributes (!3592)
--
--     -   Match DXR behavior for inactive primitive definition (!3592)
--
--     -   Use a more generic term than degenerate for inactive to avoid
--         confusion (!3592)
--
-- -   Revision 6, 2020-02-20 (Daniel Koch)
--
--     -   fix some dangling NV references (#1996)
--
--     -   rename VkCmdTraceRaysIndirectCommandKHR to
--         VkTraceRaysIndirectCommandKHR (!3607)
--
--     -   update contributor list (!3611)
--
--     -   use uint64_t instead of VkAccelerationStructureReferenceKHR in
--         VkAccelerationStructureInstanceKHR (#2004)
--
-- -   Revision 7, 2020-02-28 (Tobias Hector)
--
--     -   remove HitTKHR SPIR-V builtin (spirv\/spirv-extensions#7)
--
-- -   Revision 8, 2020-03-06 (Tobias Hector, Dae Kim, Daniel Koch, Jeff
--     Bolz, Eric Werness)
--
--     -   explicitly state that Tmax is updated when new closest
--         intersection is accepted (#2020,!3536)
--
--     -   Made references to min and max t values consistent (!3644)
--
--     -   finish enumerating differences relative to VK_NV_ray_tracing in
--         issues (1) and (2) (#1974,!3642)
--
--     -   fix formatting in some math equations (!3642)
--
--     -   Restrict the Hit Kind operand of @OpReportIntersectionKHR@ to
--         7-bits (spirv\/spirv-extensions#8,!3646)
--
--     -   Say ray tracing \'/should/\' be watertight (#2008,!3631)
--
--     -   Clarify memory requirements for ray tracing buffers
--         (#2005,!3649)
--
--     -   Add callable size limits (#1997,!3652)
--
-- -   Revision 9, 2020-04-15 (Eric Werness, Daniel Koch, Tobias Hector,
--     Joshua Barczak)
--
--     -   Add geometry flags to acceleration structure creation (!3672)
--
--     -   add build scratch memory alignment
--         (minAccelerationStructureScratchOffsetAlignment) (#2065,!3725)
--
--     -   fix naming and return enum from
--         vkGetDeviceAccelerationStructureCompatibilityKHR (#2051,!3726)
--
--     -   require SPIR-V 1.4 (#2096,!3777)
--
--     -   added creation time capture\/replay flags (#2104,!3774)
--
--     -   require Vulkan 1.1 (#2133,!3806)
--
--     -   use device addresses instead of VkBuffers for ray tracing
--         commands (#2074,!3815)
--
--     -   add interactions with Vulkan 1.2 and VK_KHR_vulkan_memory_model
--         (#2133,!3830)
--
--     -   make VK_KHR_pipeline_library an interaction instead of required
--         (#2045,#2108,!3830)
--
--     -   make VK_KHR_deferred_host_operations an interaction instead of
--         required (#2045,!3830)
--
--     -   removed maxCallableSize and added explicit stack size management
--         for ray pipelines (#1997,!3817,!3772,!3844)
--
--     -   improved documentation for VkAccelerationStructureVersionInfoKHR
--         (#2135,3835)
--
--     -   rename VkAccelerationStructureBuildOffsetInfoKHR to
--         VkAccelerationStructureBuildRangeInfoKHR (#2058,!3754)
--
--     -   Re-unify geometry description between build and create (!3754)
--
--     -   Fix ppGeometries ambiguity, add pGeometries (#2032,!3811)
--
--     -   add interactions with VK_EXT_robustness2 and allow
--         nullDescriptor support for acceleration structures (#1920,!3848)
--
--     -   added future extensibility for AS updates (#2114,!3849)
--
--     -   Fix VU for dispatchrays and add a limit on the size of the full
--         grid (#2160,!3851)
--
--     -   Add shaderGroupHandleAlignment property (#2180,!3875)
--
--     -   Clarify deferred host ops for pipeline creation (#2067,!3813)
--
--     -   Change acceleration structure build to always be sized
--         (#2131,#2197,#2198,!3854,!3883,!3880)
--
-- -   Revision 10, 2020-07-03 (Mathieu Robart, Daniel Koch, Eric Werness,
--     Tobias Hector)
--
--     -   Decomposition of the specification, from VK_KHR_ray_tracing to
--         VK_KHR_acceleration_structure (#1918,!3912)
--
--     -   clarify buffer usage flags for ray tracing (#2181,!3939)
--
--     -   add max primitive counts to build indirect command (#2233,!3944)
--
--     -   Allocate acceleration structures from VkBuffers and add a mode
--         to constrain the device address (#2131,!3936)
--
--     -   Move VK_GEOMETRY_TYPE_INSTANCES_KHR to main enum (#2243,!3952)
--
--     -   make build commands more consistent (#2247,!3958)
--
--     -   add interactions with UPDATE_AFTER_BIND (#2128,!3986)
--
--     -   correct and expand build command VUs (!4020)
--
--     -   fix copy command VUs (!4018)
--
--     -   added various alignment requirements (#2229,!3943)
--
--     -   fix valid usage for arrays of geometryCount items (#2198,!4010)
--
--     -   define what is allowed to change on RTAS updates and relevant
--         VUs (#2177,!3961)
--
-- -   Revision 11, 2020-11-12 (Eric Werness, Josh Barczak, Daniel Koch,
--     Tobias Hector)
--
--     -   de-alias NV and KHR acceleration structure types and associated
--         commands (#2271,!4035)
--
--     -   specify alignment for host copy commands (#2273,!4037)
--
--     -   document
--         'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--
--     -   specify that acceleration structures are non-linear
--         (#2289,!4068)
--
--     -   add several missing VUs for strides, vertexFormat, and indexType
--         (#2315,!4069)
--
--     -   restore VUs for VkAccelerationStructureBuildGeometryInfoKHR
--         (#2337,!4098)
--
--     -   ban multi-instance memory for host operations (#2324,!4102)
--
--     -   allow dstAccelerationStructure to be null for
--         vkGetAccelerationStructureBuildSizesKHR (#2330,!4111)
--
--     -   more build VU cleanup (#2138,#4130)
--
--     -   specify host endianness for AS serialization (#2261,!4136)
--
--     -   add invertible transform matrix VU (#1710,!4140)
--
--     -   require geometryCount to be 1 for TLAS builds (!4145)
--
--     -   improved validity conditions for build addresses (#4142)
--
--     -   add single statement SPIR-V VUs, build limit VUs (!4158)
--
--     -   document limits for vertex and aabb strides (#2390,!4184)
--
--     -   specify that
--         'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--         applies to AS copies (#2382,#4173)
--
--     -   define sync for AS build inputs and indirect buffer
--         (#2407,!4208)
--
-- = See Also
--
-- 'AabbPositionsKHR', 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildRangeInfoKHR',
-- 'AccelerationStructureBuildSizesInfoKHR',
-- 'AccelerationStructureBuildTypeKHR',
-- 'AccelerationStructureCompatibilityKHR',
-- 'AccelerationStructureCreateFlagBitsKHR',
-- 'AccelerationStructureCreateFlagsKHR',
-- 'AccelerationStructureCreateInfoKHR',
-- 'AccelerationStructureDeviceAddressInfoKHR',
-- 'AccelerationStructureGeometryAabbsDataKHR',
-- 'AccelerationStructureGeometryDataKHR',
-- 'AccelerationStructureGeometryInstancesDataKHR',
-- 'AccelerationStructureGeometryKHR',
-- 'AccelerationStructureGeometryTrianglesDataKHR',
-- 'AccelerationStructureInstanceKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'AccelerationStructureTypeKHR', 'AccelerationStructureVersionInfoKHR',
-- 'BuildAccelerationStructureFlagBitsKHR',
-- 'BuildAccelerationStructureFlagsKHR',
-- 'BuildAccelerationStructureModeKHR', 'CopyAccelerationStructureInfoKHR',
-- 'CopyAccelerationStructureModeKHR',
-- 'CopyAccelerationStructureToMemoryInfoKHR',
-- 'CopyMemoryToAccelerationStructureInfoKHR',
-- 'DeviceOrHostAddressConstKHR', 'DeviceOrHostAddressKHR',
-- 'GeometryFlagBitsKHR', 'GeometryFlagsKHR',
-- 'GeometryInstanceFlagBitsKHR', 'GeometryInstanceFlagsKHR',
-- 'GeometryTypeKHR', 'PhysicalDeviceAccelerationStructureFeaturesKHR',
-- 'PhysicalDeviceAccelerationStructurePropertiesKHR',
-- 'TransformMatrixKHR', 'WriteDescriptorSetAccelerationStructureKHR',
-- 'buildAccelerationStructuresKHR',
-- 'cmdBuildAccelerationStructuresIndirectKHR',
-- 'cmdBuildAccelerationStructuresKHR', 'cmdCopyAccelerationStructureKHR',
-- 'cmdCopyAccelerationStructureToMemoryKHR',
-- 'cmdCopyMemoryToAccelerationStructureKHR',
-- 'cmdWriteAccelerationStructuresPropertiesKHR',
-- 'copyAccelerationStructureKHR', 'copyAccelerationStructureToMemoryKHR',
-- 'copyMemoryToAccelerationStructureKHR',
-- 'createAccelerationStructureKHR', 'destroyAccelerationStructureKHR',
-- 'getAccelerationStructureBuildSizesKHR',
-- 'getAccelerationStructureDeviceAddressKHR',
-- 'getDeviceAccelerationStructureCompatibilityKHR',
-- 'writeAccelerationStructuresPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_acceleration_structure  ( destroyAccelerationStructureKHR
                                                        , cmdCopyAccelerationStructureKHR
                                                        , copyAccelerationStructureKHR
                                                        , cmdCopyAccelerationStructureToMemoryKHR
                                                        , copyAccelerationStructureToMemoryKHR
                                                        , cmdCopyMemoryToAccelerationStructureKHR
                                                        , copyMemoryToAccelerationStructureKHR
                                                        , cmdWriteAccelerationStructuresPropertiesKHR
                                                        , writeAccelerationStructuresPropertiesKHR
                                                        , getDeviceAccelerationStructureCompatibilityKHR
                                                        , createAccelerationStructureKHR
                                                        , withAccelerationStructureKHR
                                                        , cmdBuildAccelerationStructuresKHR
                                                        , cmdBuildAccelerationStructuresIndirectKHR
                                                        , buildAccelerationStructuresKHR
                                                        , getAccelerationStructureDeviceAddressKHR
                                                        , getAccelerationStructureBuildSizesKHR
                                                        , WriteDescriptorSetAccelerationStructureKHR(..)
                                                        , PhysicalDeviceAccelerationStructureFeaturesKHR(..)
                                                        , PhysicalDeviceAccelerationStructurePropertiesKHR(..)
                                                        , AccelerationStructureGeometryTrianglesDataKHR(..)
                                                        , AccelerationStructureGeometryAabbsDataKHR(..)
                                                        , AccelerationStructureGeometryInstancesDataKHR(..)
                                                        , AccelerationStructureGeometryKHR(..)
                                                        , AccelerationStructureBuildGeometryInfoKHR(..)
                                                        , AccelerationStructureBuildRangeInfoKHR(..)
                                                        , AccelerationStructureCreateInfoKHR(..)
                                                        , AabbPositionsKHR(..)
                                                        , TransformMatrixKHR(..)
                                                        , AccelerationStructureInstanceKHR(..)
                                                        , AccelerationStructureDeviceAddressInfoKHR(..)
                                                        , AccelerationStructureVersionInfoKHR(..)
                                                        , CopyAccelerationStructureInfoKHR(..)
                                                        , CopyAccelerationStructureToMemoryInfoKHR(..)
                                                        , CopyMemoryToAccelerationStructureInfoKHR(..)
                                                        , AccelerationStructureBuildSizesInfoKHR(..)
                                                        , DeviceOrHostAddressKHR(..)
                                                        , DeviceOrHostAddressConstKHR(..)
                                                        , AccelerationStructureGeometryDataKHR(..)
                                                        , GeometryInstanceFlagsKHR
                                                        , GeometryInstanceFlagBitsKHR( GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR
                                                                                     , GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR
                                                                                     , ..
                                                                                     )
                                                        , GeometryFlagsKHR
                                                        , GeometryFlagBitsKHR( GEOMETRY_OPAQUE_BIT_KHR
                                                                             , GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR
                                                                             , ..
                                                                             )
                                                        , BuildAccelerationStructureFlagsKHR
                                                        , BuildAccelerationStructureFlagBitsKHR( BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR
                                                                                               , BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR
                                                                                               , ..
                                                                                               )
                                                        , AccelerationStructureCreateFlagsKHR
                                                        , AccelerationStructureCreateFlagBitsKHR( ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR
                                                                                                , ..
                                                                                                )
                                                        , CopyAccelerationStructureModeKHR( COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR
                                                                                          , COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR
                                                                                          , ..
                                                                                          )
                                                        , BuildAccelerationStructureModeKHR( BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
                                                                                           , BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR
                                                                                           , ..
                                                                                           )
                                                        , AccelerationStructureTypeKHR( ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                                                                                      , ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
                                                                                      , ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR
                                                                                      , ..
                                                                                      )
                                                        , GeometryTypeKHR( GEOMETRY_TYPE_TRIANGLES_KHR
                                                                         , GEOMETRY_TYPE_AABBS_KHR
                                                                         , GEOMETRY_TYPE_INSTANCES_KHR
                                                                         , ..
                                                                         )
                                                        , AccelerationStructureBuildTypeKHR( ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
                                                                                           , ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
                                                                                           , ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR
                                                                                           , ..
                                                                                           )
                                                        , AccelerationStructureCompatibilityKHR( ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR
                                                                                               , ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR
                                                                                               , ..
                                                                                               )
                                                        , KHR_ACCELERATION_STRUCTURE_SPEC_VERSION
                                                        , pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION
                                                        , KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                                                        , pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                                                        , AccelerationStructureKHR(..)
                                                        , DeferredOperationKHR(..)
                                                        , DebugReportObjectTypeEXT(..)
                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import qualified Data.ByteString (length)
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkBuildAccelerationStructuresKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructuresIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructuresKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureBuildSizesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureDeviceAddressKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceAccelerationStructureCompatibilityKHR))
import Vulkan.Dynamic (DeviceCmds(pVkWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (pattern UUID_SIZE)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyAccelerationStructureKHR - Destroy an acceleration structure
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-accelerationStructure-02442#
--     All submitted commands that refer to @accelerationStructure@ /must/
--     have completed execution
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-accelerationStructure-02443#
--     If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-accelerationStructure-02444#
--     If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-accelerationStructure-parameter#
--     If @accelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @accelerationStructure@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyAccelerationStructureKHR-accelerationStructure-parent#
--     If @accelerationStructure@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @accelerationStructure@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
destroyAccelerationStructureKHR :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the logical device that destroys the buffer.
                                   Device
                                -> -- | @accelerationStructure@ is the acceleration structure to destroy.
                                   AccelerationStructureKHR
                                -> -- | @pAllocator@ controls host memory allocation as described in the
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyAccelerationStructureKHR device accelerationStructure allocator = liftIO . evalContT $ do
  let vkDestroyAccelerationStructureKHRPtr = pVkDestroyAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkDestroyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyAccelerationStructureKHR is null" Nothing Nothing
  let vkDestroyAccelerationStructureKHR' = mkVkDestroyAccelerationStructureKHR vkDestroyAccelerationStructureKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyAccelerationStructureKHR" (vkDestroyAccelerationStructureKHR' (deviceHandle (device)) (accelerationStructure) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureInfoKHR -> IO ()

-- | vkCmdCopyAccelerationStructureKHR - Copy an acceleration structure
--
-- = Description
--
-- Accesses to @pInfo@->@src@ and @pInfo@->@dst@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
-- or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'
-- as appropriate.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-buffer-03737# The @buffer@
--     used to create @pInfo@->@src@ /must/ be bound to device memory
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-buffer-03738# The @buffer@
--     used to create @pInfo@->@dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureInfoKHR' structure
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyAccelerationStructureKHR-renderpass# This command
--     /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyAccelerationStructureInfoKHR'
cmdCopyAccelerationStructureKHR :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pInfo@ is a pointer to a 'CopyAccelerationStructureInfoKHR' structure
                                   -- defining the copy operation.
                                   CopyAccelerationStructureInfoKHR
                                -> io ()
cmdCopyAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureKHRPtr = pVkCmdCopyAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureKHR is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureKHR' = mkVkCmdCopyAccelerationStructureKHR vkCmdCopyAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyAccelerationStructureKHR" (vkCmdCopyAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureInfoKHR -> IO Result

-- | vkCopyAccelerationStructureKHR - Copy an acceleration structure on the
-- host
--
-- = Description
--
-- This command fulfills the same task as 'cmdCopyAccelerationStructureKHR'
-- but is executed by the host.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyAccelerationStructureKHR-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyAccelerationStructureKHR-deferredOperation-03678# Any
--     previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCopyAccelerationStructureKHR-buffer-03727# The @buffer@ used
--     to create @pInfo@->@src@ /must/ be bound to host-visible device
--     memory
--
-- -   #VUID-vkCopyAccelerationStructureKHR-buffer-03728# The @buffer@ used
--     to create @pInfo@->@dst@ /must/ be bound to host-visible device
--     memory
--
-- -   #VUID-vkCopyAccelerationStructureKHR-accelerationStructureHostCommands-03582#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureHostCommands ::accelerationStructureHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyAccelerationStructureKHR-buffer-03780# The @buffer@ used
--     to create @pInfo@->@src@ /must/ be bound to memory that was not
--     allocated with multiple instances
--
-- -   #VUID-vkCopyAccelerationStructureKHR-buffer-03781# The @buffer@ used
--     to create @pInfo@->@dst@ /must/ be bound to memory that was not
--     allocated with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyAccelerationStructureKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyAccelerationStructureKHR-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyAccelerationStructureKHR-pInfo-parameter# @pInfo@ /must/
--     be a valid pointer to a valid 'CopyAccelerationStructureInfoKHR'
--     structure
--
-- -   #VUID-vkCopyAccelerationStructureKHR-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyAccelerationStructureKHR :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the device which owns the acceleration structures.
                                Device
                             -> -- | @deferredOperation@ is an optional
                                -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                                -- for this command.
                                DeferredOperationKHR
                             -> -- | @pInfo@ is a pointer to a 'CopyAccelerationStructureInfoKHR' structure
                                -- defining the copy operation.
                                CopyAccelerationStructureInfoKHR
                             -> io (Result)
copyAccelerationStructureKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureKHRPtr = pVkCopyAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyAccelerationStructureKHR is null" Nothing Nothing
  let vkCopyAccelerationStructureKHR' = mkVkCopyAccelerationStructureKHR vkCopyAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyAccelerationStructureKHR" (vkCopyAccelerationStructureKHR' (deviceHandle (device)) (deferredOperation) pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO ()

-- | vkCmdCopyAccelerationStructureToMemoryKHR - Copy an acceleration
-- structure to device memory
--
-- = Description
--
-- Accesses to @pInfo@->@src@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'.
-- Accesses to the buffer indicated by @pInfo@->@dst.deviceAddress@ /must/
-- be synchronized with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- pipeline stage and an access type of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT'.
--
-- This command produces the same results as
-- 'copyAccelerationStructureToMemoryKHR', but writes its result to a
-- device address, and is executed on the device rather than the host. The
-- output /may/ not necessarily be bit-for-bit identical, but it can be
-- equally used by either 'cmdCopyMemoryToAccelerationStructureKHR' or
-- 'copyMemoryToAccelerationStructureKHR'.
--
-- The defined header structure for the serialized data consists of:
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data matching
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data identifying the
--     compatibility for comparison using
--     'getDeviceAccelerationStructureCompatibilityKHR'
--
-- -   A 64-bit integer of the total size matching the value queried using
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   A 64-bit integer of the deserialized size to be passed in to
--     'AccelerationStructureCreateInfoKHR'::@size@
--
-- -   A 64-bit integer of the count of the number of acceleration
--     structure handles following. This will be zero for a bottom-level
--     acceleration structure.
--
-- The corresponding handles matching the values returned by
-- 'getAccelerationStructureDeviceAddressKHR' or
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
-- are tightly packed in the buffer following the count. The application is
-- expected to store a mapping between those handles and the original
-- application-generated bottom-level acceleration structures to provide
-- when deserializing. The serialized data is written to the buffer (or
-- read from the buffer) according to the host endianness.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-pInfo-03739#
--     @pInfo@->@dst.deviceAddress@ /must/ be a valid device address for a
--     buffer bound to device memory.
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-pInfo-03740#
--     @pInfo@->@dst.deviceAddress@ /must/ be aligned to @256@ bytes
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-pInfo-03741# If the
--     buffer pointed to by @pInfo@->@dst.deviceAddress@ is non-sparse then
--     it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-None-03559# The
--     @buffer@ used to create @pInfo@->@src@ /must/ be bound to device
--     memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureToMemoryInfoKHR' structure
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyAccelerationStructureToMemoryKHR-renderpass# This
--     command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyAccelerationStructureToMemoryInfoKHR'
cmdCopyAccelerationStructureToMemoryKHR :: forall io
                                         . (MonadIO io)
                                        => -- | @commandBuffer@ is the command buffer into which the command will be
                                           -- recorded.
                                           CommandBuffer
                                        -> -- | @pInfo@ is an a pointer to a 'CopyAccelerationStructureToMemoryInfoKHR'
                                           -- structure defining the copy operation.
                                           CopyAccelerationStructureToMemoryInfoKHR
                                        -> io ()
cmdCopyAccelerationStructureToMemoryKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureToMemoryKHRPtr = pVkCmdCopyAccelerationStructureToMemoryKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyAccelerationStructureToMemoryKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureToMemoryKHR is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureToMemoryKHR' = mkVkCmdCopyAccelerationStructureToMemoryKHR vkCmdCopyAccelerationStructureToMemoryKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyAccelerationStructureToMemoryKHR" (vkCmdCopyAccelerationStructureToMemoryKHR' (commandBufferHandle (commandBuffer)) pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyAccelerationStructureToMemoryInfoKHR -> IO Result

-- | vkCopyAccelerationStructureToMemoryKHR - Serialize an acceleration
-- structure on the host
--
-- = Description
--
-- This command fulfills the same task as
-- 'cmdCopyAccelerationStructureToMemoryKHR' but is executed by the host.
--
-- This command produces the same results as
-- 'cmdCopyAccelerationStructureToMemoryKHR', but writes its result
-- directly to a host pointer, and is executed on the host rather than the
-- device. The output /may/ not necessarily be bit-for-bit identical, but
-- it can be equally used by either
-- 'cmdCopyMemoryToAccelerationStructureKHR' or
-- 'copyMemoryToAccelerationStructureKHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-deferredOperation-03677#
--     If @deferredOperation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-deferredOperation-03678#
--     Any previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-buffer-03731# The
--     @buffer@ used to create @pInfo@->@src@ /must/ be bound to
--     host-visible device memory
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-pInfo-03732#
--     @pInfo@->@dst.hostAddress@ /must/ be a valid host pointer
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-pInfo-03751#
--     @pInfo@->@dst.hostAddress@ /must/ be aligned to 16 bytes
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-accelerationStructureHostCommands-03584#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureHostCommands ::accelerationStructureHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-buffer-03783# The
--     @buffer@ used to create @pInfo@->@src@ /must/ be bound to memory
--     that was not allocated with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-deferredOperation-parameter#
--     If @deferredOperation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @deferredOperation@ /must/
--     be a valid 'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureToMemoryInfoKHR' structure
--
-- -   #VUID-vkCopyAccelerationStructureToMemoryKHR-deferredOperation-parent#
--     If @deferredOperation@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyAccelerationStructureToMemoryInfoKHR',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyAccelerationStructureToMemoryKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the device which owns @pInfo@->@src@.
                                        Device
                                     -> -- | @deferredOperation@ is an optional
                                        -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                                        -- for this command.
                                        DeferredOperationKHR
                                     -> -- | @pInfo@ is a pointer to a 'CopyAccelerationStructureToMemoryInfoKHR'
                                        -- structure defining the copy operation.
                                        CopyAccelerationStructureToMemoryInfoKHR
                                     -> io (Result)
copyAccelerationStructureToMemoryKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureToMemoryKHRPtr = pVkCopyAccelerationStructureToMemoryKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyAccelerationStructureToMemoryKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyAccelerationStructureToMemoryKHR is null" Nothing Nothing
  let vkCopyAccelerationStructureToMemoryKHR' = mkVkCopyAccelerationStructureToMemoryKHR vkCopyAccelerationStructureToMemoryKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyAccelerationStructureToMemoryKHR" (vkCopyAccelerationStructureToMemoryKHR' (deviceHandle (device)) (deferredOperation) pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO ()

-- | vkCmdCopyMemoryToAccelerationStructureKHR - Copy device memory to an
-- acceleration structure
--
-- = Description
--
-- Accesses to @pInfo@->@dst@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'.
-- Accesses to the buffer indicated by @pInfo@->@src.deviceAddress@ /must/
-- be synchronized with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- pipeline stage and an access type of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_READ_BIT'.
--
-- This command can accept acceleration structures produced by either
-- 'cmdCopyAccelerationStructureToMemoryKHR' or
-- 'copyAccelerationStructureToMemoryKHR'.
--
-- The structure provided as input to deserialize is as described in
-- 'cmdCopyAccelerationStructureToMemoryKHR', with any acceleration
-- structure handles filled in with the newly-queried handles to bottom
-- level acceleration structures created before deserialization. These do
-- not need to be built at deserialize time, but /must/ be created.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-pInfo-03742#
--     @pInfo@->@src.deviceAddress@ /must/ be a valid device address for a
--     buffer bound to device memory.
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-pInfo-03743#
--     @pInfo@->@src.deviceAddress@ /must/ be aligned to @256@ bytes
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-pInfo-03744# If the
--     buffer pointed to by @pInfo@->@src.deviceAddress@ is non-sparse then
--     it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-buffer-03745# The
--     @buffer@ used to create @pInfo@->@dst@ /must/ be bound to device
--     memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryToAccelerationStructureInfoKHR' structure
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyMemoryToAccelerationStructureKHR-renderpass# This
--     command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyMemoryToAccelerationStructureInfoKHR'
cmdCopyMemoryToAccelerationStructureKHR :: forall io
                                         . (MonadIO io)
                                        => -- | @commandBuffer@ is the command buffer into which the command will be
                                           -- recorded.
                                           CommandBuffer
                                        -> -- | @pInfo@ is a pointer to a 'CopyMemoryToAccelerationStructureInfoKHR'
                                           -- structure defining the copy operation.
                                           CopyMemoryToAccelerationStructureInfoKHR
                                        -> io ()
cmdCopyMemoryToAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMemoryToAccelerationStructureKHRPtr = pVkCmdCopyMemoryToAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyMemoryToAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryToAccelerationStructureKHR is null" Nothing Nothing
  let vkCmdCopyMemoryToAccelerationStructureKHR' = mkVkCmdCopyMemoryToAccelerationStructureKHR vkCmdCopyMemoryToAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyMemoryToAccelerationStructureKHR" (vkCmdCopyMemoryToAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToAccelerationStructureInfoKHR -> IO Result

-- | vkCopyMemoryToAccelerationStructureKHR - Deserialize an acceleration
-- structure on the host
--
-- = Description
--
-- This command fulfills the same task as
-- 'cmdCopyMemoryToAccelerationStructureKHR' but is executed by the host.
--
-- This command can accept acceleration structures produced by either
-- 'cmdCopyAccelerationStructureToMemoryKHR' or
-- 'copyAccelerationStructureToMemoryKHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-deferredOperation-03677#
--     If @deferredOperation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-deferredOperation-03678#
--     Any previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-pInfo-03729#
--     @pInfo@->@src.hostAddress@ /must/ be a valid host pointer
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-pInfo-03750#
--     @pInfo@->@src.hostAddress@ /must/ be aligned to 16 bytes
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-buffer-03730# The
--     @buffer@ used to create @pInfo@->@dst@ /must/ be bound to
--     host-visible device memory
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-accelerationStructureHostCommands-03583#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureHostCommands ::accelerationStructureHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-buffer-03782# The
--     @buffer@ used to create @pInfo@->@dst@ /must/ be bound to memory
--     that was not allocated with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-deferredOperation-parameter#
--     If @deferredOperation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @deferredOperation@ /must/
--     be a valid 'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryToAccelerationStructureInfoKHR' structure
--
-- -   #VUID-vkCopyMemoryToAccelerationStructureKHR-deferredOperation-parent#
--     If @deferredOperation@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyMemoryToAccelerationStructureKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the device which owns @pInfo->dst@.
                                        Device
                                     -> -- | @deferredOperation@ is an optional
                                        -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                                        -- for this command.
                                        DeferredOperationKHR
                                     -> -- | @pInfo@ is a pointer to a 'CopyMemoryToAccelerationStructureInfoKHR'
                                        -- structure defining the copy operation.
                                        CopyMemoryToAccelerationStructureInfoKHR
                                     -> io (Result)
copyMemoryToAccelerationStructureKHR device deferredOperation info = liftIO . evalContT $ do
  let vkCopyMemoryToAccelerationStructureKHRPtr = pVkCopyMemoryToAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCopyMemoryToAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMemoryToAccelerationStructureKHR is null" Nothing Nothing
  let vkCopyMemoryToAccelerationStructureKHR' = mkVkCopyMemoryToAccelerationStructureKHR vkCopyMemoryToAccelerationStructureKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyMemoryToAccelerationStructureKHR" (vkCopyMemoryToAccelerationStructureKHR' (deviceHandle (device)) (deferredOperation) pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteAccelerationStructuresPropertiesKHR - Write acceleration
-- structure result parameters to query results.
--
-- = Description
--
-- Accesses to any of the acceleration structures listed in
-- @pAccelerationStructures@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-queryPool-02493#
--     @queryPool@ /must/ have been created with a @queryType@ matching
--     @queryType@
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-queryPool-02494#
--     The queries identified by @queryPool@ and @firstQuery@ /must/ be
--     /unavailable/
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-buffer-03736#
--     The @buffer@ used to create each acceleration structure in
--     @pAccelerationStructures@ /must/ be bound to device memory
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-accelerationStructures-03431#
--     All acceleration structures in @pAccelerationStructures@ /must/ have
--     been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' if
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-queryType-03432#
--     @queryType@ /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-pAccelerationStructures-parameter#
--     @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-queryType-parameter#
--     @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-queryPool-parameter#
--     @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-renderpass# This
--     command /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-accelerationStructureCount-arraylength#
--     @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesKHR-commonparent#
--     Each of @commandBuffer@, @queryPool@, and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
cmdWriteAccelerationStructuresPropertiesKHR :: forall io
                                             . (MonadIO io)
                                            => -- | @commandBuffer@ is the command buffer into which the command will be
                                               -- recorded.
                                               CommandBuffer
                                            -> -- | @pAccelerationStructures@ is a pointer to an array of existing
                                               -- previously built acceleration structures.
                                               ("accelerationStructures" ::: Vector AccelerationStructureKHR)
                                            -> -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
                                               -- specifying the type of queries managed by the pool.
                                               QueryType
                                            -> -- | @queryPool@ is the query pool that will manage the results of the query.
                                               QueryPool
                                            -> -- | @firstQuery@ is the first query index within the query pool that will
                                               -- contain the @accelerationStructureCount@ number of results.
                                               ("firstQuery" ::: Word32)
                                            -> io ()
cmdWriteAccelerationStructuresPropertiesKHR commandBuffer accelerationStructures queryType queryPool firstQuery = liftIO . evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesKHRPtr = pVkCmdWriteAccelerationStructuresPropertiesKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWriteAccelerationStructuresPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteAccelerationStructuresPropertiesKHR is null" Nothing Nothing
  let vkCmdWriteAccelerationStructuresPropertiesKHR' = mkVkCmdWriteAccelerationStructuresPropertiesKHR vkCmdWriteAccelerationStructuresPropertiesKHRPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  lift $ traceAroundEvent "vkCmdWriteAccelerationStructuresPropertiesKHR" (vkCmdWriteAccelerationStructuresPropertiesKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result) -> Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result

-- | vkWriteAccelerationStructuresPropertiesKHR - Query acceleration
-- structure meta-data on the host
--
-- = Description
--
-- This command fulfills the same task as
-- 'cmdWriteAccelerationStructuresPropertiesKHR' but is executed by the
-- host.
--
-- == Valid Usage
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-accelerationStructures-03431#
--     All acceleration structures in @pAccelerationStructures@ /must/ have
--     been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' if
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-03432#
--     @queryType@ /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-03448# If
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR',
--     then @stride@ /must/ be a multiple of the size of
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-03449# If
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR',
--     then @data@ /must/ point to a
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-03450# If
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR',
--     then @stride@ /must/ be a multiple of the size of
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-03451# If
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR',
--     then @data@ /must/ point to a
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-dataSize-03452#
--     @dataSize@ /must/ be greater than or equal to
--     @accelerationStructureCount@*@stride@
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-buffer-03733# The
--     @buffer@ used to create each acceleration structure in
--     @pAccelerationStructures@ /must/ be bound to host-visible device
--     memory
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-accelerationStructureHostCommands-03585#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureHostCommands ::accelerationStructureHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-buffer-03784# The
--     @buffer@ used to create each acceleration structure in
--     @pAccelerationStructures@ /must/ be bound to memory that was not
--     allocated with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-pAccelerationStructures-parameter#
--     @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-queryType-parameter#
--     @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-pData-parameter#
--     @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-accelerationStructureCount-arraylength#
--     @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-dataSize-arraylength#
--     @dataSize@ /must/ be greater than @0@
--
-- -   #VUID-vkWriteAccelerationStructuresPropertiesKHR-pAccelerationStructures-parent#
--     Each element of @pAccelerationStructures@ /must/ have been created,
--     allocated, or retrieved from @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
writeAccelerationStructuresPropertiesKHR :: forall io
                                          . (MonadIO io)
                                         => -- | @device@ is the device which owns the acceleration structures in
                                            -- @pAccelerationStructures@.
                                            Device
                                         -> -- | @pAccelerationStructures@ points to an array of existing previously
                                            -- built acceleration structures.
                                            ("accelerationStructures" ::: Vector AccelerationStructureKHR)
                                         -> -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
                                            -- specifying the property to be queried.
                                            QueryType
                                         -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                                            ("dataSize" ::: Word64)
                                         -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                                            -- be written.
                                            ("data" ::: Ptr ())
                                         -> -- | @stride@ is the stride in bytes between results for individual queries
                                            -- within @pData@.
                                            ("stride" ::: Word64)
                                         -> io ()
writeAccelerationStructuresPropertiesKHR device accelerationStructures queryType dataSize data' stride = liftIO . evalContT $ do
  let vkWriteAccelerationStructuresPropertiesKHRPtr = pVkWriteAccelerationStructuresPropertiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkWriteAccelerationStructuresPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWriteAccelerationStructuresPropertiesKHR is null" Nothing Nothing
  let vkWriteAccelerationStructuresPropertiesKHR' = mkVkWriteAccelerationStructuresPropertiesKHR vkWriteAccelerationStructuresPropertiesKHRPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  r <- lift $ traceAroundEvent "vkWriteAccelerationStructuresPropertiesKHR" (vkWriteAccelerationStructuresPropertiesKHR' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (CSize (dataSize)) (data') (CSize (stride)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceAccelerationStructureCompatibilityKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureVersionInfoKHR -> Ptr AccelerationStructureCompatibilityKHR -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureVersionInfoKHR -> Ptr AccelerationStructureCompatibilityKHR -> IO ()

-- | vkGetDeviceAccelerationStructureCompatibilityKHR - Check if a serialized
-- acceleration structure is compatible with the current device
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceAccelerationStructureCompatibilityKHR-rayTracingPipeline-03661#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceAccelerationStructureCompatibilityKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceAccelerationStructureCompatibilityKHR-pVersionInfo-parameter#
--     @pVersionInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureVersionInfoKHR' structure
--
-- -   #VUID-vkGetDeviceAccelerationStructureCompatibilityKHR-pCompatibility-parameter#
--     @pCompatibility@ /must/ be a valid pointer to a
--     'AccelerationStructureCompatibilityKHR' value
--
-- = See Also
--
-- 'AccelerationStructureCompatibilityKHR',
-- 'AccelerationStructureVersionInfoKHR', 'Vulkan.Core10.Handles.Device'
getDeviceAccelerationStructureCompatibilityKHR :: forall io
                                                . (MonadIO io)
                                               => -- | @device@ is the device to check the version against.
                                                  Device
                                               -> -- | @pVersionInfo@ points to the 'AccelerationStructureVersionInfoKHR'
                                                  -- version information to check against the device.
                                                  AccelerationStructureVersionInfoKHR
                                               -> io (AccelerationStructureCompatibilityKHR)
getDeviceAccelerationStructureCompatibilityKHR device versionInfo = liftIO . evalContT $ do
  let vkGetDeviceAccelerationStructureCompatibilityKHRPtr = pVkGetDeviceAccelerationStructureCompatibilityKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceAccelerationStructureCompatibilityKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceAccelerationStructureCompatibilityKHR is null" Nothing Nothing
  let vkGetDeviceAccelerationStructureCompatibilityKHR' = mkVkGetDeviceAccelerationStructureCompatibilityKHR vkGetDeviceAccelerationStructureCompatibilityKHRPtr
  pVersionInfo <- ContT $ withCStruct (versionInfo)
  pPCompatibility <- ContT $ bracket (callocBytes @AccelerationStructureCompatibilityKHR 4) free
  lift $ traceAroundEvent "vkGetDeviceAccelerationStructureCompatibilityKHR" (vkGetDeviceAccelerationStructureCompatibilityKHR' (deviceHandle (device)) pVersionInfo (pPCompatibility))
  pCompatibility <- lift $ peek @AccelerationStructureCompatibilityKHR pPCompatibility
  pure $ (pCompatibility)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result

-- | vkCreateAccelerationStructureKHR - Create a new acceleration structure
-- object
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape”. The type and quantity
-- of geometry that can be built into an acceleration structure is
-- determined by the parameters of 'AccelerationStructureCreateInfoKHR'.
--
-- Populating the data in the object after allocating and binding memory is
-- done with commands such as 'cmdBuildAccelerationStructuresKHR',
-- 'buildAccelerationStructuresKHR', 'cmdCopyAccelerationStructureKHR', and
-- 'copyAccelerationStructureKHR'.
--
-- The input buffers passed to acceleration structure build commands will
-- be referenced by the implementation for the duration of the command.
-- After the command completes, the acceleration structure /may/ hold a
-- reference to any acceleration structure specified by an active instance
-- contained therein. Apart from this referencing, acceleration structures
-- /must/ be fully self-contained. The application /may/ re-use or free any
-- memory which was used by the command as an input or as scratch without
-- affecting the results of ray traversal.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateAccelerationStructureKHR-accelerationStructure-03611#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructure accelerationStructure>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateAccelerationStructureKHR-deviceAddress-03488# If
--     'AccelerationStructureCreateInfoKHR'::@deviceAddress@ is not zero,
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureCaptureReplay accelerationStructureCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateAccelerationStructureKHR-device-03489# If @device@ was
--     created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateAccelerationStructureKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateAccelerationStructureKHR-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCreateInfoKHR' structure
--
-- -   #VUID-vkCreateAccelerationStructureKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateAccelerationStructureKHR-pAccelerationStructure-parameter#
--     @pAccelerationStructure@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
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
--     -   'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
createAccelerationStructureKHR :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the acceleration structure
                                  -- object.
                                  Device
                               -> -- | @pCreateInfo@ is a pointer to a 'AccelerationStructureCreateInfoKHR'
                                  -- structure containing parameters affecting creation of the acceleration
                                  -- structure.
                                  AccelerationStructureCreateInfoKHR
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (AccelerationStructureKHR)
createAccelerationStructureKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreateAccelerationStructureKHRPtr = pVkCreateAccelerationStructureKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateAccelerationStructureKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAccelerationStructureKHR is null" Nothing Nothing
  let vkCreateAccelerationStructureKHR' = mkVkCreateAccelerationStructureKHR vkCreateAccelerationStructureKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureKHR 8) free
  r <- lift $ traceAroundEvent "vkCreateAccelerationStructureKHR" (vkCreateAccelerationStructureKHR' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureKHR pPAccelerationStructure
  pure $ (pAccelerationStructure)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAccelerationStructureKHR' and 'destroyAccelerationStructureKHR'
--
-- To ensure that 'destroyAccelerationStructureKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAccelerationStructureKHR :: forall io r . MonadIO io => Device -> AccelerationStructureCreateInfoKHR -> Maybe AllocationCallbacks -> (io AccelerationStructureKHR -> (AccelerationStructureKHR -> io ()) -> r) -> r
withAccelerationStructureKHR device pCreateInfo pAllocator b =
  b (createAccelerationStructureKHR device pCreateInfo pAllocator)
    (\(o0) -> destroyAccelerationStructureKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructuresKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO ()

-- | vkCmdBuildAccelerationStructuresKHR - Build an acceleration structure
--
-- = Description
--
-- The 'cmdBuildAccelerationStructuresKHR' command provides the ability to
-- initiate multiple acceleration structures builds, however there is no
-- ordering or synchronization implied between any of the individual
-- acceleration structure builds.
--
-- Note
--
-- This means that an application /cannot/ build a top-level acceleration
-- structure in the same 'cmdBuildAccelerationStructuresKHR' call as the
-- associated bottom-level or instance acceleration structures are being
-- built. There also /cannot/ be any memory aliasing between any
-- acceleration structure memories or scratch memories being used by any of
-- the builds.
--
-- Accesses to the acceleration structure scratch buffers as identified by
-- the 'AccelerationStructureBuildGeometryInfoKHR'→@scratchData@ buffer
-- device addresses /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
-- or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'.
-- Similarly for accesses to each
-- 'AccelerationStructureBuildGeometryInfoKHR'→@srcAccelerationStructure@
-- and
-- 'AccelerationStructureBuildGeometryInfoKHR'→@dstAccelerationStructure@.
--
-- Accesses to other input buffers as identified by any used values of
-- 'AccelerationStructureGeometryTrianglesDataKHR'→@vertexData@,
-- 'AccelerationStructureGeometryTrianglesDataKHR'→@indexData@,
-- 'AccelerationStructureGeometryTrianglesDataKHR'→@transformData@,
-- 'AccelerationStructureGeometryAabbsDataKHR'→@data@, and
-- 'AccelerationStructureGeometryInstancesDataKHR'→@data@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_SHADER_READ_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-mode-04628# The @mode@
--     member of each element of @pInfos@ /must/ be a valid
--     'BuildAccelerationStructureModeKHR' value
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-srcAccelerationStructure-04629#
--     If the @srcAccelerationStructure@ member of any element of @pInfos@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     @srcAccelerationStructure@ member /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-04630# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03403# The
--     @srcAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03698#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03800#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03699# For each
--     element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03700# For each
--     element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03663# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive primitives>
--     in its @srcAccelerationStructure@ member /must/ not be made active
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03664# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', active primitives in
--     its @srcAccelerationStructure@ member /must/ not be made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-None-03407# The
--     @dstAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03701#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @srcAccelerationStructure@ member of
--     any other element of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', which is accessed by
--     this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03702#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @dstAccelerationStructure@ member of
--     any other element of @pInfos@, which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03703#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @scratchData@ member of any element
--     of @pInfos@ (including the same element), which is accessed by this
--     command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-scratchData-03704# The
--     range of memory backing the @scratchData@ member of any element of
--     @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @scratchData@ member of any other element of
--     @pInfos@, which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-scratchData-03705# The
--     range of memory backing the @scratchData@ member of any element of
--     @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @srcAccelerationStructure@ member of any element
--     of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' (including the same
--     element), which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-dstAccelerationStructure-03706#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing any acceleration structure referenced by
--     the @geometry.instances.data@ member of any element of @pGeometries@
--     or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@,
--     which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03666# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03667# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' set in
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03668# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ and @dstAccelerationStructure@ members
--     /must/ either be the same
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR', or not have
--     any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03758# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @geometryCount@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03759# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @flags@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03760# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @type@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03761# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @geometryType@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03762# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @flags@ member /must/
--     have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03763# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.vertexFormat@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03764# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.maxVertex@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03765# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.indexType@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03766# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was NULL when
--     @srcAccelerationStructure@ was last built, then it must be NULL.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03767# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was not NULL when
--     @srcAccelerationStructure@ was last built, then it may not be NULL.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03768# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', and @geometry.triangles.indexType@ is
--     not 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then the
--     value of each index referenced index must be the same as the
--     corresponding index value when @srcAccelerationStructure@ was last
--     built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-primitiveCount-03769# For
--     each 'AccelerationStructureBuildRangeInfoKHR' referenced by this
--     command, its @primitiveCount@ member /must/ have the same value
--     which was specified when @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-firstVertex-03770# For
--     each 'AccelerationStructureBuildRangeInfoKHR' referenced by this
--     command, if the corresponding geometry uses indices, its
--     @firstVertex@ member /must/ have the same value which was specified
--     when @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03801# For each
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', the
--     corresponding @ppBuildRangeInfos@[i][j].@primitiveCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxInstanceCount@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03707# For each
--     element of @pInfos@, the @buffer@ used to create its
--     @dstAccelerationStructure@ member /must/ be bound to device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03708# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' the @buffer@ used to
--     create its @srcAccelerationStructure@ member /must/ be bound to
--     device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03709# For each
--     element of @pInfos@, the @buffer@ used to create each acceleration
--     structure referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' /must/ be bound to device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03671# If
--     @pInfos@[i].@mode@ is 'BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR',
--     all addresses between @pInfos@[i].@scratchData.deviceAddress@ and
--     @pInfos@[i].@scratchData.deviceAddress@ + N - 1 /must/ be in the
--     buffer device address range of the same buffer, where N is given by
--     the @buildScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03672# If
--     @pInfos@[i].@mode@ is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', all addresses
--     between @pInfos@[i].@scratchData.deviceAddress@ and
--     @pInfos@[i].@scratchData.deviceAddress@ + N - 1 /must/ be in the
--     buffer device address range of the same buffer, where N is given by
--     the @updateScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-geometry-03673# The
--     buffers from which the buffer device addresses for all of the
--     @geometry.triangles.vertexData@, @geometry.triangles.indexData@,
--     @geometry.triangles.transformData@, @geometry.aabbs.data@, and
--     @geometry.instances.data@ members of all @pInfos@[i].@pGeometries@
--     and @pInfos@[i].@ppGeometries@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03674# The buffer
--     from which the buffer device address
--     @pInfos@[i].@scratchData.deviceAddress@ is queried /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03802# For each
--     element of @pInfos@, its @scratchData.deviceAddress@ member /must/
--     be a valid device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03803# For each
--     element of @pInfos@, if @scratchData.deviceAddress@ is the address
--     of a non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03710# For each
--     element of @pInfos@, its @scratchData.deviceAddress@ member /must/
--     be a multiple of
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@minAccelerationStructureScratchOffsetAlignment@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03804# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @geometry.triangles.vertexData.deviceAddress@ /must/ be a valid
--     device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03805# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.vertexData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03711# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @geometry.triangles.vertexData.deviceAddress@ /must/ be aligned to
--     the size in bytes of the smallest component of the format in
--     @vertexFormat@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03806# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR',
--     @geometry.triangles.indexData.deviceAddress@ /must/ be a valid
--     device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03807# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', if
--     @geometry.triangles.indexData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03712# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', and with
--     @geometry.triangles.indexType@ not equal to
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR',
--     @geometry.triangles.indexData.deviceAddress@ /must/ be aligned to
--     the size in bytes of the type in @indexType@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03808# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is not @0@, it
--     /must/ be a valid device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03809# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03810# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is not @0@, it
--     /must/ be aligned to @16@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03811# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_AABBS_KHR',
--     @geometry.aabbs.data.deviceAddress@ /must/ be a valid device address
--     obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03812# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_AABBS_KHR', if
--     @geometry.aabbs.data.deviceAddress@ is the address of a non-sparse
--     buffer then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03714# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_AABBS_KHR',
--     @geometry.aabbs.data.deviceAddress@ /must/ be aligned to @8@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03715# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', if
--     @geometry.arrayOfPointers@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE',
--     @geometry.instances.data.deviceAddress@ /must/ be aligned to @16@
--     bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03716# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', if
--     @geometry.arrayOfPointers@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @geometry.instances.data.deviceAddress@ /must/ be aligned to @8@
--     bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03717# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', if
--     @geometry.arrayOfPointers@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     each element of @geometry.instances.data.deviceAddress@ in device
--     memory /must/ be aligned to @16@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03813# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR',
--     @geometry.instances.data.deviceAddress@ /must/ be a valid device
--     address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03814# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', if
--     @geometry.instances.data.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03815# For any
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', each
--     'AccelerationStructureInstanceKHR'::@accelerationStructureReference@
--     value in @geometry.instances.data.deviceAddress@ /must/ be a valid
--     device address containing a value obtained from
--     'getAccelerationStructureDeviceAddressKHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-03675# For each
--     @pInfos@[i], @dstAccelerationStructure@ /must/ have been created
--     with a value of 'AccelerationStructureCreateInfoKHR'::@size@ greater
--     than or equal to the memory size required by the build operation, as
--     returned by 'getAccelerationStructureBuildSizesKHR' with
--     @pBuildInfo@ = @pInfos@[i] and with each element of the
--     @pMaxPrimitiveCounts@ array greater than or equal to the equivalent
--     @ppBuildRangeInfos@[i][j].@primitiveCount@ values for @j@ in
--     [0,@pInfos@[i].@geometryCount@)
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-ppBuildRangeInfos-03676#
--     Each element of @ppBuildRangeInfos@[i] /must/ be a valid pointer to
--     an array of @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildRangeInfoKHR' structures
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-pInfos-parameter# @pInfos@
--     /must/ be a valid pointer to an array of @infoCount@ valid
--     'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-ppBuildRangeInfos-parameter#
--     @ppBuildRangeInfos@ /must/ be a valid pointer to an array of
--     @infoCount@ 'AccelerationStructureBuildRangeInfoKHR' structures
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdBuildAccelerationStructuresKHR-infoCount-arraylength#
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildRangeInfoKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdBuildAccelerationStructuresKHR :: forall io
                                   . (MonadIO io)
                                  => -- | @commandBuffer@ is the command buffer into which the command will be
                                     -- recorded.
                                     CommandBuffer
                                  -> -- | @pInfos@ is an array of @infoCount@
                                     -- 'AccelerationStructureBuildGeometryInfoKHR' structures defining the
                                     -- geometry used to build each acceleration structure.
                                     ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                                  -> -- | @ppBuildRangeInfos@ is an array of @infoCount@ pointers to arrays of
                                     -- 'AccelerationStructureBuildRangeInfoKHR' structures. Each
                                     -- @ppBuildRangeInfos@[i] is an array of @pInfos@[i].@geometryCount@
                                     -- 'AccelerationStructureBuildRangeInfoKHR' structures defining dynamic
                                     -- offsets to the addresses where geometry data is stored, as defined by
                                     -- @pInfos@[i].
                                     ("buildRangeInfos" ::: Vector (Vector AccelerationStructureBuildRangeInfoKHR))
                                  -> io ()
cmdBuildAccelerationStructuresKHR commandBuffer infos buildRangeInfos = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructuresKHRPtr = pVkCmdBuildAccelerationStructuresKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructuresKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructuresKHR is null" Nothing Nothing
  let vkCmdBuildAccelerationStructuresKHR' = mkVkCmdBuildAccelerationStructuresKHR vkCmdBuildAccelerationStructuresKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (buildRangeInfos)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppBuildRangeInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPpBuildRangeInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildRangeInfoKHR) ((Data.Vector.length (buildRangeInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpBuildRangeInfos' <- ContT $ allocaBytesAligned @AccelerationStructureBuildRangeInfoKHR ((Data.Vector.length (e)) * 16) 4
    lift $ Data.Vector.imapM_ (\i' e' -> poke (pPpBuildRangeInfos' `plusPtr` (16 * (i')) :: Ptr AccelerationStructureBuildRangeInfoKHR) (e')) (e)
    lift $ poke (pPpBuildRangeInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) (pPpBuildRangeInfos')) (buildRangeInfos)
  lift $ traceAroundEvent "vkCmdBuildAccelerationStructuresKHR" (vkCmdBuildAccelerationStructuresKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpBuildRangeInfos))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructuresIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr DeviceAddress -> Ptr Word32 -> Ptr (Ptr Word32) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr DeviceAddress -> Ptr Word32 -> Ptr (Ptr Word32) -> IO ()

-- | vkCmdBuildAccelerationStructuresIndirectKHR - Build an acceleration
-- structure with some parameters provided on the device
--
-- = Description
--
-- Accesses to acceleration structures, scratch buffers, vertex buffers,
-- index buffers, and instance buffers must be synchronized as with
-- 'cmdBuildAccelerationStructuresKHR'.
--
-- Accesses to any element of @pIndirectDeviceAddresses@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_INDIRECT_COMMAND_READ_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-mode-04628# The
--     @mode@ member of each element of @pInfos@ /must/ be a valid
--     'BuildAccelerationStructureModeKHR' value
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-srcAccelerationStructure-04629#
--     If the @srcAccelerationStructure@ member of any element of @pInfos@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     @srcAccelerationStructure@ member /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-04630# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03403# The
--     @srcAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03698#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03800#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03699# For
--     each element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03700# For
--     each element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03663# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive primitives>
--     in its @srcAccelerationStructure@ member /must/ not be made active
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03664# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', active primitives in
--     its @srcAccelerationStructure@ member /must/ not be made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-None-03407# The
--     @dstAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03701#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @srcAccelerationStructure@ member of
--     any other element of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', which is accessed by
--     this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03702#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @dstAccelerationStructure@ member of
--     any other element of @pInfos@, which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03703#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @scratchData@ member of any element
--     of @pInfos@ (including the same element), which is accessed by this
--     command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-scratchData-03704#
--     The range of memory backing the @scratchData@ member of any element
--     of @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @scratchData@ member of any other element of
--     @pInfos@, which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-scratchData-03705#
--     The range of memory backing the @scratchData@ member of any element
--     of @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @srcAccelerationStructure@ member of any element
--     of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' (including the same
--     element), which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-dstAccelerationStructure-03706#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing any acceleration structure referenced by
--     the @geometry.instances.data@ member of any element of @pGeometries@
--     or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@,
--     which is accessed by this command
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03666# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03667# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' set in
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03668# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ and @dstAccelerationStructure@ members
--     /must/ either be the same
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR', or not have
--     any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03758# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @geometryCount@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03759# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @flags@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03760# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @type@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03761# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @geometryType@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03762# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @flags@ member /must/
--     have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03763# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.vertexFormat@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03764# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.maxVertex@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03765# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.indexType@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03766# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was NULL when
--     @srcAccelerationStructure@ was last built, then it must be NULL.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03767# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was not NULL when
--     @srcAccelerationStructure@ was last built, then it may not be NULL.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03768# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', and @geometry.triangles.indexType@ is
--     not 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then the
--     value of each index referenced index must be the same as the
--     corresponding index value when @srcAccelerationStructure@ was last
--     built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-primitiveCount-03769#
--     For each 'AccelerationStructureBuildRangeInfoKHR' referenced by this
--     command, its @primitiveCount@ member /must/ have the same value
--     which was specified when @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-firstVertex-03770#
--     For each 'AccelerationStructureBuildRangeInfoKHR' referenced by this
--     command, if the corresponding geometry uses indices, its
--     @firstVertex@ member /must/ have the same value which was specified
--     when @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03801# For
--     each element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', the corresponding
--     @ppMaxPrimitiveCounts@[i][j] /must/ be less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxInstanceCount@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03707# For
--     each element of @pInfos@, the @buffer@ used to create its
--     @dstAccelerationStructure@ member /must/ be bound to device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03708# For
--     each element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' the @buffer@ used to
--     create its @srcAccelerationStructure@ member /must/ be bound to
--     device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03709# For
--     each element of @pInfos@, the @buffer@ used to create each
--     acceleration structure referenced by the @geometry.instances.data@
--     member of any element of @pGeometries@ or @ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR' /must/ be bound to
--     device memory
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03671# If
--     @pInfos@[i].@mode@ is 'BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR',
--     all addresses between @pInfos@[i].@scratchData.deviceAddress@ and
--     @pInfos@[i].@scratchData.deviceAddress@ + N - 1 /must/ be in the
--     buffer device address range of the same buffer, where N is given by
--     the @buildScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03672# If
--     @pInfos@[i].@mode@ is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', all addresses
--     between @pInfos@[i].@scratchData.deviceAddress@ and
--     @pInfos@[i].@scratchData.deviceAddress@ + N - 1 /must/ be in the
--     buffer device address range of the same buffer, where N is given by
--     the @updateScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-geometry-03673#
--     The buffers from which the buffer device addresses for all of the
--     @geometry.triangles.vertexData@, @geometry.triangles.indexData@,
--     @geometry.triangles.transformData@, @geometry.aabbs.data@, and
--     @geometry.instances.data@ members of all @pInfos@[i].@pGeometries@
--     and @pInfos@[i].@ppGeometries@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03674# The
--     buffer from which the buffer device address
--     @pInfos@[i].@scratchData.deviceAddress@ is queried /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03802# For
--     each element of @pInfos@, its @scratchData.deviceAddress@ member
--     /must/ be a valid device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03803# For
--     each element of @pInfos@, if @scratchData.deviceAddress@ is the
--     address of a non-sparse buffer then it /must/ be bound completely
--     and contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory'
--     object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03710# For
--     each element of @pInfos@, its @scratchData.deviceAddress@ member
--     /must/ be a multiple of
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@minAccelerationStructureScratchOffsetAlignment@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03804# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @geometry.triangles.vertexData.deviceAddress@ /must/ be a valid
--     device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03805# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.vertexData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03711# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @geometry.triangles.vertexData.deviceAddress@ /must/ be aligned to
--     the size in bytes of the smallest component of the format in
--     @vertexFormat@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03806# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if @geometry.triangles.indexType@ is
--     not 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR',
--     @geometry.triangles.indexData.deviceAddress@ /must/ be a valid
--     device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03807# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if @geometry.triangles.indexType@ is
--     not 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', if
--     @geometry.triangles.indexData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03712# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', and with
--     @geometry.triangles.indexType@ not equal to
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR',
--     @geometry.triangles.indexData.deviceAddress@ /must/ be aligned to
--     the size in bytes of the type in @indexType@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03808# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is not @0@, it
--     /must/ be a valid device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03809# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03810# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.deviceAddress@ is not @0@, it
--     /must/ be aligned to @16@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03811# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_AABBS_KHR', @geometry.aabbs.data.deviceAddress@
--     /must/ be a valid device address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03812# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_AABBS_KHR', if @geometry.aabbs.data.deviceAddress@ is
--     the address of a non-sparse buffer then it /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03714# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_AABBS_KHR', @geometry.aabbs.data.deviceAddress@
--     /must/ be aligned to @8@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03715# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', if @geometry.arrayOfPointers@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE',
--     @geometry.instances.data.deviceAddress@ /must/ be aligned to @16@
--     bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03716# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', if @geometry.arrayOfPointers@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @geometry.instances.data.deviceAddress@ /must/ be aligned to @8@
--     bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03717# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', if @geometry.arrayOfPointers@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', each element of
--     @geometry.instances.data.deviceAddress@ in device memory /must/ be
--     aligned to @16@ bytes
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03813# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR',
--     @geometry.instances.data.deviceAddress@ /must/ be a valid device
--     address obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03814# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', if
--     @geometry.instances.data.deviceAddress@ is the address of a
--     non-sparse buffer then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03815# For
--     any element of @pInfos@[i].@pGeometries@ or
--     @pInfos@[i].@ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR', each
--     'AccelerationStructureInstanceKHR'::@accelerationStructureReference@
--     value in @geometry.instances.data.deviceAddress@ /must/ be a valid
--     device address containing a value obtained from
--     'getAccelerationStructureDeviceAddressKHR'
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-03645#
--     For any element of @pIndirectDeviceAddresses@, if the buffer from
--     which it was queried is non-sparse then it /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-03646#
--     For any element of @pIndirectDeviceAddresses@[i], all device
--     addresses between @pIndirectDeviceAddresses@[i] and
--     @pIndirectDeviceAddresses@[i] + (@pInfos@[i]→geometryCount ×
--     @pIndirectStrides@[i]) - 1 /must/ be in the buffer device address
--     range of the same buffer
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-03647#
--     For any element of @pIndirectDeviceAddresses@, the buffer from which
--     it was queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-03648#
--     Each element of @pIndirectDeviceAddresses@ /must/ be a multiple of
--     @4@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectStrides-03787#
--     Each element of @pIndirectStrides@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-commandBuffer-03649#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-accelerationStructureIndirectBuild-03650#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureIndirectBuild ::accelerationStructureIndirectBuild>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-03651#
--     Each 'AccelerationStructureBuildRangeInfoKHR' structure referenced
--     by any element of @pIndirectDeviceAddresses@ /must/ be a valid
--     'AccelerationStructureBuildRangeInfoKHR' structure
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-03652#
--     @pInfos@[i].@dstAccelerationStructure@ /must/ have been created with
--     a value of 'AccelerationStructureCreateInfoKHR'::@size@ greater than
--     or equal to the memory size required by the build operation, as
--     returned by 'getAccelerationStructureBuildSizesKHR' with
--     @pBuildInfo@ = @pInfos@[i] and @pMaxPrimitiveCounts@ =
--     @ppMaxPrimitiveCounts@[i]
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-ppMaxPrimitiveCounts-03653#
--     Each @ppMaxPrimitiveCounts@[i][j] /must/ be greater than or equal to
--     the the @primitiveCount@ value specified by the
--     'AccelerationStructureBuildRangeInfoKHR' structure located at
--     @pIndirectDeviceAddresses@[i] + (@j@ × @pIndirectStrides@[i])
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pInfos-parameter#
--     @pInfos@ /must/ be a valid pointer to an array of @infoCount@ valid
--     'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectDeviceAddresses-parameter#
--     @pIndirectDeviceAddresses@ /must/ be a valid pointer to an array of
--     @infoCount@ 'Vulkan.Core10.FundamentalTypes.DeviceAddress' values
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-pIndirectStrides-parameter#
--     @pIndirectStrides@ /must/ be a valid pointer to an array of
--     @infoCount@ @uint32_t@ values
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-ppMaxPrimitiveCounts-parameter#
--     @ppMaxPrimitiveCounts@ /must/ be a valid pointer to an array of
--     @infoCount@ @uint32_t@ values
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-renderpass# This
--     command /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdBuildAccelerationStructuresIndirectKHR-infoCount-arraylength#
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdBuildAccelerationStructuresIndirectKHR :: forall io
                                           . (MonadIO io)
                                          => -- | @commandBuffer@ is the command buffer into which the command will be
                                             -- recorded.
                                             CommandBuffer
                                          -> -- | @pInfos@ is an array of @infoCount@
                                             -- 'AccelerationStructureBuildGeometryInfoKHR' structures defining the
                                             -- geometry used to build each acceleration structure.
                                             ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                                          -> -- | @pIndirectDeviceAddresses@ is an array of @infoCount@ buffer device
                                             -- addresses which point to @pInfos@[i]→@geometryCount@
                                             -- 'AccelerationStructureBuildRangeInfoKHR' structures defining dynamic
                                             -- offsets to the addresses where geometry data is stored, as defined by
                                             -- @pInfos@[i].
                                             ("indirectDeviceAddresses" ::: Vector DeviceAddress)
                                          -> -- | @pIndirectStrides@ is an array of @infoCount@ byte strides between
                                             -- elements of @pIndirectDeviceAddresses@.
                                             ("indirectStrides" ::: Vector Word32)
                                          -> -- | @ppMaxPrimitiveCounts@ is an array of @infoCount@ arrays of
                                             -- @pInfo@[i]→@geometryCount@ values indicating the maximum number of
                                             -- primitives that will be built by this command for each geometry.
                                             ("maxPrimitiveCounts" ::: Vector (Vector Word32))
                                          -> io ()
cmdBuildAccelerationStructuresIndirectKHR commandBuffer infos indirectDeviceAddresses indirectStrides maxPrimitiveCounts = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructuresIndirectKHRPtr = pVkCmdBuildAccelerationStructuresIndirectKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructuresIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructuresIndirectKHR is null" Nothing Nothing
  let vkCmdBuildAccelerationStructuresIndirectKHR' = mkVkCmdBuildAccelerationStructuresIndirectKHR vkCmdBuildAccelerationStructuresIndirectKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (indirectDeviceAddresses)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pIndirectDeviceAddresses and pInfos must have the same length" Nothing Nothing
  lift $ unless ((Data.Vector.length $ (indirectStrides)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pIndirectStrides and pInfos must have the same length" Nothing Nothing
  lift $ unless ((Data.Vector.length $ (maxPrimitiveCounts)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppMaxPrimitiveCounts and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPIndirectDeviceAddresses <- ContT $ allocaBytesAligned @DeviceAddress ((Data.Vector.length (indirectDeviceAddresses)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPIndirectDeviceAddresses `plusPtr` (8 * (i)) :: Ptr DeviceAddress) (e)) (indirectDeviceAddresses)
  pPIndirectStrides <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (indirectStrides)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPIndirectStrides `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indirectStrides)
  pPpMaxPrimitiveCounts <- ContT $ allocaBytesAligned @(Ptr Word32) ((Data.Vector.length (maxPrimitiveCounts)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpMaxPrimitiveCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (e)) * 4) 4
    lift $ Data.Vector.imapM_ (\i' e' -> poke (pPpMaxPrimitiveCounts' `plusPtr` (4 * (i')) :: Ptr Word32) (e')) (e)
    lift $ poke (pPpMaxPrimitiveCounts `plusPtr` (8 * (i)) :: Ptr (Ptr Word32)) (pPpMaxPrimitiveCounts')) (maxPrimitiveCounts)
  lift $ traceAroundEvent "vkCmdBuildAccelerationStructuresIndirectKHR" (vkCmdBuildAccelerationStructuresIndirectKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPIndirectDeviceAddresses) (pPIndirectStrides) (pPpMaxPrimitiveCounts))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBuildAccelerationStructuresKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr (Ptr AccelerationStructureBuildRangeInfoKHR) -> IO Result

-- | vkBuildAccelerationStructuresKHR - Build an acceleration structure on
-- the host
--
-- = Description
--
-- This command fulfills the same task as
-- 'cmdBuildAccelerationStructuresKHR' but is executed by the host.
--
-- The 'buildAccelerationStructuresKHR' command provides the ability to
-- initiate multiple acceleration structures builds, however there is no
-- ordering or synchronization implied between any of the individual
-- acceleration structure builds.
--
-- Note
--
-- This means that an application /cannot/ build a top-level acceleration
-- structure in the same 'buildAccelerationStructuresKHR' call as the
-- associated bottom-level or instance acceleration structures are being
-- built. There also /cannot/ be any memory aliasing between any
-- acceleration structure memories or scratch memories being used by any of
-- the builds.
--
-- == Valid Usage
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-mode-04628# The @mode@ member
--     of each element of @pInfos@ /must/ be a valid
--     'BuildAccelerationStructureModeKHR' value
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-srcAccelerationStructure-04629#
--     If the @srcAccelerationStructure@ member of any element of @pInfos@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     @srcAccelerationStructure@ member /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-04630# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03403# The
--     @srcAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03698#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ not be the same acceleration structure as the
--     @dstAccelerationStructure@ member of any other element of @pInfos@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03800#
--     The @dstAccelerationStructure@ member of any element of @pInfos@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03699# For each
--     element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03700# For each
--     element of @pInfos@, if its @type@ member is
--     'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR', its
--     @dstAccelerationStructure@ member /must/ have been created with a
--     value of 'AccelerationStructureCreateInfoKHR'::@type@ equal to
--     either 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03663# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive primitives>
--     in its @srcAccelerationStructure@ member /must/ not be made active
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03664# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', active primitives in
--     its @srcAccelerationStructure@ member /must/ not be made
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims inactive>
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-None-03407# The
--     @dstAccelerationStructure@ member of any element of @pInfos@ /must/
--     not be referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03701#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @srcAccelerationStructure@ member of
--     any other element of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', which is accessed by
--     this command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03702#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @dstAccelerationStructure@ member of
--     any other element of @pInfos@, which is accessed by this command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03703#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing the @scratchData@ member of any element
--     of @pInfos@ (including the same element), which is accessed by this
--     command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-scratchData-03704# The range
--     of memory backing the @scratchData@ member of any element of
--     @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @scratchData@ member of any other element of
--     @pInfos@, which is accessed by this command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-scratchData-03705# The range
--     of memory backing the @scratchData@ member of any element of
--     @pInfos@ that is accessed by this command /must/ not overlap the
--     memory backing the @srcAccelerationStructure@ member of any element
--     of @pInfos@ with a @mode@ equal to
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' (including the same
--     element), which is accessed by this command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-dstAccelerationStructure-03706#
--     The range of memory backing the @dstAccelerationStructure@ member of
--     any element of @pInfos@ that is accessed by this command /must/ not
--     overlap the memory backing any acceleration structure referenced by
--     the @geometry.instances.data@ member of any element of @pGeometries@
--     or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' in any other element of @pInfos@,
--     which is accessed by this command
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03666# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03667# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ member /must/ have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' set in
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03668# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its
--     @srcAccelerationStructure@ and @dstAccelerationStructure@ members
--     /must/ either be the same
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR', or not have
--     any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03758# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @geometryCount@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03759# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @flags@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03760# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', its @type@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03761# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @geometryType@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03762# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, its @flags@ member /must/
--     have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03763# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.vertexFormat@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03764# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.maxVertex@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03765# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', its @geometry.triangles.indexType@
--     member /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03766# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was NULL when
--     @srcAccelerationStructure@ was last built, then it must be NULL.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03767# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', if its
--     @geometry.triangles.transformData@ member was not NULL when
--     @srcAccelerationStructure@ was last built, then it may not be NULL.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03768# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', then for each
--     'AccelerationStructureGeometryKHR' structure referred to by its
--     @pGeometries@ or @ppGeometries@ members, if @geometryType@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', and @geometry.triangles.indexType@ is
--     not 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then the
--     value of each index referenced index must be the same as the
--     corresponding index value when @srcAccelerationStructure@ was last
--     built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-primitiveCount-03769# For
--     each 'AccelerationStructureBuildRangeInfoKHR' referenced by this
--     command, its @primitiveCount@ member /must/ have the same value
--     which was specified when @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-firstVertex-03770# For each
--     'AccelerationStructureBuildRangeInfoKHR' referenced by this command,
--     if the corresponding geometry uses indices, its @firstVertex@ member
--     /must/ have the same value which was specified when
--     @srcAccelerationStructure@ was last built.
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03801# For each
--     element of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@
--     with a @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', the
--     corresponding @ppBuildRangeInfos@[i][j].@primitiveCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxInstanceCount@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03675# For each
--     @pInfos@[i], @dstAccelerationStructure@ /must/ have been created
--     with a value of 'AccelerationStructureCreateInfoKHR'::@size@ greater
--     than or equal to the memory size required by the build operation, as
--     returned by 'getAccelerationStructureBuildSizesKHR' with
--     @pBuildInfo@ = @pInfos@[i] and with each element of the
--     @pMaxPrimitiveCounts@ array greater than or equal to the equivalent
--     @ppBuildRangeInfos@[i][j].@primitiveCount@ values for @j@ in
--     [0,@pInfos@[i].@geometryCount@)
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-ppBuildRangeInfos-03676# Each
--     element of @ppBuildRangeInfos@[i] /must/ be a valid pointer to an
--     array of @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildRangeInfoKHR' structures
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-deferredOperation-03678# Any
--     previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03722# For each
--     element of @pInfos@, the @buffer@ used to create its
--     @dstAccelerationStructure@ member /must/ be bound to host-visible
--     device memory
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03723# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' the @buffer@ used to
--     create its @srcAccelerationStructure@ member /must/ be bound to
--     host-visible device memory
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03724# For each
--     element of @pInfos@, the @buffer@ used to create each acceleration
--     structure referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' /must/ be bound to host-visible device
--     memory
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-accelerationStructureHostCommands-03581#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureHostCommands ::accelerationStructureHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03725# If
--     @pInfos@[i].@mode@ is 'BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR',
--     all addresses between @pInfos@[i].@scratchData.hostAddress@ and
--     @pInfos@[i].@scratchData.hostAddress@ + N - 1 /must/ be valid host
--     memory, where N is given by the @buildScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03726# If
--     @pInfos@[i].@mode@ is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR', all addresses
--     between @pInfos@[i].@scratchData.hostAddress@ and
--     @pInfos@[i].@scratchData.hostAddress@ + N - 1 /must/ be valid host
--     memory, where N is given by the @updateScratchSize@ member of the
--     'AccelerationStructureBuildSizesInfoKHR' structure returned from a
--     call to 'getAccelerationStructureBuildSizesKHR' with an identical
--     'AccelerationStructureBuildGeometryInfoKHR' structure and primitive
--     count
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03771# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @geometry.triangles.vertexData.hostAddress@ /must/ be a valid host
--     address
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03772# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR',
--     @geometry.triangles.indexData.hostAddress@ /must/ be a valid host
--     address
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03773# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_TRIANGLES_KHR', if
--     @geometry.triangles.transformData.hostAddress@ is not @0@, it /must/
--     be a valid host address
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03774# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_AABBS_KHR',
--     @geometry.aabbs.data.hostAddress@ /must/ be a valid host address
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03775# For each
--     element of @pInfos@, the @buffer@ used to create its
--     @dstAccelerationStructure@ member /must/ be bound to memory that was
--     not allocated with multiple instances
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03776# For each
--     element of @pInfos@, if its @mode@ member is
--     'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' the @buffer@ used to
--     create its @srcAccelerationStructure@ member /must/ be bound to
--     memory that was not allocated with multiple instances
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03777# For each
--     element of @pInfos@, the @buffer@ used to create each acceleration
--     structure referenced by the @geometry.instances.data@ member of any
--     element of @pGeometries@ or @ppGeometries@ with a @geometryType@ of
--     'GEOMETRY_TYPE_INSTANCES_KHR' /must/ be bound to memory that was not
--     allocated with multiple instances
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03778# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR',
--     @geometry.instances.data.hostAddress@ /must/ be a valid host address
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-03779# For any element
--     of @pInfos@[i].@pGeometries@ or @pInfos@[i].@ppGeometries@ with a
--     @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', each
--     'AccelerationStructureInstanceKHR'::@accelerationStructureReference@
--     value in @geometry.instances.data.hostAddress@ must be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-deferredOperation-parameter#
--     If @deferredOperation@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @deferredOperation@ /must/
--     be a valid 'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-pInfos-parameter# @pInfos@
--     /must/ be a valid pointer to an array of @infoCount@ valid
--     'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-ppBuildRangeInfos-parameter#
--     @ppBuildRangeInfos@ /must/ be a valid pointer to an array of
--     @infoCount@ 'AccelerationStructureBuildRangeInfoKHR' structures
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-infoCount-arraylength#
--     @infoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkBuildAccelerationStructuresKHR-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildRangeInfoKHR',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
buildAccelerationStructuresKHR :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the 'Vulkan.Core10.Handles.Device' for which the
                                  -- acceleration structures are being built.
                                  Device
                               -> -- | @deferredOperation@ is an optional
                                  -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                                  -- for this command.
                                  DeferredOperationKHR
                               -> -- | @pInfos@ is a pointer to an array of @infoCount@
                                  -- 'AccelerationStructureBuildGeometryInfoKHR' structures defining the
                                  -- geometry used to build each acceleration structure.
                                  ("infos" ::: Vector AccelerationStructureBuildGeometryInfoKHR)
                               -> -- | @ppBuildRangeInfos@ is an array of @infoCount@ pointers to arrays of
                                  -- 'AccelerationStructureBuildRangeInfoKHR' structures. Each
                                  -- @ppBuildRangeInfos@[i] is an array of @pInfos@[i].@geometryCount@
                                  -- 'AccelerationStructureBuildRangeInfoKHR' structures defining dynamic
                                  -- offsets to the addresses where geometry data is stored, as defined by
                                  -- @pInfos@[i].
                                  ("buildRangeInfos" ::: Vector (Vector AccelerationStructureBuildRangeInfoKHR))
                               -> io (Result)
buildAccelerationStructuresKHR device deferredOperation infos buildRangeInfos = liftIO . evalContT $ do
  let vkBuildAccelerationStructuresKHRPtr = pVkBuildAccelerationStructuresKHR (deviceCmds (device :: Device))
  lift $ unless (vkBuildAccelerationStructuresKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBuildAccelerationStructuresKHR is null" Nothing Nothing
  let vkBuildAccelerationStructuresKHR' = mkVkBuildAccelerationStructuresKHR vkBuildAccelerationStructuresKHRPtr
  let pInfosLength = Data.Vector.length $ (infos)
  lift $ unless ((Data.Vector.length $ (buildRangeInfos)) == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppBuildRangeInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @AccelerationStructureBuildGeometryInfoKHR ((Data.Vector.length (infos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (80 * (i)) :: Ptr AccelerationStructureBuildGeometryInfoKHR) (e) . ($ ())) (infos)
  pPpBuildRangeInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildRangeInfoKHR) ((Data.Vector.length (buildRangeInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    pPpBuildRangeInfos' <- ContT $ allocaBytesAligned @AccelerationStructureBuildRangeInfoKHR ((Data.Vector.length (e)) * 16) 4
    lift $ Data.Vector.imapM_ (\i' e' -> poke (pPpBuildRangeInfos' `plusPtr` (16 * (i')) :: Ptr AccelerationStructureBuildRangeInfoKHR) (e')) (e)
    lift $ poke (pPpBuildRangeInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) (pPpBuildRangeInfos')) (buildRangeInfos)
  r <- lift $ traceAroundEvent "vkBuildAccelerationStructuresKHR" (vkBuildAccelerationStructuresKHR' (deviceHandle (device)) (deferredOperation) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpBuildRangeInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureDeviceAddressKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress) -> Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress

-- | vkGetAccelerationStructureDeviceAddressKHR - Query an address of a
-- acceleration structure
--
-- = Description
--
-- The 64-bit return value is an address of the acceleration structure,
-- which can be used for device and shader operations that involve
-- acceleration structures, such as ray traversal and acceleration
-- structure building.
--
-- If the acceleration structure was created with a non-zero value of
-- 'AccelerationStructureCreateInfoKHR'::@deviceAddress@ the return value
-- will be the same address.
--
-- If the acceleration structure was created with a @type@ of
-- 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR' the returned address /must/ be
-- consistent with the relative offset to other acceleration structures of
-- @type@ of 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR' allocated with the
-- same 'Vulkan.Core10.Handles.Buffer'. That is, the difference in returned
-- addresses between the two /must/ be the same as the difference in
-- offsets provided at acceleration structure creation.
--
-- Note
--
-- The acceleration structure device address /may/ be different from the
-- buffer device address corresponding to the acceleration structure’s
-- start offset in its storage buffer for acceleration structure types
-- other than 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetAccelerationStructureDeviceAddressKHR-device-03504# If
--     @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetAccelerationStructureDeviceAddressKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetAccelerationStructureDeviceAddressKHR-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureDeviceAddressInfoKHR' structure
--
-- = See Also
--
-- 'AccelerationStructureDeviceAddressInfoKHR',
-- 'Vulkan.Core10.Handles.Device'
getAccelerationStructureDeviceAddressKHR :: forall io
                                          . (MonadIO io)
                                         => -- | @device@ is the logical device that the accelerationStructure was
                                            -- created on.
                                            Device
                                         -> -- | @pInfo@ is a pointer to a 'AccelerationStructureDeviceAddressInfoKHR'
                                            -- structure specifying the acceleration structure to retrieve an address
                                            -- for.
                                            AccelerationStructureDeviceAddressInfoKHR
                                         -> io (DeviceAddress)
getAccelerationStructureDeviceAddressKHR device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureDeviceAddressKHRPtr = pVkGetAccelerationStructureDeviceAddressKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureDeviceAddressKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureDeviceAddressKHR is null" Nothing Nothing
  let vkGetAccelerationStructureDeviceAddressKHR' = mkVkGetAccelerationStructureDeviceAddressKHR vkGetAccelerationStructureDeviceAddressKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetAccelerationStructureDeviceAddressKHR" (vkGetAccelerationStructureDeviceAddressKHR' (deviceHandle (device)) pInfo)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureBuildSizesKHR
  :: FunPtr (Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr Word32 -> Ptr AccelerationStructureBuildSizesInfoKHR -> IO ()) -> Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr AccelerationStructureBuildGeometryInfoKHR -> Ptr Word32 -> Ptr AccelerationStructureBuildSizesInfoKHR -> IO ()

-- | vkGetAccelerationStructureBuildSizesKHR - Retrieve the required size for
-- an acceleration structure
--
-- = Description
--
-- The @srcAccelerationStructure@, @dstAccelerationStructure@, and @mode@
-- members of @pBuildInfo@ are ignored. Any 'DeviceOrHostAddressKHR'
-- members of @pBuildInfo@ are ignored by this command, except that the
-- @hostAddress@ member of
-- 'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@ will be
-- examined to check if it is @NULL@.
--
-- An acceleration structure created with the @accelerationStructureSize@
-- returned by this command supports any build or update with a
-- 'AccelerationStructureBuildGeometryInfoKHR' structure and array of
-- 'AccelerationStructureBuildRangeInfoKHR' structures subject to the
-- following properties:
--
-- -   The build command is a host build command, and @buildType@ is
--     'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR' or
--     'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR'
--
-- -   The build command is a device build command, and @buildType@ is
--     'ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR' or
--     'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR'
--
-- -   For 'AccelerationStructureBuildGeometryInfoKHR':
--
--     -   Its @type@, and @flags@ members are equal to those specified in
--         @pBuildInfo@.
--
--     -   @geometryCount@ is less than or equal to that specified in
--         @pBuildInfo@.
--
--     -   For each element of either @pGeometries@ or @ppGeometries@ at a
--         given index, its @geometryType@ member is equal to that
--         specified in @pBuildInfo@.
--
--     -   For each element of either @pGeometries@ or @ppGeometries@ at a
--         given index, with a @geometryType@ member equal to
--         'GEOMETRY_TYPE_TRIANGLES_KHR', the @vertexFormat@ and
--         @indexType@ members of @geometry.triangles@ are equal to those
--         specified in the same element in @pBuildInfo@.
--
--     -   For each element of either @pGeometries@ or @ppGeometries@ at a
--         given index, with a @geometryType@ member equal to
--         'GEOMETRY_TYPE_TRIANGLES_KHR', the @maxVertex@ member of
--         @geometry.triangles@ is less than or equal to that specified in
--         the same element in @pBuildInfo@.
--
--     -   For each element of either @pGeometries@ or @ppGeometries@ at a
--         given index, with a @geometryType@ member equal to
--         'GEOMETRY_TYPE_TRIANGLES_KHR', if the applicable address in the
--         @transformData@ member of @geometry.triangles@ is not @NULL@,
--         the corresponding @transformData.hostAddress@ parameter in
--         @pBuildInfo@ is not @NULL@.
--
-- -   For each 'AccelerationStructureBuildRangeInfoKHR' corresponding to
--     the 'AccelerationStructureBuildGeometryInfoKHR':
--
--     -   Its @primitiveCount@ member is less than or equal to the
--         corresponding element of @pMaxPrimitiveCounts@.
--
-- Similarly, the @updateScratchSize@ value will support any build command
-- specifying the 'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' @mode@
-- under the above conditions, and the @buildScratchSize@ value will
-- support any build command specifying the
-- 'BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR' @mode@ under the above
-- conditions.
--
-- == Valid Usage
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-rayTracingPipeline-03617#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-device-03618# If
--     @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-pBuildInfo-03619# If
--     @pBuildInfo->geometryCount@ is not @0@, @pMaxPrimitiveCounts@ /must/
--     be a valid pointer to an array of @pBuildInfo->geometryCount@
--     @uint32_t@ values
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-pBuildInfo-03785# If
--     @pBuildInfo->pGeometries@ or @pBuildInfo->ppGeometries@ has a
--     @geometryType@ of 'GEOMETRY_TYPE_INSTANCES_KHR', each
--     @pMaxPrimitiveCounts@[i] /must/ be less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxInstanceCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-buildType-parameter#
--     @buildType@ /must/ be a valid 'AccelerationStructureBuildTypeKHR'
--     value
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-pBuildInfo-parameter#
--     @pBuildInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureBuildGeometryInfoKHR' structure
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-pMaxPrimitiveCounts-parameter#
--     @pMaxPrimitiveCounts@ /must/ be a valid pointer to an array of
--     @pBuildInfo->geometryCount@ @uint32_t@ values
--
-- -   #VUID-vkGetAccelerationStructureBuildSizesKHR-pSizeInfo-parameter#
--     @pSizeInfo@ /must/ be a valid pointer to a
--     'AccelerationStructureBuildSizesInfoKHR' structure
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildSizesInfoKHR',
-- 'AccelerationStructureBuildTypeKHR', 'Vulkan.Core10.Handles.Device'
getAccelerationStructureBuildSizesKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the logical device that will be used for creating the
                                         -- acceleration structure.
                                         Device
                                      -> -- | @buildType@ defines whether host or device operations (or both) are
                                         -- being queried for.
                                         AccelerationStructureBuildTypeKHR
                                      -> -- | @pBuildInfo@ is a pointer to a
                                         -- 'AccelerationStructureBuildGeometryInfoKHR' structure describing
                                         -- parameters of a build operation.
                                         ("buildInfo" ::: AccelerationStructureBuildGeometryInfoKHR)
                                      -> -- | @pMaxPrimitiveCounts@ is a pointer to an array of
                                         -- @pBuildInfo->geometryCount@ @uint32_t@ values defining the number of
                                         -- primitives built into each geometry.
                                         ("maxPrimitiveCounts" ::: Vector Word32)
                                      -> io (("sizeInfo" ::: AccelerationStructureBuildSizesInfoKHR))
getAccelerationStructureBuildSizesKHR device buildType buildInfo maxPrimitiveCounts = liftIO . evalContT $ do
  let vkGetAccelerationStructureBuildSizesKHRPtr = pVkGetAccelerationStructureBuildSizesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureBuildSizesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureBuildSizesKHR is null" Nothing Nothing
  let vkGetAccelerationStructureBuildSizesKHR' = mkVkGetAccelerationStructureBuildSizesKHR vkGetAccelerationStructureBuildSizesKHRPtr
  pBuildInfo <- ContT $ withCStruct (buildInfo)
  pPMaxPrimitiveCounts <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (maxPrimitiveCounts)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMaxPrimitiveCounts `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (maxPrimitiveCounts)
  pPSizeInfo <- ContT (withZeroCStruct @AccelerationStructureBuildSizesInfoKHR)
  lift $ traceAroundEvent "vkGetAccelerationStructureBuildSizesKHR" (vkGetAccelerationStructureBuildSizesKHR' (deviceHandle (device)) (buildType) pBuildInfo (pPMaxPrimitiveCounts) (pPSizeInfo))
  pSizeInfo <- lift $ peekCStruct @AccelerationStructureBuildSizesInfoKHR pPSizeInfo
  pure $ (pSizeInfo)


-- | VkWriteDescriptorSetAccelerationStructureKHR - Structure specifying
-- acceleration structure descriptor info
--
-- == Valid Usage
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-accelerationStructureCount-02236#
--     @accelerationStructureCount@ /must/ be equal to @descriptorCount@ in
--     the extended structure
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-pAccelerationStructures-03579#
--     Each acceleration structure in @pAccelerationStructures@ /must/ have
--     been created with a @type@ of
--     'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' or
--     'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-pAccelerationStructures-03580#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, each member of @pAccelerationStructures@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR'
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-pAccelerationStructures-parameter#
--     @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureKHR-accelerationStructureCount-arraylength#
--     @accelerationStructureCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetAccelerationStructureKHR = WriteDescriptorSetAccelerationStructureKHR
  { -- | @pAccelerationStructures@ are the acceleration structures to update.
    accelerationStructures :: Vector AccelerationStructureKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetAccelerationStructureKHR)
#endif
deriving instance Show WriteDescriptorSetAccelerationStructureKHR

instance ToCStruct WriteDescriptorSetAccelerationStructureKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct WriteDescriptorSetAccelerationStructureKHR where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureKHR) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureKHR ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureKHR)))
    pure $ WriteDescriptorSetAccelerationStructureKHR
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureKHR where
  zero = WriteDescriptorSetAccelerationStructureKHR
           mempty


-- | VkPhysicalDeviceAccelerationStructureFeaturesKHR - Structure describing
-- the acceleration structure features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceAccelerationStructureFeaturesKHR'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceAccelerationStructureFeaturesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceAccelerationStructureFeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAccelerationStructureFeaturesKHR = PhysicalDeviceAccelerationStructureFeaturesKHR
  { -- | #features-accelerationStructure# @accelerationStructure@ indicates
    -- whether the implementation supports the acceleration structure
    -- functionality. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure Acceleration Structures>.
    accelerationStructure :: Bool
  , -- | #features-accelerationStructureCaptureReplay#
    -- @accelerationStructureCaptureReplay@ indicates whether the
    -- implementation supports saving and reusing acceleration structure device
    -- addresses, e.g. for trace capture and replay.
    accelerationStructureCaptureReplay :: Bool
  , -- | #features-accelerationStructureIndirectBuild#
    -- @accelerationStructureIndirectBuild@ indicates whether the
    -- implementation supports indirect acceleration structure build commands,
    -- e.g. 'cmdBuildAccelerationStructuresIndirectKHR'.
    accelerationStructureIndirectBuild :: Bool
  , -- | #features-accelerationStructureHostCommands#
    -- @accelerationStructureHostCommands@ indicates whether the implementation
    -- supports host side acceleration structure commands, e.g.
    -- 'buildAccelerationStructuresKHR', 'copyAccelerationStructureKHR',
    -- 'copyAccelerationStructureToMemoryKHR',
    -- 'copyMemoryToAccelerationStructureKHR',
    -- 'writeAccelerationStructuresPropertiesKHR'.
    accelerationStructureHostCommands :: Bool
  , -- | #features-descriptorBindingAccelerationStructureUpdateAfterBind#
    -- @descriptorBindingAccelerationStructureUpdateAfterBind@ indicates
    -- whether the implementation supports updating acceleration structure
    -- descriptors after a set is bound. If this feature is not enabled,
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'.
    descriptorBindingAccelerationStructureUpdateAfterBind :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAccelerationStructureFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceAccelerationStructureFeaturesKHR

instance ToCStruct PhysicalDeviceAccelerationStructureFeaturesKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAccelerationStructureFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (accelerationStructure))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (accelerationStructureCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (accelerationStructureIndirectBuild))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (accelerationStructureHostCommands))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (descriptorBindingAccelerationStructureUpdateAfterBind))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceAccelerationStructureFeaturesKHR where
  peekCStruct p = do
    accelerationStructure <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    accelerationStructureCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    accelerationStructureIndirectBuild <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    accelerationStructureHostCommands <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    descriptorBindingAccelerationStructureUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceAccelerationStructureFeaturesKHR
             (bool32ToBool accelerationStructure) (bool32ToBool accelerationStructureCaptureReplay) (bool32ToBool accelerationStructureIndirectBuild) (bool32ToBool accelerationStructureHostCommands) (bool32ToBool descriptorBindingAccelerationStructureUpdateAfterBind)

instance Storable PhysicalDeviceAccelerationStructureFeaturesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAccelerationStructureFeaturesKHR where
  zero = PhysicalDeviceAccelerationStructureFeaturesKHR
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceAccelerationStructurePropertiesKHR - Properties of the
-- physical device for acceleration structure
--
-- = Description
--
-- If the 'PhysicalDeviceAccelerationStructurePropertiesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Limits specified by this structure /must/ match those specified with the
-- same name in
-- 'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceAccelerationStructurePropertiesKHR = PhysicalDeviceAccelerationStructurePropertiesKHR
  { -- | @maxGeometryCount@ is the maximum number of geometries in the bottom
    -- level acceleration structure.
    maxGeometryCount :: Word64
  , -- | @maxInstanceCount@ is the maximum number of instances in the top level
    -- acceleration structure.
    maxInstanceCount :: Word64
  , -- | @maxPrimitiveCount@ is the maximum number of triangles or AABBs in all
    -- geometries in the bottom level acceleration structure.
    maxPrimitiveCount :: Word64
  , -- | #limits-maxPerStageDescriptorAccelerationStructures#
    -- @maxPerStageDescriptorAccelerationStructures@ is the maximum number of
    -- acceleration structure bindings that /can/ be accessible to a single
    -- shader stage in a pipeline layout. Descriptor bindings with a descriptor
    -- type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxPerStageDescriptorAccelerationStructures :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindAccelerationStructures#
    -- @maxPerStageDescriptorUpdateAfterBindAccelerationStructures@ is similar
    -- to @maxPerStageDescriptorAccelerationStructures@ but counts descriptor
    -- bindings from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindAccelerationStructures :: Word32
  , -- | #limits-maxDescriptorSetAccelerationStructures#
    -- @maxDescriptorSetAccelerationStructures@ is the maximum number of
    -- acceleration structure descriptors that /can/ be included in descriptor
    -- bindings in a pipeline layout across all pipeline shader stages and
    -- descriptor set numbers. Descriptor bindings with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetAccelerationStructures :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindAccelerationStructures#
    -- @maxDescriptorSetUpdateAfterBindAccelerationStructures@ is similar to
    -- @maxDescriptorSetAccelerationStructures@ but counts descriptor bindings
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindAccelerationStructures :: Word32
  , -- | #limits-minAccelerationStructureScratchOffsetAlignment#
    -- @minAccelerationStructureScratchOffsetAlignment@ is the minimum
    -- /required/ alignment, in bytes, for scratch data passed in to an
    -- acceleration structure build command.
    minAccelerationStructureScratchOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceAccelerationStructurePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceAccelerationStructurePropertiesKHR

instance ToCStruct PhysicalDeviceAccelerationStructurePropertiesKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceAccelerationStructurePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxPrimitiveCount)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxPerStageDescriptorAccelerationStructures)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindAccelerationStructures)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindAccelerationStructures)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (minAccelerationStructureScratchOffsetAlignment)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceAccelerationStructurePropertiesKHR where
  peekCStruct p = do
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    maxPrimitiveCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxPerStageDescriptorAccelerationStructures <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindAccelerationStructures <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindAccelerationStructures <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    minAccelerationStructureScratchOffsetAlignment <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceAccelerationStructurePropertiesKHR
             maxGeometryCount maxInstanceCount maxPrimitiveCount maxPerStageDescriptorAccelerationStructures maxPerStageDescriptorUpdateAfterBindAccelerationStructures maxDescriptorSetAccelerationStructures maxDescriptorSetUpdateAfterBindAccelerationStructures minAccelerationStructureScratchOffsetAlignment

instance Storable PhysicalDeviceAccelerationStructurePropertiesKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceAccelerationStructurePropertiesKHR where
  zero = PhysicalDeviceAccelerationStructurePropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureGeometryTrianglesDataKHR - Structure specifying a
-- triangle geometry in a bottom-level acceleration structure
--
-- = Description
--
-- Note
--
-- Unlike the stride for vertex buffers in
-- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription' for graphics
-- pipelines which must not exceed @maxVertexInputBindingStride@,
-- @vertexStride@ for acceleration structure geometry is instead restricted
-- to being a 32-bit value.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-vertexStride-03735#
--     @vertexStride@ /must/ be a multiple of the size in bytes of the
--     smallest component of @vertexFormat@
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-vertexStride-03819#
--     @vertexStride@ /must/ be less than or equal to 232-1
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-vertexFormat-03797#
--     @vertexFormat@ /must/ support the
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--     in
--     'Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
--     as returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-indexType-03798#
--     @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR'
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-vertexFormat-parameter#
--     @vertexFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-vertexData-parameter#
--     @vertexData@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-indexData-parameter#
--     If @indexData@ is not @0@, @indexData@ /must/ be a valid
--     'DeviceOrHostAddressConstKHR' union
--
-- -   #VUID-VkAccelerationStructureGeometryTrianglesDataKHR-transformData-parameter#
--     If @transformData@ is not @0@, @transformData@ /must/ be a valid
--     'DeviceOrHostAddressConstKHR' union
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryTrianglesDataKHR = AccelerationStructureGeometryTrianglesDataKHR
  { -- | @vertexFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each vertex
    -- element.
    vertexFormat :: Format
  , -- | @vertexData@ is a device or host address to memory containing vertex
    -- data for this geometry.
    vertexData :: DeviceOrHostAddressConstKHR
  , -- | @vertexStride@ is the stride in bytes between each vertex.
    vertexStride :: DeviceSize
  , -- | @maxVertex@ is the highest index of a vertex that will be addressed by a
    -- build command using this structure.
    maxVertex :: Word32
  , -- | @indexType@ is the 'Vulkan.Core10.Enums.IndexType.IndexType' of each
    -- index element.
    indexType :: IndexType
  , -- | @indexData@ is a device or host address to memory containing index data
    -- for this geometry.
    indexData :: DeviceOrHostAddressConstKHR
  , -- | @transformData@ is a device or host address to memory containing an
    -- optional reference to a 'TransformMatrixKHR' structure defining a
    -- transformation that should be applied to vertices in this geometry.
    transformData :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryTrianglesDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryTrianglesDataKHR

instance ToCStruct AccelerationStructureGeometryTrianglesDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryTrianglesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (vertexFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (vertexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (maxVertex)
    lift $ poke ((p `plusPtr` 44 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (indexData) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr DeviceOrHostAddressConstKHR)) (transformData) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr IndexType)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryTrianglesDataKHR where
  zero = AccelerationStructureGeometryTrianglesDataKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureGeometryAabbsDataKHR - Structure specifying
-- axis-aligned bounding box geometry in a bottom-level acceleration
-- structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryAabbsDataKHR = AccelerationStructureGeometryAabbsDataKHR
  { -- | @data@ is a device or host address to memory containing
    -- 'AabbPositionsKHR' structures containing position data for each
    -- axis-aligned bounding box in the geometry.
    --
    -- #VUID-VkAccelerationStructureGeometryAabbsDataKHR-data-parameter# @data@
    -- /must/ be a valid 'DeviceOrHostAddressConstKHR' union
    data' :: DeviceOrHostAddressConstKHR
  , -- | @stride@ is the stride in bytes between each entry in @data@. The stride
    -- /must/ be a multiple of @8@.
    --
    -- #VUID-VkAccelerationStructureGeometryAabbsDataKHR-stride-03545# @stride@
    -- /must/ be a multiple of @8@
    --
    -- #VUID-VkAccelerationStructureGeometryAabbsDataKHR-stride-03820# @stride@
    -- /must/ be less than or equal to 232-1
    stride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryAabbsDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryAabbsDataKHR

instance ToCStruct AccelerationStructureGeometryAabbsDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryAabbsDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (stride)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryAabbsDataKHR where
  zero = AccelerationStructureGeometryAabbsDataKHR
           zero
           zero


-- | VkAccelerationStructureGeometryInstancesDataKHR - Structure specifying a
-- geometry consisting of instances of other acceleration structures
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR',
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryInstancesDataKHR = AccelerationStructureGeometryInstancesDataKHR
  { -- | @arrayOfPointers@ specifies whether @data@ is used as an array of
    -- addresses or just an array.
    arrayOfPointers :: Bool
  , -- | @data@ is either the address of an array of device or host addresses
    -- referencing individual 'AccelerationStructureInstanceKHR' structures if
    -- @arrayOfPointers@ is 'Vulkan.Core10.FundamentalTypes.TRUE', or the
    -- address of an array of 'AccelerationStructureInstanceKHR' structures.
    --
    -- #VUID-VkAccelerationStructureGeometryInstancesDataKHR-data-parameter#
    -- @data@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
    data' :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryInstancesDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryInstancesDataKHR

instance ToCStruct AccelerationStructureGeometryInstancesDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryInstancesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (arrayOfPointers))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryInstancesDataKHR where
  zero = AccelerationStructureGeometryInstancesDataKHR
           zero
           zero


-- | VkAccelerationStructureGeometryKHR - Structure specifying geometries to
-- be built into an acceleration structure
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-geometryType-03541# If
--     @geometryType@ is 'GEOMETRY_TYPE_AABBS_KHR', the @aabbs@ member of
--     @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryAabbsDataKHR' structure
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-geometryType-03542# If
--     @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR', the @triangles@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryTrianglesDataKHR' structure
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-geometryType-03543# If
--     @geometryType@ is 'GEOMETRY_TYPE_INSTANCES_KHR', the @instances@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryInstancesDataKHR' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR'
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-geometryType-parameter#
--     @geometryType@ /must/ be a valid 'GeometryTypeKHR' value
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-triangles-parameter# If
--     @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR', the @triangles@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryTrianglesDataKHR' structure
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-aabbs-parameter# If
--     @geometryType@ is 'GEOMETRY_TYPE_AABBS_KHR', the @aabbs@ member of
--     @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryAabbsDataKHR' structure
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-instances-parameter# If
--     @geometryType@ is 'GEOMETRY_TYPE_INSTANCES_KHR', the @instances@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryInstancesDataKHR' structure
--
-- -   #VUID-VkAccelerationStructureGeometryKHR-flags-parameter# @flags@
--     /must/ be a valid combination of 'GeometryFlagBitsKHR' values
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureGeometryDataKHR', 'GeometryFlagsKHR',
-- 'GeometryTypeKHR', 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryKHR = AccelerationStructureGeometryKHR
  { -- | @geometryType@ describes which type of geometry this
    -- 'AccelerationStructureGeometryKHR' refers to.
    geometryType :: GeometryTypeKHR
  , -- | @geometry@ is a 'AccelerationStructureGeometryDataKHR' union describing
    -- the geometry data for the relevant geometry type.
    geometry :: AccelerationStructureGeometryDataKHR
  , -- | @flags@ is a bitmask of 'GeometryFlagBitsKHR' values describing
    -- additional properties of how the geometry should be built.
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryKHR)
#endif
deriving instance Show AccelerationStructureGeometryKHR

instance ToCStruct AccelerationStructureGeometryKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (geometry) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr GeometryFlagsKHR)) (flags)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryKHR where
  zero = AccelerationStructureGeometryKHR
           zero
           zero
           zero


-- | VkAccelerationStructureBuildGeometryInfoKHR - Structure specifying the
-- geometry data used to build an acceleration structure
--
-- = Description
--
-- Only one of @pGeometries@ or @ppGeometries@ /can/ be a valid pointer,
-- the other /must/ be @NULL@. Each element of the non-@NULL@ array
-- describes the data used to build each acceleration structure geometry.
--
-- The index of each element of the @pGeometries@ or @ppGeometries@ members
-- of 'AccelerationStructureBuildGeometryInfoKHR' is used as the /geometry
-- index/ during ray traversal. The geometry index is available in ray
-- shaders via the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raygeometryindex RayGeometryIndexKHR built-in>,
-- and is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-binding-table-hit-shader-indexing used to determine hit and intersection shaders executed during traversal>.
-- The geometry index is available to ray queries via the
-- @OpRayQueryGetIntersectionGeometryIndexKHR@ instruction.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03654# @type@
--     /must/ not be 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-pGeometries-03788#
--     Only one of @pGeometries@ or @ppGeometries@ /can/ be a valid
--     pointer, the other /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03789# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR', the
--     @geometryType@ member of elements of either @pGeometries@ or
--     @ppGeometries@ /must/ be 'GEOMETRY_TYPE_INSTANCES_KHR'
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03790# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR',
--     @geometryCount@ /must/ be @1@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03791# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' the
--     @geometryType@ member of elements of either @pGeometries@ or
--     @ppGeometries@ /must/ not be 'GEOMETRY_TYPE_INSTANCES_KHR'
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03792# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' then the
--     @geometryType@ member of each geometry in either @pGeometries@ or
--     @ppGeometries@ /must/ be the same
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03793# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' then
--     @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxGeometryCount@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03794# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' and the
--     @geometryType@ member of either @pGeometries@ or @ppGeometries@ is
--     'GEOMETRY_TYPE_AABBS_KHR', the total number of AABBs in all
--     geometries /must/ be less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxPrimitiveCount@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-03795# If
--     @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' and the
--     @geometryType@ member of either @pGeometries@ or @ppGeometries@ is
--     'GEOMETRY_TYPE_TRIANGLES_KHR', the total number of triangles in all
--     geometries /must/ be less than or equal to
--     'PhysicalDeviceAccelerationStructurePropertiesKHR'::@maxPrimitiveCount@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-flags-03796# If
--     @flags@ has the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR' bit set,
--     then it /must/ not have the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR' bit set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR'
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-type-parameter#
--     @type@ /must/ be a valid 'AccelerationStructureTypeKHR' value
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'BuildAccelerationStructureFlagBitsKHR' values
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-pGeometries-parameter#
--     If @geometryCount@ is not @0@, and @pGeometries@ is not @NULL@,
--     @pGeometries@ /must/ be a valid pointer to an array of
--     @geometryCount@ valid 'AccelerationStructureGeometryKHR' structures
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-ppGeometries-parameter#
--     If @geometryCount@ is not @0@, and @ppGeometries@ is not @NULL@,
--     @ppGeometries@ /must/ be a valid pointer to an array of
--     @geometryCount@ valid pointers to valid
--     'AccelerationStructureGeometryKHR' structures
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-scratchData-parameter#
--     @scratchData@ /must/ be a valid 'DeviceOrHostAddressKHR' union
--
-- -   #VUID-VkAccelerationStructureBuildGeometryInfoKHR-commonparent# Both
--     of @dstAccelerationStructure@, and @srcAccelerationStructure@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'AccelerationStructureGeometryKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'AccelerationStructureTypeKHR', 'BuildAccelerationStructureFlagsKHR',
-- 'BuildAccelerationStructureModeKHR', 'DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'buildAccelerationStructuresKHR',
-- 'cmdBuildAccelerationStructuresIndirectKHR',
-- 'cmdBuildAccelerationStructuresKHR',
-- 'getAccelerationStructureBuildSizesKHR'
data AccelerationStructureBuildGeometryInfoKHR = AccelerationStructureBuildGeometryInfoKHR
  { -- | @type@ is a 'AccelerationStructureTypeKHR' value specifying the type of
    -- acceleration structure being built.
    type' :: AccelerationStructureTypeKHR
  , -- | @flags@ is a bitmask of 'BuildAccelerationStructureFlagBitsKHR'
    -- specifying additional parameters of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsKHR
  , -- | @mode@ is a 'BuildAccelerationStructureModeKHR' value specifying the
    -- type of operation to perform.
    mode :: BuildAccelerationStructureModeKHR
  , -- | @srcAccelerationStructure@ points to an existing acceleration structure
    -- that is to be used to update the @dst@ acceleration structure when
    -- @mode@ is 'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR'.
    srcAccelerationStructure :: AccelerationStructureKHR
  , -- | @dstAccelerationStructure@ points to the target acceleration structure
    -- for the build.
    dstAccelerationStructure :: AccelerationStructureKHR
  , -- | @pGeometries@ is a pointer to an array of
    -- 'AccelerationStructureGeometryKHR' structures.
    geometries :: Vector AccelerationStructureGeometryKHR
  , -- | @scratchData@ is the device or host address to memory that will be used
    -- as scratch memory for the build.
    scratchData :: DeviceOrHostAddressKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildGeometryInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildGeometryInfoKHR

instance ToCStruct AccelerationStructureBuildGeometryInfoKHR where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildGeometryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildAccelerationStructureModeKHR)) (mode)
    lift $ poke ((p `plusPtr` 32 :: Ptr AccelerationStructureKHR)) (srcAccelerationStructure)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccelerationStructureKHR)) (dstAccelerationStructure)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometries)) :: Word32))
    pPGeometries' <- ContT $ allocaBytesAligned @AccelerationStructureGeometryKHR ((Data.Vector.length (geometries)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries' `plusPtr` (96 * (i)) :: Ptr AccelerationStructureGeometryKHR) (e) . ($ ())) (geometries)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr AccelerationStructureGeometryKHR))) (pPGeometries')
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr AccelerationStructureGeometryKHR)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (scratchData) . ($ ())
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildAccelerationStructureModeKHR)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr AccelerationStructureGeometryKHR)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureBuildGeometryInfoKHR where
  zero = AccelerationStructureBuildGeometryInfoKHR
           zero
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkAccelerationStructureBuildRangeInfoKHR - Structure specifying build
-- offsets and counts for acceleration structure builds
--
-- = Description
--
-- The primitive count and primitive offset are interpreted differently
-- depending on the 'GeometryTypeKHR' used:
--
-- -   For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @primitiveCount@ is the number of triangles to be built, where each
--     triangle is treated as 3 vertices.
--
--     -   If the geometry uses indices, @primitiveCount@ × 3 indices are
--         consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@indexData@,
--         starting at an offset of @primitiveOffset@. The value of
--         @firstVertex@ is added to the index values before fetching
--         vertices.
--
--     -   If the geometry does not use indices, @primitiveCount@ × 3
--         vertices are consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@vertexData@,
--         starting at an offset of @primitiveOffset@ +
--         'AccelerationStructureGeometryTrianglesDataKHR'::@vertexStride@
--         × @firstVertex@.
--
--     -   A single 'TransformMatrixKHR' structure is consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@,
--         at an offset of @transformOffset@. This transformation matrix is
--         used by all triangles.
--
-- -   For geometries of type 'GEOMETRY_TYPE_AABBS_KHR', @primitiveCount@
--     is the number of axis-aligned bounding boxes. @primitiveCount@
--     'AabbPositionsKHR' structures are consumed from
--     'AccelerationStructureGeometryAabbsDataKHR'::@data@, starting at an
--     offset of @primitiveOffset@.
--
-- -   For geometries of type 'GEOMETRY_TYPE_INSTANCES_KHR',
--     @primitiveCount@ is the number of acceleration structures.
--     @primitiveCount@ 'AccelerationStructureInstanceKHR' structures are
--     consumed from
--     'AccelerationStructureGeometryInstancesDataKHR'::@data@, starting at
--     an offset of @primitiveOffset@.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureBuildRangeInfoKHR-primitiveOffset-03656#
--     For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', if the
--     geometry uses indices, the offset @primitiveOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@indexData@ /must/
--     be a multiple of the element size of
--     'AccelerationStructureGeometryTrianglesDataKHR'::@indexType@
--
-- -   #VUID-VkAccelerationStructureBuildRangeInfoKHR-primitiveOffset-03657#
--     For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', if the
--     geometry doesn’t use indices, the offset @primitiveOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@vertexData@ /must/
--     be a multiple of the component size of
--     'AccelerationStructureGeometryTrianglesDataKHR'::@vertexFormat@
--
-- -   #VUID-VkAccelerationStructureBuildRangeInfoKHR-transformOffset-03658#
--     For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', the offset
--     @transformOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@
--     /must/ be a multiple of 16
--
-- -   #VUID-VkAccelerationStructureBuildRangeInfoKHR-primitiveOffset-03659#
--     For geometries of type 'GEOMETRY_TYPE_AABBS_KHR', the offset
--     @primitiveOffset@ from
--     'AccelerationStructureGeometryAabbsDataKHR'::@data@ /must/ be a
--     multiple of 8
--
-- -   #VUID-VkAccelerationStructureBuildRangeInfoKHR-primitiveOffset-03660#
--     For geometries of type 'GEOMETRY_TYPE_INSTANCES_KHR', the offset
--     @primitiveOffset@ from
--     'AccelerationStructureGeometryInstancesDataKHR'::@data@ /must/ be a
--     multiple of 16
--
-- = See Also
--
-- 'buildAccelerationStructuresKHR', 'cmdBuildAccelerationStructuresKHR'
data AccelerationStructureBuildRangeInfoKHR = AccelerationStructureBuildRangeInfoKHR
  { -- | @primitiveCount@ defines the number of primitives for a corresponding
    -- acceleration structure geometry.
    primitiveCount :: Word32
  , -- | @primitiveOffset@ defines an offset in bytes into the memory where
    -- primitive data is defined.
    primitiveOffset :: Word32
  , -- | @firstVertex@ is the index of the first vertex to build from for
    -- triangle geometry.
    firstVertex :: Word32
  , -- | @transformOffset@ defines an offset in bytes into the memory where a
    -- transform matrix is defined.
    transformOffset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildRangeInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildRangeInfoKHR

instance ToCStruct AccelerationStructureBuildRangeInfoKHR where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildRangeInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (primitiveCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (primitiveOffset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (transformOffset)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct AccelerationStructureBuildRangeInfoKHR where
  peekCStruct p = do
    primitiveCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    primitiveOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    transformOffset <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ AccelerationStructureBuildRangeInfoKHR
             primitiveCount primitiveOffset firstVertex transformOffset

instance Storable AccelerationStructureBuildRangeInfoKHR where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureBuildRangeInfoKHR where
  zero = AccelerationStructureBuildRangeInfoKHR
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureCreateInfoKHR - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- = Description
--
-- If @deviceAddress@ is zero, no specific address is requested.
--
-- If @deviceAddress@ is not zero, @deviceAddress@ /must/ be an address
-- retrieved from an identically created acceleration structure on the same
-- implementation. The acceleration structure /must/ also be placed on an
-- identically created @buffer@ and at the same @offset@.
--
-- Applications /should/ avoid creating acceleration structures with
-- application-provided addresses and implementation-provided addresses in
-- the same process, to reduce the likelihood of
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
-- errors.
--
-- Note
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
-- flag to all buffers that use
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT',
-- and
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
-- to all buffers used as storage for an acceleration structure where
-- @deviceAddress@ is not zero. During capture the tool will save the
-- queried opaque device addresses in the trace. During replay, the buffers
-- will be created specifying the original address so any address values
-- stored in the trace data will remain valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
-- to avoid address space allocation conflicts.
--
-- Applications /should/ create an acceleration structure with a specific
-- 'AccelerationStructureTypeKHR' other than
-- 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'.
--
-- If the acceleration structure will be the target of a build operation,
-- the required size for an acceleration structure /can/ be queried with
-- 'getAccelerationStructureBuildSizesKHR'. If the acceleration structure
-- is going to be the target of a compacting copy,
-- 'cmdWriteAccelerationStructuresPropertiesKHR' or
-- 'writeAccelerationStructuresPropertiesKHR' /can/ be used to obtain the
-- compacted size required.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-deviceAddress-03612# If
--     @deviceAddress@ is not zero, @createFlags@ /must/ include
--     'ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-createFlags-03613# If
--     @createFlags@ includes
--     'ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR',
--     'PhysicalDeviceAccelerationStructureFeaturesKHR'::@accelerationStructureCaptureReplay@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-buffer-03614# @buffer@
--     /must/ have been created with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR'
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-buffer-03615# @buffer@
--     /must/ not have been created with
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-offset-03616# The sum of
--     @offset@ and @size@ /must/ be less than the size of @buffer@
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-offset-03734# @offset@
--     /must/ be a multiple of @256@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR'
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-createFlags-parameter#
--     @createFlags@ /must/ be a valid combination of
--     'AccelerationStructureCreateFlagBitsKHR' values
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-buffer-parameter#
--     @buffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkAccelerationStructureCreateInfoKHR-type-parameter# @type@
--     /must/ be a valid 'AccelerationStructureTypeKHR' value
--
-- = See Also
--
-- 'AccelerationStructureCreateFlagsKHR', 'AccelerationStructureTypeKHR',
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAccelerationStructureKHR'
data AccelerationStructureCreateInfoKHR = AccelerationStructureCreateInfoKHR
  { -- | @createFlags@ is a bitmask of 'AccelerationStructureCreateFlagBitsKHR'
    -- specifying additional creation parameters of the acceleration structure.
    createFlags :: AccelerationStructureCreateFlagsKHR
  , -- | @buffer@ is the buffer on which the acceleration structure will be
    -- stored.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes from the base address of the buffer at
    -- which the acceleration structure will be stored, and /must/ be a
    -- multiple of @256@.
    offset :: DeviceSize
  , -- | @size@ is the size required for the acceleration structure.
    size :: DeviceSize
  , -- | @type@ is a 'AccelerationStructureTypeKHR' value specifying the type of
    -- acceleration structure that will be created.
    type' :: AccelerationStructureTypeKHR
  , -- | @deviceAddress@ is the device address requested for the acceleration
    -- structure if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructureCaptureReplay accelerationStructureCaptureReplay>
    -- feature is being used.
    deviceAddress :: DeviceAddress
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureCreateInfoKHR)
#endif
deriving instance Show AccelerationStructureCreateInfoKHR

instance ToCStruct AccelerationStructureCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureCreateFlagsKHR)) (createFlags)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR)) (type')
    poke ((p `plusPtr` 56 :: Ptr DeviceAddress)) (deviceAddress)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR)) (zero)
    f

instance FromCStruct AccelerationStructureCreateInfoKHR where
  peekCStruct p = do
    createFlags <- peek @AccelerationStructureCreateFlagsKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureCreateFlagsKHR))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    type' <- peek @AccelerationStructureTypeKHR ((p `plusPtr` 48 :: Ptr AccelerationStructureTypeKHR))
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 56 :: Ptr DeviceAddress))
    pure $ AccelerationStructureCreateInfoKHR
             createFlags buffer offset size type' deviceAddress

instance Storable AccelerationStructureCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureCreateInfoKHR where
  zero = AccelerationStructureCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAabbPositionsKHR - Structure specifying two opposing corners of an
-- axis-aligned bounding box
--
-- == Valid Usage
--
-- = See Also
--
-- No cross-references are available
data AabbPositionsKHR = AabbPositionsKHR
  { -- | @minX@ is the x position of one opposing corner of a bounding box.
    --
    -- #VUID-VkAabbPositionsKHR-minX-03546# @minX@ /must/ be less than or equal
    -- to @maxX@
    minX :: Float
  , -- | @minY@ is the y position of one opposing corner of a bounding box.
    --
    -- #VUID-VkAabbPositionsKHR-minY-03547# @minY@ /must/ be less than or equal
    -- to @maxY@
    minY :: Float
  , -- | @minZ@ is the z position of one opposing corner of a bounding box.
    --
    -- #VUID-VkAabbPositionsKHR-minZ-03548# @minZ@ /must/ be less than or equal
    -- to @maxZ@
    minZ :: Float
  , -- | @maxX@ is the x position of the other opposing corner of a bounding box.
    maxX :: Float
  , -- | @maxY@ is the y position of the other opposing corner of a bounding box.
    maxY :: Float
  , -- | @maxZ@ is the z position of the other opposing corner of a bounding box.
    maxZ :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AabbPositionsKHR)
#endif
deriving instance Show AabbPositionsKHR

instance ToCStruct AabbPositionsKHR where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AabbPositionsKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (minX))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (minY))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (minZ))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (maxX))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (maxY))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxZ))
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct AabbPositionsKHR where
  peekCStruct p = do
    minX <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    minY <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    minZ <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    maxX <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    maxY <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxZ <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ AabbPositionsKHR
             (coerce @CFloat @Float minX) (coerce @CFloat @Float minY) (coerce @CFloat @Float minZ) (coerce @CFloat @Float maxX) (coerce @CFloat @Float maxY) (coerce @CFloat @Float maxZ)

instance Storable AabbPositionsKHR where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AabbPositionsKHR where
  zero = AabbPositionsKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkTransformMatrixKHR - Structure specifying a 3x4 affine transformation
-- matrix
--
-- == Valid Usage
--
-- -   #VUID-VkTransformMatrixKHR-matrix-03799# The first three columns of
--     @matrix@ /must/ define an invertible 3x3 matrix
--
-- = See Also
--
-- 'AccelerationStructureInstanceKHR'
data TransformMatrixKHR = TransformMatrixKHR
  { -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow0"
    matrixRow0 :: (Float, Float, Float, Float)
  , -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow1"
    matrixRow1 :: (Float, Float, Float, Float)
  , -- No documentation found for Nested "VkTransformMatrixKHR" "matrixRow2"
    matrixRow2 :: (Float, Float, Float, Float)
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TransformMatrixKHR)
#endif
deriving instance Show TransformMatrixKHR

instance ToCStruct TransformMatrixKHR where
  withCStruct x f = allocaBytesAligned 48 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TransformMatrixKHR{..} f = do
    let pMatrixRow0' = lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow0) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow0' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow0' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow0' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow0' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow1' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow1) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow1' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow1' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow1' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow1' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    let pMatrixRow2' = lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray 4 CFloat)))
    case (matrixRow2) of
      (e0, e1, e2, e3) -> do
        poke (pMatrixRow2' :: Ptr CFloat) (CFloat (e0))
        poke (pMatrixRow2' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pMatrixRow2' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pMatrixRow2' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    f
  cStructSize = 48
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke (p `plusPtr` 0) (CFloat 1)
    poke (p `plusPtr` 20) (CFloat 1)
    poke (p `plusPtr` 40) (CFloat 1)
    f

instance FromCStruct TransformMatrixKHR where
  peekCStruct p = do
    let pmatrixRow0 = lowerArrayPtr @CFloat ((p `plusPtr` 0 :: Ptr (FixedArray 4 CFloat)))
    matrixRow00 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow01 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow02 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow03 <- peek @CFloat ((pmatrixRow0 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrixRow1 = lowerArrayPtr @CFloat ((p `plusPtr` 16 :: Ptr (FixedArray 4 CFloat)))
    matrixRow10 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow11 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow12 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow13 <- peek @CFloat ((pmatrixRow1 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrixRow2 = lowerArrayPtr @CFloat ((p `plusPtr` 32 :: Ptr (FixedArray 4 CFloat)))
    matrixRow20 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 0 :: Ptr CFloat))
    matrixRow21 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 4 :: Ptr CFloat))
    matrixRow22 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 8 :: Ptr CFloat))
    matrixRow23 <- peek @CFloat ((pmatrixRow2 `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ TransformMatrixKHR
             (((coerce @CFloat @Float matrixRow00), (coerce @CFloat @Float matrixRow01), (coerce @CFloat @Float matrixRow02), (coerce @CFloat @Float matrixRow03))) (((coerce @CFloat @Float matrixRow10), (coerce @CFloat @Float matrixRow11), (coerce @CFloat @Float matrixRow12), (coerce @CFloat @Float matrixRow13))) (((coerce @CFloat @Float matrixRow20), (coerce @CFloat @Float matrixRow21), (coerce @CFloat @Float matrixRow22), (coerce @CFloat @Float matrixRow23)))

instance Storable TransformMatrixKHR where
  sizeOf ~_ = 48
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TransformMatrixKHR where
 zero = TransformMatrixKHR
          (1,0,0,0)
          (0,1,0,0)
          (0,0,1,0)


-- | VkAccelerationStructureInstanceKHR - Structure specifying a single
-- acceleration structure instance for building into an acceleration
-- structure geometry
--
-- = Description
--
-- The C language spec does not define the ordering of bit-fields, but in
-- practice, this struct produces the correct layout with existing
-- compilers. The intended bit pattern is for the following:
--
-- If a compiler produces code that diverges from that pattern,
-- applications /must/ employ another method to set values according to the
-- correct bit pattern.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'GeometryInstanceFlagsKHR', 'TransformMatrixKHR'
data AccelerationStructureInstanceKHR = AccelerationStructureInstanceKHR
  { -- | @transform@ is a 'TransformMatrixKHR' structure describing a
    -- transformation to be applied to the acceleration structure.
    transform :: TransformMatrixKHR
  , -- | @instanceCustomIndex@ is a 24-bit user-specified index value accessible
    -- to ray shaders in the @InstanceCustomIndexKHR@ built-in.
    --
    -- @instanceCustomIndex@ and @mask@ occupy the same memory as if a single
    -- @int32_t@ was specified in their place
    --
    -- -   @instanceCustomIndex@ occupies the 24 least significant bits of that
    --     memory
    --
    -- -   @mask@ occupies the 8 most significant bits of that memory
    instanceCustomIndex :: Word32
  , -- | @mask@ is an 8-bit visibility mask for the geometry. The instance /may/
    -- only be hit if @rayMask & instance.mask != 0@
    mask :: Word32
  , -- | @instanceShaderBindingTableRecordOffset@ is a 24-bit offset used in
    -- calculating the hit shader binding table index.
    --
    -- @instanceShaderBindingTableRecordOffset@ and @flags@ occupy the same
    -- memory as if a single @int32_t@ was specified in their place
    --
    -- -   @instanceShaderBindingTableRecordOffset@ occupies the 24 least
    --     significant bits of that memory
    --
    -- -   @flags@ occupies the 8 most significant bits of that memory
    instanceShaderBindingTableRecordOffset :: Word32
  , -- | @flags@ is an 8-bit mask of 'GeometryInstanceFlagBitsKHR' values to
    -- apply to this instance.
    --
    -- #VUID-VkAccelerationStructureInstanceKHR-flags-parameter# @flags@ /must/
    -- be a valid combination of 'GeometryInstanceFlagBitsKHR' values
    flags :: GeometryInstanceFlagsKHR
  , -- | @accelerationStructureReference@ is either:
    --
    -- -   a device address containing the value obtained from
    --     'getAccelerationStructureDeviceAddressKHR' or
    --     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
    --     (used by device operations which reference acceleration structures)
    --     or,
    --
    -- -   a 'Vulkan.Extensions.Handles.AccelerationStructureKHR' object (used
    --     by host operations which reference acceleration structures).
    accelerationStructureReference :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureInstanceKHR)
#endif
deriving instance Show AccelerationStructureInstanceKHR

instance ToCStruct AccelerationStructureInstanceKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInstanceKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (transform)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (((coerce @_ @Word32 (mask)) `shiftL` 24) .|. (instanceCustomIndex))
    poke ((p `plusPtr` 52 :: Ptr Word32)) (((coerce @_ @Word32 (flags)) `shiftL` 24) .|. (instanceShaderBindingTableRecordOffset))
    poke ((p `plusPtr` 56 :: Ptr Word64)) (accelerationStructureReference)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (zero)
    f

instance FromCStruct AccelerationStructureInstanceKHR where
  peekCStruct p = do
    transform <- peekCStruct @TransformMatrixKHR ((p `plusPtr` 0 :: Ptr TransformMatrixKHR))
    instanceCustomIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let instanceCustomIndex' = ((instanceCustomIndex .&. coerce @Word32 0xffffff))
    mask <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let mask' = ((((mask `shiftR` 24)) .&. coerce @Word32 0xff))
    instanceShaderBindingTableRecordOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    let instanceShaderBindingTableRecordOffset' = ((instanceShaderBindingTableRecordOffset .&. coerce @Word32 0xffffff))
    flags <- peek @GeometryInstanceFlagsKHR ((p `plusPtr` 52 :: Ptr GeometryInstanceFlagsKHR))
    let flags' = ((((flags `shiftR` 24)) .&. coerce @Word32 0xff))
    accelerationStructureReference <- peek @Word64 ((p `plusPtr` 56 :: Ptr Word64))
    pure $ AccelerationStructureInstanceKHR
             transform instanceCustomIndex' mask' instanceShaderBindingTableRecordOffset' flags' accelerationStructureReference

instance Storable AccelerationStructureInstanceKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureInstanceKHR where
  zero = AccelerationStructureInstanceKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureDeviceAddressInfoKHR - Structure specifying the
-- acceleration structure to query an address for
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureDeviceAddressKHR'
data AccelerationStructureDeviceAddressInfoKHR = AccelerationStructureDeviceAddressInfoKHR
  { -- | @accelerationStructure@ specifies the acceleration structure whose
    -- address is being queried.
    --
    -- #VUID-VkAccelerationStructureDeviceAddressInfoKHR-accelerationStructure-parameter#
    -- @accelerationStructure@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
    accelerationStructure :: AccelerationStructureKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureDeviceAddressInfoKHR)
#endif
deriving instance Show AccelerationStructureDeviceAddressInfoKHR

instance ToCStruct AccelerationStructureDeviceAddressInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureDeviceAddressInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    f

instance FromCStruct AccelerationStructureDeviceAddressInfoKHR where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    pure $ AccelerationStructureDeviceAddressInfoKHR
             accelerationStructure

instance Storable AccelerationStructureDeviceAddressInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureDeviceAddressInfoKHR where
  zero = AccelerationStructureDeviceAddressInfoKHR
           zero


-- | VkAccelerationStructureVersionInfoKHR - Acceleration structure version
-- information
--
-- = Description
--
-- Note
--
-- @pVersionData@ is a /pointer/ to an array of
-- 2*'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values instead of two
-- 'Vulkan.Core10.APIConstants.UUID_SIZE' arrays as the expected use case
-- for this member is to be pointed at the header of an previously
-- serialized acceleration structure (via
-- 'cmdCopyAccelerationStructureToMemoryKHR' or
-- 'copyAccelerationStructureToMemoryKHR') that is loaded in memory. Using
-- arrays would necessitate extra memory copies of the UUIDs.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceAccelerationStructureCompatibilityKHR'
data AccelerationStructureVersionInfoKHR = AccelerationStructureVersionInfoKHR
  { -- | @pVersionData@ is a pointer to the version header of an acceleration
    -- structure as defined in 'cmdCopyAccelerationStructureToMemoryKHR'
    --
    -- #VUID-VkAccelerationStructureVersionInfoKHR-pVersionData-parameter#
    -- @pVersionData@ /must/ be a valid pointer to an array of
    -- @2@*'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values
    versionData :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureVersionInfoKHR)
#endif
deriving instance Show AccelerationStructureVersionInfoKHR

instance ToCStruct AccelerationStructureVersionInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureVersionInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ unless (Data.ByteString.length (versionData) == 2 * UUID_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "AccelerationStructureVersionKHR::versionData must be 2*VK_UUID_SIZE bytes" Nothing Nothing
    versionData' <- fmap (castPtr @CChar @Word8) . ContT $ unsafeUseAsCString (versionData)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Word8))) versionData'
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AccelerationStructureVersionInfoKHR where
  peekCStruct p = do
    versionData <- peek @(Ptr Word8) ((p `plusPtr` 16 :: Ptr (Ptr Word8)))
    versionData' <- packCStringLen (castPtr @Word8 @CChar versionData, 2 * UUID_SIZE)
    pure $ AccelerationStructureVersionInfoKHR
             versionData'

instance Zero AccelerationStructureVersionInfoKHR where
  zero = AccelerationStructureVersionInfoKHR
           mempty


-- | VkCopyAccelerationStructureInfoKHR - Parameters for copying an
-- acceleration structure
--
-- == Valid Usage
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-mode-03410# @mode@ /must/
--     be 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' or
--     'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR'
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-src-03411# If @mode@ is
--     'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR', @src@ /must/ have
--     been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR'
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-buffer-03718# The @buffer@
--     used to create @src@ /must/ be bound to device memory
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-buffer-03719# The @buffer@
--     used to create @dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR'
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-src-parameter# @src@ /must/
--     be a valid 'Vulkan.Extensions.Handles.AccelerationStructureKHR'
--     handle
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-dst-parameter# @dst@ /must/
--     be a valid 'Vulkan.Extensions.Handles.AccelerationStructureKHR'
--     handle
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-mode-parameter# @mode@
--     /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- -   #VUID-VkCopyAccelerationStructureInfoKHR-commonparent# Both of
--     @dst@, and @src@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyAccelerationStructureKHR', 'copyAccelerationStructureKHR'
data CopyAccelerationStructureInfoKHR = CopyAccelerationStructureInfoKHR
  { -- | @src@ is the source acceleration structure for the copy.
    src :: AccelerationStructureKHR
  , -- | @dst@ is the target acceleration structure for the copy.
    dst :: AccelerationStructureKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyAccelerationStructureInfoKHR)
#endif
deriving instance Show CopyAccelerationStructureInfoKHR

instance ToCStruct CopyAccelerationStructureInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    f

instance FromCStruct CopyAccelerationStructureInfoKHR where
  peekCStruct p = do
    src <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    dst <- peek @AccelerationStructureKHR ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR))
    mode <- peek @CopyAccelerationStructureModeKHR ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR))
    pure $ CopyAccelerationStructureInfoKHR
             src dst mode

instance Storable CopyAccelerationStructureInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyAccelerationStructureInfoKHR where
  zero = CopyAccelerationStructureInfoKHR
           zero
           zero
           zero


-- | VkCopyAccelerationStructureToMemoryInfoKHR - Parameters for serializing
-- an acceleration structure
--
-- == Valid Usage
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-dst-03561# The
--     memory pointed to by @dst@ /must/ be at least as large as the
--     serialization size of @src@, as reported by
--     'writeAccelerationStructuresPropertiesKHR' or
--     'cmdWriteAccelerationStructuresPropertiesKHR' with a query type of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-mode-03412# @mode@
--     /must/ be 'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR'
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-src-parameter#
--     @src@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-dst-parameter#
--     @dst@ /must/ be a valid 'DeviceOrHostAddressKHR' union
--
-- -   #VUID-VkCopyAccelerationStructureToMemoryInfoKHR-mode-parameter#
--     @mode@ /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR', 'DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyAccelerationStructureToMemoryKHR',
-- 'copyAccelerationStructureToMemoryKHR'
data CopyAccelerationStructureToMemoryInfoKHR = CopyAccelerationStructureToMemoryInfoKHR
  { -- | @src@ is the source acceleration structure for the copy
    src :: AccelerationStructureKHR
  , -- | @dst@ is the device or host address to memory which is the target for
    -- the copy
    dst :: DeviceOrHostAddressKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyAccelerationStructureToMemoryInfoKHR)
#endif
deriving instance Show CopyAccelerationStructureToMemoryInfoKHR

instance ToCStruct CopyAccelerationStructureToMemoryInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureToMemoryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (dst) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance Zero CopyAccelerationStructureToMemoryInfoKHR where
  zero = CopyAccelerationStructureToMemoryInfoKHR
           zero
           zero
           zero


-- | VkCopyMemoryToAccelerationStructureInfoKHR - Parameters for
-- deserializing an acceleration structure
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-mode-03413# @mode@
--     /must/ be 'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR'
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-pInfo-03414# The
--     data in @src@ /must/ have a format compatible with the destination
--     physical device as returned by
--     'getDeviceAccelerationStructureCompatibilityKHR'
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-dst-03746# @dst@
--     /must/ have been created with a @size@ greater than or equal to that
--     used to serialize the data in @src@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR'
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-src-parameter#
--     @src@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-dst-parameter#
--     @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   #VUID-VkCopyMemoryToAccelerationStructureInfoKHR-mode-parameter#
--     @mode@ /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMemoryToAccelerationStructureKHR',
-- 'copyMemoryToAccelerationStructureKHR'
data CopyMemoryToAccelerationStructureInfoKHR = CopyMemoryToAccelerationStructureInfoKHR
  { -- | @src@ is the device or host address to memory containing the source data
    -- for the copy.
    src :: DeviceOrHostAddressConstKHR
  , -- | @dst@ is the target acceleration structure for the copy.
    dst :: AccelerationStructureKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToAccelerationStructureInfoKHR)
#endif
deriving instance Show CopyMemoryToAccelerationStructureInfoKHR

instance ToCStruct CopyMemoryToAccelerationStructureInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToAccelerationStructureInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (src) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance Zero CopyMemoryToAccelerationStructureInfoKHR where
  zero = CopyMemoryToAccelerationStructureInfoKHR
           zero
           zero
           zero


-- | VkAccelerationStructureBuildSizesInfoKHR - Structure specifying build
-- sizes for an acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureBuildSizesKHR'
data AccelerationStructureBuildSizesInfoKHR = AccelerationStructureBuildSizesInfoKHR
  { -- | @accelerationStructureSize@ is the size in bytes required in a
    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' for a build or
    -- update operation.
    accelerationStructureSize :: DeviceSize
  , -- | @updateScratchSize@ is the size in bytes required in a scratch buffer
    -- for an update operation.
    updateScratchSize :: DeviceSize
  , -- | @buildScratchSize@ is the size in bytes required in a scratch buffer for
    -- a build operation.
    buildScratchSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureBuildSizesInfoKHR)
#endif
deriving instance Show AccelerationStructureBuildSizesInfoKHR

instance ToCStruct AccelerationStructureBuildSizesInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildSizesInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (accelerationStructureSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (updateScratchSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (buildScratchSize)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct AccelerationStructureBuildSizesInfoKHR where
  peekCStruct p = do
    accelerationStructureSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    updateScratchSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    buildScratchSize <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ AccelerationStructureBuildSizesInfoKHR
             accelerationStructureSize updateScratchSize buildScratchSize

instance Storable AccelerationStructureBuildSizesInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureBuildSizesInfoKHR where
  zero = AccelerationStructureBuildSizesInfoKHR
           zero
           zero
           zero


data DeviceOrHostAddressKHR
  = DeviceAddress DeviceAddress
  | HostAddress (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressKHR -> DeviceOrHostAddressKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddress v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddress v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressKHR where
  zero = DeviceAddress zero


data DeviceOrHostAddressConstKHR
  = DeviceAddressConst DeviceAddress
  | HostAddressConst (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressConstKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressConstKHR -> DeviceOrHostAddressConstKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddressConst v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddressConst v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressConstKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressConstKHR where
  zero = DeviceAddressConst zero


data AccelerationStructureGeometryDataKHR
  = Triangles AccelerationStructureGeometryTrianglesDataKHR
  | Aabbs AccelerationStructureGeometryAabbsDataKHR
  | Instances AccelerationStructureGeometryInstancesDataKHR
  deriving (Show)

instance ToCStruct AccelerationStructureGeometryDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr AccelerationStructureGeometryDataKHR -> AccelerationStructureGeometryDataKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Triangles v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryTrianglesDataKHR p) (v) . ($ ())
    Aabbs v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryAabbsDataKHR p) (v) . ($ ())
    Instances v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryInstancesDataKHR p) (v) . ($ ())
  pokeZeroCStruct :: Ptr AccelerationStructureGeometryDataKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 64
  cStructAlignment = 8

instance Zero AccelerationStructureGeometryDataKHR where
  zero = Triangles zero


type GeometryInstanceFlagsKHR = GeometryInstanceFlagBitsKHR

-- | VkGeometryInstanceFlagBitsKHR - Instance flag bits
--
-- = Description
--
-- 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR' and
-- 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR' /must/ not be used in the same
-- flag.
--
-- = See Also
--
-- 'GeometryInstanceFlagsKHR'
newtype GeometryInstanceFlagBitsKHR = GeometryInstanceFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR' disables face
-- culling for this instance.
pattern GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR    = GeometryInstanceFlagBitsKHR 0x00000001
-- | 'GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR' indicates
-- that the front face of the triangle for culling purposes is the face
-- that is counter clockwise in object space relative to the ray origin.
-- Because the facing is determined in object space, an instance transform
-- matrix does not change the winding, but a geometry transform does.
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000002
-- | 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR' causes this instance to act as
-- though 'GEOMETRY_OPAQUE_BIT_KHR' were specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- SPIR-V @NoOpaqueKHR@ ray flag.
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR                    = GeometryInstanceFlagBitsKHR 0x00000004
-- | 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR' causes this instance to act
-- as though 'GEOMETRY_OPAQUE_BIT_KHR' were not specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- SPIR-V @OpaqueKHR@ ray flag.
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR                 = GeometryInstanceFlagBitsKHR 0x00000008

conNameGeometryInstanceFlagBitsKHR :: String
conNameGeometryInstanceFlagBitsKHR = "GeometryInstanceFlagBitsKHR"

enumPrefixGeometryInstanceFlagBitsKHR :: String
enumPrefixGeometryInstanceFlagBitsKHR = "GEOMETRY_INSTANCE_"

showTableGeometryInstanceFlagBitsKHR :: [(GeometryInstanceFlagBitsKHR, String)]
showTableGeometryInstanceFlagBitsKHR =
  [ (GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR   , "TRIANGLE_FACING_CULL_DISABLE_BIT_KHR")
  , (GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR, "TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR")
  , (GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR                   , "FORCE_OPAQUE_BIT_KHR")
  , (GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR                , "FORCE_NO_OPAQUE_BIT_KHR")
  ]

instance Show GeometryInstanceFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixGeometryInstanceFlagBitsKHR
                            showTableGeometryInstanceFlagBitsKHR
                            conNameGeometryInstanceFlagBitsKHR
                            (\(GeometryInstanceFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read GeometryInstanceFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixGeometryInstanceFlagBitsKHR
                          showTableGeometryInstanceFlagBitsKHR
                          conNameGeometryInstanceFlagBitsKHR
                          GeometryInstanceFlagBitsKHR


type GeometryFlagsKHR = GeometryFlagBitsKHR

-- | VkGeometryFlagBitsKHR - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- 'GeometryFlagsKHR'
newtype GeometryFlagBitsKHR = GeometryFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'GEOMETRY_OPAQUE_BIT_KHR' indicates that this geometry does not invoke
-- the any-hit shaders even if present in a hit group.
pattern GEOMETRY_OPAQUE_BIT_KHR                          = GeometryFlagBitsKHR 0x00000001
-- | 'GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR' indicates that the
-- implementation /must/ only call the any-hit shader a single time for
-- each primitive in this geometry. If this bit is absent an implementation
-- /may/ invoke the any-hit shader more than once for this geometry.
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR = GeometryFlagBitsKHR 0x00000002

conNameGeometryFlagBitsKHR :: String
conNameGeometryFlagBitsKHR = "GeometryFlagBitsKHR"

enumPrefixGeometryFlagBitsKHR :: String
enumPrefixGeometryFlagBitsKHR = "GEOMETRY_"

showTableGeometryFlagBitsKHR :: [(GeometryFlagBitsKHR, String)]
showTableGeometryFlagBitsKHR =
  [ (GEOMETRY_OPAQUE_BIT_KHR                         , "OPAQUE_BIT_KHR")
  , (GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR, "NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR")
  ]

instance Show GeometryFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixGeometryFlagBitsKHR
                            showTableGeometryFlagBitsKHR
                            conNameGeometryFlagBitsKHR
                            (\(GeometryFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read GeometryFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixGeometryFlagBitsKHR
                          showTableGeometryFlagBitsKHR
                          conNameGeometryFlagBitsKHR
                          GeometryFlagBitsKHR


type BuildAccelerationStructureFlagsKHR = BuildAccelerationStructureFlagBitsKHR

-- | VkBuildAccelerationStructureFlagBitsKHR - Bitmask specifying additional
-- parameters for acceleration structure builds
--
-- = Description
--
-- Note
--
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' and
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' /may/ take more
-- time and memory than a normal build, and so /should/ only be used when
-- those features are needed.
--
-- = See Also
--
-- 'BuildAccelerationStructureFlagsKHR'
newtype BuildAccelerationStructureFlagBitsKHR = BuildAccelerationStructureFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' indicates that the
-- specified acceleration structure /can/ be updated with @update@ of
-- 'Vulkan.Core10.FundamentalTypes.TRUE' in
-- 'cmdBuildAccelerationStructuresKHR' or
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV' .
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR      = BuildAccelerationStructureFlagBitsKHR 0x00000001
-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' indicates that
-- the specified acceleration structure /can/ act as the source for a copy
-- acceleration structure command with @mode@ of
-- 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' to produce a compacted
-- acceleration structure.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR  = BuildAccelerationStructureFlagBitsKHR 0x00000002
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR' indicates that
-- the given acceleration structure build /should/ prioritize trace
-- performance over build time.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000004
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR' indicates that
-- the given acceleration structure build /should/ prioritize build time
-- over trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000008
-- | 'BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR' indicates that this
-- acceleration structure /should/ minimize the size of the scratch memory
-- and the final result build, potentially at the expense of build time or
-- trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR        = BuildAccelerationStructureFlagBitsKHR 0x00000010

conNameBuildAccelerationStructureFlagBitsKHR :: String
conNameBuildAccelerationStructureFlagBitsKHR = "BuildAccelerationStructureFlagBitsKHR"

enumPrefixBuildAccelerationStructureFlagBitsKHR :: String
enumPrefixBuildAccelerationStructureFlagBitsKHR = "BUILD_ACCELERATION_STRUCTURE_"

showTableBuildAccelerationStructureFlagBitsKHR :: [(BuildAccelerationStructureFlagBitsKHR, String)]
showTableBuildAccelerationStructureFlagBitsKHR =
  [ (BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR     , "ALLOW_UPDATE_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR , "ALLOW_COMPACTION_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR, "PREFER_FAST_TRACE_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR, "PREFER_FAST_BUILD_BIT_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR       , "LOW_MEMORY_BIT_KHR")
  ]

instance Show BuildAccelerationStructureFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixBuildAccelerationStructureFlagBitsKHR
                            showTableBuildAccelerationStructureFlagBitsKHR
                            conNameBuildAccelerationStructureFlagBitsKHR
                            (\(BuildAccelerationStructureFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read BuildAccelerationStructureFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixBuildAccelerationStructureFlagBitsKHR
                          showTableBuildAccelerationStructureFlagBitsKHR
                          conNameBuildAccelerationStructureFlagBitsKHR
                          BuildAccelerationStructureFlagBitsKHR


type AccelerationStructureCreateFlagsKHR = AccelerationStructureCreateFlagBitsKHR

-- | VkAccelerationStructureCreateFlagBitsKHR - Bitmask specifying additional
-- creation parameters for acceleration structure
--
-- = See Also
--
-- 'AccelerationStructureCreateFlagsKHR'
newtype AccelerationStructureCreateFlagBitsKHR = AccelerationStructureCreateFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR'
-- specifies that the acceleration structure’s address /can/ be saved and
-- reused on a subsequent run.
pattern ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR =
  AccelerationStructureCreateFlagBitsKHR 0x00000001

conNameAccelerationStructureCreateFlagBitsKHR :: String
conNameAccelerationStructureCreateFlagBitsKHR = "AccelerationStructureCreateFlagBitsKHR"

enumPrefixAccelerationStructureCreateFlagBitsKHR :: String
enumPrefixAccelerationStructureCreateFlagBitsKHR =
  "ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR"

showTableAccelerationStructureCreateFlagBitsKHR :: [(AccelerationStructureCreateFlagBitsKHR, String)]
showTableAccelerationStructureCreateFlagBitsKHR =
  [(ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR, "")]

instance Show AccelerationStructureCreateFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixAccelerationStructureCreateFlagBitsKHR
                            showTableAccelerationStructureCreateFlagBitsKHR
                            conNameAccelerationStructureCreateFlagBitsKHR
                            (\(AccelerationStructureCreateFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AccelerationStructureCreateFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureCreateFlagBitsKHR
                          showTableAccelerationStructureCreateFlagBitsKHR
                          conNameAccelerationStructureCreateFlagBitsKHR
                          AccelerationStructureCreateFlagBitsKHR


-- | VkCopyAccelerationStructureModeKHR - Acceleration structure copy mode
--
-- = See Also
--
-- 'CopyAccelerationStructureInfoKHR',
-- 'CopyAccelerationStructureToMemoryInfoKHR',
-- 'CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV'
newtype CopyAccelerationStructureModeKHR = CopyAccelerationStructureModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR' creates a direct copy of
-- the acceleration structure specified in @src@ into the one specified by
-- @dst@. The @dst@ acceleration structure /must/ have been created with
-- the same parameters as @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR       = CopyAccelerationStructureModeKHR 0
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' creates a more compact
-- version of an acceleration structure @src@ into @dst@. The acceleration
-- structure @dst@ /must/ have been created with a size at least as large
-- as that returned by 'cmdWriteAccelerationStructuresPropertiesKHR' or
-- 'writeAccelerationStructuresPropertiesKHR' after the build of the
-- acceleration structure specified by @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR     = CopyAccelerationStructureModeKHR 1
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR' serializes the
-- acceleration structure to a semi-opaque format which can be reloaded on
-- a compatible implementation.
pattern COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR   = CopyAccelerationStructureModeKHR 2
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR' deserializes the
-- semi-opaque serialization format in the buffer to the acceleration
-- structure.
pattern COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR = CopyAccelerationStructureModeKHR 3
{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR :: CopyAccelerationStructureModeKHR #-}

conNameCopyAccelerationStructureModeKHR :: String
conNameCopyAccelerationStructureModeKHR = "CopyAccelerationStructureModeKHR"

enumPrefixCopyAccelerationStructureModeKHR :: String
enumPrefixCopyAccelerationStructureModeKHR = "COPY_ACCELERATION_STRUCTURE_MODE_"

showTableCopyAccelerationStructureModeKHR :: [(CopyAccelerationStructureModeKHR, String)]
showTableCopyAccelerationStructureModeKHR =
  [ (COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR      , "CLONE_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR    , "COMPACT_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR  , "SERIALIZE_KHR")
  , (COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR, "DESERIALIZE_KHR")
  ]

instance Show CopyAccelerationStructureModeKHR where
  showsPrec = enumShowsPrec enumPrefixCopyAccelerationStructureModeKHR
                            showTableCopyAccelerationStructureModeKHR
                            conNameCopyAccelerationStructureModeKHR
                            (\(CopyAccelerationStructureModeKHR x) -> x)
                            (showsPrec 11)

instance Read CopyAccelerationStructureModeKHR where
  readPrec = enumReadPrec enumPrefixCopyAccelerationStructureModeKHR
                          showTableCopyAccelerationStructureModeKHR
                          conNameCopyAccelerationStructureModeKHR
                          CopyAccelerationStructureModeKHR


-- | VkBuildAccelerationStructureModeKHR - Enum specifying the type of build
-- operation to perform
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR'
newtype BuildAccelerationStructureModeKHR = BuildAccelerationStructureModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR' specifies that the
-- destination acceleration structure will be built using the specified
-- geometries.
pattern BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR  = BuildAccelerationStructureModeKHR 0
-- | 'BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR' specifies that the
-- destination acceleration structure will be built using data in a source
-- acceleration structure, updated by the specified geometries.
pattern BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR = BuildAccelerationStructureModeKHR 1
{-# complete BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR,
             BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR :: BuildAccelerationStructureModeKHR #-}

conNameBuildAccelerationStructureModeKHR :: String
conNameBuildAccelerationStructureModeKHR = "BuildAccelerationStructureModeKHR"

enumPrefixBuildAccelerationStructureModeKHR :: String
enumPrefixBuildAccelerationStructureModeKHR = "BUILD_ACCELERATION_STRUCTURE_MODE_"

showTableBuildAccelerationStructureModeKHR :: [(BuildAccelerationStructureModeKHR, String)]
showTableBuildAccelerationStructureModeKHR =
  [ (BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR , "BUILD_KHR")
  , (BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR, "UPDATE_KHR")
  ]

instance Show BuildAccelerationStructureModeKHR where
  showsPrec = enumShowsPrec enumPrefixBuildAccelerationStructureModeKHR
                            showTableBuildAccelerationStructureModeKHR
                            conNameBuildAccelerationStructureModeKHR
                            (\(BuildAccelerationStructureModeKHR x) -> x)
                            (showsPrec 11)

instance Read BuildAccelerationStructureModeKHR where
  readPrec = enumReadPrec enumPrefixBuildAccelerationStructureModeKHR
                          showTableBuildAccelerationStructureModeKHR
                          conNameBuildAccelerationStructureModeKHR
                          BuildAccelerationStructureModeKHR


-- | VkAccelerationStructureTypeKHR - Type of acceleration structure
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureCreateInfoKHR'
newtype AccelerationStructureTypeKHR = AccelerationStructureTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' is a top-level acceleration
-- structure containing instance data referring to bottom-level
-- acceleration structures.
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR    = AccelerationStructureTypeKHR 0
-- | 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' is a bottom-level
-- acceleration structure containing the AABBs or geometry to be
-- intersected.
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR = AccelerationStructureTypeKHR 1
-- | 'ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR' is an acceleration structure
-- whose type is determined at build time used for special circumstances.
pattern ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR      = AccelerationStructureTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
             ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR,
             ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR :: AccelerationStructureTypeKHR #-}

conNameAccelerationStructureTypeKHR :: String
conNameAccelerationStructureTypeKHR = "AccelerationStructureTypeKHR"

enumPrefixAccelerationStructureTypeKHR :: String
enumPrefixAccelerationStructureTypeKHR = "ACCELERATION_STRUCTURE_TYPE_"

showTableAccelerationStructureTypeKHR :: [(AccelerationStructureTypeKHR, String)]
showTableAccelerationStructureTypeKHR =
  [ (ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR   , "TOP_LEVEL_KHR")
  , (ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR, "BOTTOM_LEVEL_KHR")
  , (ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR     , "GENERIC_KHR")
  ]

instance Show AccelerationStructureTypeKHR where
  showsPrec = enumShowsPrec enumPrefixAccelerationStructureTypeKHR
                            showTableAccelerationStructureTypeKHR
                            conNameAccelerationStructureTypeKHR
                            (\(AccelerationStructureTypeKHR x) -> x)
                            (showsPrec 11)

instance Read AccelerationStructureTypeKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureTypeKHR
                          showTableAccelerationStructureTypeKHR
                          conNameAccelerationStructureTypeKHR
                          AccelerationStructureTypeKHR


-- | VkGeometryTypeKHR - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- 'AccelerationStructureGeometryKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryNV'
newtype GeometryTypeKHR = GeometryTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'GEOMETRY_TYPE_TRIANGLES_KHR' specifies a geometry type consisting of
-- triangles.
pattern GEOMETRY_TYPE_TRIANGLES_KHR = GeometryTypeKHR 0
-- | 'GEOMETRY_TYPE_AABBS_KHR' specifies a geometry type consisting of
-- axis-aligned bounding boxes.
pattern GEOMETRY_TYPE_AABBS_KHR     = GeometryTypeKHR 1
-- | 'GEOMETRY_TYPE_INSTANCES_KHR' specifies a geometry type consisting of
-- acceleration structure instances.
pattern GEOMETRY_TYPE_INSTANCES_KHR = GeometryTypeKHR 2
{-# complete GEOMETRY_TYPE_TRIANGLES_KHR,
             GEOMETRY_TYPE_AABBS_KHR,
             GEOMETRY_TYPE_INSTANCES_KHR :: GeometryTypeKHR #-}

conNameGeometryTypeKHR :: String
conNameGeometryTypeKHR = "GeometryTypeKHR"

enumPrefixGeometryTypeKHR :: String
enumPrefixGeometryTypeKHR = "GEOMETRY_TYPE_"

showTableGeometryTypeKHR :: [(GeometryTypeKHR, String)]
showTableGeometryTypeKHR =
  [ (GEOMETRY_TYPE_TRIANGLES_KHR, "TRIANGLES_KHR")
  , (GEOMETRY_TYPE_AABBS_KHR    , "AABBS_KHR")
  , (GEOMETRY_TYPE_INSTANCES_KHR, "INSTANCES_KHR")
  ]

instance Show GeometryTypeKHR where
  showsPrec = enumShowsPrec enumPrefixGeometryTypeKHR
                            showTableGeometryTypeKHR
                            conNameGeometryTypeKHR
                            (\(GeometryTypeKHR x) -> x)
                            (showsPrec 11)

instance Read GeometryTypeKHR where
  readPrec = enumReadPrec enumPrefixGeometryTypeKHR showTableGeometryTypeKHR conNameGeometryTypeKHR GeometryTypeKHR


-- | VkAccelerationStructureBuildTypeKHR - Acceleration structure build type
--
-- = See Also
--
-- 'getAccelerationStructureBuildSizesKHR'
newtype AccelerationStructureBuildTypeKHR = AccelerationStructureBuildTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR' requests the memory
-- requirement for operations performed by the host.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR           = AccelerationStructureBuildTypeKHR 0
-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR' requests the memory
-- requirement for operations performed by the device.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR         = AccelerationStructureBuildTypeKHR 1
-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR' requests the
-- memory requirement for operations performed by either the host, or the
-- device.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR = AccelerationStructureBuildTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR :: AccelerationStructureBuildTypeKHR #-}

conNameAccelerationStructureBuildTypeKHR :: String
conNameAccelerationStructureBuildTypeKHR = "AccelerationStructureBuildTypeKHR"

enumPrefixAccelerationStructureBuildTypeKHR :: String
enumPrefixAccelerationStructureBuildTypeKHR = "ACCELERATION_STRUCTURE_BUILD_TYPE_"

showTableAccelerationStructureBuildTypeKHR :: [(AccelerationStructureBuildTypeKHR, String)]
showTableAccelerationStructureBuildTypeKHR =
  [ (ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR          , "HOST_KHR")
  , (ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR        , "DEVICE_KHR")
  , (ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR, "HOST_OR_DEVICE_KHR")
  ]

instance Show AccelerationStructureBuildTypeKHR where
  showsPrec = enumShowsPrec enumPrefixAccelerationStructureBuildTypeKHR
                            showTableAccelerationStructureBuildTypeKHR
                            conNameAccelerationStructureBuildTypeKHR
                            (\(AccelerationStructureBuildTypeKHR x) -> x)
                            (showsPrec 11)

instance Read AccelerationStructureBuildTypeKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureBuildTypeKHR
                          showTableAccelerationStructureBuildTypeKHR
                          conNameAccelerationStructureBuildTypeKHR
                          AccelerationStructureBuildTypeKHR


-- | VkAccelerationStructureCompatibilityKHR - Acceleration structure
-- compatibility
--
-- = See Also
--
-- 'getDeviceAccelerationStructureCompatibilityKHR'
newtype AccelerationStructureCompatibilityKHR = AccelerationStructureCompatibilityKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR' when the
-- @pVersion@ version acceleration structure is compatibile with @device@.
pattern ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR   = AccelerationStructureCompatibilityKHR 0
-- | 'ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR' when the
-- @pVersion@ version acceleration structure is not compatibile with
-- @device@.
pattern ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR = AccelerationStructureCompatibilityKHR 1
{-# complete ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR,
             ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR :: AccelerationStructureCompatibilityKHR #-}

conNameAccelerationStructureCompatibilityKHR :: String
conNameAccelerationStructureCompatibilityKHR = "AccelerationStructureCompatibilityKHR"

enumPrefixAccelerationStructureCompatibilityKHR :: String
enumPrefixAccelerationStructureCompatibilityKHR = "ACCELERATION_STRUCTURE_COMPATIBILITY_"

showTableAccelerationStructureCompatibilityKHR :: [(AccelerationStructureCompatibilityKHR, String)]
showTableAccelerationStructureCompatibilityKHR =
  [ (ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR  , "COMPATIBLE_KHR")
  , (ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR, "INCOMPATIBLE_KHR")
  ]

instance Show AccelerationStructureCompatibilityKHR where
  showsPrec = enumShowsPrec enumPrefixAccelerationStructureCompatibilityKHR
                            showTableAccelerationStructureCompatibilityKHR
                            conNameAccelerationStructureCompatibilityKHR
                            (\(AccelerationStructureCompatibilityKHR x) -> x)
                            (showsPrec 11)

instance Read AccelerationStructureCompatibilityKHR where
  readPrec = enumReadPrec enumPrefixAccelerationStructureCompatibilityKHR
                          showTableAccelerationStructureCompatibilityKHR
                          conNameAccelerationStructureCompatibilityKHR
                          AccelerationStructureCompatibilityKHR


type KHR_ACCELERATION_STRUCTURE_SPEC_VERSION = 11

-- No documentation found for TopLevel "VK_KHR_ACCELERATION_STRUCTURE_SPEC_VERSION"
pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ACCELERATION_STRUCTURE_SPEC_VERSION = 11


type KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME = "VK_KHR_acceleration_structure"

-- No documentation found for TopLevel "VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME"
pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME = "VK_KHR_acceleration_structure"

