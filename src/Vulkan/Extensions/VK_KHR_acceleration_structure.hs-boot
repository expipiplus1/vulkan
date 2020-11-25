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
module Vulkan.Extensions.VK_KHR_acceleration_structure  ( AabbPositionsKHR
                                                        , AccelerationStructureBuildGeometryInfoKHR
                                                        , AccelerationStructureBuildRangeInfoKHR
                                                        , AccelerationStructureBuildSizesInfoKHR
                                                        , AccelerationStructureCreateInfoKHR
                                                        , AccelerationStructureDeviceAddressInfoKHR
                                                        , AccelerationStructureGeometryAabbsDataKHR
                                                        , AccelerationStructureGeometryInstancesDataKHR
                                                        , AccelerationStructureGeometryKHR
                                                        , AccelerationStructureGeometryTrianglesDataKHR
                                                        , AccelerationStructureInstanceKHR
                                                        , AccelerationStructureVersionInfoKHR
                                                        , CopyAccelerationStructureInfoKHR
                                                        , CopyAccelerationStructureToMemoryInfoKHR
                                                        , CopyMemoryToAccelerationStructureInfoKHR
                                                        , PhysicalDeviceAccelerationStructureFeaturesKHR
                                                        , PhysicalDeviceAccelerationStructurePropertiesKHR
                                                        , TransformMatrixKHR
                                                        , WriteDescriptorSetAccelerationStructureKHR
                                                        , AccelerationStructureBuildTypeKHR
                                                        , AccelerationStructureCompatibilityKHR
                                                        , CopyAccelerationStructureModeKHR
                                                        , GeometryFlagBitsKHR
                                                        , GeometryFlagsKHR
                                                        , GeometryInstanceFlagBitsKHR
                                                        , GeometryInstanceFlagsKHR
                                                        , BuildAccelerationStructureFlagBitsKHR
                                                        , BuildAccelerationStructureFlagsKHR
                                                        , AccelerationStructureTypeKHR
                                                        , GeometryTypeKHR
                                                        ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data AabbPositionsKHR

instance ToCStruct AabbPositionsKHR
instance Show AabbPositionsKHR

instance FromCStruct AabbPositionsKHR


data AccelerationStructureBuildGeometryInfoKHR

instance ToCStruct AccelerationStructureBuildGeometryInfoKHR
instance Show AccelerationStructureBuildGeometryInfoKHR


data AccelerationStructureBuildRangeInfoKHR

instance ToCStruct AccelerationStructureBuildRangeInfoKHR
instance Show AccelerationStructureBuildRangeInfoKHR

instance FromCStruct AccelerationStructureBuildRangeInfoKHR


data AccelerationStructureBuildSizesInfoKHR

instance ToCStruct AccelerationStructureBuildSizesInfoKHR
instance Show AccelerationStructureBuildSizesInfoKHR

instance FromCStruct AccelerationStructureBuildSizesInfoKHR


data AccelerationStructureCreateInfoKHR

instance ToCStruct AccelerationStructureCreateInfoKHR
instance Show AccelerationStructureCreateInfoKHR

instance FromCStruct AccelerationStructureCreateInfoKHR


data AccelerationStructureDeviceAddressInfoKHR

instance ToCStruct AccelerationStructureDeviceAddressInfoKHR
instance Show AccelerationStructureDeviceAddressInfoKHR

instance FromCStruct AccelerationStructureDeviceAddressInfoKHR


data AccelerationStructureGeometryAabbsDataKHR

instance ToCStruct AccelerationStructureGeometryAabbsDataKHR
instance Show AccelerationStructureGeometryAabbsDataKHR


data AccelerationStructureGeometryInstancesDataKHR

instance ToCStruct AccelerationStructureGeometryInstancesDataKHR
instance Show AccelerationStructureGeometryInstancesDataKHR


data AccelerationStructureGeometryKHR

instance ToCStruct AccelerationStructureGeometryKHR
instance Show AccelerationStructureGeometryKHR


data AccelerationStructureGeometryTrianglesDataKHR

instance ToCStruct AccelerationStructureGeometryTrianglesDataKHR
instance Show AccelerationStructureGeometryTrianglesDataKHR


data AccelerationStructureInstanceKHR

instance ToCStruct AccelerationStructureInstanceKHR
instance Show AccelerationStructureInstanceKHR

instance FromCStruct AccelerationStructureInstanceKHR


data AccelerationStructureVersionInfoKHR

instance ToCStruct AccelerationStructureVersionInfoKHR
instance Show AccelerationStructureVersionInfoKHR

instance FromCStruct AccelerationStructureVersionInfoKHR


data CopyAccelerationStructureInfoKHR

instance ToCStruct CopyAccelerationStructureInfoKHR
instance Show CopyAccelerationStructureInfoKHR

instance FromCStruct CopyAccelerationStructureInfoKHR


data CopyAccelerationStructureToMemoryInfoKHR

instance ToCStruct CopyAccelerationStructureToMemoryInfoKHR
instance Show CopyAccelerationStructureToMemoryInfoKHR


data CopyMemoryToAccelerationStructureInfoKHR

instance ToCStruct CopyMemoryToAccelerationStructureInfoKHR
instance Show CopyMemoryToAccelerationStructureInfoKHR


data PhysicalDeviceAccelerationStructureFeaturesKHR

instance ToCStruct PhysicalDeviceAccelerationStructureFeaturesKHR
instance Show PhysicalDeviceAccelerationStructureFeaturesKHR

instance FromCStruct PhysicalDeviceAccelerationStructureFeaturesKHR


data PhysicalDeviceAccelerationStructurePropertiesKHR

instance ToCStruct PhysicalDeviceAccelerationStructurePropertiesKHR
instance Show PhysicalDeviceAccelerationStructurePropertiesKHR

instance FromCStruct PhysicalDeviceAccelerationStructurePropertiesKHR


data TransformMatrixKHR

instance ToCStruct TransformMatrixKHR
instance Show TransformMatrixKHR

instance FromCStruct TransformMatrixKHR


data WriteDescriptorSetAccelerationStructureKHR

instance ToCStruct WriteDescriptorSetAccelerationStructureKHR
instance Show WriteDescriptorSetAccelerationStructureKHR

instance FromCStruct WriteDescriptorSetAccelerationStructureKHR


data AccelerationStructureBuildTypeKHR


data AccelerationStructureCompatibilityKHR


data CopyAccelerationStructureModeKHR


data GeometryFlagBitsKHR

type GeometryFlagsKHR = GeometryFlagBitsKHR


data GeometryInstanceFlagBitsKHR

type GeometryInstanceFlagsKHR = GeometryInstanceFlagBitsKHR


data BuildAccelerationStructureFlagBitsKHR

type BuildAccelerationStructureFlagsKHR = BuildAccelerationStructureFlagBitsKHR


data AccelerationStructureTypeKHR


data GeometryTypeKHR

