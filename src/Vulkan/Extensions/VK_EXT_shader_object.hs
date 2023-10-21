{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_object - device extension
--
-- == VK_EXT_shader_object
--
-- [__Name String__]
--     @VK_EXT_shader_object@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     483
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
--
-- [__Contact__]
--
--     -   Daniel Story
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_object] @daniel-story%0A*Here describe the issue or question you have about the VK_EXT_shader_object extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_object.adoc VK_EXT_shader_object>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-30
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_EXT_extended_dynamic_state@
--
--     -   Interacts with @VK_EXT_extended_dynamic_state2@
--
--     -   Interacts with @VK_EXT_extended_dynamic_state3@
--
--     -   Interacts with @VK_EXT_vertex_input_dynamic_state@
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Sandy Jamieson, Nintendo
--
--     -   Žiga Markuš, LunarG
--
--     -   Tobias Hector, AMD
--
--     -   Alex Walters, Imagination
--
--     -   Shahbaz Youssefi, Google
--
--     -   Ralph Potter, Samsung
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Connor Abott, Valve
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   Patrick Doane, Activision
--
--     -   Jeff Leger, Qualcomm
--
--     -   Stu Smith, AMD
--
--     -   Chris Glover, Google
--
--     -   Ricardo Garcia, Igalia
--
--     -   Faith Ekstrand, Collabora
--
--     -   Timur Kristóf, Valve
--
--     -   Constantine Shablya, Collabora
--
--     -   Daniel Koch, NVIDIA
--
--     -   Alyssa Rosenzweig, Collabora
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Samuel Pitoiset, Valve
--
--     -   Qun Lin, AMD
--
--     -   Spencer Fricke, LunarG
--
--     -   Soroush Faghihi Kashani, Imagination
--
-- == Description
--
-- This extension introduces a new 'Vulkan.Extensions.Handles.ShaderEXT'
-- object type which represents a single compiled shader stage. Shader
-- objects provide a more flexible alternative to
-- 'Vulkan.Core10.Handles.Pipeline' objects, which may be helpful in
-- certain use cases.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ShaderEXT'
--
-- == New Commands
--
-- -   'cmdBindShadersEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetCullModeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthCompareOpEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthTestEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetExtraPrimitiveOverestimationSizeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetFrontFaceEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetProvokingVertexModeEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilTestEnableEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   'createShadersEXT'
--
-- -   'destroyShaderEXT'
--
-- -   'getShaderBinaryDataEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageReductionModeNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationModeNV'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRepresentativeFragmentTestEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_swizzle VK_NV_viewport_swizzle>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--
-- == New Structures
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendAdvancedEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendEquationEXT'
--
-- -   'ShaderCreateInfoEXT'
--
-- -   'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'
--
-- -   'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputBindingDescription2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderObjectFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderObjectPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
--     'ShaderCreateInfoEXT':
--
--     -   'ShaderRequiredSubgroupSizeCreateInfoEXT'
--
-- == New Enums
--
-- -   'ShaderCodeTypeEXT'
--
-- -   'ShaderCreateFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ShaderCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_OBJECT_EXTENSION_NAME'
--
-- -   'EXT_SHADER_OBJECT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_SHADER_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_SHADER_BINARY_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_SHADER_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'ShaderCreateFlagBitsEXT':
--
--     -   'SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader VK_NV_mesh_shader>
-- is supported:
--
-- -   Extending 'ShaderCreateFlagBitsEXT':
--
--     -   'SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subgroup_size_control VK_EXT_subgroup_size_control>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
-- is supported:
--
-- -   Extending 'ShaderCreateFlagBitsEXT':
--
--     -   'SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--
--     -   'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   Extending 'ShaderCreateFlagBitsEXT':
--
--     -   'SHADER_CREATE_DISPATCH_BASE_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending 'ShaderCreateFlagBitsEXT':
--
--     -   'SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT'
--
-- == Examples
--
-- __Example 1__
--
-- Create linked pair of vertex and fragment shaders.
--
-- > // Logical device created with the shaderObject feature enabled
-- > VkDevice device;
-- >
-- > // SPIR-V shader code for a vertex shader, along with its size in bytes
-- > void* pVertexSpirv;
-- > size_t vertexSpirvSize;
-- >
-- > // SPIR-V shader code for a fragment shader, along with its size in bytes
-- > void* pFragmentSpirv;
-- > size_t fragmentSpirvSize;
-- >
-- > // Descriptor set layout compatible with the shaders
-- > VkDescriptorSetLayout descriptorSetLayout;
-- >
-- > VkShaderCreateInfoEXT shaderCreateInfos[2] =
-- > {
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = VK_SHADER_CREATE_LINK_STAGE_BIT_EXT,
-- >         .stage = VK_SHADER_STAGE_VERTEX_BIT,
-- >         .nextStage = VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = vertexSpirvSize,
-- >         .pCode = pVertexSpirv,
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     },
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = VK_SHADER_CREATE_LINK_STAGE_BIT_EXT,
-- >         .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .nextStage = 0,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = fragmentSpirvSize,
-- >         .pCode = pFragmentSpirv,
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     }
-- > };
-- >
-- > VkResult result;
-- > VkShaderEXT shaders[2];
-- >
-- > result = vkCreateShadersEXT(device, 2, &shaderCreateInfos, NULL, shaders);
-- > if (result != VK_SUCCESS)
-- > {
-- >     // Handle error
-- > }
--
-- Later, during command buffer recording, bind the linked shaders and
-- draw.
--
-- > // Command buffer in the recording state
-- > VkCommandBuffer commandBuffer;
-- >
-- > // Vertex and fragment shader objects created above
-- > VkShaderEXT shaders[2];
-- >
-- > // Assume vertex buffers, descriptor sets, etc. have been bound, and existing
-- > // state setting commands have been called to set all required state
-- >
-- > const VkShaderStageFlagBits stages[2] =
-- > {
-- >     VK_SHADER_STAGE_VERTEX_BIT,
-- >     VK_SHADER_STAGE_FRAGMENT_BIT
-- > };
-- >
-- > // Bind linked shaders
-- > vkCmdBindShadersEXT(commandBuffer, 2, stages, shaders);
-- >
-- > // Equivalent to the previous line. Linked shaders can be bound one at a time,
-- > // in any order:
-- > // vkCmdBindShadersEXT(commandBuffer, 1, &stages[1], &shaders[1]);
-- > // vkCmdBindShadersEXT(commandBuffer, 1, &stages[0], &shaders[0]);
-- >
-- > // The above is sufficient to draw if the device was created with the
-- > // tessellationShader and geometryShader features disabled. Otherwise, since
-- > // those stages should not execute, vkCmdBindShadersEXT() must be called at
-- > // least once with each of their stages in pStages before drawing:
-- >
-- > const VkShaderStageFlagBits unusedStages[3] =
-- > {
-- >     VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,
-- >     VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,
-- >     VK_SHADER_STAGE_GEOMETRY_BIT
-- > };
-- >
-- > // NULL pShaders is equivalent to an array of stageCount VK_NULL_HANDLE values,
-- > // meaning no shaders are bound to those stages, and that any previously bound
-- > // shaders are unbound
-- > vkCmdBindShadersEXT(commandBuffer, 3, unusedStages, NULL);
-- >
-- > // Graphics shader objects may only be used to draw inside dynamic render pass
-- > // instances begun with vkCmdBeginRendering(), assume one has already been begun
-- >
-- > // Draw a triangle
-- > vkCmdDraw(commandBuffer, 3, 1, 0, 0);
--
-- __Example 2__
--
-- Create unlinked vertex, geometry, and fragment shaders.
--
-- > // Logical device created with the shaderObject feature enabled
-- > VkDevice device;
-- >
-- > // SPIR-V shader code for vertex shaders, along with their sizes in bytes
-- > void* pVertexSpirv[2];
-- > size_t vertexSpirvSize[2];
-- >
-- > // SPIR-V shader code for a geometry shader, along with its size in bytes
-- > void pGeometrySpirv;
-- > size_t geometrySpirvSize;
-- >
-- > // SPIR-V shader code for fragment shaders, along with their sizes in bytes
-- > void* pFragmentSpirv[2];
-- > size_t fragmentSpirvSize[2];
-- >
-- > // Descriptor set layout compatible with the shaders
-- > VkDescriptorSetLayout descriptorSetLayout;
-- >
-- > VkShaderCreateInfoEXT shaderCreateInfos[5] =
-- > {
-- >     // Stage order does not matter
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .stage = VK_SHADER_STAGE_GEOMETRY_BIT,
-- >         .nextStage = VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = pGeometrySpirv,
-- >         .pCode = geometrySpirvSize,
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     },
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .stage = VK_SHADER_STAGE_VERTEX_BIT,
-- >         .nextStage = VK_SHADER_STAGE_GEOMETRY_BIT,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = vertexSpirvSize[0],
-- >         .pCode = pVertexSpirv[0],
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     },
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .nextStage = 0,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = fragmentSpirvSize[0],
-- >         .pCode = pFragmentSpirv[0],
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     },
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .nextStage = 0,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = fragmentSpirvSize[1],
-- >         .pCode = pFragmentSpirv[1],
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     },
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .stage = VK_SHADER_STAGE_VERTEX_BIT,
-- >         // Suppose we want this vertex shader to be able to be followed by
-- >         // either a geometry shader or fragment shader:
-- >         .nextStage = VK_SHADER_STAGE_GEOMETRY_BIT | VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
-- >         .codeSize = vertexSpirvSize[1],
-- >         .pCode = pVertexSpirv[1],
-- >         .pName = "main",
-- >         .setLayoutCount = 1,
-- >         .pSetLayouts = &descriptorSetLayout;
-- >         .pushConstantRangeCount = 0,
-- >         .pPushConstantRanges = NULL,
-- >         .pSpecializationInfo = NULL
-- >     }
-- > };
-- >
-- > VkResult result;
-- > VkShaderEXT shaders[5];
-- >
-- > result = vkCreateShadersEXT(device, 5, &shaderCreateInfos, NULL, shaders);
-- > if (result != VK_SUCCESS)
-- > {
-- >     // Handle error
-- > }
--
-- Later, during command buffer recording, bind the linked shaders in
-- different combinations and draw.
--
-- > // Command buffer in the recording state
-- > VkCommandBuffer commandBuffer;
-- >
-- > // Vertex, geometry, and fragment shader objects created above
-- > VkShaderEXT shaders[5];
-- >
-- > // Assume vertex buffers, descriptor sets, etc. have been bound, and existing
-- > // state setting commands have been called to set all required state
-- >
-- > const VkShaderStageFlagBits stages[3] =
-- > {
-- >     // Any order is allowed
-- >     VK_SHADER_STAGE_FRAGMENT_BIT,
-- >     VK_SHADER_STAGE_VERTEX_BIT,
-- >     VK_SHADER_STAGE_GEOMETRY_BIT,
-- > };
-- >
-- > VkShaderEXT bindShaders[3] =
-- > {
-- >     shaders[2], // FS
-- >     shaders[1], // VS
-- >     shaders[0]  // GS
-- > };
-- >
-- > // Bind unlinked shaders
-- > vkCmdBindShadersEXT(commandBuffer, 3, stages, bindShaders);
-- >
-- > // Assume the tessellationShader feature is disabled, so vkCmdBindShadersEXT()
-- > // need not have been called with either tessellation stage
-- >
-- > // Graphics shader objects may only be used to draw inside dynamic render pass
-- > // instances begun with vkCmdBeginRendering(), assume one has already been begun
-- >
-- > // Draw a triangle
-- > vkCmdDraw(commandBuffer, 3, 1, 0, 0);
-- >
-- > // Bind a different unlinked fragment shader
-- > const VkShaderStageFlagBits fragmentStage = VK_SHADER_STAGE_FRAGMENT_BIT;
-- > vkCmdBindShadersEXT(commandBuffer, 1, &fragmentStage, &shaders[3]);
-- >
-- > // Draw another triangle
-- > vkCmdDraw(commandBuffer, 3, 1, 0, 0);
-- >
-- > // Bind a different unlinked vertex shader
-- > const VkShaderStageFlagBits vertexStage = VK_SHADER_STAGE_VERTEX_BIT;
-- > vkCmdBindShadersEXT(commandBuffer, 1, &vertexStage, &shaders[4]);
-- >
-- > // Draw another triangle
-- > vkCmdDraw(commandBuffer, 3, 1, 0, 0);
--
-- == Version History
--
-- -   Revision 1, 2023-03-30 (Daniel Story)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendAdvancedEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendEquationEXT',
-- 'PhysicalDeviceShaderObjectFeaturesEXT',
-- 'PhysicalDeviceShaderObjectPropertiesEXT', 'ShaderCodeTypeEXT',
-- 'ShaderCreateFlagBitsEXT', 'ShaderCreateFlagsEXT',
-- 'ShaderCreateInfoEXT', 'Vulkan.Extensions.Handles.ShaderEXT',
-- 'ShaderRequiredSubgroupSizeCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputBindingDescription2EXT',
-- 'cmdBindShadersEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetCullModeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthCompareOpEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetExtraPrimitiveOverestimationSizeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetFrontFaceEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetProvokingVertexModeEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT',
-- 'createShadersEXT', 'destroyShaderEXT', 'getShaderBinaryDataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_object Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_object  ( createShadersEXT
                                               , withShadersEXT
                                               , destroyShaderEXT
                                               , getShaderBinaryDataEXT
                                               , cmdBindShadersEXT
                                               , pattern STRUCTURE_TYPE_SHADER_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT
                                               , PhysicalDeviceShaderObjectFeaturesEXT(..)
                                               , PhysicalDeviceShaderObjectPropertiesEXT(..)
                                               , ShaderCreateInfoEXT(..)
                                               , ShaderCreateFlagsEXT
                                               , ShaderCreateFlagBitsEXT( SHADER_CREATE_LINK_STAGE_BIT_EXT
                                                                        , SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
                                                                        , SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT
                                                                        , SHADER_CREATE_DISPATCH_BASE_BIT_EXT
                                                                        , SHADER_CREATE_NO_TASK_SHADER_BIT_EXT
                                                                        , SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT
                                                                        , SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT
                                                                        , ..
                                                                        )
                                               , ShaderCodeTypeEXT( SHADER_CODE_TYPE_BINARY_EXT
                                                                  , SHADER_CODE_TYPE_SPIRV_EXT
                                                                  , ..
                                                                  )
                                               , ShaderRequiredSubgroupSizeCreateInfoEXT
                                               , EXT_SHADER_OBJECT_SPEC_VERSION
                                               , pattern EXT_SHADER_OBJECT_SPEC_VERSION
                                               , EXT_SHADER_OBJECT_EXTENSION_NAME
                                               , pattern EXT_SHADER_OBJECT_EXTENSION_NAME
                                               , ShaderEXT(..)
                                               , ViewportSwizzleNV(..)
                                               , ColorBlendEquationEXT(..)
                                               , ColorBlendAdvancedEXT(..)
                                               , VertexInputBindingDescription2EXT(..)
                                               , VertexInputAttributeDescription2EXT(..)
                                               , cmdSetPatchControlPointsEXT
                                               , cmdSetLogicOpEXT
                                               , cmdSetTessellationDomainOriginEXT
                                               , cmdSetDepthClampEnableEXT
                                               , cmdSetPolygonModeEXT
                                               , cmdSetRasterizationSamplesEXT
                                               , cmdSetSampleMaskEXT
                                               , cmdSetAlphaToCoverageEnableEXT
                                               , cmdSetAlphaToOneEnableEXT
                                               , cmdSetLogicOpEnableEXT
                                               , cmdSetColorBlendEnableEXT
                                               , cmdSetColorBlendEquationEXT
                                               , cmdSetColorWriteMaskEXT
                                               , cmdSetRasterizationStreamEXT
                                               , cmdSetConservativeRasterizationModeEXT
                                               , cmdSetExtraPrimitiveOverestimationSizeEXT
                                               , cmdSetDepthClipEnableEXT
                                               , cmdSetSampleLocationsEnableEXT
                                               , cmdSetColorBlendAdvancedEXT
                                               , cmdSetProvokingVertexModeEXT
                                               , cmdSetLineRasterizationModeEXT
                                               , cmdSetLineStippleEnableEXT
                                               , cmdSetDepthClipNegativeOneToOneEXT
                                               , cmdSetViewportWScalingEnableNV
                                               , cmdSetViewportSwizzleNV
                                               , cmdSetCoverageToColorEnableNV
                                               , cmdSetCoverageToColorLocationNV
                                               , cmdSetCoverageModulationModeNV
                                               , cmdSetCoverageModulationTableEnableNV
                                               , cmdSetCoverageModulationTableNV
                                               , cmdSetShadingRateImageEnableNV
                                               , cmdSetCoverageReductionModeNV
                                               , cmdSetRepresentativeFragmentTestEnableNV
                                               , cmdSetVertexInputEXT
                                               , ViewportCoordinateSwizzleNV(..)
                                               , BlendOverlapEXT(..)
                                               , CoverageModulationModeNV(..)
                                               , CoverageReductionModeNV(..)
                                               , ConservativeRasterizationModeEXT(..)
                                               , LineRasterizationModeEXT(..)
                                               , ProvokingVertexModeEXT(..)
                                               , cmdSetCullModeEXT
                                               , cmdSetFrontFaceEXT
                                               , cmdSetPrimitiveTopologyEXT
                                               , cmdSetViewportWithCountEXT
                                               , cmdSetScissorWithCountEXT
                                               , cmdBindVertexBuffers2EXT
                                               , cmdSetDepthTestEnableEXT
                                               , cmdSetDepthWriteEnableEXT
                                               , cmdSetDepthCompareOpEXT
                                               , cmdSetDepthBoundsTestEnableEXT
                                               , cmdSetStencilTestEnableEXT
                                               , cmdSetStencilOpEXT
                                               , cmdSetRasterizerDiscardEnableEXT
                                               , cmdSetDepthBiasEnableEXT
                                               , cmdSetPrimitiveRestartEnableEXT
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
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (packCStringLen)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindShadersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCreateShadersEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyShaderEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetShaderBinaryDataEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.PipelineLayout (PushConstantRange)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (ShaderEXT)
import Vulkan.Extensions.Handles (ShaderEXT(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Pipeline (SpecializationInfo)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdBindVertexBuffers2EXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetAlphaToCoverageEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetAlphaToOneEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetColorBlendAdvancedEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetColorBlendEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetColorBlendEquationEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetColorWriteMaskEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetConservativeRasterizationModeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageModulationModeNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageModulationTableEnableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageModulationTableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageReductionModeNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageToColorEnableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetCoverageToColorLocationNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetCullModeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (cmdSetDepthBiasEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetDepthBoundsTestEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetDepthClampEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetDepthClipEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetDepthClipNegativeOneToOneEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetDepthCompareOpEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetDepthTestEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetDepthWriteEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetExtraPrimitiveOverestimationSizeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetFrontFaceEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetLineRasterizationModeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetLineStippleEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (cmdSetLogicOpEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetLogicOpEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (cmdSetPatchControlPointsEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetPolygonModeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (cmdSetPrimitiveRestartEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetPrimitiveTopologyEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetProvokingVertexModeEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetRasterizationSamplesEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetRasterizationStreamEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (cmdSetRasterizerDiscardEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetRepresentativeFragmentTestEnableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetSampleLocationsEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetSampleMaskEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetScissorWithCountEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetShadingRateImageEnableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetStencilOpEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetStencilTestEnableEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetTessellationDomainOriginEXT)
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (cmdSetVertexInputEXT)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetViewportSwizzleNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (cmdSetViewportWScalingEnableNV)
import Vulkan.Extensions.VK_EXT_extended_dynamic_state (cmdSetViewportWithCountEXT)
import Vulkan.Extensions.VK_EXT_blend_operation_advanced (BlendOverlapEXT(..))
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendAdvancedEXT(..))
import Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendEquationEXT(..))
import Vulkan.Extensions.VK_EXT_conservative_rasterization (ConservativeRasterizationModeEXT(..))
import Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (CoverageModulationModeNV(..))
import Vulkan.Extensions.VK_NV_coverage_reduction_mode (CoverageReductionModeNV(..))
import Vulkan.Extensions.VK_EXT_line_rasterization (LineRasterizationModeEXT(..))
import Vulkan.Extensions.VK_EXT_provoking_vertex (ProvokingVertexModeEXT(..))
import Vulkan.Extensions.Handles (ShaderEXT(..))
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputAttributeDescription2EXT(..))
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputBindingDescription2EXT(..))
import Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportCoordinateSwizzleNV(..))
import Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportSwizzleNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateShadersEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct ShaderCreateInfoEXT) -> Ptr AllocationCallbacks -> Ptr ShaderEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct ShaderCreateInfoEXT) -> Ptr AllocationCallbacks -> Ptr ShaderEXT -> IO Result

-- | vkCreateShadersEXT - Create one or more new shaders
--
-- = Description
--
-- When this function returns, whether or not it succeeds, it is guaranteed
-- that every element of @pShaders@ will have been overwritten by either
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a valid
-- 'Vulkan.Extensions.Handles.ShaderEXT' handle.
--
-- This means that whenever shader creation fails, the application /can/
-- determine which shader the returned error pertains to by locating the
-- first 'Vulkan.Core10.APIConstants.NULL_HANDLE' element in @pShaders@. It
-- also means that an application /can/ reliably clean up from a failed
-- call by iterating over the @pShaders@ array and destroying every element
-- that is not 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateShadersEXT-None-08400# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08401# If @createInfoCount@ is
--     1, there /must/ be no element of @pCreateInfos@ whose @flags@ member
--     includes 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08402# If the @flags@ member
--     of any element of @pCreateInfos@ includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT', the @flags@ member of all other
--     elements of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     /must/ also include 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08403# If the @flags@ member
--     of any element of @pCreateInfos@ includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT', the @flags@ member of all other
--     elements of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     /must/ also include 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08404# If the @flags@ member
--     of any element of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     includes 'SHADER_CREATE_LINK_STAGE_BIT_EXT', there /must/ be no
--     member of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     and whose @flags@ member includes 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08405# If there is any element
--     of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     and whose @flags@ member includes both
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT' and
--     'SHADER_CREATE_NO_TASK_SHADER_BIT_EXT', there /must/ be no element
--     of @pCreateInfos@ whose @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     and whose @flags@ member includes 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08409# For each element of
--     @pCreateInfos@ whose @flags@ member includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT', if there is any other element of
--     @pCreateInfos@ whose @stage@ is logically later than the @stage@ of
--     the former and whose @flags@ member also includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT', the @nextStage@ of the former
--     /must/ be equal to the @stage@ of the element with the logically
--     earliest @stage@ following the @stage@ of the former whose @flags@
--     member also includes 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08410# The @stage@ member of
--     each element of @pCreateInfos@ whose @flags@ member includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT' /must/ be unique
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08411# The @codeType@ member
--     of all elements of @pCreateInfos@ whose @flags@ member includes
--     'SHADER_CREATE_LINK_STAGE_BIT_EXT' /must/ be the same
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08867# If @pCreateInfos@
--     contains elements with both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     both elements\' @flags@ include 'SHADER_CREATE_LINK_STAGE_BIT_EXT',
--     both elements\' @codeType@ is 'SHADER_CODE_TYPE_SPIRV_EXT', and the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage’s @pCode@ contains an @OpExecutionMode@ instruction specifying
--     the type of subdivision, it /must/ match the subdivision type
--     specified in the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08868# If @pCreateInfos@
--     contains elements with both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     both elements\' @flags@ include 'SHADER_CREATE_LINK_STAGE_BIT_EXT',
--     both elements\' @codeType@ is 'SHADER_CODE_TYPE_SPIRV_EXT', and the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage’s @pCode@ contains an @OpExecutionMode@ instruction specifying
--     the orientation of triangles, it /must/ match the triangle
--     orientation specified in the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08869# If @pCreateInfos@
--     contains elements with both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     both elements\' @flags@ include 'SHADER_CREATE_LINK_STAGE_BIT_EXT',
--     both elements\' @codeType@ is 'SHADER_CODE_TYPE_SPIRV_EXT', and the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage’s @pCode@ contains an @OpExecutionMode@ instruction specifying
--     @PointMode@, the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage /must/ also contain an @OpExecutionMode@ instruction
--     specifying @PointMode@
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08870# If @pCreateInfos@
--     contains elements with both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     both elements\' @flags@ include 'SHADER_CREATE_LINK_STAGE_BIT_EXT',
--     both elements\' @codeType@ is 'SHADER_CODE_TYPE_SPIRV_EXT', and the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage’s @pCode@ contains an @OpExecutionMode@ instruction specifying
--     the spacing of segments on the edges of tessellated primitives, it
--     /must/ match the segment spacing specified in the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-08871# If @pCreateInfos@
--     contains elements with both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     both elements\' @flags@ include 'SHADER_CREATE_LINK_STAGE_BIT_EXT',
--     both elements\' @codeType@ is 'SHADER_CODE_TYPE_SPIRV_EXT', and the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage’s @pCode@ contains an @OpExecutionMode@ instruction specifying
--     the output patch size, it /must/ match the output patch size
--     specified in the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateShadersEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateShadersEXT-pCreateInfos-parameter# @pCreateInfos@
--     /must/ be a valid pointer to an array of @createInfoCount@ valid
--     'ShaderCreateInfoEXT' structures
--
-- -   #VUID-vkCreateShadersEXT-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateShadersEXT-pShaders-parameter# @pShaders@ /must/ be a
--     valid pointer to an array of @createInfoCount@
--     'Vulkan.Extensions.Handles.ShaderEXT' handles
--
-- -   #VUID-vkCreateShadersEXT-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_SHADER_BINARY_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'ShaderCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.ShaderEXT'
createShadersEXT :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that creates the shader objects.
                    Device
                 -> -- | @pCreateInfos@ is a pointer to an array of 'ShaderCreateInfoEXT'
                    -- structures.
                    ("createInfos" ::: Vector (SomeStruct ShaderCreateInfoEXT))
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io (("shaders" ::: Vector ShaderEXT))
createShadersEXT device createInfos allocator = liftIO . evalContT $ do
  let vkCreateShadersEXTPtr = pVkCreateShadersEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateShadersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateShadersEXT is null" Nothing Nothing
  let vkCreateShadersEXT' = mkVkCreateShadersEXT vkCreateShadersEXTPtr
  pPCreateInfos <- ContT $ allocaBytes @(ShaderCreateInfoEXT _) ((Data.Vector.length (createInfos)) * 96)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (96 * (i)) :: Ptr (ShaderCreateInfoEXT _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPShaders <- ContT $ bracket (callocBytes @ShaderEXT ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateShadersEXT" (vkCreateShadersEXT'
                                                       (deviceHandle (device))
                                                       ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))
                                                       (forgetExtensions (pPCreateInfos))
                                                       pAllocator
                                                       (pPShaders))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pShaders <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @ShaderEXT ((pPShaders `advancePtrBytes` (8 * (i)) :: Ptr ShaderEXT)))
  pure $ (pShaders)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createShadersEXT' and 'destroyShaderEXT'
--
-- To ensure that 'destroyShaderEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withShadersEXT :: forall io r . MonadIO io => Device -> Vector (SomeStruct ShaderCreateInfoEXT) -> Maybe AllocationCallbacks -> (io (Vector ShaderEXT) -> (Vector ShaderEXT -> io ()) -> r) -> r
withShadersEXT device pCreateInfos pAllocator b =
  b (createShadersEXT device pCreateInfos pAllocator)
    (\(o0) -> traverse_ (\o0Elem -> destroyShaderEXT device
                                                       o0Elem
                                                       pAllocator) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyShaderEXT
  :: FunPtr (Ptr Device_T -> ShaderEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ShaderEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyShaderEXT - Destroy a shader object
--
-- = Description
--
-- Destroying a shader object used by one or more command buffers in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- causes those command buffers to move into the /invalid state/.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyShaderEXT-None-08481# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature /must/ be enabled
--
-- -   #VUID-vkDestroyShaderEXT-shader-08482# All submitted commands that
--     refer to @shader@ /must/ have completed execution
--
-- -   #VUID-vkDestroyShaderEXT-pAllocator-08483# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @shader@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyShaderEXT-pAllocator-08484# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @shader@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyShaderEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyShaderEXT-shader-parameter# @shader@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.ShaderEXT' handle
--
-- -   #VUID-vkDestroyShaderEXT-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyShaderEXT-shader-parent# @shader@ /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @shader@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.ShaderEXT'
destroyShaderEXT :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the shader object.
                    Device
                 -> -- | @shader@ is the handle of the shader object to destroy.
                    ShaderEXT
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyShaderEXT device shader allocator = liftIO . evalContT $ do
  let vkDestroyShaderEXTPtr = pVkDestroyShaderEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyShaderEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyShaderEXT is null" Nothing Nothing
  let vkDestroyShaderEXT' = mkVkDestroyShaderEXT vkDestroyShaderEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyShaderEXT" (vkDestroyShaderEXT'
                                                  (deviceHandle (device))
                                                  (shader)
                                                  pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderBinaryDataEXT
  :: FunPtr (Ptr Device_T -> ShaderEXT -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> ShaderEXT -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetShaderBinaryDataEXT - Get the binary shader code from a shader
-- object
--
-- = Description
--
-- If @pData@ is @NULL@, then the size of the binary shader code of the
-- shader object, in bytes, is returned in @pDataSize@. Otherwise,
-- @pDataSize@ /must/ point to a variable set by the user to the size of
-- the buffer, in bytes, pointed to by @pData@, and on return the variable
-- is overwritten with the amount of data actually written to @pData@. If
-- @pDataSize@ is less than the size of the binary shader code, nothing is
-- written to @pData@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- Note
--
-- The behavior of this command when @pDataSize@ is too small differs from
-- how some other getter-type commands work in Vulkan. Because shader
-- binary data is only usable in its entirety, it would never be useful for
-- the implementation to return partial data. Because of this, nothing is
-- written to @pData@ unless @pDataSize@ is large enough to fit the data it
-- its entirety.
--
-- Binary shader code retrieved using 'getShaderBinaryDataEXT' /can/ be
-- passed to a subsequent call to 'createShadersEXT' on a compatible
-- physical device by specifying 'SHADER_CODE_TYPE_BINARY_EXT' in the
-- @codeType@ member of 'ShaderCreateInfoEXT'.
--
-- The shader code returned by repeated calls to this function with the
-- same 'Vulkan.Extensions.Handles.ShaderEXT' is guaranteed to be invariant
-- for the lifetime of the 'Vulkan.Extensions.Handles.ShaderEXT' object.
--
-- == Valid Usage
--
-- -   #VUID-vkGetShaderBinaryDataEXT-None-08461# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetShaderBinaryDataEXT-None-08499# If @pData@ is not @NULL@,
--     it /must/ be aligned to @16@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetShaderBinaryDataEXT-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetShaderBinaryDataEXT-shader-parameter# @shader@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.ShaderEXT' handle
--
-- -   #VUID-vkGetShaderBinaryDataEXT-pDataSize-parameter# @pDataSize@
--     /must/ be a valid pointer to a @size_t@ value
--
-- -   #VUID-vkGetShaderBinaryDataEXT-pData-parameter# If the value
--     referenced by @pDataSize@ is not @0@, and @pData@ is not @NULL@,
--     @pData@ /must/ be a valid pointer to an array of @pDataSize@ bytes
--
-- -   #VUID-vkGetShaderBinaryDataEXT-shader-parent# @shader@ /must/ have
--     been created, allocated, or retrieved from @device@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.ShaderEXT'
getShaderBinaryDataEXT :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that shader object was created from.
                          Device
                       -> -- | @shader@ is the shader object to retrieve binary shader code from.
                          ShaderEXT
                       -> io (Result, ("data" ::: ByteString))
getShaderBinaryDataEXT device shader = liftIO . evalContT $ do
  let vkGetShaderBinaryDataEXTPtr = pVkGetShaderBinaryDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetShaderBinaryDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderBinaryDataEXT is null" Nothing Nothing
  let vkGetShaderBinaryDataEXT' = mkVkGetShaderBinaryDataEXT vkGetShaderBinaryDataEXTPtr
  let device' = deviceHandle (device)
  pPDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetShaderBinaryDataEXT" (vkGetShaderBinaryDataEXT'
                                                             device'
                                                             (shader)
                                                             (pPDataSize)
                                                             (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDataSize <- lift $ peek @CSize pPDataSize
  pPData <- ContT $ bracket (callocBytes @(()) (fromIntegral ((coerce @CSize @Word64 pDataSize)))) free
  r' <- lift $ traceAroundEvent "vkGetShaderBinaryDataEXT" (vkGetShaderBinaryDataEXT'
                                                              device'
                                                              (shader)
                                                              (pPDataSize)
                                                              (pPData))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDataSize'' <- lift $ peek @CSize pPDataSize
  pData' <- lift $ packCStringLen  ( castPtr @() @CChar pPData
                                   , (fromIntegral ((coerce @CSize @Word64 pDataSize''))) )
  pure $ ((r'), pData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadersEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr ShaderStageFlagBits -> Ptr ShaderEXT -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr ShaderStageFlagBits -> Ptr ShaderEXT -> IO ()

-- | vkCmdBindShadersEXT - Bind shader objects to a command buffer
--
-- = Description
--
-- When binding linked shaders, an application /may/ bind them in any
-- combination of one or more calls to 'cmdBindShadersEXT' (i.e., shaders
-- that were created linked together do not need to be bound in the same
-- 'cmdBindShadersEXT' call).
--
-- Any shader object bound to a particular stage /may/ be unbound by
-- setting its value in @pShaders@ to
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'. If @pShaders@ is @NULL@,
-- 'cmdBindShadersEXT' behaves as if @pShaders@ was an array of
-- @stageCount@ 'Vulkan.Core10.APIConstants.NULL_HANDLE' values (i.e., any
-- shaders bound to the stages specified in @pStages@ are unbound).
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindShadersEXT-None-08462# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-08463# Every element of @pStages@
--     /must/ be unique
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-08464# @pStages@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL_GRAPHICS'
--     or 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-08465# @pStages@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ANY_HIT_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLOSEST_HIT_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_INTERSECTION_BIT_KHR',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-08467# @pStages@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI'
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-08468# @pStages@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08469# For each element of
--     @pStages@, if @pShaders@ is not @NULL@, and the element of the
--     @pShaders@ array with the same index is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ have been
--     created with a @stage@ equal to the corresponding element of
--     @pStages@
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08470# If @pStages@ contains both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     and @pShaders@ is not @NULL@, and the same index in @pShaders@ as
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     in @pStages@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     same index in @pShaders@ as
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' in
--     @pStages@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08471# If @pStages@ contains both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     and @pShaders@ is not @NULL@, and the same index in @pShaders@ as
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     in @pStages@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     same index in @pShaders@ as
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' in
--     @pStages@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08474# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, and @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and @pShaders@ is not @NULL@, the same index or indices in
--     @pShaders@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08475# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, and @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     and @pShaders@ is not @NULL@, the same index in @pShaders@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08490# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, and @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     and @pShaders@ is not @NULL@, the same index in @pShaders@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08491# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, and @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     and @pShaders@ is not @NULL@, the same index in @pShaders@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08476# If @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08477# If @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-08478# If @pStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindShadersEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindShadersEXT-pStages-parameter# @pStages@ /must/ be a
--     valid pointer to an array of @stageCount@ valid
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-vkCmdBindShadersEXT-pShaders-parameter# If @pShaders@ is not
--     @NULL@, @pShaders@ /must/ be a valid pointer to an array of
--     @stageCount@ valid or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Extensions.Handles.ShaderEXT' handles
--
-- -   #VUID-vkCmdBindShadersEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindShadersEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindShadersEXT-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdBindShadersEXT-stageCount-arraylength# @stageCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdBindShadersEXT-commonparent# Both of @commandBuffer@, and
--     the elements of @pShaders@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.ShaderEXT',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
cmdBindShadersEXT :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer that the shader object will be
                     -- bound to.
                     CommandBuffer
                  -> -- | @pStages@ is a pointer to an array of
                     -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
                     -- specifying one stage per array index that is affected by the
                     -- corresponding value in the @pShaders@ array.
                     ("stages" ::: Vector ShaderStageFlagBits)
                  -> -- | @pShaders@ is a pointer to an array of
                     -- 'Vulkan.Extensions.Handles.ShaderEXT' handles and\/or
                     -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' values describing the shader
                     -- binding operations to be performed on each stage in @pStages@.
                     ("shaders" ::: Vector ShaderEXT)
                  -> io ()
cmdBindShadersEXT commandBuffer stages shaders = liftIO . evalContT $ do
  let vkCmdBindShadersEXTPtr = pVkCmdBindShadersEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindShadersEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindShadersEXT is null" Nothing Nothing
  let vkCmdBindShadersEXT' = mkVkCmdBindShadersEXT vkCmdBindShadersEXTPtr
  let pStagesLength = Data.Vector.length $ (stages)
  let pShadersLength = Data.Vector.length $ (shaders)
  lift $ unless (fromIntegral pShadersLength == pStagesLength || pShadersLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "pShaders and pStages must have the same length" Nothing Nothing
  pPStages <- ContT $ allocaBytes @ShaderStageFlagBits ((Data.Vector.length (stages)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPStages `plusPtr` (4 * (i)) :: Ptr ShaderStageFlagBits) (e)) (stages)
  pShaders <- if Data.Vector.null (shaders)
    then pure nullPtr
    else do
      pPShaders <- ContT $ allocaBytes @ShaderEXT (((Data.Vector.length (shaders))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pPShaders `plusPtr` (8 * (i)) :: Ptr ShaderEXT) (e)) ((shaders))
      pure $ pPShaders
  lift $ traceAroundEvent "vkCmdBindShadersEXT" (vkCmdBindShadersEXT'
                                                   (commandBufferHandle (commandBuffer))
                                                   ((fromIntegral pStagesLength :: Word32))
                                                   (pPStages)
                                                   pShaders)
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SHADER_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SHADER_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO


-- | VkPhysicalDeviceShaderObjectFeaturesEXT - Structure describing whether
-- shader objects can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderObjectFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderObjectFeaturesEXT' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderObjectFeaturesEXT = PhysicalDeviceShaderObjectFeaturesEXT
  { -- | #features-shaderObject# @shaderObject@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>.
    shaderObject :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderObjectFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderObjectFeaturesEXT

instance ToCStruct PhysicalDeviceShaderObjectFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderObjectFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderObject))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderObjectFeaturesEXT where
  peekCStruct p = do
    shaderObject <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderObjectFeaturesEXT
             (bool32ToBool shaderObject)

instance Storable PhysicalDeviceShaderObjectFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderObjectFeaturesEXT where
  zero = PhysicalDeviceShaderObjectFeaturesEXT
           zero


-- | VkPhysicalDeviceShaderObjectPropertiesEXT - Structure describing shader
-- object properties supported by an implementation
--
-- = Description
--
-- The purpose and usage of the values of this structure are described in
-- greater detail in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects-binary-compatibility Binary Shader Compatibility>.
--
-- If the 'PhysicalDeviceShaderObjectPropertiesEXT' structure is included
-- in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderObjectPropertiesEXT = PhysicalDeviceShaderObjectPropertiesEXT
  { -- | #limits-shaderBinaryUUID# @shaderBinaryUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values representing a
    -- universally unique identifier for one or more implementations whose
    -- shader binaries are guaranteed to be compatible with each other.
    shaderBinaryUUID :: ByteString
  , -- | #limits-shaderBinaryVersion# @shaderBinaryVersion@ is an unsigned
    -- integer incremented to represent backwards compatible differences
    -- between implementations with the same @shaderBinaryUUID@.
    shaderBinaryVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderObjectPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceShaderObjectPropertiesEXT

instance ToCStruct PhysicalDeviceShaderObjectPropertiesEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderObjectPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (shaderBinaryUUID)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (shaderBinaryVersion)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderObjectPropertiesEXT where
  peekCStruct p = do
    shaderBinaryUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    shaderBinaryVersion <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ PhysicalDeviceShaderObjectPropertiesEXT
             shaderBinaryUUID shaderBinaryVersion

instance Storable PhysicalDeviceShaderObjectPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderObjectPropertiesEXT where
  zero = PhysicalDeviceShaderObjectPropertiesEXT
           mempty
           zero


-- | VkShaderCreateInfoEXT - Structure specifying parameters of a newly
-- created shader
--
-- == Valid Usage
--
-- -   #VUID-VkShaderCreateInfoEXT-codeSize-08735# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @codeSize@ /must/ be a multiple of 4
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08736# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ point to valid SPIR-V
--     code, formatted and packed as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08737# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ adhere to the
--     validation rules described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-module-validation Validation Rules within a Module>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08738# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ declare the @Shader@
--     capability for SPIR-V code
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08739# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ not declare any
--     capability that is not supported by the API, as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-module-validation Capabilities>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08740# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @pCode@ declares any of the
--     capabilities listed in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table SPIR-V Environment>
--     appendix, one of the corresponding requirements /must/ be satisfied
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08741# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ not declare any SPIR-V
--     extension that is not supported by the API, as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-extensions Extension>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08742# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @pCode@ declares any of the SPIR-V
--     extensions listed in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-extensions-table SPIR-V Environment>
--     appendix, one of the corresponding requirements /must/ be satisfied
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08412# If @stage@ is not
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     @flags@ /must/ not include 'SHADER_CREATE_LINK_STAGE_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08486# If @stage@ is not
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     @flags@ /must/ not include
--     'SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08487# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not enabled, @flags@ /must/ not include
--     'SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08488# If @stage@ is not
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     @flags@ /must/ not include
--     'SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08489# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @flags@ /must/ not include
--     'SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-09404# If @flags@ includes
--     'SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT', the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-09405# If @flags@ includes
--     'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT', the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-computeFullSubgroups computeFullSubgroups>
--     feature /must/ be enabled
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08992# If @flags@ includes
--     'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT', @stage@ /must/ be
--     one of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08485# If @stage@ is not
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     @flags@ /must/ not include 'SHADER_CREATE_DISPATCH_BASE_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08414# If @stage@ is not
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     @flags@ /must/ not include 'SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08416# If @flags@ includes both
--     'SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT' and
--     'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT', the local workgroup
--     size in the X dimension of the shader /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxSubgroupSize maxSubgroupSize>
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-08417# If @flags@ includes
--     'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT' but not
--     'SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT' and no
--     'ShaderRequiredSubgroupSizeCreateInfoEXT' structure is included in
--     the @pNext@ chain, the local workgroup size in the X dimension of
--     the shader /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08418# @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL_GRAPHICS'
--     or 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08419# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08420# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08421# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08422# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08425# @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI'
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-08426# @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08427# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08428# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @nextStage@ /must/ not include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08429# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @nextStage@ /must/ not include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08430# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08431# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08433# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08434# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     @nextStage@ /must/ be 0
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08435# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-08436# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     @nextStage@ /must/ not include any bits other than
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkShaderCreateInfoEXT-pName-08440# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pName@ /must/ be the name of an
--     @OpEntryPoint@ in @pCode@ with an execution model that matches
--     @stage@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08492# If @codeType@ is
--     'SHADER_CODE_TYPE_BINARY_EXT', @pCode@ /must/ be aligned to @16@
--     bytes
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08493# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', @pCode@ /must/ be aligned to @4@ bytes
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08448# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and the identified entry point
--     includes any variable in its interface that is declared with the
--     @ClipDistance@ @BuiltIn@ decoration, that variable /must/ not have
--     an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxClipDistances@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08449# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and the identified entry point
--     includes any variable in its interface that is declared with the
--     @CullDistance@ @BuiltIn@ decoration, that variable /must/ not have
--     an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCullDistances@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08450# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and the identified entry point
--     includes any variables in its interface that are declared with the
--     @ClipDistance@ or @CullDistance@ @BuiltIn@ decoration, those
--     variables /must/ not have array sizes which sum to more than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCombinedClipAndCullDistances@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08451# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and the identified entry point
--     includes any variable in its interface that is declared with the
--     'Vulkan.Core10.FundamentalTypes.SampleMask' @BuiltIn@ decoration,
--     that variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSampleMaskWords@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08452# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     the identified entry point /must/ not include any input variable in
--     its interface that is decorated with @CullDistance@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08453# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and the identified entry point has an @OpExecutionMode@ instruction
--     specifying a patch size with @OutputVertices@, the patch size /must/
--     be greater than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08454# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction specifying a maximum output vertex count that is greater
--     than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryOutputVertices@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08455# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction specifying an invocation count that is greater than @0@
--     and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryShaderInvocations@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08456# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>,
--     and the identified entry point writes to @Layer@ for any primitive,
--     it /must/ write the same value to @Layer@ for all vertices of a
--     given primitive
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08457# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>,
--     and the identified entry point writes to @ViewportIndex@ for any
--     primitive, it /must/ write the same value to @ViewportIndex@ for all
--     vertices of a given primitive
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08458# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     the identified entry point /must/ not include any output variables
--     in its interface decorated with @CullDistance@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08459# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragDepth@ in any
--     execution path, all execution paths that are not exclusive to helper
--     invocations /must/ either discard the fragment, or write or
--     initialize the value of @FragDepth@
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-08460# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', the shader code in @pCode@ /must/ be
--     valid as described by the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--     after applying the specializations provided in
--     @pSpecializationInfo@, if any, and then converting all
--     specialization constants into fixed constants
--
-- -   #VUID-VkShaderCreateInfoEXT-codeType-08872# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     @pCode@ /must/ contain an @OpExecutionMode@ instruction specifying
--     the type of subdivision
--
-- -   #VUID-VkShaderCreateInfoEXT-codeType-08873# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     @pCode@ /must/ contain an @OpExecutionMode@ instruction specifying
--     the orientation of triangles generated by the tessellator
--
-- -   #VUID-VkShaderCreateInfoEXT-codeType-08874# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     @pCode@ /must/ contain an @OpExecutionMode@ instruction specifying
--     the spacing of segments on the edges of tessellated primitives
--
-- -   #VUID-VkShaderCreateInfoEXT-codeType-08875# If @codeType@ is
--     'SHADER_CODE_TYPE_SPIRV_EXT', and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     @pCode@ /must/ contain an @OpExecutionMode@ instruction specifying
--     the output patch size
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkShaderCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT'
--
-- -   #VUID-VkShaderCreateInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--
-- -   #VUID-VkShaderCreateInfoEXT-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkShaderCreateInfoEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of 'ShaderCreateFlagBitsEXT' values
--
-- -   #VUID-VkShaderCreateInfoEXT-stage-parameter# @stage@ /must/ be a
--     valid 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
--     value
--
-- -   #VUID-VkShaderCreateInfoEXT-nextStage-parameter# @nextStage@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkShaderCreateInfoEXT-codeType-parameter# @codeType@ /must/ be
--     a valid 'ShaderCodeTypeEXT' value
--
-- -   #VUID-VkShaderCreateInfoEXT-pCode-parameter# @pCode@ /must/ be a
--     valid pointer to an array of @codeSize@ bytes
--
-- -   #VUID-VkShaderCreateInfoEXT-pName-parameter# If @pName@ is not
--     @NULL@, @pName@ /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-VkShaderCreateInfoEXT-pSetLayouts-parameter# If
--     @setLayoutCount@ is not @0@, and @pSetLayouts@ is not @NULL@,
--     @pSetLayouts@ /must/ be a valid pointer to an array of
--     @setLayoutCount@ valid 'Vulkan.Core10.Handles.DescriptorSetLayout'
--     handles
--
-- -   #VUID-VkShaderCreateInfoEXT-pPushConstantRanges-parameter# If
--     @pushConstantRangeCount@ is not @0@, and @pPushConstantRanges@ is
--     not @NULL@, @pPushConstantRanges@ /must/ be a valid pointer to an
--     array of @pushConstantRangeCount@ valid
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' structures
--
-- -   #VUID-VkShaderCreateInfoEXT-pSpecializationInfo-parameter# If
--     @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/ be
--     a valid pointer to a valid
--     'Vulkan.Core10.Pipeline.SpecializationInfo' structure
--
-- -   #VUID-VkShaderCreateInfoEXT-codeSize-arraylength# @codeSize@ /must/
--     be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.PipelineLayout.PushConstantRange', 'ShaderCodeTypeEXT',
-- 'ShaderCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Pipeline.SpecializationInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createShadersEXT'
data ShaderCreateInfoEXT (es :: [Type]) = ShaderCreateInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'ShaderCreateFlagBitsEXT' describing additional
    -- parameters of the shader.
    flags :: ShaderCreateFlagsEXT
  , -- | @stage@ is a
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
    -- specifying a single shader stage.
    stage :: ShaderStageFlagBits
  , -- | @nextStage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
    -- zero or stages which /may/ be used as a logically next bound stage when
    -- drawing with the shader bound.
    nextStage :: ShaderStageFlags
  , -- | @codeType@ is a 'ShaderCodeTypeEXT' value specifying the type of the
    -- shader code pointed to be @pCode@.
    codeType :: ShaderCodeTypeEXT
  , -- | @codeSize@ is the size in bytes of the shader code pointed to be
    -- @pCode@.
    codeSize :: Word64
  , -- | @pCode@ is a pointer to the shader code to use to create the shader.
    code :: Ptr ()
  , -- | @pName@ is a pointer to a null-terminated UTF-8 string specifying the
    -- entry point name of the shader for this stage.
    name :: Maybe ByteString
  , -- | @setLayoutCount@ is the number of descriptor set layouts pointed to by
    -- @pSetLayouts@.
    setLayoutCount :: Word32
  , -- | @pSetLayouts@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.DescriptorSetLayout' objects used by the shader
    -- stage.
    setLayouts :: Vector DescriptorSetLayout
  , -- | @pushConstantRangeCount@ is the number of push constant ranges pointed
    -- to by @pPushConstantRanges@.
    pushConstantRangeCount :: Word32
  , -- | @pPushConstantRanges@ is a pointer to an array of
    -- 'Vulkan.Core10.PipelineLayout.PushConstantRange' structures used by the
    -- shader stage.
    pushConstantRanges :: Vector PushConstantRange
  , -- | @pSpecializationInfo@ is a pointer to a
    -- 'Vulkan.Core10.Pipeline.SpecializationInfo' structure, as described in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-specialization-constants Specialization Constants>,
    -- or @NULL@.
    specializationInfo :: Maybe SpecializationInfo
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderCreateInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ShaderCreateInfoEXT es)

instance Extensible ShaderCreateInfoEXT where
  extensibleTypeName = "ShaderCreateInfoEXT"
  setNext ShaderCreateInfoEXT{..} next' = ShaderCreateInfoEXT{next = next', ..}
  getNext ShaderCreateInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ShaderCreateInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineShaderStageRequiredSubgroupSizeCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss ShaderCreateInfoEXT es
         , PokeChain es ) => ToCStruct (ShaderCreateInfoEXT es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderCreateFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits)) (stage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderStageFlags)) (nextStage)
    lift $ poke ((p `plusPtr` 28 :: Ptr ShaderCodeTypeEXT)) (codeType)
    lift $ poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (codeSize))
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (code)
    pName'' <- case (name) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr CChar))) pName''
    let pSetLayoutsLength = Data.Vector.length $ (setLayouts)
    setLayoutCount'' <- lift $ if (setLayoutCount) == 0
      then pure $ fromIntegral pSetLayoutsLength
      else do
        unless (fromIntegral pSetLayoutsLength == (setLayoutCount) || pSetLayoutsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pSetLayouts must be empty or have 'setLayoutCount' elements" Nothing Nothing
        pure (setLayoutCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (setLayoutCount'')
    pSetLayouts'' <- if Data.Vector.null (setLayouts)
      then pure nullPtr
      else do
        pPSetLayouts <- ContT $ allocaBytes @DescriptorSetLayout (((Data.Vector.length (setLayouts))) * 8)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) ((setLayouts))
        pure $ pPSetLayouts
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr DescriptorSetLayout))) pSetLayouts''
    let pPushConstantRangesLength = Data.Vector.length $ (pushConstantRanges)
    pushConstantRangeCount'' <- lift $ if (pushConstantRangeCount) == 0
      then pure $ fromIntegral pPushConstantRangesLength
      else do
        unless (fromIntegral pPushConstantRangesLength == (pushConstantRangeCount) || pPushConstantRangesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pPushConstantRanges must be empty or have 'pushConstantRangeCount' elements" Nothing Nothing
        pure (pushConstantRangeCount)
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) (pushConstantRangeCount'')
    pPushConstantRanges'' <- if Data.Vector.null (pushConstantRanges)
      then pure nullPtr
      else do
        pPPushConstantRanges <- ContT $ allocaBytes @PushConstantRange (((Data.Vector.length (pushConstantRanges))) * 12)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPPushConstantRanges `plusPtr` (12 * (i)) :: Ptr PushConstantRange) (e)) ((pushConstantRanges))
        pure $ pPPushConstantRanges
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr PushConstantRange))) pPushConstantRanges''
    pSpecializationInfo'' <- case (specializationInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr SpecializationInfo))) pSpecializationInfo''
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr ShaderCodeTypeEXT)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss ShaderCreateInfoEXT es
         , PeekChain es ) => FromCStruct (ShaderCreateInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ShaderCreateFlagsEXT ((p `plusPtr` 16 :: Ptr ShaderCreateFlagsEXT))
    stage <- peek @ShaderStageFlagBits ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits))
    nextStage <- peek @ShaderStageFlags ((p `plusPtr` 24 :: Ptr ShaderStageFlags))
    codeType <- peek @ShaderCodeTypeEXT ((p `plusPtr` 28 :: Ptr ShaderCodeTypeEXT))
    codeSize <- peek @CSize ((p `plusPtr` 32 :: Ptr CSize))
    pCode <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pName <- peek @(Ptr CChar) ((p `plusPtr` 48 :: Ptr (Ptr CChar)))
    pName' <- maybePeek (\j -> packCString (j)) pName
    setLayoutCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pSetLayouts <- peek @(Ptr DescriptorSetLayout) ((p `plusPtr` 64 :: Ptr (Ptr DescriptorSetLayout)))
    let pSetLayoutsLength = if pSetLayouts == nullPtr then 0 else (fromIntegral setLayoutCount)
    pSetLayouts' <- generateM pSetLayoutsLength (\i -> peek @DescriptorSetLayout ((pSetLayouts `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSetLayout)))
    pushConstantRangeCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    pPushConstantRanges <- peek @(Ptr PushConstantRange) ((p `plusPtr` 80 :: Ptr (Ptr PushConstantRange)))
    let pPushConstantRangesLength = if pPushConstantRanges == nullPtr then 0 else (fromIntegral pushConstantRangeCount)
    pPushConstantRanges' <- generateM pPushConstantRangesLength (\i -> peekCStruct @PushConstantRange ((pPushConstantRanges `advancePtrBytes` (12 * (i)) :: Ptr PushConstantRange)))
    pSpecializationInfo <- peek @(Ptr SpecializationInfo) ((p `plusPtr` 88 :: Ptr (Ptr SpecializationInfo)))
    pSpecializationInfo' <- maybePeek (\j -> peekCStruct @SpecializationInfo (j)) pSpecializationInfo
    pure $ ShaderCreateInfoEXT
             next
             flags
             stage
             nextStage
             codeType
             (coerce @CSize @Word64 codeSize)
             pCode
             pName'
             setLayoutCount
             pSetLayouts'
             pushConstantRangeCount
             pPushConstantRanges'
             pSpecializationInfo'

instance es ~ '[] => Zero (ShaderCreateInfoEXT es) where
  zero = ShaderCreateInfoEXT
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           Nothing
           zero
           mempty
           zero
           mempty
           Nothing


type ShaderCreateFlagsEXT = ShaderCreateFlagBitsEXT

-- | VkShaderCreateFlagBitsEXT - Bitmask controlling how a shader object is
-- created
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'ShaderCreateFlagsEXT'
newtype ShaderCreateFlagBitsEXT = ShaderCreateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SHADER_CREATE_LINK_STAGE_BIT_EXT' specifies that a shader is linked to
-- all other shaders created in the same 'createShadersEXT' call whose
-- 'ShaderCreateInfoEXT' structures\' @flags@ include
-- 'SHADER_CREATE_LINK_STAGE_BIT_EXT'.
pattern SHADER_CREATE_LINK_STAGE_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000001

-- | 'SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT' specifies that a
-- fragment shader /can/ be used with a fragment density map attachment.
pattern SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000040

-- | 'SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT' specifies that
-- a fragment shader /can/ be used with a fragment shading rate attachment.
pattern SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000020

-- | 'SHADER_CREATE_DISPATCH_BASE_BIT_EXT' specifies that a compute shader
-- /can/ be used with
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase' with a
-- non-zero base workgroup.
pattern SHADER_CREATE_DISPATCH_BASE_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000010

-- | 'SHADER_CREATE_NO_TASK_SHADER_BIT_EXT' specifies that a mesh shader
-- /must/ only be used without a task shader. Otherwise, the mesh shader
-- /must/ only be used with a task shader.
pattern SHADER_CREATE_NO_TASK_SHADER_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000008

-- | 'SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT' specifies that the
-- subgroup sizes /must/ be launched with all invocations active in a task,
-- mesh, or compute shader.
pattern SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000004

-- | 'SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT' specifies that the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-sgs SubgroupSize>
-- /may/ vary in a task, mesh, or compute shader.
pattern SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT = ShaderCreateFlagBitsEXT 0x00000002

conNameShaderCreateFlagBitsEXT :: String
conNameShaderCreateFlagBitsEXT = "ShaderCreateFlagBitsEXT"

enumPrefixShaderCreateFlagBitsEXT :: String
enumPrefixShaderCreateFlagBitsEXT = "SHADER_CREATE_"

showTableShaderCreateFlagBitsEXT :: [(ShaderCreateFlagBitsEXT, String)]
showTableShaderCreateFlagBitsEXT =
  [
    ( SHADER_CREATE_LINK_STAGE_BIT_EXT
    , "LINK_STAGE_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
    , "FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_DISPATCH_BASE_BIT_EXT
    , "DISPATCH_BASE_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_NO_TASK_SHADER_BIT_EXT
    , "NO_TASK_SHADER_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT
    , "REQUIRE_FULL_SUBGROUPS_BIT_EXT"
    )
  ,
    ( SHADER_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT
    , "ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT"
    )
  ]

instance Show ShaderCreateFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixShaderCreateFlagBitsEXT
      showTableShaderCreateFlagBitsEXT
      conNameShaderCreateFlagBitsEXT
      (\(ShaderCreateFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ShaderCreateFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixShaderCreateFlagBitsEXT
      showTableShaderCreateFlagBitsEXT
      conNameShaderCreateFlagBitsEXT
      ShaderCreateFlagBitsEXT

-- | VkShaderCodeTypeEXT - Indicate a shader code type
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'ShaderCreateInfoEXT'
newtype ShaderCodeTypeEXT = ShaderCodeTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHADER_CODE_TYPE_BINARY_EXT' specifies shader code in an opaque,
-- implementation-defined binary format specific to the physical device.
pattern SHADER_CODE_TYPE_BINARY_EXT = ShaderCodeTypeEXT 0

-- | 'SHADER_CODE_TYPE_SPIRV_EXT' specifies shader code in SPIR-V format.
pattern SHADER_CODE_TYPE_SPIRV_EXT = ShaderCodeTypeEXT 1

{-# COMPLETE
  SHADER_CODE_TYPE_BINARY_EXT
  , SHADER_CODE_TYPE_SPIRV_EXT ::
    ShaderCodeTypeEXT
  #-}

conNameShaderCodeTypeEXT :: String
conNameShaderCodeTypeEXT = "ShaderCodeTypeEXT"

enumPrefixShaderCodeTypeEXT :: String
enumPrefixShaderCodeTypeEXT = "SHADER_CODE_TYPE_"

showTableShaderCodeTypeEXT :: [(ShaderCodeTypeEXT, String)]
showTableShaderCodeTypeEXT =
  [ (SHADER_CODE_TYPE_BINARY_EXT, "BINARY_EXT")
  , (SHADER_CODE_TYPE_SPIRV_EXT, "SPIRV_EXT")
  ]

instance Show ShaderCodeTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixShaderCodeTypeEXT
      showTableShaderCodeTypeEXT
      conNameShaderCodeTypeEXT
      (\(ShaderCodeTypeEXT x) -> x)
      (showsPrec 11)

instance Read ShaderCodeTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixShaderCodeTypeEXT
      showTableShaderCodeTypeEXT
      conNameShaderCodeTypeEXT
      ShaderCodeTypeEXT

-- No documentation found for TopLevel "VkShaderRequiredSubgroupSizeCreateInfoEXT"
type ShaderRequiredSubgroupSizeCreateInfoEXT = PipelineShaderStageRequiredSubgroupSizeCreateInfo


type EXT_SHADER_OBJECT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_OBJECT_SPEC_VERSION"
pattern EXT_SHADER_OBJECT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_OBJECT_SPEC_VERSION = 1


type EXT_SHADER_OBJECT_EXTENSION_NAME = "VK_EXT_shader_object"

-- No documentation found for TopLevel "VK_EXT_SHADER_OBJECT_EXTENSION_NAME"
pattern EXT_SHADER_OBJECT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_OBJECT_EXTENSION_NAME = "VK_EXT_shader_object"

