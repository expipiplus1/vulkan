{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( IndirectCommandsLayoutNVX
  , IndirectCommandsLayoutUsageFlagBitsNVX
  , IndirectCommandsLayoutUsageFlagsNVX
  , IndirectCommandsTokenTypeNVX
  , ObjectEntryTypeNVX
  , ObjectEntryUsageFlagBitsNVX
  , ObjectEntryUsageFlagsNVX
  , ObjectTableNVX
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkIndirectCommandsLayoutUsageFlagBitsNVX
  , VkIndirectCommandsTokenTypeNVX
  , VkObjectEntryTypeNVX
  , VkObjectEntryUsageFlagBitsNVX
  , VkIndirectCommandsLayoutNVX
  , VkObjectTableNVX
  )


-- | VkIndirectCommandsLayoutNVX - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCreateIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkDestroyIndirectCommandsLayoutNVX'
type IndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX

-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagsNVX'
type IndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX

-- | VkIndirectCommandsLayoutUsageFlagsNVX - Bitmask of
-- VkIndirectCommandsLayoutUsageFlagBitsNVX
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagsNVX'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutCreateInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutUsageFlagBitsNVX'
type IndirectCommandsLayoutUsageFlagsNVX = IndirectCommandsLayoutUsageFlagBitsNVX

-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- > +-----------------------------------------------+----------------------+
-- > | Token type                                    | Equivalent command   |
-- > +===============================================+======================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_PIPELINE_NVX'                            | uilding.vkCmdBindPip |
-- > |                                               | eline'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DESCRIPTOR_SET_NVX'                      | uilding.vkCmdBindDes |
-- > |                                               | criptorSets'         |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_INDEX_BUFFER_NVX'                        | uilding.vkCmdBindInd |
-- > |                                               | exBuffer'            |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_VERTEX_BUFFER_NVX'                       | uilding.vkCmdBindVer |
-- > |                                               | texBuffers'          |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_PUSH_CONSTANT_NVX'                       | uilding.vkCmdPushCon |
-- > |                                               | stants'              |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DRAW_INDEXED_NVX'                        | uilding.vkCmdDrawInd |
-- > |                                               | exedIndirect'        |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DRAW_NVX'                                | uilding.vkCmdDrawInd |
-- > |                                               | irect'               |
-- > +-----------------------------------------------+----------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_device_g | 'Graphics.Vulkan.C.C |
-- > | enerated_commands.VK_INDIRECT_COMMANDS_TOKEN_ | ore10.CommandBufferB |
-- > | TYPE_DISPATCH_NVX'                            | uilding.vkCmdDispatc |
-- > |                                               | hIndirect'           |
-- > +-----------------------------------------------+----------------------+
-- >
-- > Supported indirect command tokens
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutTokenNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX'
type IndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX

-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableCreateInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkUnregisterObjectsNVX'
type ObjectEntryTypeNVX = VkObjectEntryTypeNVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagsNVX'
type ObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX

-- | VkObjectEntryUsageFlagsNVX - Bitmask of VkObjectEntryUsageFlagBitsNVX
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagsNVX'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagBitsNVX'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectEntryUsageFlagBitsNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableCreateInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX'
type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX

-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCreateObjectTableNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkDestroyObjectTableNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkRegisterObjectsNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkUnregisterObjectsNVX'
type ObjectTableNVX = VkObjectTableNVX
