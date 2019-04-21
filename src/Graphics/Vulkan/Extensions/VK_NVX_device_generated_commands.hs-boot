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
-- No cross-references are available
type IndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX

-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
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
-- No cross-references are available
type IndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX

-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- No cross-references are available
type ObjectEntryTypeNVX = VkObjectEntryTypeNVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
type ObjectEntryUsageFlagsNVX = ObjectEntryUsageFlagBitsNVX

-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- No cross-references are available
type ObjectTableNVX = VkObjectTableNVX
