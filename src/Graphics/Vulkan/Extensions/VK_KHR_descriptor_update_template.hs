{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateInfoKHR
  , DescriptorUpdateTemplateEntryKHR
  , DescriptorUpdateTemplateKHR
  , createDescriptorUpdateTemplateKHR
  , destroyDescriptorUpdateTemplateKHR
  , updateDescriptorSetWithTemplateKHR
  , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
  , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
  , pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
  , DescriptorUpdateTemplateCreateFlagsKHR
  , DescriptorUpdateTemplateTypeKHR
  , cmdPushDescriptorSetWithTemplateKHR
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkObjectType(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template
  ( pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateInfo(..)
  , DescriptorUpdateTemplateEntry(..)
  , DescriptorUpdateTemplate
  , createDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateFlagsKHR
  , DescriptorUpdateTemplateTypeKHR
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( cmdPushDescriptorSetWithTemplateKHR
  )


type DescriptorUpdateTemplateCreateInfoKHR = DescriptorUpdateTemplateCreateInfo
-- TODO: Pattern constructor alias)

type DescriptorUpdateTemplateEntryKHR = DescriptorUpdateTemplateEntry
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "DescriptorUpdateTemplateKHR"
type DescriptorUpdateTemplateKHR = DescriptorUpdateTemplate

createDescriptorUpdateTemplateKHR :: Device ->  DescriptorUpdateTemplateCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorUpdateTemplate)
createDescriptorUpdateTemplateKHR = createDescriptorUpdateTemplate

destroyDescriptorUpdateTemplateKHR :: Device ->  DescriptorUpdateTemplate ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorUpdateTemplateKHR = destroyDescriptorUpdateTemplate

updateDescriptorSetWithTemplateKHR :: Device ->  DescriptorSet ->  DescriptorUpdateTemplate ->  Ptr () ->  IO ()
updateDescriptorSetWithTemplateKHR = updateDescriptorSetWithTemplate

-- No documentation found for TopLevel "DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT

-- No documentation found for TopLevel "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR :: VkDescriptorUpdateTemplateType
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION :: Integral a => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION

-- No documentation found for TopLevel "OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR"
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR :: VkObjectType
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE

-- No documentation found for TopLevel "STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
