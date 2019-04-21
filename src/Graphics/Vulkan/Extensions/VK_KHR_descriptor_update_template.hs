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
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , DescriptorUpdateTemplateCreateFlagsKHR
  , DescriptorUpdateTemplateTypeKHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , cmdPushDescriptorSetWithTemplateKHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  ) where

import Foreign.Ptr
  ( Ptr
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
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template
  ( pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateFlagsKHR
  , DescriptorUpdateTemplateTypeKHR
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
