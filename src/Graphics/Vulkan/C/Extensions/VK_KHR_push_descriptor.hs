{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , FN_vkCmdPushDescriptorSetKHR
  , PFN_vkCmdPushDescriptorSetKHR
  , vkCmdPushDescriptorSetKHR
  , FN_vkCmdPushDescriptorSetWithTemplateKHR
  , PFN_vkCmdPushDescriptorSetWithTemplateKHR
  , vkCmdPushDescriptorSetWithTemplateKHR
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateFlagBits(..)
  , VkWriteDescriptorSet(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineLayout
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateType(..)
  , VkDescriptorUpdateTemplate
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkPhysicalDevicePushDescriptorPropertiesKHR"
data VkPhysicalDevicePushDescriptorPropertiesKHR = VkPhysicalDevicePushDescriptorPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
  vkMaxPushDescriptors :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePushDescriptorPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMaxPushDescriptors (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))

instance Zero VkPhysicalDevicePushDescriptorPropertiesKHR where
  zero = VkPhysicalDevicePushDescriptorPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
                                                     zero
                                                     zero

-- No documentation found for TopLevel "vkCmdPushDescriptorSetKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushDescriptorSetKHR" vkCmdPushDescriptorSetKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()
#else
vkCmdPushDescriptorSetKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()
vkCmdPushDescriptorSetKHR deviceCmds = mkVkCmdPushDescriptorSetKHR (pVkCmdPushDescriptorSetKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ())
#endif

type FN_vkCmdPushDescriptorSetKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()
type PFN_vkCmdPushDescriptorSetKHR = FunPtr FN_vkCmdPushDescriptorSetKHR

-- No documentation found for TopLevel "vkCmdPushDescriptorSetWithTemplateKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushDescriptorSetWithTemplateKHR" vkCmdPushDescriptorSetWithTemplateKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()
#else
vkCmdPushDescriptorSetWithTemplateKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()
vkCmdPushDescriptorSetWithTemplateKHR deviceCmds = mkVkCmdPushDescriptorSetWithTemplateKHR (pVkCmdPushDescriptorSetWithTemplateKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplateKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ())
#endif

type FN_vkCmdPushDescriptorSetWithTemplateKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkCmdPushDescriptorSetWithTemplateKHR = FunPtr FN_vkCmdPushDescriptorSetWithTemplateKHR

-- No documentation found for Nested "VkDescriptorSetLayoutCreateFlagBits" "VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = VkDescriptorSetLayoutCreateFlagBits 0x00000001

-- No documentation found for Nested "VkDescriptorUpdateTemplateType" "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = VkDescriptorUpdateTemplateType 1

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: Integral a => a
pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR = VkStructureType 1000080000
