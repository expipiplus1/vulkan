{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( withCStructPhysicalDevicePushDescriptorPropertiesKHR
  , fromCStructPhysicalDevicePushDescriptorPropertiesKHR
  , PhysicalDevicePushDescriptorPropertiesKHR(..)
  , cmdPushDescriptorSetKHR
  , cmdPushDescriptorSetWithTemplateKHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  ) where

import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdPushDescriptorSetKHR
  , cmdPushDescriptorSetWithTemplateKHR
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( WriteDescriptorSet(..)
  , withCStructWriteDescriptorSet
  )
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplate
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDevicePushDescriptorPropertiesKHR"
data PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorPropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
  vkMaxPushDescriptors :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDevicePushDescriptorPropertiesKHR :: PhysicalDevicePushDescriptorPropertiesKHR -> (VkPhysicalDevicePushDescriptorPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDevicePushDescriptorPropertiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDevicePushDescriptorPropertiesKHR)) (\pPNext -> cont (VkPhysicalDevicePushDescriptorPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR pPNext (vkMaxPushDescriptors (from :: PhysicalDevicePushDescriptorPropertiesKHR))))
fromCStructPhysicalDevicePushDescriptorPropertiesKHR :: VkPhysicalDevicePushDescriptorPropertiesKHR -> IO PhysicalDevicePushDescriptorPropertiesKHR
fromCStructPhysicalDevicePushDescriptorPropertiesKHR c = PhysicalDevicePushDescriptorPropertiesKHR <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePushDescriptorPropertiesKHR)))
                                                                                                   <*> pure (vkMaxPushDescriptors (c :: VkPhysicalDevicePushDescriptorPropertiesKHR))

-- | Wrapper for 'vkCmdPushDescriptorSetKHR'
cmdPushDescriptorSetKHR :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector WriteDescriptorSet ->  IO (  )
cmdPushDescriptorSetKHR = \(CommandBuffer commandBuffer commandTable) -> \pipelineBindPoint -> \layout -> \set -> \descriptorWrites -> withVec withCStructWriteDescriptorSet descriptorWrites (\pDescriptorWrites -> Graphics.Vulkan.C.Dynamic.cmdPushDescriptorSetKHR commandTable commandBuffer pipelineBindPoint layout set (fromIntegral $ Data.Vector.length descriptorWrites) pDescriptorWrites *> (pure ()))

-- | Wrapper for 'vkCmdPushDescriptorSetWithTemplateKHR'
cmdPushDescriptorSetWithTemplateKHR :: CommandBuffer ->  DescriptorUpdateTemplate ->  PipelineLayout ->  Word32 ->  Ptr () ->  IO (  )
cmdPushDescriptorSetWithTemplateKHR = \(CommandBuffer commandBuffer commandTable) -> \descriptorUpdateTemplate -> \layout -> \set -> \pData -> Graphics.Vulkan.C.Dynamic.cmdPushDescriptorSetWithTemplateKHR commandTable commandBuffer descriptorUpdateTemplate layout set pData *> (pure ())
