{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDevicePushDescriptorPropertiesKHR(..)
  , 
#endif
  cmdPushDescriptorSetKHR
  , cmdPushDescriptorSetWithTemplateKHR
  , pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( vkCmdPushDescriptorSetKHR
  , vkCmdPushDescriptorSetWithTemplateKHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( WriteDescriptorSet(..)
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

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDevicePushDescriptorPropertiesKHR"
data PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorPropertiesKHR
  { -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
  maxPushDescriptors :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDevicePushDescriptorPropertiesKHR where
  zero = PhysicalDevicePushDescriptorPropertiesKHR Nothing
                                                   zero

#endif


-- No documentation found for TopLevel "vkCmdPushDescriptorSetKHR"
cmdPushDescriptorSetKHR :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector WriteDescriptorSet ->  IO ()
cmdPushDescriptorSetKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdPushDescriptorSetWithTemplateKHR"
cmdPushDescriptorSetWithTemplateKHR :: CommandBuffer ->  DescriptorUpdateTemplate ->  PipelineLayout ->  Word32 ->  Ptr () ->  IO ()
cmdPushDescriptorSetWithTemplateKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: Integral a => a
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION = VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
