{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_multiview
  ( pattern VK_KHR_MULTIVIEW_SPEC_VERSION
  , pattern VK_KHR_MULTIVIEW_EXTENSION_NAME
  , VkPhysicalDeviceMultiviewFeaturesKHR
  , pattern VkPhysicalDeviceMultiviewFeaturesKHR
  , VkPhysicalDeviceMultiviewPropertiesKHR
  , pattern VkPhysicalDeviceMultiviewPropertiesKHR
  , VkRenderPassMultiviewCreateInfoKHR
  , pattern VkRenderPassMultiviewCreateInfoKHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , VkRenderPassMultiviewCreateInfo(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkPhysicalDeviceMultiviewFeatures(..)
  )


pattern VK_KHR_MULTIVIEW_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MULTIVIEW_SPEC_VERSION = 1
pattern VK_KHR_MULTIVIEW_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MULTIVIEW_EXTENSION_NAME = "VK_KHR_multiview"
type VkPhysicalDeviceMultiviewFeaturesKHR = VkPhysicalDeviceMultiviewFeatures


pattern VkPhysicalDeviceMultiviewFeaturesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("multiview" ::: VkBool32) -> ("multiviewGeometryShader" ::: VkBool32) -> ("multiviewTessellationShader" ::: VkBool32) -> VkPhysicalDeviceMultiviewFeaturesKHR
pattern VkPhysicalDeviceMultiviewFeaturesKHR vkSType vkPNext vkMultiview vkMultiviewGeometryShader vkMultiviewTessellationShader = VkPhysicalDeviceMultiviewFeatures vkSType vkPNext vkMultiview vkMultiviewGeometryShader vkMultiviewTessellationShader
type VkPhysicalDeviceMultiviewPropertiesKHR = VkPhysicalDeviceMultiviewProperties


pattern VkPhysicalDeviceMultiviewPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("maxMultiviewViewCount" ::: Word32) -> ("maxMultiviewInstanceIndex" ::: Word32) -> VkPhysicalDeviceMultiviewPropertiesKHR
pattern VkPhysicalDeviceMultiviewPropertiesKHR vkSType vkPNext vkMaxMultiviewViewCount vkMaxMultiviewInstanceIndex = VkPhysicalDeviceMultiviewProperties vkSType vkPNext vkMaxMultiviewViewCount vkMaxMultiviewInstanceIndex
type VkRenderPassMultiviewCreateInfoKHR = VkRenderPassMultiviewCreateInfo


pattern VkRenderPassMultiviewCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("subpassCount" ::: Word32) -> ("pViewMasks" ::: Ptr Word32) -> ("dependencyCount" ::: Word32) -> ("pViewOffsets" ::: Ptr Int32) -> ("correlationMaskCount" ::: Word32) -> ("pCorrelationMasks" ::: Ptr Word32) -> VkRenderPassMultiviewCreateInfoKHR
pattern VkRenderPassMultiviewCreateInfoKHR vkSType vkPNext vkSubpassCount vkPViewMasks vkDependencyCount vkPViewOffsets vkCorrelationMaskCount vkPCorrelationMasks = VkRenderPassMultiviewCreateInfo vkSType vkPNext vkSubpassCount vkPViewMasks vkDependencyCount vkPViewOffsets vkCorrelationMaskCount vkPCorrelationMasks
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR :: VkDependencyFlagBits
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR = VK_DEPENDENCY_VIEW_LOCAL_BIT
