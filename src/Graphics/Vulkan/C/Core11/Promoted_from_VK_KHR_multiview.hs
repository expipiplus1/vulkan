{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewFeatures"
data VkPhysicalDeviceMultiviewFeatures = VkPhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiview"
  vkMultiview :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
  vkMultiviewGeometryShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
  vkMultiviewTessellationShader :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMultiviewFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewFeatures <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 16) (vkMultiview (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 20) (vkMultiviewGeometryShader (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 24) (vkMultiviewTessellationShader (poked :: VkPhysicalDeviceMultiviewFeatures))
-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewProperties"
data VkPhysicalDeviceMultiviewProperties = VkPhysicalDeviceMultiviewProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
  vkMaxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
  vkMaxMultiviewInstanceIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMultiviewProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewProperties <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxMultiviewViewCount (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 20) (vkMaxMultiviewInstanceIndex (poked :: VkPhysicalDeviceMultiviewProperties))
-- No documentation found for TopLevel "VkRenderPassMultiviewCreateInfo"
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo
  { -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "subpassCount"
  vkSubpassCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pViewMasks"
  vkPViewMasks :: Ptr Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "dependencyCount"
  vkDependencyCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pViewOffsets"
  vkPViewOffsets :: Ptr Int32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "correlationMaskCount"
  vkCorrelationMaskCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pCorrelationMasks"
  vkPCorrelationMasks :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkRenderPassMultiviewCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassMultiviewCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
                                             <*> peek (ptr `plusPtr` 48)
                                             <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkSubpassCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPViewMasks (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDependencyCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPViewOffsets (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkCorrelationMaskCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPCorrelationMasks (poked :: VkRenderPassMultiviewCreateInfo))
-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_VIEW_LOCAL_BIT"
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyFlagBits 0x00000002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES = VkStructureType 1000053001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES = VkStructureType 1000053002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO = VkStructureType 1000053000
