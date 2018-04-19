{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  , VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO = VkStructureType 1000053000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES = VkStructureType 1000053001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES = VkStructureType 1000053002
-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_VIEW_LOCAL_BIT"
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyFlagBits 0x00000002
-- | VkPhysicalDeviceMultiviewFeatures - Structure describing multiview
-- features that can be supported by an implementation
data VkPhysicalDeviceMultiviewFeatures = VkPhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "vkMultiview"
  vkMultiview :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "vkMultiviewGeometryShader"
  vkMultiviewGeometryShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "vkMultiviewTessellationShader"
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
-- | VkPhysicalDeviceMultiviewProperties - Structure describing multiview
-- limits that can be supported by an implementation
data VkPhysicalDeviceMultiviewProperties = VkPhysicalDeviceMultiviewProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "vkMaxMultiviewViewCount"
  vkMaxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "vkMaxMultiviewInstanceIndex"
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
-- | VkRenderPassMultiviewCreateInfo - Structure containing multiview info
-- for all subpasses
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo
  { -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkSubpassCount"
  vkSubpassCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkPViewMasks"
  vkPViewMasks :: Ptr Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkDependencyCount"
  vkDependencyCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkPViewOffsets"
  vkPViewOffsets :: Ptr Int32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkCorrelationMaskCount"
  vkCorrelationMaskCount :: Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "vkPCorrelationMasks"
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
