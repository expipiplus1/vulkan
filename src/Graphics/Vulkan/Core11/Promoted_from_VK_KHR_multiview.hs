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


-- | Nothing
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO = VkStructureType 1000053000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES = VkStructureType 1000053001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES = VkStructureType 1000053002
-- | Nothing
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyFlagBits 0x00000002
-- | TODO: Struct comments
data VkPhysicalDeviceMultiviewFeatures = VkPhysicalDeviceMultiviewFeatures
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMultiview :: VkBool32
  , vkMultiviewGeometryShader :: VkBool32
  , vkMultiviewTessellationShader :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 16) (vkMultiview (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 20) (vkMultiviewGeometryShader (poked :: VkPhysicalDeviceMultiviewFeatures))
                *> poke (ptr `plusPtr` 24) (vkMultiviewTessellationShader (poked :: VkPhysicalDeviceMultiviewFeatures))
-- | TODO: Struct comments
data VkPhysicalDeviceMultiviewProperties = VkPhysicalDeviceMultiviewProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxMultiviewViewCount :: Word32
  , vkMaxMultiviewInstanceIndex :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxMultiviewViewCount (poked :: VkPhysicalDeviceMultiviewProperties))
                *> poke (ptr `plusPtr` 20) (vkMaxMultiviewInstanceIndex (poked :: VkPhysicalDeviceMultiviewProperties))
-- | TODO: Struct comments
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSubpassCount :: Word32
  , vkViewMasks :: Ptr Word32
  , vkDependencyCount :: Word32
  , vkViewOffsets :: Ptr Int32
  , vkCorrelationMaskCount :: Word32
  , vkCorrelationMasks :: Ptr Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkSubpassCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkViewMasks (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDependencyCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkViewOffsets (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkCorrelationMaskCount (poked :: VkRenderPassMultiviewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkCorrelationMasks (poked :: VkRenderPassMultiviewCreateInfo))
