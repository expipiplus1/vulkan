{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceMultiviewFeatures(..)
  , 
  PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
#endif
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern DEPENDENCY_VIEW_LOCAL_BIT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Int
  ( Int32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern DEPENDENCY_VIEW_LOCAL_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewFeatures"
data PhysicalDeviceMultiviewFeatures = PhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiview"
  multiview :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
  multiviewGeometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
  multiviewTessellationShader :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMultiviewFeatures where
  zero = PhysicalDeviceMultiviewFeatures Nothing
                                         False
                                         False
                                         False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewProperties"
data PhysicalDeviceMultiviewProperties = PhysicalDeviceMultiviewProperties
  { -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
  maxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
  maxMultiviewInstanceIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMultiviewProperties where
  zero = PhysicalDeviceMultiviewProperties Nothing
                                           zero
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassMultiviewCreateInfo"
data RenderPassMultiviewCreateInfo = RenderPassMultiviewCreateInfo
  { -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewMasks"
  viewMasks :: Vector Word32
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewOffsets"
  viewOffsets :: Vector Int32
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pCorrelationMasks"
  correlationMasks :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero RenderPassMultiviewCreateInfo where
  zero = RenderPassMultiviewCreateInfo Nothing
                                       mempty
                                       mempty
                                       mempty

#endif
