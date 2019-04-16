{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( withCStructPhysicalDeviceMultiviewFeatures
  , fromCStructPhysicalDeviceMultiviewFeatures
  , PhysicalDeviceMultiviewFeatures(..)
  , withCStructPhysicalDeviceMultiviewProperties
  , fromCStructPhysicalDeviceMultiviewProperties
  , PhysicalDeviceMultiviewProperties(..)
  , withCStructRenderPassMultiviewCreateInfo
  , fromCStructRenderPassMultiviewCreateInfo
  , RenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  ) where

import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( pattern VK_DEPENDENCY_VIEW_LOCAL_BIT
  )


-- No documentation found for TopLevel "PhysicalDeviceMultiviewFeatures"
data PhysicalDeviceMultiviewFeatures = PhysicalDeviceMultiviewFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiview"
  vkMultiview :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
  vkMultiviewGeometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
  vkMultiviewTessellationShader :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMultiviewFeatures :: PhysicalDeviceMultiviewFeatures -> (VkPhysicalDeviceMultiviewFeatures -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMultiviewFeatures)) (\pPNext -> cont (VkPhysicalDeviceMultiviewFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES pPNext (boolToBool32 (vkMultiview (from :: PhysicalDeviceMultiviewFeatures))) (boolToBool32 (vkMultiviewGeometryShader (from :: PhysicalDeviceMultiviewFeatures))) (boolToBool32 (vkMultiviewTessellationShader (from :: PhysicalDeviceMultiviewFeatures)))))
fromCStructPhysicalDeviceMultiviewFeatures :: VkPhysicalDeviceMultiviewFeatures -> IO PhysicalDeviceMultiviewFeatures
fromCStructPhysicalDeviceMultiviewFeatures c = PhysicalDeviceMultiviewFeatures <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiview (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiviewGeometryShader (c :: VkPhysicalDeviceMultiviewFeatures)))
                                                                               <*> pure (bool32ToBool (vkMultiviewTessellationShader (c :: VkPhysicalDeviceMultiviewFeatures)))
-- No documentation found for TopLevel "PhysicalDeviceMultiviewProperties"
data PhysicalDeviceMultiviewProperties = PhysicalDeviceMultiviewProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
  vkMaxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
  vkMaxMultiviewInstanceIndex :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMultiviewProperties :: PhysicalDeviceMultiviewProperties -> (VkPhysicalDeviceMultiviewProperties -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMultiviewProperties)) (\pPNext -> cont (VkPhysicalDeviceMultiviewProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES pPNext (vkMaxMultiviewViewCount (from :: PhysicalDeviceMultiviewProperties)) (vkMaxMultiviewInstanceIndex (from :: PhysicalDeviceMultiviewProperties))))
fromCStructPhysicalDeviceMultiviewProperties :: VkPhysicalDeviceMultiviewProperties -> IO PhysicalDeviceMultiviewProperties
fromCStructPhysicalDeviceMultiviewProperties c = PhysicalDeviceMultiviewProperties <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewProperties)))
                                                                                   <*> pure (vkMaxMultiviewViewCount (c :: VkPhysicalDeviceMultiviewProperties))
                                                                                   <*> pure (vkMaxMultiviewInstanceIndex (c :: VkPhysicalDeviceMultiviewProperties))
-- No documentation found for TopLevel "RenderPassMultiviewCreateInfo"
data RenderPassMultiviewCreateInfo = RenderPassMultiviewCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewMasks"
  vkPViewMasks :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pViewOffsets"
  vkPViewOffsets :: Vector Int32
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassMultiviewCreateInfo" "pCorrelationMasks"
  vkPCorrelationMasks :: Vector Word32
  }
  deriving (Show, Eq)
withCStructRenderPassMultiviewCreateInfo :: RenderPassMultiviewCreateInfo -> (VkRenderPassMultiviewCreateInfo -> IO a) -> IO a
withCStructRenderPassMultiviewCreateInfo from cont = withVec (&) (vkPCorrelationMasks (from :: RenderPassMultiviewCreateInfo)) (\pCorrelationMasks -> withVec (&) (vkPViewOffsets (from :: RenderPassMultiviewCreateInfo)) (\pViewOffsets -> withVec (&) (vkPViewMasks (from :: RenderPassMultiviewCreateInfo)) (\pViewMasks -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassMultiviewCreateInfo)) (\pPNext -> cont (VkRenderPassMultiviewCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (vkPViewMasks (from :: RenderPassMultiviewCreateInfo)))) pViewMasks (fromIntegral (Data.Vector.length (vkPViewOffsets (from :: RenderPassMultiviewCreateInfo)))) pViewOffsets (fromIntegral (Data.Vector.length (vkPCorrelationMasks (from :: RenderPassMultiviewCreateInfo)))) pCorrelationMasks)))))
fromCStructRenderPassMultiviewCreateInfo :: VkRenderPassMultiviewCreateInfo -> IO RenderPassMultiviewCreateInfo
fromCStructRenderPassMultiviewCreateInfo c = RenderPassMultiviewCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassMultiviewCreateInfo)))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkSubpassCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPViewMasks (c :: VkRenderPassMultiviewCreateInfo))))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkDependencyCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPViewOffsets (c :: VkRenderPassMultiviewCreateInfo))))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkCorrelationMaskCount (c :: VkRenderPassMultiviewCreateInfo))) (peekElemOff (vkPCorrelationMasks (c :: VkRenderPassMultiviewCreateInfo))))
