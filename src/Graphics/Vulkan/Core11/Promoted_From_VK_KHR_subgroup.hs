{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( withCStructPhysicalDeviceSubgroupProperties
  , fromCStructPhysicalDeviceSubgroupProperties
  , PhysicalDeviceSubgroupProperties(..)
  , SubgroupFeatureFlagBits
  , SubgroupFeatureFlags
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  ) where

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


import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkPhysicalDeviceSubgroupProperties(..)
  , VkSubgroupFeatureFlagBits(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "PhysicalDeviceSubgroupProperties"
data PhysicalDeviceSubgroupProperties = PhysicalDeviceSubgroupProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "subgroupSize"
  vkSubgroupSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "supportedStages"
  vkSupportedStages :: ShaderStageFlags
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "supportedOperations"
  vkSupportedOperations :: SubgroupFeatureFlags
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "quadOperationsInAllStages"
  vkQuadOperationsInAllStages :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSubgroupProperties :: PhysicalDeviceSubgroupProperties -> (VkPhysicalDeviceSubgroupProperties -> IO a) -> IO a
withCStructPhysicalDeviceSubgroupProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSubgroupProperties)) (\pPNext -> cont (VkPhysicalDeviceSubgroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES pPNext (vkSubgroupSize (from :: PhysicalDeviceSubgroupProperties)) (vkSupportedStages (from :: PhysicalDeviceSubgroupProperties)) (vkSupportedOperations (from :: PhysicalDeviceSubgroupProperties)) (boolToBool32 (vkQuadOperationsInAllStages (from :: PhysicalDeviceSubgroupProperties)))))
fromCStructPhysicalDeviceSubgroupProperties :: VkPhysicalDeviceSubgroupProperties -> IO PhysicalDeviceSubgroupProperties
fromCStructPhysicalDeviceSubgroupProperties c = PhysicalDeviceSubgroupProperties <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSubgroupProperties)))
                                                                                 <*> pure (vkSubgroupSize (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (vkSupportedStages (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (vkSupportedOperations (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (bool32ToBool (vkQuadOperationsInAllStages (c :: VkPhysicalDeviceSubgroupProperties)))
-- No documentation found for TopLevel "SubgroupFeatureFlagBits"
type SubgroupFeatureFlagBits = VkSubgroupFeatureFlagBits
-- No documentation found for TopLevel "SubgroupFeatureFlags"
type SubgroupFeatureFlags = SubgroupFeatureFlagBits
