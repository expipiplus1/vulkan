{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test
  ( withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  , fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  , PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
  , fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
  , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , VkPipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceRepresentativeFragmentTestFeaturesNV"
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "representativeFragmentTest"
  vkRepresentativeFragmentTest :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV -> (VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV pPNext (boolToBool32 (vkRepresentativeFragmentTest (from :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)))))
fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV -> IO PhysicalDeviceRepresentativeFragmentTestFeaturesNV
fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV c = PhysicalDeviceRepresentativeFragmentTestFeaturesNV <$> -- Univalued Member elided
                                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV)))
                                                                                                                     <*> pure (bool32ToBool (vkRepresentativeFragmentTest (c :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV)))
instance Zero PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = PhysicalDeviceRepresentativeFragmentTestFeaturesNV Nothing
                                                            False
-- No documentation found for TopLevel "PipelineRepresentativeFragmentTestStateCreateInfoNV"
data PipelineRepresentativeFragmentTestStateCreateInfoNV = PipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "representativeFragmentTestEnable"
  vkRepresentativeFragmentTestEnable :: Bool
  }
  deriving (Show, Eq)
withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV :: PipelineRepresentativeFragmentTestStateCreateInfoNV -> (VkPipelineRepresentativeFragmentTestStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRepresentativeFragmentTestStateCreateInfoNV)) (\pPNext -> cont (VkPipelineRepresentativeFragmentTestStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV pPNext (boolToBool32 (vkRepresentativeFragmentTestEnable (from :: PipelineRepresentativeFragmentTestStateCreateInfoNV)))))
fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV -> IO PipelineRepresentativeFragmentTestStateCreateInfoNV
fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV c = PipelineRepresentativeFragmentTestStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV)))
                                                                                                                       <*> pure (bool32ToBool (vkRepresentativeFragmentTestEnable (c :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV)))
instance Zero PipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = PipelineRepresentativeFragmentTestStateCreateInfoNV Nothing
                                                             False
