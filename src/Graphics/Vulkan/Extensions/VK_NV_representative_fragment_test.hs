{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , 
  PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
#endif
  , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
  , pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV"
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "representativeFragmentTest"
  representativeFragmentTest :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = PhysicalDeviceRepresentativeFragmentTestFeaturesNV Nothing
                                                            False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRepresentativeFragmentTestStateCreateInfoNV"
data PipelineRepresentativeFragmentTestStateCreateInfoNV = PipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "representativeFragmentTestEnable"
  representativeFragmentTestEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero PipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = PipelineRepresentativeFragmentTestStateCreateInfoNV Nothing
                                                             False

#endif

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION"
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION :: Integral a => a
pattern NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
