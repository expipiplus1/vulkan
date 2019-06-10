{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , VkPipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME
  , pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
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
  , Zero(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV"
data VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV = VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "representativeFragmentTest"
  vkRepresentativeFragmentTest :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                                  <*> peek (ptr `plusPtr` 8)
                                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkRepresentativeFragmentTest (poked :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV))

instance Zero VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
                                                              zero
                                                              zero

-- No documentation found for TopLevel "VkPipelineRepresentativeFragmentTestStateCreateInfoNV"
data VkPipelineRepresentativeFragmentTestStateCreateInfoNV = VkPipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineRepresentativeFragmentTestStateCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineRepresentativeFragmentTestStateCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineRepresentativeFragmentTestStateCreateInfoNV" "representativeFragmentTestEnable"
  vkRepresentativeFragmentTestEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPipelineRepresentativeFragmentTestStateCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineRepresentativeFragmentTestStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkRepresentativeFragmentTestEnable (poked :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV))

instance Zero VkPipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = VkPipelineRepresentativeFragmentTestStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
                                                               zero
                                                               zero

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME"
pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = "VK_NV_representative_fragment_test"

-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION"
pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION :: Integral a => a
pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV = VkStructureType 1000166000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV = VkStructureType 1000166001
