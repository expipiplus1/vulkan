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


-- | VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV - Structure
-- describing the representative fragment test features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- @VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV@ structure
-- describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV@ structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- @VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV@ /can/ also be
-- used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- feature.
--
-- Unresolved directive in
-- VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV = VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @representativeFragmentTest@ indicates whether the implementation
  -- supports the representative fragment test. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-rep-frag-test Representative Fragment Test>.
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
  zero = VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV zero
                                                              zero
                                                              zero
-- | VkPipelineRepresentativeFragmentTestStateCreateInfoNV - Structure
-- specifying representative fragment test
--
-- = Description
--
-- If this structure is not present, @representativeFragmentTestEnable@ is
-- considered to be @VK_FALSE@, and the representative fragment test is
-- disabled.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-early-mode early fragment tests>
-- are not enabled in the active fragment shader, the representative
-- fragment shader test has no effect, even if enabled.
--
-- Unresolved directive in
-- VkPipelineRepresentativeFragmentTestStateCreateInfoNV.txt -
-- include::..\/validity\/structs\/VkPipelineRepresentativeFragmentTestStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPipelineRepresentativeFragmentTestStateCreateInfoNV = VkPipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @representativeFragmentTestEnable@ controls whether the representative
  -- fragment test is enabled.
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
  zero = VkPipelineRepresentativeFragmentTestStateCreateInfoNV zero
                                                               zero
                                                               zero
-- No documentation found for TopLevel "VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME"
pattern VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
