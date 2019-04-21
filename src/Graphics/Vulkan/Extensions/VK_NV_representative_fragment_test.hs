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



-- | VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV - Structure
-- describing the representative fragment test features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test.VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test.VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test.VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- feature.
--
-- Unresolved directive in
-- VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV = PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceRepresentativeFragmentTestFeaturesNV" "representativeFragmentTest"
  representativeFragmentTest :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV' and
-- marshal a 'PhysicalDeviceRepresentativeFragmentTestFeaturesNV' into it. The 'VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV -> (VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV pPNext (boolToBool32 (representativeFragmentTest (marshalled :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceRepresentativeFragmentTestFeaturesNV'.
fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV -> IO PhysicalDeviceRepresentativeFragmentTestFeaturesNV
fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV c = PhysicalDeviceRepresentativeFragmentTestFeaturesNV <$> -- Univalued Member elided
                                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV)))
                                                                                                                     <*> pure (bool32ToBool (vkRepresentativeFragmentTest (c :: VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV)))

instance Zero PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  zero = PhysicalDeviceRepresentativeFragmentTestFeaturesNV Nothing
                                                            False



-- | VkPipelineRepresentativeFragmentTestStateCreateInfoNV - Structure
-- specifying representative fragment test
--
-- = Description
--
-- If this structure is not present, @representativeFragmentTestEnable@ is
-- considered to be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and the
-- representative fragment test is disabled.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-early-mode early fragment tests>
-- are not enabled in the active fragment shader, the representative
-- fragment shader test has no effect, even if enabled.
--
-- Unresolved directive in
-- VkPipelineRepresentativeFragmentTestStateCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRepresentativeFragmentTestStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineRepresentativeFragmentTestStateCreateInfoNV = PipelineRepresentativeFragmentTestStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRepresentativeFragmentTestStateCreateInfoNV" "representativeFragmentTestEnable"
  representativeFragmentTestEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRepresentativeFragmentTestStateCreateInfoNV' and
-- marshal a 'PipelineRepresentativeFragmentTestStateCreateInfoNV' into it. The 'VkPipelineRepresentativeFragmentTestStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV :: PipelineRepresentativeFragmentTestStateCreateInfoNV -> (VkPipelineRepresentativeFragmentTestStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRepresentativeFragmentTestStateCreateInfoNV)) (\pPNext -> cont (VkPipelineRepresentativeFragmentTestStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV pPNext (boolToBool32 (representativeFragmentTestEnable (marshalled :: PipelineRepresentativeFragmentTestStateCreateInfoNV)))))

-- | A function to read a 'VkPipelineRepresentativeFragmentTestStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineRepresentativeFragmentTestStateCreateInfoNV'.
fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV -> IO PipelineRepresentativeFragmentTestStateCreateInfoNV
fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV c = PipelineRepresentativeFragmentTestStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV)))
                                                                                                                       <*> pure (bool32ToBool (vkRepresentativeFragmentTestEnable (c :: VkPipelineRepresentativeFragmentTestStateCreateInfoNV)))

instance Zero PipelineRepresentativeFragmentTestStateCreateInfoNV where
  zero = PipelineRepresentativeFragmentTestStateCreateInfoNV Nothing
                                                             False

