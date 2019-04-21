{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( BlendOverlapEXT
  , pattern BLEND_OVERLAP_UNCORRELATED_EXT
  , pattern BLEND_OVERLAP_DISJOINT_EXT
  , pattern BLEND_OVERLAP_CONJOINT_EXT
  , withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , withCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  , fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  , pattern VK_BLEND_OP_ZERO_EXT
  , pattern VK_BLEND_OP_SRC_EXT
  , pattern VK_BLEND_OP_DST_EXT
  , pattern VK_BLEND_OP_SRC_OVER_EXT
  , pattern VK_BLEND_OP_DST_OVER_EXT
  , pattern VK_BLEND_OP_SRC_IN_EXT
  , pattern VK_BLEND_OP_DST_IN_EXT
  , pattern VK_BLEND_OP_SRC_OUT_EXT
  , pattern VK_BLEND_OP_DST_OUT_EXT
  , pattern VK_BLEND_OP_SRC_ATOP_EXT
  , pattern VK_BLEND_OP_DST_ATOP_EXT
  , pattern VK_BLEND_OP_XOR_EXT
  , pattern VK_BLEND_OP_MULTIPLY_EXT
  , pattern VK_BLEND_OP_SCREEN_EXT
  , pattern VK_BLEND_OP_OVERLAY_EXT
  , pattern VK_BLEND_OP_DARKEN_EXT
  , pattern VK_BLEND_OP_LIGHTEN_EXT
  , pattern VK_BLEND_OP_COLORDODGE_EXT
  , pattern VK_BLEND_OP_COLORBURN_EXT
  , pattern VK_BLEND_OP_HARDLIGHT_EXT
  , pattern VK_BLEND_OP_SOFTLIGHT_EXT
  , pattern VK_BLEND_OP_DIFFERENCE_EXT
  , pattern VK_BLEND_OP_EXCLUSION_EXT
  , pattern VK_BLEND_OP_INVERT_EXT
  , pattern VK_BLEND_OP_INVERT_RGB_EXT
  , pattern VK_BLEND_OP_LINEARDODGE_EXT
  , pattern VK_BLEND_OP_LINEARBURN_EXT
  , pattern VK_BLEND_OP_VIVIDLIGHT_EXT
  , pattern VK_BLEND_OP_LINEARLIGHT_EXT
  , pattern VK_BLEND_OP_PINLIGHT_EXT
  , pattern VK_BLEND_OP_HARDMIX_EXT
  , pattern VK_BLEND_OP_HSL_HUE_EXT
  , pattern VK_BLEND_OP_HSL_SATURATION_EXT
  , pattern VK_BLEND_OP_HSL_COLOR_EXT
  , pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern VK_BLEND_OP_PLUS_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern VK_BLEND_OP_PLUS_DARKER_EXT
  , pattern VK_BLEND_OP_MINUS_EXT
  , pattern VK_BLEND_OP_MINUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_CONTRAST_EXT
  , pattern VK_BLEND_OP_INVERT_OVG_EXT
  , pattern VK_BLEND_OP_RED_EXT
  , pattern VK_BLEND_OP_GREEN_EXT
  , pattern VK_BLEND_OP_BLUE_EXT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_BLEND_OVERLAP_CONJOINT_EXT
  , pattern VK_BLEND_OVERLAP_DISJOINT_EXT
  , pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  , pattern VK_BLEND_OP_BLUE_EXT
  , pattern VK_BLEND_OP_COLORBURN_EXT
  , pattern VK_BLEND_OP_COLORDODGE_EXT
  , pattern VK_BLEND_OP_CONTRAST_EXT
  , pattern VK_BLEND_OP_DARKEN_EXT
  , pattern VK_BLEND_OP_DIFFERENCE_EXT
  , pattern VK_BLEND_OP_DST_ATOP_EXT
  , pattern VK_BLEND_OP_DST_EXT
  , pattern VK_BLEND_OP_DST_IN_EXT
  , pattern VK_BLEND_OP_DST_OUT_EXT
  , pattern VK_BLEND_OP_DST_OVER_EXT
  , pattern VK_BLEND_OP_EXCLUSION_EXT
  , pattern VK_BLEND_OP_GREEN_EXT
  , pattern VK_BLEND_OP_HARDLIGHT_EXT
  , pattern VK_BLEND_OP_HARDMIX_EXT
  , pattern VK_BLEND_OP_HSL_COLOR_EXT
  , pattern VK_BLEND_OP_HSL_HUE_EXT
  , pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern VK_BLEND_OP_HSL_SATURATION_EXT
  , pattern VK_BLEND_OP_INVERT_EXT
  , pattern VK_BLEND_OP_INVERT_OVG_EXT
  , pattern VK_BLEND_OP_INVERT_RGB_EXT
  , pattern VK_BLEND_OP_LIGHTEN_EXT
  , pattern VK_BLEND_OP_LINEARBURN_EXT
  , pattern VK_BLEND_OP_LINEARDODGE_EXT
  , pattern VK_BLEND_OP_LINEARLIGHT_EXT
  , pattern VK_BLEND_OP_MINUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_MINUS_EXT
  , pattern VK_BLEND_OP_MULTIPLY_EXT
  , pattern VK_BLEND_OP_OVERLAY_EXT
  , pattern VK_BLEND_OP_PINLIGHT_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_PLUS_DARKER_EXT
  , pattern VK_BLEND_OP_PLUS_EXT
  , pattern VK_BLEND_OP_RED_EXT
  , pattern VK_BLEND_OP_SCREEN_EXT
  , pattern VK_BLEND_OP_SOFTLIGHT_EXT
  , pattern VK_BLEND_OP_SRC_ATOP_EXT
  , pattern VK_BLEND_OP_SRC_EXT
  , pattern VK_BLEND_OP_SRC_IN_EXT
  , pattern VK_BLEND_OP_SRC_OUT_EXT
  , pattern VK_BLEND_OP_SRC_OVER_EXT
  , pattern VK_BLEND_OP_VIVIDLIGHT_EXT
  , pattern VK_BLEND_OP_XOR_EXT
  , pattern VK_BLEND_OP_ZERO_EXT
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  )


-- | VkBlendOverlapEXT - Enumerant specifying the blend overlap parameter
--
-- = Description
--
-- \'
--
-- > +-----------------------------------+-----------------------------------+
-- > | Overlap Mode                      | Weighting Equations               |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_UNCORRELATED_EXT'    |                                   |
-- > |                                   |                p_0(A_s,A_d) & = A |
-- > |                                   | _sA_d \\                          |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = A |
-- > |                                   | _s(1-A_d) \\                      |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = A |
-- > |                                   | _d(1-A_s) \\                      |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_CONJOINT_EXT'        |                                   |
-- > |                                   |                p_0(A_s,A_d) & = m |
-- > |                                   | in(A_s,A_d) \\                    |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = m |
-- > |                                   | ax(A_s-A_d,0) \\                  |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = m |
-- > |                                   | ax(A_d-A_s,0) \\                  |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | \[                                |
-- > | EXT_blend_operation_advanced.VK_B |                \begin{aligned}    |
-- > | LEND_OVERLAP_DISJOINT_EXT'        |                                   |
-- > |                                   |                p_0(A_s,A_d) & = m |
-- > |                                   | ax(A_s+A_d-1,0) \\                |
-- > |                                   |                                   |
-- > |                                   |                p_1(A_s,A_d) & = m |
-- > |                                   | in(A_s,1-A_d) \\                  |
-- > |                                   |                                   |
-- > |                                   |                p_2(A_s,A_d) & = m |
-- > |                                   | in(A_d,1-A_s) \\                  |
-- > |                                   |                                   |
-- > |                                   |              \end{aligned}\]      |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Advanced Blend Overlap Modes
--
-- = See Also
--
-- No cross-references are available
type BlendOverlapEXT = VkBlendOverlapEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OVERLAP_UNCORRELATED_EXT'
-- specifies that there is no correlation between the source and
-- destination coverage.
pattern BLEND_OVERLAP_UNCORRELATED_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_UNCORRELATED_EXT = VK_BLEND_OVERLAP_UNCORRELATED_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OVERLAP_DISJOINT_EXT'
-- specifies that the source and destination coverage are considered to
-- have minimal overlap.
pattern BLEND_OVERLAP_DISJOINT_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_DISJOINT_EXT = VK_BLEND_OVERLAP_DISJOINT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OVERLAP_CONJOINT_EXT'
-- specifies that the source and destination coverage are considered to
-- have maximal overlap.
pattern BLEND_OVERLAP_CONJOINT_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_CONJOINT_EXT = VK_BLEND_OVERLAP_CONJOINT_EXT


-- | VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT - Structure describing
-- advanced blending features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT'
-- /can/ also be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- Unresolved directive in
-- VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "advancedBlendCoherentOperations"
  advancedBlendCoherentOperations :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT' and
-- marshal a 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT' into it. The 'VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT -> (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT pPNext (boolToBool32 (advancedBlendCoherentOperations (marshalled :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'.
fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT -> IO PhysicalDeviceBlendOperationAdvancedFeaturesEXT
fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT c = PhysicalDeviceBlendOperationAdvancedFeaturesEXT <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkAdvancedBlendCoherentOperations (c :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)))

instance Zero PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedFeaturesEXT Nothing
                                                         False



-- | VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT - Structure
-- describing advanced blending limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendMaxColorAttachments"
  advancedBlendMaxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendIndependentBlend"
  advancedBlendIndependentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedSrcColor"
  advancedBlendNonPremultipliedSrcColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedDstColor"
  advancedBlendNonPremultipliedDstColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendCorrelatedOverlap"
  advancedBlendCorrelatedOverlap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendAllOperations"
  advancedBlendAllOperations :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT' and
-- marshal a 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT' into it. The 'VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT -> (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT pPNext (advancedBlendMaxColorAttachments (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)) (boolToBool32 (advancedBlendIndependentBlend (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (advancedBlendNonPremultipliedSrcColor (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (advancedBlendNonPremultipliedDstColor (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (advancedBlendCorrelatedOverlap (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (advancedBlendAllOperations (marshalled :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)))))

-- | A function to read a 'VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'.
fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT -> IO PhysicalDeviceBlendOperationAdvancedPropertiesEXT
fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT c = PhysicalDeviceBlendOperationAdvancedPropertiesEXT <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (vkAdvancedBlendMaxColorAttachments (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendIndependentBlend (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendNonPremultipliedSrcColor (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendNonPremultipliedDstColor (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendCorrelatedOverlap (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendAllOperations (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))

instance Zero PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedPropertiesEXT Nothing
                                                           zero
                                                           False
                                                           False
                                                           False
                                                           False
                                                           False



-- | VkPipelineColorBlendAdvancedStateCreateInfoEXT - Structure specifying
-- parameters that affect advanced blend operations
--
-- = Description
--
-- If this structure is not present, @srcPremultiplied@ and
-- @dstPremultiplied@ are both considered to be
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', and @blendOverlap@ is
-- considered to be
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OVERLAP_UNCORRELATED_EXT'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedSrcColor non-premultiplied source color>
--     property is not supported, @srcPremultiplied@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedDstColor non-premultiplied destination color>
--     property is not supported, @dstPremultiplied@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-advancedBlendCorrelatedOverlap correlated overlap>
--     property is not supported, @blendOverlap@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OVERLAP_UNCORRELATED_EXT'
--
-- Unresolved directive in
-- VkPipelineColorBlendAdvancedStateCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineColorBlendAdvancedStateCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "srcPremultiplied"
  srcPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "dstPremultiplied"
  dstPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "blendOverlap"
  blendOverlap :: BlendOverlapEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineColorBlendAdvancedStateCreateInfoEXT' and
-- marshal a 'PipelineColorBlendAdvancedStateCreateInfoEXT' into it. The 'VkPipelineColorBlendAdvancedStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineColorBlendAdvancedStateCreateInfoEXT :: PipelineColorBlendAdvancedStateCreateInfoEXT -> (VkPipelineColorBlendAdvancedStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineColorBlendAdvancedStateCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineColorBlendAdvancedStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineColorBlendAdvancedStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT pPNext (boolToBool32 (srcPremultiplied (marshalled :: PipelineColorBlendAdvancedStateCreateInfoEXT))) (boolToBool32 (dstPremultiplied (marshalled :: PipelineColorBlendAdvancedStateCreateInfoEXT))) (blendOverlap (marshalled :: PipelineColorBlendAdvancedStateCreateInfoEXT))))

-- | A function to read a 'VkPipelineColorBlendAdvancedStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineColorBlendAdvancedStateCreateInfoEXT'.
fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT :: VkPipelineColorBlendAdvancedStateCreateInfoEXT -> IO PipelineColorBlendAdvancedStateCreateInfoEXT
fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT c = PipelineColorBlendAdvancedStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (bool32ToBool (vkSrcPremultiplied (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (bool32ToBool (vkDstPremultiplied (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (vkBlendOverlap (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))

instance Zero PipelineColorBlendAdvancedStateCreateInfoEXT where
  zero = PipelineColorBlendAdvancedStateCreateInfoEXT Nothing
                                                      False
                                                      False
                                                      zero

