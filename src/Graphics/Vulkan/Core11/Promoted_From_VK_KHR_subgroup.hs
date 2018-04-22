{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( VkSubgroupFeatureFlagBits(..)
  , pattern VK_SUBGROUP_FEATURE_BASIC_BIT
  , pattern VK_SUBGROUP_FEATURE_VOTE_BIT
  , pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT
  , pattern VK_SUBGROUP_FEATURE_BALLOT_BIT
  , pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT
  , pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
  , pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT
  , pattern VK_SUBGROUP_FEATURE_QUAD_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  , VkPhysicalDeviceSubgroupProperties(..)
  , VkSubgroupFeatureFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )


-- ** VkSubgroupFeatureFlagBits

-- | VkSubgroupFeatureFlagBits - Enum describing what subgroup operations are
-- supported
--
-- = See Also
--
-- 'VkSubgroupFeatureFlags'
newtype VkSubgroupFeatureFlagBits = VkSubgroupFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSubgroupFeatureFlagBits where
  showsPrec _ VK_SUBGROUP_FEATURE_BASIC_BIT = showString "VK_SUBGROUP_FEATURE_BASIC_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_VOTE_BIT = showString "VK_SUBGROUP_FEATURE_VOTE_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_ARITHMETIC_BIT = showString "VK_SUBGROUP_FEATURE_ARITHMETIC_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_BALLOT_BIT = showString "VK_SUBGROUP_FEATURE_BALLOT_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_SHUFFLE_BIT = showString "VK_SUBGROUP_FEATURE_SHUFFLE_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = showString "VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_CLUSTERED_BIT = showString "VK_SUBGROUP_FEATURE_CLUSTERED_BIT"
  showsPrec _ VK_SUBGROUP_FEATURE_QUAD_BIT = showString "VK_SUBGROUP_FEATURE_QUAD_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSubgroupFeatureFlagBits 0x00000100) = showString "VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
  showsPrec p (VkSubgroupFeatureFlagBits x) = showParen (p >= 11) (showString "VkSubgroupFeatureFlagBits " . showsPrec 11 x)

instance Read VkSubgroupFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_SUBGROUP_FEATURE_BASIC_BIT",            pure VK_SUBGROUP_FEATURE_BASIC_BIT)
                             , ("VK_SUBGROUP_FEATURE_VOTE_BIT",             pure VK_SUBGROUP_FEATURE_VOTE_BIT)
                             , ("VK_SUBGROUP_FEATURE_ARITHMETIC_BIT",       pure VK_SUBGROUP_FEATURE_ARITHMETIC_BIT)
                             , ("VK_SUBGROUP_FEATURE_BALLOT_BIT",           pure VK_SUBGROUP_FEATURE_BALLOT_BIT)
                             , ("VK_SUBGROUP_FEATURE_SHUFFLE_BIT",          pure VK_SUBGROUP_FEATURE_SHUFFLE_BIT)
                             , ("VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT", pure VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT)
                             , ("VK_SUBGROUP_FEATURE_CLUSTERED_BIT",        pure VK_SUBGROUP_FEATURE_CLUSTERED_BIT)
                             , ("VK_SUBGROUP_FEATURE_QUAD_BIT",             pure VK_SUBGROUP_FEATURE_QUAD_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV", pure (VkSubgroupFeatureFlagBits 0x00000100))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubgroupFeatureFlagBits")
                        v <- step readPrec
                        pure (VkSubgroupFeatureFlagBits v)
                        )
                    )

-- | @VK_SUBGROUP_FEATURE_BASIC_BIT@ specifies the device will accept SPIR-V
-- shader modules that contain the @GroupNonUniform@ capability.
pattern VK_SUBGROUP_FEATURE_BASIC_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_BASIC_BIT = VkSubgroupFeatureFlagBits 0x00000001

-- | @VK_SUBGROUP_FEATURE_VOTE_BIT@ specifies the device will accept SPIR-V
-- shader modules that contain the @GroupNonUniformVote@ capability.
pattern VK_SUBGROUP_FEATURE_VOTE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_VOTE_BIT = VkSubgroupFeatureFlagBits 0x00000002

-- | @VK_SUBGROUP_FEATURE_ARITHMETIC_BIT@ specifies the device will accept
-- SPIR-V shader modules that contain the @GroupNonUniformArithmetic@
-- capability.
pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT = VkSubgroupFeatureFlagBits 0x00000004

-- | @VK_SUBGROUP_FEATURE_BALLOT_BIT@ specifies the device will accept SPIR-V
-- shader modules that contain the @GroupNonUniformBallot@ capability.
pattern VK_SUBGROUP_FEATURE_BALLOT_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_BALLOT_BIT = VkSubgroupFeatureFlagBits 0x00000008

-- | @VK_SUBGROUP_FEATURE_SHUFFLE_BIT@ specifies the device will accept
-- SPIR-V shader modules that contain the @GroupNonUniformShuffle@
-- capability.
pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT = VkSubgroupFeatureFlagBits 0x00000010

-- | @VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT@ specifies the device will
-- accept SPIR-V shader modules that contain the
-- @GroupNonUniformShuffleRelative@ capability.
pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = VkSubgroupFeatureFlagBits 0x00000020

-- | @VK_SUBGROUP_FEATURE_CLUSTERED_BIT@ specifies the device will accept
-- SPIR-V shader modules that contain the @GroupNonUniformClustered@
-- capability.
pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT = VkSubgroupFeatureFlagBits 0x00000040

-- | @VK_SUBGROUP_FEATURE_QUAD_BIT@ specifies the device will accept SPIR-V
-- shader modules that contain the @GroupNonUniformQuad@ capability.
pattern VK_SUBGROUP_FEATURE_QUAD_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_QUAD_BIT = VkSubgroupFeatureFlagBits 0x00000080
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES = VkStructureType 1000094000
-- | VkPhysicalDeviceSubgroupProperties - Structure describing subgroup
-- support for an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceSubgroupProperties@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to an extension-specific structure.
--
-- -   @subgroupSize@ is the number of invocations in each subgroup. This
--     will match any
--     [@SubgroupSize@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#interfaces-builtin-variables-sgs)
--     decorated variable used in any shader module created on this device.
--     @subgroupSize@ is at least 1 if any of the physical device’s queues
--     support @VK_QUEUE_GRAPHICS_BIT@ or @VK_QUEUE_COMPUTE_BIT@.
--
-- -   @supportedStages@ is a bitfield of
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' describing
--     the shader stages that subgroup operations are supported in.
--     @supportedStages@ will have the @VK_SHADER_STAGE_COMPUTE_BIT@ bit
--     set if any of any of the physical device’s queues support
--     @VK_QUEUE_COMPUTE_BIT@.
--
-- -   @supportedOperations@ is a bitmask of 'VkSubgroupFeatureFlagBits'
--     specifying the sets of subgroup operations supported on this device.
--     @supportedOperations@ will have the @VK_SUBGROUP_FEATURE_BASIC_BIT@
--     bit set if any of the physical device’s queues support
--     @VK_QUEUE_GRAPHICS_BIT@ or @VK_QUEUE_COMPUTE_BIT@.
--
-- -   @quadOperationsInAllStages@ is a boolean that specifies whether
--     [quad subgroup
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-subgroup-quad)
--     are available in all stages, or are restricted to fragment and
--     compute stages.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES@
--
-- If the @VkPhysicalDeviceSubgroupProperties@ structure is included in the
-- @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkSubgroupFeatureFlags'
data VkPhysicalDeviceSubgroupProperties = VkPhysicalDeviceSubgroupProperties
  { -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "subgroupSize"
  vkSubgroupSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "supportedStages"
  vkSupportedStages :: VkShaderStageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "supportedOperations"
  vkSupportedOperations :: VkSubgroupFeatureFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "quadOperationsInAllStages"
  vkQuadOperationsInAllStages :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSubgroupProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSubgroupProperties <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 16) (vkSubgroupSize (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 20) (vkSupportedStages (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 24) (vkSupportedOperations (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 28) (vkQuadOperationsInAllStages (poked :: VkPhysicalDeviceSubgroupProperties))
-- | VkSubgroupFeatureFlags - Bitmask of VkSubgroupFeatureFlagBits
--
-- = Description
--
-- @VkSubgroupFeatureFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkSubgroupFeatureFlagBits'.
--
-- = See Also
--
-- 'VkPhysicalDeviceSubgroupProperties', 'VkSubgroupFeatureFlagBits'
type VkSubgroupFeatureFlags = VkSubgroupFeatureFlagBits
