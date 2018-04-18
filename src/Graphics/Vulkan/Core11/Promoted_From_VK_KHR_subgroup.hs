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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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

-- | 
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

-- | Basic subgroup operations
pattern VK_SUBGROUP_FEATURE_BASIC_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_BASIC_BIT = VkSubgroupFeatureFlagBits 0x00000001

-- | Vote subgroup operations
pattern VK_SUBGROUP_FEATURE_VOTE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_VOTE_BIT = VkSubgroupFeatureFlagBits 0x00000002

-- | Arithmetic subgroup operations
pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT = VkSubgroupFeatureFlagBits 0x00000004

-- | Ballot subgroup operations
pattern VK_SUBGROUP_FEATURE_BALLOT_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_BALLOT_BIT = VkSubgroupFeatureFlagBits 0x00000008

-- | Shuffle subgroup operations
pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT = VkSubgroupFeatureFlagBits 0x00000010

-- | Shuffle relative subgroup operations
pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = VkSubgroupFeatureFlagBits 0x00000020

-- | Clustered subgroup operations
pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT = VkSubgroupFeatureFlagBits 0x00000040

-- | Quad subgroup operations
pattern VK_SUBGROUP_FEATURE_QUAD_BIT :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_QUAD_BIT = VkSubgroupFeatureFlagBits 0x00000080
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES = VkStructureType 1000094000
-- | TODO: Struct comments
data VkPhysicalDeviceSubgroupProperties = VkPhysicalDeviceSubgroupProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSubgroupSize :: Word32
  , vkSupportedStages :: VkShaderStageFlags
  , vkSupportedOperations :: VkSubgroupFeatureFlags
  , vkQuadOperationsInAllStages :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 16) (vkSubgroupSize (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 20) (vkSupportedStages (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 24) (vkSupportedOperations (poked :: VkPhysicalDeviceSubgroupProperties))
                *> poke (ptr `plusPtr` 28) (vkQuadOperationsInAllStages (poked :: VkPhysicalDeviceSubgroupProperties))
type VkSubgroupFeatureFlags = VkSubgroupFeatureFlagBits
