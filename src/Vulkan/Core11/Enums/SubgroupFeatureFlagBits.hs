{-# language CPP #-}
module Vulkan.Core11.Enums.SubgroupFeatureFlagBits  ( SubgroupFeatureFlagBits( SUBGROUP_FEATURE_BASIC_BIT
                                                                             , SUBGROUP_FEATURE_VOTE_BIT
                                                                             , SUBGROUP_FEATURE_ARITHMETIC_BIT
                                                                             , SUBGROUP_FEATURE_BALLOT_BIT
                                                                             , SUBGROUP_FEATURE_SHUFFLE_BIT
                                                                             , SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
                                                                             , SUBGROUP_FEATURE_CLUSTERED_BIT
                                                                             , SUBGROUP_FEATURE_QUAD_BIT
                                                                             , SUBGROUP_FEATURE_PARTITIONED_BIT_NV
                                                                             , ..
                                                                             )
                                                    , SubgroupFeatureFlags
                                                    ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkSubgroupFeatureFlagBits - Enum describing what group operations are
-- supported with subgroup scope
--
-- = See Also
--
-- 'SubgroupFeatureFlags'
newtype SubgroupFeatureFlagBits = SubgroupFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'SUBGROUP_FEATURE_BASIC_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniform@ capability.
pattern SUBGROUP_FEATURE_BASIC_BIT = SubgroupFeatureFlagBits 0x00000001
-- | 'SUBGROUP_FEATURE_VOTE_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniformVote@ capability.
pattern SUBGROUP_FEATURE_VOTE_BIT = SubgroupFeatureFlagBits 0x00000002
-- | 'SUBGROUP_FEATURE_ARITHMETIC_BIT' specifies the device will accept
-- SPIR-V shader modules containing the @GroupNonUniformArithmetic@
-- capability.
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT = SubgroupFeatureFlagBits 0x00000004
-- | 'SUBGROUP_FEATURE_BALLOT_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniformBallot@ capability.
pattern SUBGROUP_FEATURE_BALLOT_BIT = SubgroupFeatureFlagBits 0x00000008
-- | 'SUBGROUP_FEATURE_SHUFFLE_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniformShuffle@ capability.
pattern SUBGROUP_FEATURE_SHUFFLE_BIT = SubgroupFeatureFlagBits 0x00000010
-- | 'SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT' specifies the device will accept
-- SPIR-V shader modules containing the @GroupNonUniformShuffleRelative@
-- capability.
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = SubgroupFeatureFlagBits 0x00000020
-- | 'SUBGROUP_FEATURE_CLUSTERED_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniformClustered@ capability.
pattern SUBGROUP_FEATURE_CLUSTERED_BIT = SubgroupFeatureFlagBits 0x00000040
-- | 'SUBGROUP_FEATURE_QUAD_BIT' specifies the device will accept SPIR-V
-- shader modules containing the @GroupNonUniformQuad@ capability.
pattern SUBGROUP_FEATURE_QUAD_BIT = SubgroupFeatureFlagBits 0x00000080
-- | 'SUBGROUP_FEATURE_PARTITIONED_BIT_NV' specifies the device will accept
-- SPIR-V shader modules containing the @GroupNonUniformPartitionedNV@
-- capability.
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV = SubgroupFeatureFlagBits 0x00000100

type SubgroupFeatureFlags = SubgroupFeatureFlagBits

instance Show SubgroupFeatureFlagBits where
  showsPrec p = \case
    SUBGROUP_FEATURE_BASIC_BIT -> showString "SUBGROUP_FEATURE_BASIC_BIT"
    SUBGROUP_FEATURE_VOTE_BIT -> showString "SUBGROUP_FEATURE_VOTE_BIT"
    SUBGROUP_FEATURE_ARITHMETIC_BIT -> showString "SUBGROUP_FEATURE_ARITHMETIC_BIT"
    SUBGROUP_FEATURE_BALLOT_BIT -> showString "SUBGROUP_FEATURE_BALLOT_BIT"
    SUBGROUP_FEATURE_SHUFFLE_BIT -> showString "SUBGROUP_FEATURE_SHUFFLE_BIT"
    SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT -> showString "SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
    SUBGROUP_FEATURE_CLUSTERED_BIT -> showString "SUBGROUP_FEATURE_CLUSTERED_BIT"
    SUBGROUP_FEATURE_QUAD_BIT -> showString "SUBGROUP_FEATURE_QUAD_BIT"
    SUBGROUP_FEATURE_PARTITIONED_BIT_NV -> showString "SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
    SubgroupFeatureFlagBits x -> showParen (p >= 11) (showString "SubgroupFeatureFlagBits 0x" . showHex x)

instance Read SubgroupFeatureFlagBits where
  readPrec = parens (choose [("SUBGROUP_FEATURE_BASIC_BIT", pure SUBGROUP_FEATURE_BASIC_BIT)
                            , ("SUBGROUP_FEATURE_VOTE_BIT", pure SUBGROUP_FEATURE_VOTE_BIT)
                            , ("SUBGROUP_FEATURE_ARITHMETIC_BIT", pure SUBGROUP_FEATURE_ARITHMETIC_BIT)
                            , ("SUBGROUP_FEATURE_BALLOT_BIT", pure SUBGROUP_FEATURE_BALLOT_BIT)
                            , ("SUBGROUP_FEATURE_SHUFFLE_BIT", pure SUBGROUP_FEATURE_SHUFFLE_BIT)
                            , ("SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT", pure SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT)
                            , ("SUBGROUP_FEATURE_CLUSTERED_BIT", pure SUBGROUP_FEATURE_CLUSTERED_BIT)
                            , ("SUBGROUP_FEATURE_QUAD_BIT", pure SUBGROUP_FEATURE_QUAD_BIT)
                            , ("SUBGROUP_FEATURE_PARTITIONED_BIT_NV", pure SUBGROUP_FEATURE_PARTITIONED_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "SubgroupFeatureFlagBits")
                       v <- step readPrec
                       pure (SubgroupFeatureFlagBits v)))

