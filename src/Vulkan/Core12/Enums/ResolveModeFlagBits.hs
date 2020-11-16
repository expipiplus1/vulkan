{-# language CPP #-}
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
                                                                     , ..
                                                                     )
                                                , ResolveModeFlags
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkResolveModeFlagBits - Bitmask indicating supported depth and stencil
-- resolve modes
--
-- = See Also
--
-- 'ResolveModeFlags',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
newtype ResolveModeFlagBits = ResolveModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RESOLVE_MODE_NONE' indicates that no resolve operation is done.
pattern RESOLVE_MODE_NONE = ResolveModeFlagBits 0x00000000
-- | 'RESOLVE_MODE_SAMPLE_ZERO_BIT' indicates that result of the resolve
-- operation is equal to the value of sample 0.
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001
-- | 'RESOLVE_MODE_AVERAGE_BIT' indicates that result of the resolve
-- operation is the average of the sample values.
pattern RESOLVE_MODE_AVERAGE_BIT = ResolveModeFlagBits 0x00000002
-- | 'RESOLVE_MODE_MIN_BIT' indicates that result of the resolve operation is
-- the minimum of the sample values.
pattern RESOLVE_MODE_MIN_BIT = ResolveModeFlagBits 0x00000004
-- | 'RESOLVE_MODE_MAX_BIT' indicates that result of the resolve operation is
-- the maximum of the sample values.
pattern RESOLVE_MODE_MAX_BIT = ResolveModeFlagBits 0x00000008

type ResolveModeFlags = ResolveModeFlagBits

instance Show ResolveModeFlagBits where
  showsPrec p = \case
    RESOLVE_MODE_NONE -> showString "RESOLVE_MODE_NONE"
    RESOLVE_MODE_SAMPLE_ZERO_BIT -> showString "RESOLVE_MODE_SAMPLE_ZERO_BIT"
    RESOLVE_MODE_AVERAGE_BIT -> showString "RESOLVE_MODE_AVERAGE_BIT"
    RESOLVE_MODE_MIN_BIT -> showString "RESOLVE_MODE_MIN_BIT"
    RESOLVE_MODE_MAX_BIT -> showString "RESOLVE_MODE_MAX_BIT"
    ResolveModeFlagBits x -> showParen (p >= 11) (showString "ResolveModeFlagBits 0x" . showHex x)

instance Read ResolveModeFlagBits where
  readPrec = parens (choose [("RESOLVE_MODE_NONE", pure RESOLVE_MODE_NONE)
                            , ("RESOLVE_MODE_SAMPLE_ZERO_BIT", pure RESOLVE_MODE_SAMPLE_ZERO_BIT)
                            , ("RESOLVE_MODE_AVERAGE_BIT", pure RESOLVE_MODE_AVERAGE_BIT)
                            , ("RESOLVE_MODE_MIN_BIT", pure RESOLVE_MODE_MIN_BIT)
                            , ("RESOLVE_MODE_MAX_BIT", pure RESOLVE_MODE_MAX_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ResolveModeFlagBits")
                       v <- step readPrec
                       pure (ResolveModeFlagBits v)))

