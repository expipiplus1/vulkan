{-# language CPP #-}
-- No documentation found for Chapter "ResolveModeFlagBits"
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlags
                                                , ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ResolveModeFlags = ResolveModeFlagBits

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
pattern RESOLVE_MODE_NONE            = ResolveModeFlagBits 0x00000000
-- | 'RESOLVE_MODE_SAMPLE_ZERO_BIT' indicates that result of the resolve
-- operation is equal to the value of sample 0.
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001
-- | 'RESOLVE_MODE_AVERAGE_BIT' indicates that result of the resolve
-- operation is the average of the sample values.
pattern RESOLVE_MODE_AVERAGE_BIT     = ResolveModeFlagBits 0x00000002
-- | 'RESOLVE_MODE_MIN_BIT' indicates that result of the resolve operation is
-- the minimum of the sample values.
pattern RESOLVE_MODE_MIN_BIT         = ResolveModeFlagBits 0x00000004
-- | 'RESOLVE_MODE_MAX_BIT' indicates that result of the resolve operation is
-- the maximum of the sample values.
pattern RESOLVE_MODE_MAX_BIT         = ResolveModeFlagBits 0x00000008

conNameResolveModeFlagBits :: String
conNameResolveModeFlagBits = "ResolveModeFlagBits"

enumPrefixResolveModeFlagBits :: String
enumPrefixResolveModeFlagBits = "RESOLVE_MODE_"

showTableResolveModeFlagBits :: [(ResolveModeFlagBits, String)]
showTableResolveModeFlagBits =
  [ (RESOLVE_MODE_NONE           , "NONE")
  , (RESOLVE_MODE_SAMPLE_ZERO_BIT, "SAMPLE_ZERO_BIT")
  , (RESOLVE_MODE_AVERAGE_BIT    , "AVERAGE_BIT")
  , (RESOLVE_MODE_MIN_BIT        , "MIN_BIT")
  , (RESOLVE_MODE_MAX_BIT        , "MAX_BIT")
  ]

instance Show ResolveModeFlagBits where
  showsPrec p e = case lookup e showTableResolveModeFlagBits of
    Just s -> showString enumPrefixResolveModeFlagBits . showString s
    Nothing ->
      let ResolveModeFlagBits x = e
      in  showParen (p >= 11) (showString conNameResolveModeFlagBits . showString " 0x" . showHex x)

instance Read ResolveModeFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixResolveModeFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableResolveModeFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameResolveModeFlagBits)
            v <- step readPrec
            pure (ResolveModeFlagBits v)
          )
    )

