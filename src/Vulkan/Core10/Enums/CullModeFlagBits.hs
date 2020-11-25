{-# language CPP #-}
-- No documentation found for Chapter "CullModeFlagBits"
module Vulkan.Core10.Enums.CullModeFlagBits  ( CullModeFlags
                                             , CullModeFlagBits( CULL_MODE_NONE
                                                               , CULL_MODE_FRONT_BIT
                                                               , CULL_MODE_BACK_BIT
                                                               , CULL_MODE_FRONT_AND_BACK
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
type CullModeFlags = CullModeFlagBits

-- | VkCullModeFlagBits - Bitmask controlling triangle culling
--
-- = Description
--
-- Following culling, fragments are produced for any triangles which have
-- not been discarded.
--
-- = See Also
--
-- 'CullModeFlags'
newtype CullModeFlagBits = CullModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'CULL_MODE_NONE' specifies that no triangles are discarded
pattern CULL_MODE_NONE           = CullModeFlagBits 0x00000000
-- | 'CULL_MODE_FRONT_BIT' specifies that front-facing triangles are
-- discarded
pattern CULL_MODE_FRONT_BIT      = CullModeFlagBits 0x00000001
-- | 'CULL_MODE_BACK_BIT' specifies that back-facing triangles are discarded
pattern CULL_MODE_BACK_BIT       = CullModeFlagBits 0x00000002
-- | 'CULL_MODE_FRONT_AND_BACK' specifies that all triangles are discarded.
pattern CULL_MODE_FRONT_AND_BACK = CullModeFlagBits 0x00000003

conNameCullModeFlagBits :: String
conNameCullModeFlagBits = "CullModeFlagBits"

enumPrefixCullModeFlagBits :: String
enumPrefixCullModeFlagBits = "CULL_MODE_"

showTableCullModeFlagBits :: [(CullModeFlagBits, String)]
showTableCullModeFlagBits =
  [ (CULL_MODE_NONE          , "NONE")
  , (CULL_MODE_FRONT_BIT     , "FRONT_BIT")
  , (CULL_MODE_BACK_BIT      , "BACK_BIT")
  , (CULL_MODE_FRONT_AND_BACK, "FRONT_AND_BACK")
  ]

instance Show CullModeFlagBits where
  showsPrec p e = case lookup e showTableCullModeFlagBits of
    Just s -> showString enumPrefixCullModeFlagBits . showString s
    Nothing ->
      let CullModeFlagBits x = e
      in  showParen (p >= 11) (showString conNameCullModeFlagBits . showString " 0x" . showHex x)

instance Read CullModeFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixCullModeFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableCullModeFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameCullModeFlagBits)
            v <- step readPrec
            pure (CullModeFlagBits v)
          )
    )

