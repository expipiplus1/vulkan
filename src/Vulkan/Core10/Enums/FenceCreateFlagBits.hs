{-# language CPP #-}
-- No documentation found for Chapter "FenceCreateFlagBits"
module Vulkan.Core10.Enums.FenceCreateFlagBits  ( FenceCreateFlags
                                                , FenceCreateFlagBits( FENCE_CREATE_SIGNALED_BIT
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
type FenceCreateFlags = FenceCreateFlagBits

-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'FenceCreateFlags'
newtype FenceCreateFlagBits = FenceCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FENCE_CREATE_SIGNALED_BIT' specifies that the fence object is created
-- in the signaled state. Otherwise, it is created in the unsignaled state.
pattern FENCE_CREATE_SIGNALED_BIT = FenceCreateFlagBits 0x00000001

conNameFenceCreateFlagBits :: String
conNameFenceCreateFlagBits = "FenceCreateFlagBits"

enumPrefixFenceCreateFlagBits :: String
enumPrefixFenceCreateFlagBits = "FENCE_CREATE_SIGNALED_BIT"

showTableFenceCreateFlagBits :: [(FenceCreateFlagBits, String)]
showTableFenceCreateFlagBits = [(FENCE_CREATE_SIGNALED_BIT, "")]

instance Show FenceCreateFlagBits where
  showsPrec p e = case lookup e showTableFenceCreateFlagBits of
    Just s -> showString enumPrefixFenceCreateFlagBits . showString s
    Nothing ->
      let FenceCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameFenceCreateFlagBits . showString " 0x" . showHex x)

instance Read FenceCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixFenceCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableFenceCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameFenceCreateFlagBits)
            v <- step readPrec
            pure (FenceCreateFlagBits v)
          )
    )

