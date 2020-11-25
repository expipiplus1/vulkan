{-# language CPP #-}
-- No documentation found for Chapter "FenceImportFlagBits"
module Vulkan.Core11.Enums.FenceImportFlagBits  ( FenceImportFlags
                                                , FenceImportFlagBits( FENCE_IMPORT_TEMPORARY_BIT
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
type FenceImportFlags = FenceImportFlagBits

-- | VkFenceImportFlagBits - Bitmask specifying additional parameters of
-- fence payload import
--
-- = See Also
--
-- 'FenceImportFlags'
newtype FenceImportFlagBits = FenceImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FENCE_IMPORT_TEMPORARY_BIT' specifies that the fence payload will be
-- imported only temporarily, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>,
-- regardless of the permanence of @handleType@.
pattern FENCE_IMPORT_TEMPORARY_BIT = FenceImportFlagBits 0x00000001

conNameFenceImportFlagBits :: String
conNameFenceImportFlagBits = "FenceImportFlagBits"

enumPrefixFenceImportFlagBits :: String
enumPrefixFenceImportFlagBits = "FENCE_IMPORT_TEMPORARY_BIT"

showTableFenceImportFlagBits :: [(FenceImportFlagBits, String)]
showTableFenceImportFlagBits = [(FENCE_IMPORT_TEMPORARY_BIT, "")]

instance Show FenceImportFlagBits where
  showsPrec p e = case lookup e showTableFenceImportFlagBits of
    Just s -> showString enumPrefixFenceImportFlagBits . showString s
    Nothing ->
      let FenceImportFlagBits x = e
      in  showParen (p >= 11) (showString conNameFenceImportFlagBits . showString " 0x" . showHex x)

instance Read FenceImportFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixFenceImportFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableFenceImportFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameFenceImportFlagBits)
            v <- step readPrec
            pure (FenceImportFlagBits v)
          )
    )

