{-# language CPP #-}
-- No documentation found for Chapter "FenceImportFlagBits"
module Vulkan.Core11.Enums.FenceImportFlagBits  ( FenceImportFlags
                                                , FenceImportFlagBits( FENCE_IMPORT_TEMPORARY_BIT
                                                                     , ..
                                                                     )
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

instance Show FenceImportFlagBits where
  showsPrec p = \case
    FENCE_IMPORT_TEMPORARY_BIT -> showString "FENCE_IMPORT_TEMPORARY_BIT"
    FenceImportFlagBits x -> showParen (p >= 11) (showString "FenceImportFlagBits 0x" . showHex x)

instance Read FenceImportFlagBits where
  readPrec = parens (choose [("FENCE_IMPORT_TEMPORARY_BIT", pure FENCE_IMPORT_TEMPORARY_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "FenceImportFlagBits")
                       v <- step readPrec
                       pure (FenceImportFlagBits v)))

