{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreImportFlagBits"
module Vulkan.Core11.Enums.SemaphoreImportFlagBits  ( SemaphoreImportFlags
                                                    , SemaphoreImportFlagBits( SEMAPHORE_IMPORT_TEMPORARY_BIT
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
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- | VkSemaphoreImportFlagBits - Bitmask specifying additional parameters of
-- semaphore payload import
--
-- = Description
--
-- These bits have the following meanings:
--
-- = See Also
--
-- 'SemaphoreImportFlags'
newtype SemaphoreImportFlagBits = SemaphoreImportFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SEMAPHORE_IMPORT_TEMPORARY_BIT' specifies that the semaphore payload
-- will be imported only temporarily, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
-- regardless of the permanence of @handleType@.
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT = SemaphoreImportFlagBits 0x00000001

instance Show SemaphoreImportFlagBits where
  showsPrec p = \case
    SEMAPHORE_IMPORT_TEMPORARY_BIT -> showString "SEMAPHORE_IMPORT_TEMPORARY_BIT"
    SemaphoreImportFlagBits x -> showParen (p >= 11) (showString "SemaphoreImportFlagBits 0x" . showHex x)

instance Read SemaphoreImportFlagBits where
  readPrec = parens (choose [("SEMAPHORE_IMPORT_TEMPORARY_BIT", pure SEMAPHORE_IMPORT_TEMPORARY_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SemaphoreImportFlagBits")
                       v <- step readPrec
                       pure (SemaphoreImportFlagBits v)))

