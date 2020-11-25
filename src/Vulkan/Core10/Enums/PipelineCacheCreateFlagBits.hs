{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheCreateFlagBits"
module Vulkan.Core10.Enums.PipelineCacheCreateFlagBits  ( PipelineCacheCreateFlags
                                                        , PipelineCacheCreateFlagBits( PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT
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
type PipelineCacheCreateFlags = PipelineCacheCreateFlagBits

-- | VkPipelineCacheCreateFlagBits - Bitmask specifying the behavior of the
-- pipeline cache
--
-- = See Also
--
-- 'PipelineCacheCreateFlags'
newtype PipelineCacheCreateFlagBits = PipelineCacheCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT' specifies that
-- all commands that modify the created
-- 'Vulkan.Core10.Handles.PipelineCache' will be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>.
-- When set, the implementation /may/ skip any unnecessary processing
-- needed to support simultaneous modification from multiple threads where
-- allowed.
pattern PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT = PipelineCacheCreateFlagBits 0x00000001

conNamePipelineCacheCreateFlagBits :: String
conNamePipelineCacheCreateFlagBits = "PipelineCacheCreateFlagBits"

enumPrefixPipelineCacheCreateFlagBits :: String
enumPrefixPipelineCacheCreateFlagBits = "PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT"

showTablePipelineCacheCreateFlagBits :: [(PipelineCacheCreateFlagBits, String)]
showTablePipelineCacheCreateFlagBits = [(PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT, "")]

instance Show PipelineCacheCreateFlagBits where
  showsPrec p e = case lookup e showTablePipelineCacheCreateFlagBits of
    Just s -> showString enumPrefixPipelineCacheCreateFlagBits . showString s
    Nothing ->
      let PipelineCacheCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNamePipelineCacheCreateFlagBits . showString " 0x" . showHex x)

instance Read PipelineCacheCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineCacheCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineCacheCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineCacheCreateFlagBits)
            v <- step readPrec
            pure (PipelineCacheCreateFlagBits v)
          )
    )

