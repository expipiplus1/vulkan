{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheHeaderVersion"
module Vulkan.Core10.Enums.PipelineCacheHeaderVersion  (PipelineCacheHeaderVersion( PIPELINE_CACHE_HEADER_VERSION_ONE
                                                                                  , ..
                                                                                  )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- 'Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Vulkan.Core10.PipelineCache.getPipelineCacheData'
newtype PipelineCacheHeaderVersion = PipelineCacheHeaderVersion Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'PIPELINE_CACHE_HEADER_VERSION_ONE' specifies version one of the
-- pipeline cache.
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = PipelineCacheHeaderVersion 1
{-# complete PIPELINE_CACHE_HEADER_VERSION_ONE :: PipelineCacheHeaderVersion #-}

conNamePipelineCacheHeaderVersion :: String
conNamePipelineCacheHeaderVersion = "PipelineCacheHeaderVersion"

enumPrefixPipelineCacheHeaderVersion :: String
enumPrefixPipelineCacheHeaderVersion = "PIPELINE_CACHE_HEADER_VERSION_ONE"

showTablePipelineCacheHeaderVersion :: [(PipelineCacheHeaderVersion, String)]
showTablePipelineCacheHeaderVersion = [(PIPELINE_CACHE_HEADER_VERSION_ONE, "")]

instance Show PipelineCacheHeaderVersion where
  showsPrec p e = case lookup e showTablePipelineCacheHeaderVersion of
    Just s -> showString enumPrefixPipelineCacheHeaderVersion . showString s
    Nothing ->
      let PipelineCacheHeaderVersion x = e
      in  showParen (p >= 11) (showString conNamePipelineCacheHeaderVersion . showString " " . showsPrec 11 x)

instance Read PipelineCacheHeaderVersion where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineCacheHeaderVersion
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineCacheHeaderVersion)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineCacheHeaderVersion)
            v <- step readPrec
            pure (PipelineCacheHeaderVersion v)
          )
    )

