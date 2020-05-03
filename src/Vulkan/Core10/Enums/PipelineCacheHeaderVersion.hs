{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineCacheHeaderVersion  (PipelineCacheHeaderVersion( PIPELINE_CACHE_HEADER_VERSION_ONE
                                                                                  , ..
                                                                                  )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
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

instance Show PipelineCacheHeaderVersion where
  showsPrec p = \case
    PIPELINE_CACHE_HEADER_VERSION_ONE -> showString "PIPELINE_CACHE_HEADER_VERSION_ONE"
    PipelineCacheHeaderVersion x -> showParen (p >= 11) (showString "PipelineCacheHeaderVersion " . showsPrec 11 x)

instance Read PipelineCacheHeaderVersion where
  readPrec = parens (choose [("PIPELINE_CACHE_HEADER_VERSION_ONE", pure PIPELINE_CACHE_HEADER_VERSION_ONE)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCacheHeaderVersion")
                       v <- step readPrec
                       pure (PipelineCacheHeaderVersion v)))

