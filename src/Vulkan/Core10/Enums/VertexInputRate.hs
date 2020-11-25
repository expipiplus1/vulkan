{-# language CPP #-}
-- No documentation found for Chapter "VertexInputRate"
module Vulkan.Core10.Enums.VertexInputRate  (VertexInputRate( VERTEX_INPUT_RATE_VERTEX
                                                            , VERTEX_INPUT_RATE_INSTANCE
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
-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'
newtype VertexInputRate = VertexInputRate Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VERTEX_INPUT_RATE_VERTEX' specifies that vertex attribute addressing is
-- a function of the vertex index.
pattern VERTEX_INPUT_RATE_VERTEX   = VertexInputRate 0
-- | 'VERTEX_INPUT_RATE_INSTANCE' specifies that vertex attribute addressing
-- is a function of the instance index.
pattern VERTEX_INPUT_RATE_INSTANCE = VertexInputRate 1
{-# complete VERTEX_INPUT_RATE_VERTEX,
             VERTEX_INPUT_RATE_INSTANCE :: VertexInputRate #-}

conNameVertexInputRate :: String
conNameVertexInputRate = "VertexInputRate"

enumPrefixVertexInputRate :: String
enumPrefixVertexInputRate = "VERTEX_INPUT_RATE_"

showTableVertexInputRate :: [(VertexInputRate, String)]
showTableVertexInputRate = [(VERTEX_INPUT_RATE_VERTEX, "VERTEX"), (VERTEX_INPUT_RATE_INSTANCE, "INSTANCE")]

instance Show VertexInputRate where
  showsPrec p e = case lookup e showTableVertexInputRate of
    Just s -> showString enumPrefixVertexInputRate . showString s
    Nothing ->
      let VertexInputRate x = e
      in  showParen (p >= 11) (showString conNameVertexInputRate . showString " " . showsPrec 11 x)

instance Read VertexInputRate where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixVertexInputRate
          asum ((\(e, s) -> e <$ string s) <$> showTableVertexInputRate)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameVertexInputRate)
            v <- step readPrec
            pure (VertexInputRate v)
          )
    )

