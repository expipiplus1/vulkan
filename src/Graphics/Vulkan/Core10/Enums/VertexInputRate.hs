{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.VertexInputRate  (VertexInputRate( VERTEX_INPUT_RATE_VERTEX
                                                                     , VERTEX_INPUT_RATE_INSTANCE
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
import Graphics.Vulkan.Zero (Zero)
-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VertexInputBindingDescription'
newtype VertexInputRate = VertexInputRate Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VERTEX_INPUT_RATE_VERTEX' specifies that vertex attribute addressing is
-- a function of the vertex index.
pattern VERTEX_INPUT_RATE_VERTEX = VertexInputRate 0
-- | 'VERTEX_INPUT_RATE_INSTANCE' specifies that vertex attribute addressing
-- is a function of the instance index.
pattern VERTEX_INPUT_RATE_INSTANCE = VertexInputRate 1
{-# complete VERTEX_INPUT_RATE_VERTEX,
             VERTEX_INPUT_RATE_INSTANCE :: VertexInputRate #-}

instance Show VertexInputRate where
  showsPrec p = \case
    VERTEX_INPUT_RATE_VERTEX -> showString "VERTEX_INPUT_RATE_VERTEX"
    VERTEX_INPUT_RATE_INSTANCE -> showString "VERTEX_INPUT_RATE_INSTANCE"
    VertexInputRate x -> showParen (p >= 11) (showString "VertexInputRate " . showsPrec 11 x)

instance Read VertexInputRate where
  readPrec = parens (choose [("VERTEX_INPUT_RATE_VERTEX", pure VERTEX_INPUT_RATE_VERTEX)
                            , ("VERTEX_INPUT_RATE_INSTANCE", pure VERTEX_INPUT_RATE_INSTANCE)]
                     +++
                     prec 10 (do
                       expectP (Ident "VertexInputRate")
                       v <- step readPrec
                       pure (VertexInputRate v)))

