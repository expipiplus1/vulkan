{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.FrontFace  (FrontFace( FRONT_FACE_COUNTER_CLOCKWISE
                                                         , FRONT_FACE_CLOCKWISE
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
-- | VkFrontFace - Interpret polygon front-facing orientation
--
-- = Description
--
-- Any triangle which is not front-facing is back-facing, including
-- zero-area triangles.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
newtype FrontFace = FrontFace Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRONT_FACE_COUNTER_CLOCKWISE' specifies that a triangle with positive
-- area is considered front-facing.
pattern FRONT_FACE_COUNTER_CLOCKWISE = FrontFace 0
-- | 'FRONT_FACE_CLOCKWISE' specifies that a triangle with negative area is
-- considered front-facing.
pattern FRONT_FACE_CLOCKWISE = FrontFace 1
{-# complete FRONT_FACE_COUNTER_CLOCKWISE,
             FRONT_FACE_CLOCKWISE :: FrontFace #-}

instance Show FrontFace where
  showsPrec p = \case
    FRONT_FACE_COUNTER_CLOCKWISE -> showString "FRONT_FACE_COUNTER_CLOCKWISE"
    FRONT_FACE_CLOCKWISE -> showString "FRONT_FACE_CLOCKWISE"
    FrontFace x -> showParen (p >= 11) (showString "FrontFace " . showsPrec 11 x)

instance Read FrontFace where
  readPrec = parens (choose [("FRONT_FACE_COUNTER_CLOCKWISE", pure FRONT_FACE_COUNTER_CLOCKWISE)
                            , ("FRONT_FACE_CLOCKWISE", pure FRONT_FACE_CLOCKWISE)]
                     +++
                     prec 10 (do
                       expectP (Ident "FrontFace")
                       v <- step readPrec
                       pure (FrontFace v)))

