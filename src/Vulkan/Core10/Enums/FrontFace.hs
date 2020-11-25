{-# language CPP #-}
-- No documentation found for Chapter "FrontFace"
module Vulkan.Core10.Enums.FrontFace  (FrontFace( FRONT_FACE_COUNTER_CLOCKWISE
                                                , FRONT_FACE_CLOCKWISE
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
-- | VkFrontFace - Interpret polygon front-facing orientation
--
-- = Description
--
-- Any triangle which is not front-facing is back-facing, including
-- zero-area triangles.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetFrontFaceEXT'
newtype FrontFace = FrontFace Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRONT_FACE_COUNTER_CLOCKWISE' specifies that a triangle with positive
-- area is considered front-facing.
pattern FRONT_FACE_COUNTER_CLOCKWISE = FrontFace 0
-- | 'FRONT_FACE_CLOCKWISE' specifies that a triangle with negative area is
-- considered front-facing.
pattern FRONT_FACE_CLOCKWISE         = FrontFace 1
{-# complete FRONT_FACE_COUNTER_CLOCKWISE,
             FRONT_FACE_CLOCKWISE :: FrontFace #-}

conNameFrontFace :: String
conNameFrontFace = "FrontFace"

enumPrefixFrontFace :: String
enumPrefixFrontFace = "FRONT_FACE_C"

showTableFrontFace :: [(FrontFace, String)]
showTableFrontFace = [(FRONT_FACE_COUNTER_CLOCKWISE, "OUNTER_CLOCKWISE"), (FRONT_FACE_CLOCKWISE, "LOCKWISE")]

instance Show FrontFace where
  showsPrec p e = case lookup e showTableFrontFace of
    Just s -> showString enumPrefixFrontFace . showString s
    Nothing ->
      let FrontFace x = e in showParen (p >= 11) (showString conNameFrontFace . showString " " . showsPrec 11 x)

instance Read FrontFace where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixFrontFace
          asum ((\(e, s) -> e <$ string s) <$> showTableFrontFace)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameFrontFace)
            v <- step readPrec
            pure (FrontFace v)
          )
    )

