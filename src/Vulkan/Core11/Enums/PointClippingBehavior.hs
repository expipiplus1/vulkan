{-# language CPP #-}
-- No documentation found for Chapter "PointClippingBehavior"
module Vulkan.Core11.Enums.PointClippingBehavior  (PointClippingBehavior( POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
                                                                        , POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
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
-- | VkPointClippingBehavior - Enum specifying the point clipping behavior
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PhysicalDevicePointClippingProperties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties'
newtype PointClippingBehavior = PointClippingBehavior Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES' specifies that the primitive
-- is discarded if the vertex lies outside any clip plane, including the
-- planes bounding the view volume.
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES       = PointClippingBehavior 0
-- | 'POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY' specifies that the
-- primitive is discarded only if the vertex lies outside any user clip
-- plane.
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = PointClippingBehavior 1
{-# complete POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES,
             POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: PointClippingBehavior #-}

conNamePointClippingBehavior :: String
conNamePointClippingBehavior = "PointClippingBehavior"

enumPrefixPointClippingBehavior :: String
enumPrefixPointClippingBehavior = "POINT_CLIPPING_BEHAVIOR_"

showTablePointClippingBehavior :: [(PointClippingBehavior, String)]
showTablePointClippingBehavior =
  [ (POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES      , "ALL_CLIP_PLANES")
  , (POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY, "USER_CLIP_PLANES_ONLY")
  ]

instance Show PointClippingBehavior where
  showsPrec p e = case lookup e showTablePointClippingBehavior of
    Just s -> showString enumPrefixPointClippingBehavior . showString s
    Nothing ->
      let PointClippingBehavior x = e
      in  showParen (p >= 11) (showString conNamePointClippingBehavior . showString " " . showsPrec 11 x)

instance Read PointClippingBehavior where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPointClippingBehavior
          asum ((\(e, s) -> e <$ string s) <$> showTablePointClippingBehavior)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePointClippingBehavior)
            v <- step readPrec
            pure (PointClippingBehavior v)
          )
    )

