{-# language CPP #-}
module Vulkan.Core11.Enums.PointClippingBehavior  (PointClippingBehavior( POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
                                                                        , POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
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
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = PointClippingBehavior 0
-- | 'POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY' specifies that the
-- primitive is discarded only if the vertex lies outside any user clip
-- plane.
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = PointClippingBehavior 1
{-# complete POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES,
             POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: PointClippingBehavior #-}

instance Show PointClippingBehavior where
  showsPrec p = \case
    POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES -> showString "POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES"
    POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY -> showString "POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY"
    PointClippingBehavior x -> showParen (p >= 11) (showString "PointClippingBehavior " . showsPrec 11 x)

instance Read PointClippingBehavior where
  readPrec = parens (choose [("POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES", pure POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES)
                            , ("POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY", pure POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY)]
                     +++
                     prec 10 (do
                       expectP (Ident "PointClippingBehavior")
                       v <- step readPrec
                       pure (PointClippingBehavior v)))

