{-# language CPP #-}
-- No documentation found for Chapter "PointClippingBehavior"
module Vulkan.Core11.Enums.PointClippingBehavior  (PointClippingBehavior( POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
                                                                        , POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
                                                                        , ..
                                                                        )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkPointClippingBehavior"
newtype PointClippingBehavior = PointClippingBehavior Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPointClippingBehavior" "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES"
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES       = PointClippingBehavior 0
-- No documentation found for Nested "VkPointClippingBehavior" "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY"
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
showsPrec = enumShowsPrec enumPrefixPointClippingBehavior
                          showTablePointClippingBehavior
                          conNamePointClippingBehavior
                          (\(PointClippingBehavior x) -> x)
                          (showsPrec 11)


instance Read PointClippingBehavior where
  readPrec = enumReadPrec enumPrefixPointClippingBehavior
                          showTablePointClippingBehavior
                          conNamePointClippingBehavior
                          PointClippingBehavior

