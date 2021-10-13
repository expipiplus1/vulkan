{-# language CPP #-}
-- No documentation found for Chapter "PointClippingBehavior"
module Vulkan.Core11.Enums.PointClippingBehavior  (PointClippingBehavior( POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
                                                                        , POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
                                                                        , ..
                                                                        )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPointClippingBehavior - Enum specifying the point clipping behavior
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
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

