{-# language CPP #-}
-- No documentation found for Chapter "FrontFace"
module Vulkan.Core10.Enums.FrontFace  (FrontFace( FRONT_FACE_COUNTER_CLOCKWISE
                                                , FRONT_FACE_CLOCKWISE
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

-- | VkFrontFace - Interpret polygon front-facing orientation
--
-- = Description
--
-- Any triangle which is not front-facing is back-facing, including
-- zero-area triangles.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  showsPrec =
    enumShowsPrec enumPrefixFrontFace showTableFrontFace conNameFrontFace (\(FrontFace x) -> x) (showsPrec 11)

instance Read FrontFace where
  readPrec = enumReadPrec enumPrefixFrontFace showTableFrontFace conNameFrontFace FrontFace

