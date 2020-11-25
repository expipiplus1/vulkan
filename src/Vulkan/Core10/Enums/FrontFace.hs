{-# language CPP #-}
-- No documentation found for Chapter "FrontFace"
module Vulkan.Core10.Enums.FrontFace  (FrontFace( FRONT_FACE_COUNTER_CLOCKWISE
                                                , FRONT_FACE_CLOCKWISE
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
-- No documentation found for TopLevel "VkFrontFace"
newtype FrontFace = FrontFace Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFrontFace" "VK_FRONT_FACE_COUNTER_CLOCKWISE"
pattern FRONT_FACE_COUNTER_CLOCKWISE = FrontFace 0
-- No documentation found for Nested "VkFrontFace" "VK_FRONT_FACE_CLOCKWISE"
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
showsPrec = enumShowsPrec enumPrefixFrontFace showTableFrontFace conNameFrontFace (\(FrontFace x) -> x) (showsPrec 11)


instance Read FrontFace where
  readPrec = enumReadPrec enumPrefixFrontFace showTableFrontFace conNameFrontFace FrontFace

