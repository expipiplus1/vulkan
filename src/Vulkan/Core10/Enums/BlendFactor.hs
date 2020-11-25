{-# language CPP #-}
-- No documentation found for Chapter "BlendFactor"
module Vulkan.Core10.Enums.BlendFactor  (BlendFactor( BLEND_FACTOR_ZERO
                                                    , BLEND_FACTOR_ONE
                                                    , BLEND_FACTOR_SRC_COLOR
                                                    , BLEND_FACTOR_ONE_MINUS_SRC_COLOR
                                                    , BLEND_FACTOR_DST_COLOR
                                                    , BLEND_FACTOR_ONE_MINUS_DST_COLOR
                                                    , BLEND_FACTOR_SRC_ALPHA
                                                    , BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
                                                    , BLEND_FACTOR_DST_ALPHA
                                                    , BLEND_FACTOR_ONE_MINUS_DST_ALPHA
                                                    , BLEND_FACTOR_CONSTANT_COLOR
                                                    , BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
                                                    , BLEND_FACTOR_CONSTANT_ALPHA
                                                    , BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
                                                    , BLEND_FACTOR_SRC_ALPHA_SATURATE
                                                    , BLEND_FACTOR_SRC1_COLOR
                                                    , BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
                                                    , BLEND_FACTOR_SRC1_ALPHA
                                                    , BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
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
-- No documentation found for TopLevel "VkBlendFactor"
newtype BlendFactor = BlendFactor Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ZERO"
pattern BLEND_FACTOR_ZERO                     = BlendFactor 0
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE"
pattern BLEND_FACTOR_ONE                      = BlendFactor 1
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_COLOR"
pattern BLEND_FACTOR_SRC_COLOR                = BlendFactor 2
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_SRC_COLOR      = BlendFactor 3
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_DST_COLOR"
pattern BLEND_FACTOR_DST_COLOR                = BlendFactor 4
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_DST_COLOR      = BlendFactor 5
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_ALPHA"
pattern BLEND_FACTOR_SRC_ALPHA                = BlendFactor 6
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_SRC_ALPHA      = BlendFactor 7
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_DST_ALPHA"
pattern BLEND_FACTOR_DST_ALPHA                = BlendFactor 8
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_DST_ALPHA      = BlendFactor 9
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_CONSTANT_COLOR"
pattern BLEND_FACTOR_CONSTANT_COLOR           = BlendFactor 10
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = BlendFactor 11
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_CONSTANT_ALPHA"
pattern BLEND_FACTOR_CONSTANT_ALPHA           = BlendFactor 12
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = BlendFactor 13
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
pattern BLEND_FACTOR_SRC_ALPHA_SATURATE       = BlendFactor 14
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC1_COLOR"
pattern BLEND_FACTOR_SRC1_COLOR               = BlendFactor 15
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_SRC1_COLOR     = BlendFactor 16
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC1_ALPHA"
pattern BLEND_FACTOR_SRC1_ALPHA               = BlendFactor 17
-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA     = BlendFactor 18
{-# complete BLEND_FACTOR_ZERO,
             BLEND_FACTOR_ONE,
             BLEND_FACTOR_SRC_COLOR,
             BLEND_FACTOR_ONE_MINUS_SRC_COLOR,
             BLEND_FACTOR_DST_COLOR,
             BLEND_FACTOR_ONE_MINUS_DST_COLOR,
             BLEND_FACTOR_SRC_ALPHA,
             BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
             BLEND_FACTOR_DST_ALPHA,
             BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
             BLEND_FACTOR_CONSTANT_COLOR,
             BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR,
             BLEND_FACTOR_CONSTANT_ALPHA,
             BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA,
             BLEND_FACTOR_SRC_ALPHA_SATURATE,
             BLEND_FACTOR_SRC1_COLOR,
             BLEND_FACTOR_ONE_MINUS_SRC1_COLOR,
             BLEND_FACTOR_SRC1_ALPHA,
             BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: BlendFactor #-}

conNameBlendFactor :: String
conNameBlendFactor = "BlendFactor"

enumPrefixBlendFactor :: String
enumPrefixBlendFactor = "BLEND_FACTOR_"

showTableBlendFactor :: [(BlendFactor, String)]
showTableBlendFactor =
  [ (BLEND_FACTOR_ZERO                    , "ZERO")
  , (BLEND_FACTOR_ONE                     , "ONE")
  , (BLEND_FACTOR_SRC_COLOR               , "SRC_COLOR")
  , (BLEND_FACTOR_ONE_MINUS_SRC_COLOR     , "ONE_MINUS_SRC_COLOR")
  , (BLEND_FACTOR_DST_COLOR               , "DST_COLOR")
  , (BLEND_FACTOR_ONE_MINUS_DST_COLOR     , "ONE_MINUS_DST_COLOR")
  , (BLEND_FACTOR_SRC_ALPHA               , "SRC_ALPHA")
  , (BLEND_FACTOR_ONE_MINUS_SRC_ALPHA     , "ONE_MINUS_SRC_ALPHA")
  , (BLEND_FACTOR_DST_ALPHA               , "DST_ALPHA")
  , (BLEND_FACTOR_ONE_MINUS_DST_ALPHA     , "ONE_MINUS_DST_ALPHA")
  , (BLEND_FACTOR_CONSTANT_COLOR          , "CONSTANT_COLOR")
  , (BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR, "ONE_MINUS_CONSTANT_COLOR")
  , (BLEND_FACTOR_CONSTANT_ALPHA          , "CONSTANT_ALPHA")
  , (BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA, "ONE_MINUS_CONSTANT_ALPHA")
  , (BLEND_FACTOR_SRC_ALPHA_SATURATE      , "SRC_ALPHA_SATURATE")
  , (BLEND_FACTOR_SRC1_COLOR              , "SRC1_COLOR")
  , (BLEND_FACTOR_ONE_MINUS_SRC1_COLOR    , "ONE_MINUS_SRC1_COLOR")
  , (BLEND_FACTOR_SRC1_ALPHA              , "SRC1_ALPHA")
  , (BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA    , "ONE_MINUS_SRC1_ALPHA")
  ]


instance Show BlendFactor where
showsPrec =
  enumShowsPrec enumPrefixBlendFactor showTableBlendFactor conNameBlendFactor (\(BlendFactor x) -> x) (showsPrec 11)


instance Read BlendFactor where
  readPrec = enumReadPrec enumPrefixBlendFactor showTableBlendFactor conNameBlendFactor BlendFactor

