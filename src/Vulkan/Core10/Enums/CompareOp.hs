{-# language CPP #-}
-- No documentation found for Chapter "CompareOp"
module Vulkan.Core10.Enums.CompareOp  (CompareOp( COMPARE_OP_NEVER
                                                , COMPARE_OP_LESS
                                                , COMPARE_OP_EQUAL
                                                , COMPARE_OP_LESS_OR_EQUAL
                                                , COMPARE_OP_GREATER
                                                , COMPARE_OP_NOT_EQUAL
                                                , COMPARE_OP_GREATER_OR_EQUAL
                                                , COMPARE_OP_ALWAYS
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
-- No documentation found for TopLevel "VkCompareOp"
newtype CompareOp = CompareOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NEVER"
pattern COMPARE_OP_NEVER            = CompareOp 0
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS"
pattern COMPARE_OP_LESS             = CompareOp 1
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_EQUAL"
pattern COMPARE_OP_EQUAL            = CompareOp 2
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS_OR_EQUAL"
pattern COMPARE_OP_LESS_OR_EQUAL    = CompareOp 3
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER"
pattern COMPARE_OP_GREATER          = CompareOp 4
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NOT_EQUAL"
pattern COMPARE_OP_NOT_EQUAL        = CompareOp 5
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER_OR_EQUAL"
pattern COMPARE_OP_GREATER_OR_EQUAL = CompareOp 6
-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_ALWAYS"
pattern COMPARE_OP_ALWAYS           = CompareOp 7
{-# complete COMPARE_OP_NEVER,
             COMPARE_OP_LESS,
             COMPARE_OP_EQUAL,
             COMPARE_OP_LESS_OR_EQUAL,
             COMPARE_OP_GREATER,
             COMPARE_OP_NOT_EQUAL,
             COMPARE_OP_GREATER_OR_EQUAL,
             COMPARE_OP_ALWAYS :: CompareOp #-}

conNameCompareOp :: String
conNameCompareOp = "CompareOp"

enumPrefixCompareOp :: String
enumPrefixCompareOp = "COMPARE_OP_"

showTableCompareOp :: [(CompareOp, String)]
showTableCompareOp =
  [ (COMPARE_OP_NEVER           , "NEVER")
  , (COMPARE_OP_LESS            , "LESS")
  , (COMPARE_OP_EQUAL           , "EQUAL")
  , (COMPARE_OP_LESS_OR_EQUAL   , "LESS_OR_EQUAL")
  , (COMPARE_OP_GREATER         , "GREATER")
  , (COMPARE_OP_NOT_EQUAL       , "NOT_EQUAL")
  , (COMPARE_OP_GREATER_OR_EQUAL, "GREATER_OR_EQUAL")
  , (COMPARE_OP_ALWAYS          , "ALWAYS")
  ]


instance Show CompareOp where
showsPrec = enumShowsPrec enumPrefixCompareOp showTableCompareOp conNameCompareOp (\(CompareOp x) -> x) (showsPrec 11)


instance Read CompareOp where
  readPrec = enumReadPrec enumPrefixCompareOp showTableCompareOp conNameCompareOp CompareOp

