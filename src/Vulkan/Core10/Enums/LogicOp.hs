{-# language CPP #-}
-- No documentation found for Chapter "LogicOp"
module Vulkan.Core10.Enums.LogicOp  (LogicOp( LOGIC_OP_CLEAR
                                            , LOGIC_OP_AND
                                            , LOGIC_OP_AND_REVERSE
                                            , LOGIC_OP_COPY
                                            , LOGIC_OP_AND_INVERTED
                                            , LOGIC_OP_NO_OP
                                            , LOGIC_OP_XOR
                                            , LOGIC_OP_OR
                                            , LOGIC_OP_NOR
                                            , LOGIC_OP_EQUIVALENT
                                            , LOGIC_OP_INVERT
                                            , LOGIC_OP_OR_REVERSE
                                            , LOGIC_OP_COPY_INVERTED
                                            , LOGIC_OP_OR_INVERTED
                                            , LOGIC_OP_NAND
                                            , LOGIC_OP_SET
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
-- No documentation found for TopLevel "VkLogicOp"
newtype LogicOp = LogicOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_CLEAR"
pattern LOGIC_OP_CLEAR         = LogicOp 0
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND"
pattern LOGIC_OP_AND           = LogicOp 1
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND_REVERSE"
pattern LOGIC_OP_AND_REVERSE   = LogicOp 2
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_COPY"
pattern LOGIC_OP_COPY          = LogicOp 3
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND_INVERTED"
pattern LOGIC_OP_AND_INVERTED  = LogicOp 4
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NO_OP"
pattern LOGIC_OP_NO_OP         = LogicOp 5
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_XOR"
pattern LOGIC_OP_XOR           = LogicOp 6
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR"
pattern LOGIC_OP_OR            = LogicOp 7
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NOR"
pattern LOGIC_OP_NOR           = LogicOp 8
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_EQUIVALENT"
pattern LOGIC_OP_EQUIVALENT    = LogicOp 9
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_INVERT"
pattern LOGIC_OP_INVERT        = LogicOp 10
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR_REVERSE"
pattern LOGIC_OP_OR_REVERSE    = LogicOp 11
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_COPY_INVERTED"
pattern LOGIC_OP_COPY_INVERTED = LogicOp 12
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR_INVERTED"
pattern LOGIC_OP_OR_INVERTED   = LogicOp 13
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NAND"
pattern LOGIC_OP_NAND          = LogicOp 14
-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_SET"
pattern LOGIC_OP_SET           = LogicOp 15
{-# complete LOGIC_OP_CLEAR,
             LOGIC_OP_AND,
             LOGIC_OP_AND_REVERSE,
             LOGIC_OP_COPY,
             LOGIC_OP_AND_INVERTED,
             LOGIC_OP_NO_OP,
             LOGIC_OP_XOR,
             LOGIC_OP_OR,
             LOGIC_OP_NOR,
             LOGIC_OP_EQUIVALENT,
             LOGIC_OP_INVERT,
             LOGIC_OP_OR_REVERSE,
             LOGIC_OP_COPY_INVERTED,
             LOGIC_OP_OR_INVERTED,
             LOGIC_OP_NAND,
             LOGIC_OP_SET :: LogicOp #-}

conNameLogicOp :: String
conNameLogicOp = "LogicOp"

enumPrefixLogicOp :: String
enumPrefixLogicOp = "LOGIC_OP_"

showTableLogicOp :: [(LogicOp, String)]
showTableLogicOp =
  [ (LOGIC_OP_CLEAR        , "CLEAR")
  , (LOGIC_OP_AND          , "AND")
  , (LOGIC_OP_AND_REVERSE  , "AND_REVERSE")
  , (LOGIC_OP_COPY         , "COPY")
  , (LOGIC_OP_AND_INVERTED , "AND_INVERTED")
  , (LOGIC_OP_NO_OP        , "NO_OP")
  , (LOGIC_OP_XOR          , "XOR")
  , (LOGIC_OP_OR           , "OR")
  , (LOGIC_OP_NOR          , "NOR")
  , (LOGIC_OP_EQUIVALENT   , "EQUIVALENT")
  , (LOGIC_OP_INVERT       , "INVERT")
  , (LOGIC_OP_OR_REVERSE   , "OR_REVERSE")
  , (LOGIC_OP_COPY_INVERTED, "COPY_INVERTED")
  , (LOGIC_OP_OR_INVERTED  , "OR_INVERTED")
  , (LOGIC_OP_NAND         , "NAND")
  , (LOGIC_OP_SET          , "SET")
  ]


instance Show LogicOp where
showsPrec = enumShowsPrec enumPrefixLogicOp showTableLogicOp conNameLogicOp (\(LogicOp x) -> x) (showsPrec 11)


instance Read LogicOp where
  readPrec = enumReadPrec enumPrefixLogicOp showTableLogicOp conNameLogicOp LogicOp

