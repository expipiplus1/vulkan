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
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkLogicOp - Framebuffer logical operations
--
-- = Description
--
-- The logical operations supported by Vulkan are summarized in the
-- following table in which
--
-- -   ¬ is bitwise invert,
--
-- -   ∧ is bitwise and,
--
-- -   ∨ is bitwise or,
--
-- -   ⊕ is bitwise exclusive or,
--
-- -   s is the fragment’s Rs0, Gs0, Bs0 or As0 component value for the
--     fragment output corresponding to the color attachment being updated,
--     and
--
-- -   d is the color attachment’s R, G, B or A component value:
--
-- +--------------------------+-------------------------+
-- | Mode                     | Operation               |
-- +==========================+=========================+
-- | 'LOGIC_OP_CLEAR'         | 0                       |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_AND'           | s ∧ d                   |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_AND_REVERSE'   | s ∧ ¬ d                 |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_COPY'          | s                       |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_AND_INVERTED'  | ¬ s ∧ d                 |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_NO_OP'         | d                       |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_XOR'           | s ⊕ d                   |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_OR'            | s ∨ d                   |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_NOR'           | ¬ (s ∨ d)               |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_EQUIVALENT'    | ¬ (s ⊕ d)               |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_INVERT'        | ¬ d                     |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_OR_REVERSE'    | s ∨ ¬ d                 |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_COPY_INVERTED' | ¬ s                     |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_OR_INVERTED'   | ¬ s ∨ d                 |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_NAND'          | ¬ (s ∧ d)               |
-- +--------------------------+-------------------------+
-- | 'LOGIC_OP_SET'           | all 1s                  |
-- +--------------------------+-------------------------+
--
-- Logical Operations
--
-- The result of the logical operation is then written to the color
-- attachment as controlled by the component write mask, described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blendoperations Blend Operations>.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'
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

