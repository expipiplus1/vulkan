{-# language CPP #-}
-- No documentation found for Chapter "StencilOp"
module Vulkan.Core10.Enums.StencilOp  (StencilOp( STENCIL_OP_KEEP
                                                , STENCIL_OP_ZERO
                                                , STENCIL_OP_REPLACE
                                                , STENCIL_OP_INCREMENT_AND_CLAMP
                                                , STENCIL_OP_DECREMENT_AND_CLAMP
                                                , STENCIL_OP_INVERT
                                                , STENCIL_OP_INCREMENT_AND_WRAP
                                                , STENCIL_OP_DECREMENT_AND_WRAP
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

-- | VkStencilOp - Stencil comparison function
--
-- = Description
--
-- For purposes of increment and decrement, the stencil bits are considered
-- as an unsigned integer.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.StencilOpState',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT'
newtype StencilOp = StencilOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'STENCIL_OP_KEEP' keeps the current value.
pattern STENCIL_OP_KEEP = StencilOp 0

-- | 'STENCIL_OP_ZERO' sets the value to 0.
pattern STENCIL_OP_ZERO = StencilOp 1

-- | 'STENCIL_OP_REPLACE' sets the value to @reference@.
pattern STENCIL_OP_REPLACE = StencilOp 2

-- | 'STENCIL_OP_INCREMENT_AND_CLAMP' increments the current value and clamps
-- to the maximum representable unsigned value.
pattern STENCIL_OP_INCREMENT_AND_CLAMP = StencilOp 3

-- | 'STENCIL_OP_DECREMENT_AND_CLAMP' decrements the current value and clamps
-- to 0.
pattern STENCIL_OP_DECREMENT_AND_CLAMP = StencilOp 4

-- | 'STENCIL_OP_INVERT' bitwise-inverts the current value.
pattern STENCIL_OP_INVERT = StencilOp 5

-- | 'STENCIL_OP_INCREMENT_AND_WRAP' increments the current value and wraps
-- to 0 when the maximum value would have been exceeded.
pattern STENCIL_OP_INCREMENT_AND_WRAP = StencilOp 6

-- | 'STENCIL_OP_DECREMENT_AND_WRAP' decrements the current value and wraps
-- to the maximum possible value when the value would go below 0.
pattern STENCIL_OP_DECREMENT_AND_WRAP = StencilOp 7

{-# COMPLETE
  STENCIL_OP_KEEP
  , STENCIL_OP_ZERO
  , STENCIL_OP_REPLACE
  , STENCIL_OP_INCREMENT_AND_CLAMP
  , STENCIL_OP_DECREMENT_AND_CLAMP
  , STENCIL_OP_INVERT
  , STENCIL_OP_INCREMENT_AND_WRAP
  , STENCIL_OP_DECREMENT_AND_WRAP ::
    StencilOp
  #-}

conNameStencilOp :: String
conNameStencilOp = "StencilOp"

enumPrefixStencilOp :: String
enumPrefixStencilOp = "STENCIL_OP_"

showTableStencilOp :: [(StencilOp, String)]
showTableStencilOp =
  [ (STENCIL_OP_KEEP, "KEEP")
  , (STENCIL_OP_ZERO, "ZERO")
  , (STENCIL_OP_REPLACE, "REPLACE")
  , (STENCIL_OP_INCREMENT_AND_CLAMP, "INCREMENT_AND_CLAMP")
  , (STENCIL_OP_DECREMENT_AND_CLAMP, "DECREMENT_AND_CLAMP")
  , (STENCIL_OP_INVERT, "INVERT")
  , (STENCIL_OP_INCREMENT_AND_WRAP, "INCREMENT_AND_WRAP")
  , (STENCIL_OP_DECREMENT_AND_WRAP, "DECREMENT_AND_WRAP")
  ]

instance Show StencilOp where
  showsPrec =
    enumShowsPrec
      enumPrefixStencilOp
      showTableStencilOp
      conNameStencilOp
      (\(StencilOp x) -> x)
      (showsPrec 11)

instance Read StencilOp where
  readPrec =
    enumReadPrec
      enumPrefixStencilOp
      showTableStencilOp
      conNameStencilOp
      StencilOp
