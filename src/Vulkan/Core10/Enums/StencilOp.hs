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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkStencilOp"
newtype StencilOp = StencilOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_KEEP"
pattern STENCIL_OP_KEEP                = StencilOp 0
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_ZERO"
pattern STENCIL_OP_ZERO                = StencilOp 1
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_REPLACE"
pattern STENCIL_OP_REPLACE             = StencilOp 2
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
pattern STENCIL_OP_INCREMENT_AND_CLAMP = StencilOp 3
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
pattern STENCIL_OP_DECREMENT_AND_CLAMP = StencilOp 4
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INVERT"
pattern STENCIL_OP_INVERT              = StencilOp 5
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INCREMENT_AND_WRAP"
pattern STENCIL_OP_INCREMENT_AND_WRAP  = StencilOp 6
-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_DECREMENT_AND_WRAP"
pattern STENCIL_OP_DECREMENT_AND_WRAP  = StencilOp 7
{-# complete STENCIL_OP_KEEP,
             STENCIL_OP_ZERO,
             STENCIL_OP_REPLACE,
             STENCIL_OP_INCREMENT_AND_CLAMP,
             STENCIL_OP_DECREMENT_AND_CLAMP,
             STENCIL_OP_INVERT,
             STENCIL_OP_INCREMENT_AND_WRAP,
             STENCIL_OP_DECREMENT_AND_WRAP :: StencilOp #-}

conNameStencilOp :: String
conNameStencilOp = "StencilOp"

enumPrefixStencilOp :: String
enumPrefixStencilOp = "STENCIL_OP_"

showTableStencilOp :: [(StencilOp, String)]
showTableStencilOp =
  [ (STENCIL_OP_KEEP               , "KEEP")
  , (STENCIL_OP_ZERO               , "ZERO")
  , (STENCIL_OP_REPLACE            , "REPLACE")
  , (STENCIL_OP_INCREMENT_AND_CLAMP, "INCREMENT_AND_CLAMP")
  , (STENCIL_OP_DECREMENT_AND_CLAMP, "DECREMENT_AND_CLAMP")
  , (STENCIL_OP_INVERT             , "INVERT")
  , (STENCIL_OP_INCREMENT_AND_WRAP , "INCREMENT_AND_WRAP")
  , (STENCIL_OP_DECREMENT_AND_WRAP , "DECREMENT_AND_WRAP")
  ]


instance Show StencilOp where
showsPrec = enumShowsPrec enumPrefixStencilOp showTableStencilOp conNameStencilOp (\(StencilOp x) -> x) (showsPrec 11)


instance Read StencilOp where
  readPrec = enumReadPrec enumPrefixStencilOp showTableStencilOp conNameStencilOp StencilOp

