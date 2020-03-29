{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.FenceCreateFlagBits  ( FenceCreateFlagBits( FENCE_CREATE_SIGNALED_BIT
                                                                              , ..
                                                                              )
                                                         , FenceCreateFlags
                                                         ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'FenceCreateFlags'
newtype FenceCreateFlagBits = FenceCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'FENCE_CREATE_SIGNALED_BIT' specifies that the fence object is created
-- in the signaled state. Otherwise, it is created in the unsignaled state.
pattern FENCE_CREATE_SIGNALED_BIT = FenceCreateFlagBits 0x00000001

type FenceCreateFlags = FenceCreateFlagBits

instance Show FenceCreateFlagBits where
  showsPrec p = \case
    FENCE_CREATE_SIGNALED_BIT -> showString "FENCE_CREATE_SIGNALED_BIT"
    FenceCreateFlagBits x -> showParen (p >= 11) (showString "FenceCreateFlagBits 0x" . showHex x)

instance Read FenceCreateFlagBits where
  readPrec = parens (choose [("FENCE_CREATE_SIGNALED_BIT", pure FENCE_CREATE_SIGNALED_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "FenceCreateFlagBits")
                       v <- step readPrec
                       pure (FenceCreateFlagBits v)))

