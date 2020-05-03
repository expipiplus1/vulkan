{-# language CPP #-}
module Vulkan.Core10.Enums.InternalAllocationType  (InternalAllocationType( INTERNAL_ALLOCATION_TYPE_EXECUTABLE
                                                                          , ..
                                                                          )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkInternalAllocationType - Allocation type
--
-- = See Also
--
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification',
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification'
newtype InternalAllocationType = InternalAllocationType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INTERNAL_ALLOCATION_TYPE_EXECUTABLE' specifies that the allocation is
-- intended for execution by the host.
pattern INTERNAL_ALLOCATION_TYPE_EXECUTABLE = InternalAllocationType 0
{-# complete INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: InternalAllocationType #-}

instance Show InternalAllocationType where
  showsPrec p = \case
    INTERNAL_ALLOCATION_TYPE_EXECUTABLE -> showString "INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
    InternalAllocationType x -> showParen (p >= 11) (showString "InternalAllocationType " . showsPrec 11 x)

instance Read InternalAllocationType where
  readPrec = parens (choose [("INTERNAL_ALLOCATION_TYPE_EXECUTABLE", pure INTERNAL_ALLOCATION_TYPE_EXECUTABLE)]
                     +++
                     prec 10 (do
                       expectP (Ident "InternalAllocationType")
                       v <- step readPrec
                       pure (InternalAllocationType v)))

