{-# language CPP #-}
-- No documentation found for Chapter "InternalAllocationType"
module Vulkan.Core10.Enums.InternalAllocationType  (InternalAllocationType( INTERNAL_ALLOCATION_TYPE_EXECUTABLE
                                                                          , ..
                                                                          )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
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

conNameInternalAllocationType :: String
conNameInternalAllocationType = "InternalAllocationType"

enumPrefixInternalAllocationType :: String
enumPrefixInternalAllocationType = "INTERNAL_ALLOCATION_TYPE_EXECUTABLE"

showTableInternalAllocationType :: [(InternalAllocationType, String)]
showTableInternalAllocationType = [(INTERNAL_ALLOCATION_TYPE_EXECUTABLE, "")]

instance Show InternalAllocationType where
  showsPrec p e = case lookup e showTableInternalAllocationType of
    Just s -> showString enumPrefixInternalAllocationType . showString s
    Nothing ->
      let InternalAllocationType x = e
      in  showParen (p >= 11) (showString conNameInternalAllocationType . showString " " . showsPrec 11 x)

instance Read InternalAllocationType where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixInternalAllocationType
          asum ((\(e, s) -> e <$ string s) <$> showTableInternalAllocationType)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameInternalAllocationType)
            v <- step readPrec
            pure (InternalAllocationType v)
          )
    )

