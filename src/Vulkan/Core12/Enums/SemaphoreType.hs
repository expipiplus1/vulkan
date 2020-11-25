{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreType"
module Vulkan.Core12.Enums.SemaphoreType  (SemaphoreType( SEMAPHORE_TYPE_BINARY
                                                        , SEMAPHORE_TYPE_TIMELINE
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
-- | VkSemaphoreType - Sepcifies the type of a semaphore object
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
newtype SemaphoreType = SemaphoreType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SEMAPHORE_TYPE_BINARY' specifies a /binary semaphore/ type that has a
-- boolean payload indicating whether the semaphore is currently signaled
-- or unsignaled. When created, the semaphore is in the unsignaled state.
pattern SEMAPHORE_TYPE_BINARY   = SemaphoreType 0
-- | 'SEMAPHORE_TYPE_TIMELINE' specifies a /timeline semaphore/ type that has
-- a monotonically increasing 64-bit unsigned integer payload indicating
-- whether the semaphore is signaled with respect to a particular reference
-- value. When created, the semaphore payload has the value given by the
-- @initialValue@ field of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'.
pattern SEMAPHORE_TYPE_TIMELINE = SemaphoreType 1
{-# complete SEMAPHORE_TYPE_BINARY,
             SEMAPHORE_TYPE_TIMELINE :: SemaphoreType #-}

conNameSemaphoreType :: String
conNameSemaphoreType = "SemaphoreType"

enumPrefixSemaphoreType :: String
enumPrefixSemaphoreType = "SEMAPHORE_TYPE_"

showTableSemaphoreType :: [(SemaphoreType, String)]
showTableSemaphoreType = [(SEMAPHORE_TYPE_BINARY, "BINARY"), (SEMAPHORE_TYPE_TIMELINE, "TIMELINE")]

instance Show SemaphoreType where
  showsPrec p e = case lookup e showTableSemaphoreType of
    Just s -> showString enumPrefixSemaphoreType . showString s
    Nothing ->
      let SemaphoreType x = e
      in  showParen (p >= 11) (showString conNameSemaphoreType . showString " " . showsPrec 11 x)

instance Read SemaphoreType where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSemaphoreType
          asum ((\(e, s) -> e <$ string s) <$> showTableSemaphoreType)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSemaphoreType)
            v <- step readPrec
            pure (SemaphoreType v)
          )
    )

