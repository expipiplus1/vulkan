{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreType"
module Vulkan.Core12.Enums.SemaphoreType  (SemaphoreType( SEMAPHORE_TYPE_BINARY
                                                        , SEMAPHORE_TYPE_TIMELINE
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
pattern SEMAPHORE_TYPE_BINARY = SemaphoreType 0
-- | 'SEMAPHORE_TYPE_TIMELINE' specifies a /timeline semaphore/ type that has
-- a monotonically increasing 64-bit unsigned integer payload indicating
-- whether the semaphore is signaled with respect to a particular reference
-- value. When created, the semaphore payload has the value given by the
-- @initialValue@ field of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'.
pattern SEMAPHORE_TYPE_TIMELINE = SemaphoreType 1
{-# complete SEMAPHORE_TYPE_BINARY,
             SEMAPHORE_TYPE_TIMELINE :: SemaphoreType #-}

instance Show SemaphoreType where
  showsPrec p = \case
    SEMAPHORE_TYPE_BINARY -> showString "SEMAPHORE_TYPE_BINARY"
    SEMAPHORE_TYPE_TIMELINE -> showString "SEMAPHORE_TYPE_TIMELINE"
    SemaphoreType x -> showParen (p >= 11) (showString "SemaphoreType " . showsPrec 11 x)

instance Read SemaphoreType where
  readPrec = parens (choose [("SEMAPHORE_TYPE_BINARY", pure SEMAPHORE_TYPE_BINARY)
                            , ("SEMAPHORE_TYPE_TIMELINE", pure SEMAPHORE_TYPE_TIMELINE)]
                     +++
                     prec 10 (do
                       expectP (Ident "SemaphoreType")
                       v <- step readPrec
                       pure (SemaphoreType v)))

