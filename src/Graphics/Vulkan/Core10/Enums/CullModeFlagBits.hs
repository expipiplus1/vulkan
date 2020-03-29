{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.CullModeFlagBits  ( CullModeFlagBits( CULL_MODE_NONE
                                                                        , CULL_MODE_FRONT_BIT
                                                                        , CULL_MODE_BACK_BIT
                                                                        , CULL_MODE_FRONT_AND_BACK
                                                                        , ..
                                                                        )
                                                      , CullModeFlags
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
-- | VkCullModeFlagBits - Bitmask controlling triangle culling
--
-- = Description
--
-- Following culling, fragments are produced for any triangles which have
-- not been discarded.
--
-- = See Also
--
-- 'CullModeFlags'
newtype CullModeFlagBits = CullModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'CULL_MODE_NONE' specifies that no triangles are discarded
pattern CULL_MODE_NONE = CullModeFlagBits 0x00000000
-- | 'CULL_MODE_FRONT_BIT' specifies that front-facing triangles are
-- discarded
pattern CULL_MODE_FRONT_BIT = CullModeFlagBits 0x00000001
-- | 'CULL_MODE_BACK_BIT' specifies that back-facing triangles are discarded
pattern CULL_MODE_BACK_BIT = CullModeFlagBits 0x00000002
-- | 'CULL_MODE_FRONT_AND_BACK' specifies that all triangles are discarded.
pattern CULL_MODE_FRONT_AND_BACK = CullModeFlagBits 0x00000003

type CullModeFlags = CullModeFlagBits

instance Show CullModeFlagBits where
  showsPrec p = \case
    CULL_MODE_NONE -> showString "CULL_MODE_NONE"
    CULL_MODE_FRONT_BIT -> showString "CULL_MODE_FRONT_BIT"
    CULL_MODE_BACK_BIT -> showString "CULL_MODE_BACK_BIT"
    CULL_MODE_FRONT_AND_BACK -> showString "CULL_MODE_FRONT_AND_BACK"
    CullModeFlagBits x -> showParen (p >= 11) (showString "CullModeFlagBits 0x" . showHex x)

instance Read CullModeFlagBits where
  readPrec = parens (choose [("CULL_MODE_NONE", pure CULL_MODE_NONE)
                            , ("CULL_MODE_FRONT_BIT", pure CULL_MODE_FRONT_BIT)
                            , ("CULL_MODE_BACK_BIT", pure CULL_MODE_BACK_BIT)
                            , ("CULL_MODE_FRONT_AND_BACK", pure CULL_MODE_FRONT_AND_BACK)]
                     +++
                     prec 10 (do
                       expectP (Ident "CullModeFlagBits")
                       v <- step readPrec
                       pure (CullModeFlagBits v)))

