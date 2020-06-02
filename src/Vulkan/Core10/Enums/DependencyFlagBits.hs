{-# language CPP #-}
module Vulkan.Core10.Enums.DependencyFlagBits  ( DependencyFlagBits( DEPENDENCY_BY_REGION_BIT
                                                                   , DEPENDENCY_VIEW_LOCAL_BIT
                                                                   , DEPENDENCY_DEVICE_GROUP_BIT
                                                                   , ..
                                                                   )
                                               , DependencyFlags
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
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
--
-- 'DependencyFlags'
newtype DependencyFlagBits = DependencyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEPENDENCY_BY_REGION_BIT' specifies that dependencies will be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>.
pattern DEPENDENCY_BY_REGION_BIT = DependencyFlagBits 0x00000001
-- | 'DEPENDENCY_VIEW_LOCAL_BIT' specifies that a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies subpass has more than one view>.
pattern DEPENDENCY_VIEW_LOCAL_BIT = DependencyFlagBits 0x00000002
-- | 'DEPENDENCY_DEVICE_GROUP_BIT' specifies that dependencies are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-device-local-dependencies non-device-local dependency>.
pattern DEPENDENCY_DEVICE_GROUP_BIT = DependencyFlagBits 0x00000004

type DependencyFlags = DependencyFlagBits

instance Show DependencyFlagBits where
  showsPrec p = \case
    DEPENDENCY_BY_REGION_BIT -> showString "DEPENDENCY_BY_REGION_BIT"
    DEPENDENCY_VIEW_LOCAL_BIT -> showString "DEPENDENCY_VIEW_LOCAL_BIT"
    DEPENDENCY_DEVICE_GROUP_BIT -> showString "DEPENDENCY_DEVICE_GROUP_BIT"
    DependencyFlagBits x -> showParen (p >= 11) (showString "DependencyFlagBits 0x" . showHex x)

instance Read DependencyFlagBits where
  readPrec = parens (choose [("DEPENDENCY_BY_REGION_BIT", pure DEPENDENCY_BY_REGION_BIT)
                            , ("DEPENDENCY_VIEW_LOCAL_BIT", pure DEPENDENCY_VIEW_LOCAL_BIT)
                            , ("DEPENDENCY_DEVICE_GROUP_BIT", pure DEPENDENCY_DEVICE_GROUP_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DependencyFlagBits")
                       v <- step readPrec
                       pure (DependencyFlagBits v)))

