{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.IndexType  (IndexType( INDEX_TYPE_UINT16
                                                         , INDEX_TYPE_UINT32
                                                         , INDEX_TYPE_UINT8_EXT
                                                         , INDEX_TYPE_NONE_NV
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
import Graphics.Vulkan.Zero (Zero)
-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'
newtype IndexType = IndexType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INDEX_TYPE_UINT16' specifies that indices are 16-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT16 = IndexType 0
-- | 'INDEX_TYPE_UINT32' specifies that indices are 32-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT32 = IndexType 1
-- | 'INDEX_TYPE_UINT8_EXT' specifies that indices are 8-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT8_EXT = IndexType 1000265000
-- | 'INDEX_TYPE_NONE_NV' specifies that no indices are provided.
pattern INDEX_TYPE_NONE_NV = IndexType 1000165000
{-# complete INDEX_TYPE_UINT16,
             INDEX_TYPE_UINT32,
             INDEX_TYPE_UINT8_EXT,
             INDEX_TYPE_NONE_NV :: IndexType #-}

instance Show IndexType where
  showsPrec p = \case
    INDEX_TYPE_UINT16 -> showString "INDEX_TYPE_UINT16"
    INDEX_TYPE_UINT32 -> showString "INDEX_TYPE_UINT32"
    INDEX_TYPE_UINT8_EXT -> showString "INDEX_TYPE_UINT8_EXT"
    INDEX_TYPE_NONE_NV -> showString "INDEX_TYPE_NONE_NV"
    IndexType x -> showParen (p >= 11) (showString "IndexType " . showsPrec 11 x)

instance Read IndexType where
  readPrec = parens (choose [("INDEX_TYPE_UINT16", pure INDEX_TYPE_UINT16)
                            , ("INDEX_TYPE_UINT32", pure INDEX_TYPE_UINT32)
                            , ("INDEX_TYPE_UINT8_EXT", pure INDEX_TYPE_UINT8_EXT)
                            , ("INDEX_TYPE_NONE_NV", pure INDEX_TYPE_NONE_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "IndexType")
                       v <- step readPrec
                       pure (IndexType v)))

