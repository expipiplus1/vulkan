{-# language CPP #-}
-- No documentation found for Chapter "IndexType"
module Vulkan.Core10.Enums.IndexType  (IndexType( INDEX_TYPE_UINT16
                                                , INDEX_TYPE_UINT32
                                                , INDEX_TYPE_UINT8_EXT
                                                , INDEX_TYPE_NONE_KHR
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
-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'
newtype IndexType = IndexType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INDEX_TYPE_UINT16' specifies that indices are 16-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT16    = IndexType 0
-- | 'INDEX_TYPE_UINT32' specifies that indices are 32-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT32    = IndexType 1
-- | 'INDEX_TYPE_UINT8_EXT' specifies that indices are 8-bit unsigned integer
-- values.
pattern INDEX_TYPE_UINT8_EXT = IndexType 1000265000
-- | 'INDEX_TYPE_NONE_KHR' specifies that no indices are provided.
pattern INDEX_TYPE_NONE_KHR  = IndexType 1000165000
{-# complete INDEX_TYPE_UINT16,
             INDEX_TYPE_UINT32,
             INDEX_TYPE_UINT8_EXT,
             INDEX_TYPE_NONE_KHR :: IndexType #-}

conNameIndexType :: String
conNameIndexType = "IndexType"

enumPrefixIndexType :: String
enumPrefixIndexType = "INDEX_TYPE_"

showTableIndexType :: [(IndexType, String)]
showTableIndexType =
  [ (INDEX_TYPE_UINT16   , "UINT16")
  , (INDEX_TYPE_UINT32   , "UINT32")
  , (INDEX_TYPE_UINT8_EXT, "UINT8_EXT")
  , (INDEX_TYPE_NONE_KHR , "NONE_KHR")
  ]

instance Show IndexType where
  showsPrec p e = case lookup e showTableIndexType of
    Just s -> showString enumPrefixIndexType . showString s
    Nothing ->
      let IndexType x = e in showParen (p >= 11) (showString conNameIndexType . showString " " . showsPrec 11 x)

instance Read IndexType where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixIndexType
          asum ((\(e, s) -> e <$ string s) <$> showTableIndexType)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameIndexType)
            v <- step readPrec
            pure (IndexType v)
          )
    )

