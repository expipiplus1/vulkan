{-# language CPP #-}
-- No documentation found for Chapter "IndexType"
module Vulkan.Core10.Enums.IndexType  (IndexType( INDEX_TYPE_UINT16
                                                , INDEX_TYPE_UINT32
                                                , INDEX_TYPE_UINT8_EXT
                                                , INDEX_TYPE_NONE_KHR
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
-- No documentation found for TopLevel "VkIndexType"
newtype IndexType = IndexType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT16"
pattern INDEX_TYPE_UINT16    = IndexType 0
-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT32"
pattern INDEX_TYPE_UINT32    = IndexType 1
-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT8_EXT"
pattern INDEX_TYPE_UINT8_EXT = IndexType 1000265000
-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_NONE_KHR"
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
showsPrec = enumShowsPrec enumPrefixIndexType showTableIndexType conNameIndexType (\(IndexType x) -> x) (showsPrec 11)


instance Read IndexType where
  readPrec = enumReadPrec enumPrefixIndexType showTableIndexType conNameIndexType IndexType

