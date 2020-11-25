{-# language CPP #-}
-- No documentation found for Chapter "ExternalFenceHandleTypeFlagBits"
module Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits  ( ExternalFenceHandleTypeFlags
                                                            , ExternalFenceHandleTypeFlagBits( EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                             , EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
                                                                                             , ..
                                                                                             )
                                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagBits"
newtype ExternalFenceHandleTypeFlagBits = ExternalFenceHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT        = ExternalFenceHandleTypeFlagBits 0x00000001
-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT     = ExternalFenceHandleTypeFlagBits 0x00000002
-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = ExternalFenceHandleTypeFlagBits 0x00000004
-- No documentation found for Nested "VkExternalFenceHandleTypeFlagBits" "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT          = ExternalFenceHandleTypeFlagBits 0x00000008

conNameExternalFenceHandleTypeFlagBits :: String
conNameExternalFenceHandleTypeFlagBits = "ExternalFenceHandleTypeFlagBits"

enumPrefixExternalFenceHandleTypeFlagBits :: String
enumPrefixExternalFenceHandleTypeFlagBits = "EXTERNAL_FENCE_HANDLE_TYPE_"

showTableExternalFenceHandleTypeFlagBits :: [(ExternalFenceHandleTypeFlagBits, String)]
showTableExternalFenceHandleTypeFlagBits =
  [ (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT       , "OPAQUE_FD_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT    , "OPAQUE_WIN32_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, "OPAQUE_WIN32_KMT_BIT")
  , (EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT         , "SYNC_FD_BIT")
  ]


instance Show ExternalFenceHandleTypeFlagBits where
showsPrec = enumShowsPrec enumPrefixExternalFenceHandleTypeFlagBits
                          showTableExternalFenceHandleTypeFlagBits
                          conNameExternalFenceHandleTypeFlagBits
                          (\(ExternalFenceHandleTypeFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalFenceHandleTypeFlagBits where
  readPrec = enumReadPrec enumPrefixExternalFenceHandleTypeFlagBits
                          showTableExternalFenceHandleTypeFlagBits
                          conNameExternalFenceHandleTypeFlagBits
                          ExternalFenceHandleTypeFlagBits

