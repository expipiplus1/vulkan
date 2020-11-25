{-# language CPP #-}
-- No documentation found for Chapter "ExternalSemaphoreHandleTypeFlagBits"
module Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits  ( pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT
                                                                , ExternalSemaphoreHandleTypeFlags
                                                                , ExternalSemaphoreHandleTypeFlagBits( EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
                                                                                                     , EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
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
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT = EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT


type ExternalSemaphoreHandleTypeFlags = ExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagBits"
newtype ExternalSemaphoreHandleTypeFlagBits = ExternalSemaphoreHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT        = ExternalSemaphoreHandleTypeFlagBits 0x00000001
-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT     = ExternalSemaphoreHandleTypeFlagBits 0x00000002
-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = ExternalSemaphoreHandleTypeFlagBits 0x00000004
-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT      = ExternalSemaphoreHandleTypeFlagBits 0x00000008
-- No documentation found for Nested "VkExternalSemaphoreHandleTypeFlagBits" "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT          = ExternalSemaphoreHandleTypeFlagBits 0x00000010

conNameExternalSemaphoreHandleTypeFlagBits :: String
conNameExternalSemaphoreHandleTypeFlagBits = "ExternalSemaphoreHandleTypeFlagBits"

enumPrefixExternalSemaphoreHandleTypeFlagBits :: String
enumPrefixExternalSemaphoreHandleTypeFlagBits = "EXTERNAL_SEMAPHORE_HANDLE_TYPE_"

showTableExternalSemaphoreHandleTypeFlagBits :: [(ExternalSemaphoreHandleTypeFlagBits, String)]
showTableExternalSemaphoreHandleTypeFlagBits =
  [ (EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT       , "OPAQUE_FD_BIT")
  , (EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT    , "OPAQUE_WIN32_BIT")
  , (EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, "OPAQUE_WIN32_KMT_BIT")
  , (EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT     , "D3D12_FENCE_BIT")
  , (EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT         , "SYNC_FD_BIT")
  ]


instance Show ExternalSemaphoreHandleTypeFlagBits where
showsPrec = enumShowsPrec enumPrefixExternalSemaphoreHandleTypeFlagBits
                          showTableExternalSemaphoreHandleTypeFlagBits
                          conNameExternalSemaphoreHandleTypeFlagBits
                          (\(ExternalSemaphoreHandleTypeFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalSemaphoreHandleTypeFlagBits where
  readPrec = enumReadPrec enumPrefixExternalSemaphoreHandleTypeFlagBits
                          showTableExternalSemaphoreHandleTypeFlagBits
                          conNameExternalSemaphoreHandleTypeFlagBits
                          ExternalSemaphoreHandleTypeFlagBits

