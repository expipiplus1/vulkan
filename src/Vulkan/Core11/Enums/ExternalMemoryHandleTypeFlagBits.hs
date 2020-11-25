{-# language CPP #-}
-- No documentation found for Chapter "ExternalMemoryHandleTypeFlagBits"
module Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits  ( ExternalMemoryHandleTypeFlags
                                                             , ExternalMemoryHandleTypeFlagBits( EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
                                                                                               , EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
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
type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagBits"
newtype ExternalMemoryHandleTypeFlagBits = ExternalMemoryHandleTypeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT                       = ExternalMemoryHandleTypeFlagBits 0x00000001
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT                    = ExternalMemoryHandleTypeFlagBits 0x00000002
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT                = ExternalMemoryHandleTypeFlagBits 0x00000004
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT                   = ExternalMemoryHandleTypeFlagBits 0x00000008
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT               = ExternalMemoryHandleTypeFlagBits 0x00000010
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT                      = ExternalMemoryHandleTypeFlagBits 0x00000020
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT                  = ExternalMemoryHandleTypeFlagBits 0x00000040
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT  = ExternalMemoryHandleTypeFlagBits 0x00000100
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT             = ExternalMemoryHandleTypeFlagBits 0x00000080
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = ExternalMemoryHandleTypeFlagBits 0x00000400
-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT                     = ExternalMemoryHandleTypeFlagBits 0x00000200

conNameExternalMemoryHandleTypeFlagBits :: String
conNameExternalMemoryHandleTypeFlagBits = "ExternalMemoryHandleTypeFlagBits"

enumPrefixExternalMemoryHandleTypeFlagBits :: String
enumPrefixExternalMemoryHandleTypeFlagBits = "EXTERNAL_MEMORY_HANDLE_TYPE_"

showTableExternalMemoryHandleTypeFlagBits :: [(ExternalMemoryHandleTypeFlagBits, String)]
showTableExternalMemoryHandleTypeFlagBits =
  [ (EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT                      , "OPAQUE_FD_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT                   , "OPAQUE_WIN32_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT               , "OPAQUE_WIN32_KMT_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT                  , "D3D11_TEXTURE_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT              , "D3D11_TEXTURE_KMT_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT                     , "D3D12_HEAP_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT                 , "D3D12_RESOURCE_BIT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT , "HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT            , "HOST_ALLOCATION_BIT_EXT")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID, "ANDROID_HARDWARE_BUFFER_BIT_ANDROID")
  , (EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT                    , "DMA_BUF_BIT_EXT")
  ]


instance Show ExternalMemoryHandleTypeFlagBits where
showsPrec = enumShowsPrec enumPrefixExternalMemoryHandleTypeFlagBits
                          showTableExternalMemoryHandleTypeFlagBits
                          conNameExternalMemoryHandleTypeFlagBits
                          (\(ExternalMemoryHandleTypeFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalMemoryHandleTypeFlagBits where
  readPrec = enumReadPrec enumPrefixExternalMemoryHandleTypeFlagBits
                          showTableExternalMemoryHandleTypeFlagBits
                          conNameExternalMemoryHandleTypeFlagBits
                          ExternalMemoryHandleTypeFlagBits

