{-# language CPP #-}
-- No documentation found for Chapter "BufferCreateFlagBits"
module Vulkan.Core10.Enums.BufferCreateFlagBits  ( BufferCreateFlags
                                                 , BufferCreateFlagBits( BUFFER_CREATE_SPARSE_BINDING_BIT
                                                                       , BUFFER_CREATE_SPARSE_RESIDENCY_BIT
                                                                       , BUFFER_CREATE_SPARSE_ALIASED_BIT
                                                                       , BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                       , BUFFER_CREATE_PROTECTED_BIT
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
type BufferCreateFlags = BufferCreateFlagBits

-- No documentation found for TopLevel "VkBufferCreateFlagBits"
newtype BufferCreateFlagBits = BufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
pattern BUFFER_CREATE_SPARSE_BINDING_BIT                = BufferCreateFlagBits 0x00000001
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT              = BufferCreateFlagBits 0x00000002
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT                = BufferCreateFlagBits 0x00000004
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = BufferCreateFlagBits 0x00000010
-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_PROTECTED_BIT"
pattern BUFFER_CREATE_PROTECTED_BIT                     = BufferCreateFlagBits 0x00000008

conNameBufferCreateFlagBits :: String
conNameBufferCreateFlagBits = "BufferCreateFlagBits"

enumPrefixBufferCreateFlagBits :: String
enumPrefixBufferCreateFlagBits = "BUFFER_CREATE_"

showTableBufferCreateFlagBits :: [(BufferCreateFlagBits, String)]
showTableBufferCreateFlagBits =
  [ (BUFFER_CREATE_SPARSE_BINDING_BIT               , "SPARSE_BINDING_BIT")
  , (BUFFER_CREATE_SPARSE_RESIDENCY_BIT             , "SPARSE_RESIDENCY_BIT")
  , (BUFFER_CREATE_SPARSE_ALIASED_BIT               , "SPARSE_ALIASED_BIT")
  , (BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT, "DEVICE_ADDRESS_CAPTURE_REPLAY_BIT")
  , (BUFFER_CREATE_PROTECTED_BIT                    , "PROTECTED_BIT")
  ]


instance Show BufferCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixBufferCreateFlagBits
                          showTableBufferCreateFlagBits
                          conNameBufferCreateFlagBits
                          (\(BufferCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read BufferCreateFlagBits where
  readPrec = enumReadPrec enumPrefixBufferCreateFlagBits
                          showTableBufferCreateFlagBits
                          conNameBufferCreateFlagBits
                          BufferCreateFlagBits

