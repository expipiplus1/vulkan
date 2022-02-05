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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type BufferCreateFlags = BufferCreateFlagBits

-- | VkBufferCreateFlagBits - Bitmask specifying additional parameters of a
-- buffer
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features Physical Device Features>
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BufferCreateFlags'
newtype BufferCreateFlagBits = BufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUFFER_CREATE_SPARSE_BINDING_BIT' specifies that the buffer will be
-- backed using sparse memory binding.
pattern BUFFER_CREATE_SPARSE_BINDING_BIT                = BufferCreateFlagBits 0x00000001
-- | 'BUFFER_CREATE_SPARSE_RESIDENCY_BIT' specifies that the buffer /can/ be
-- partially backed using sparse memory binding. Buffers created with this
-- flag /must/ also be created with the 'BUFFER_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT              = BufferCreateFlagBits 0x00000002
-- | 'BUFFER_CREATE_SPARSE_ALIASED_BIT' specifies that the buffer will be
-- backed using sparse memory binding with memory ranges that might also
-- simultaneously be backing another buffer (or another portion of the same
-- buffer). Buffers created with this flag /must/ also be created with the
-- 'BUFFER_CREATE_SPARSE_BINDING_BIT' flag.
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT                = BufferCreateFlagBits 0x00000004
-- | 'BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
-- buffer’s address /can/ be saved and reused on a subsequent run (e.g. for
-- trace capture and replay), see
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
-- for more detail.
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = BufferCreateFlagBits 0x00000010
-- | 'BUFFER_CREATE_PROTECTED_BIT' specifies that the buffer is a protected
-- buffer.
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

