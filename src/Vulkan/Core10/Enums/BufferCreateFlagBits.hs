{-# language CPP #-}
-- No documentation found for Chapter "BufferCreateFlagBits"
module Vulkan.Core10.Enums.BufferCreateFlagBits  ( BufferCreateFlags
                                                 , BufferCreateFlagBits( BUFFER_CREATE_SPARSE_BINDING_BIT
                                                                       , BUFFER_CREATE_SPARSE_RESIDENCY_BIT
                                                                       , BUFFER_CREATE_SPARSE_ALIASED_BIT
                                                                       , BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT
                                                                       , BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                       , BUFFER_CREATE_PROTECTED_BIT
                                                                       , ..
                                                                       )
                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
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
-- -   'BUFFER_CREATE_SPARSE_BINDING_BIT' specifies that the buffer will be
--     backed using sparse memory binding.
--
-- -   'BUFFER_CREATE_SPARSE_RESIDENCY_BIT' specifies that the buffer /can/
--     be partially backed using sparse memory binding. Buffers created
--     with this flag /must/ also be created with the
--     'BUFFER_CREATE_SPARSE_BINDING_BIT' flag.
--
-- -   'BUFFER_CREATE_SPARSE_ALIASED_BIT' specifies that the buffer will be
--     backed using sparse memory binding with memory ranges that might
--     also simultaneously be backing another buffer (or another portion of
--     the same buffer). Buffers created with this flag /must/ also be
--     created with the 'BUFFER_CREATE_SPARSE_BINDING_BIT' flag.
--
-- -   'BUFFER_CREATE_PROTECTED_BIT' specifies that the buffer is a
--     protected buffer.
--
-- -   'BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
--     buffer’s address /can/ be saved and reused on a subsequent run (e.g.
--     for trace capture and replay), see
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
--     for more detail.
--
-- -   'BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT' specifies
--     that the buffer /can/ be used with descriptor buffers when capturing
--     and replaying (e.g. for trace capture and replay), see
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     for more detail.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBufferCreateFlagBits VK_BUFFER_CREATE_VIDEO_PROFILE_INDEPENDENT_BIT_KHR>
--     specifies that the buffer /can/ be used in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-coding video coding operations>
--     without having to specify at buffer creation time the set of video
--     profiles the buffer will be used with.
--
-- See
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features Physical Device Features>
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BufferCreateFlags'
newtype BufferCreateFlagBits = BufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
pattern BUFFER_CREATE_SPARSE_BINDING_BIT = BufferCreateFlagBits 0x00000001

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT = BufferCreateFlagBits 0x00000002

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT = BufferCreateFlagBits 0x00000004

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT"
pattern BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT = BufferCreateFlagBits 0x00000020

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = BufferCreateFlagBits 0x00000010

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_PROTECTED_BIT"
pattern BUFFER_CREATE_PROTECTED_BIT = BufferCreateFlagBits 0x00000008

conNameBufferCreateFlagBits :: String
conNameBufferCreateFlagBits = "BufferCreateFlagBits"

enumPrefixBufferCreateFlagBits :: String
enumPrefixBufferCreateFlagBits = "BUFFER_CREATE_"

showTableBufferCreateFlagBits :: [(BufferCreateFlagBits, String)]
showTableBufferCreateFlagBits =
  [
    ( BUFFER_CREATE_SPARSE_BINDING_BIT
    , "SPARSE_BINDING_BIT"
    )
  ,
    ( BUFFER_CREATE_SPARSE_RESIDENCY_BIT
    , "SPARSE_RESIDENCY_BIT"
    )
  ,
    ( BUFFER_CREATE_SPARSE_ALIASED_BIT
    , "SPARSE_ALIASED_BIT"
    )
  ,
    ( BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT
    , "DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT"
    )
  ,
    ( BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
    , "DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
    )
  ,
    ( BUFFER_CREATE_PROTECTED_BIT
    , "PROTECTED_BIT"
    )
  ]

instance Show BufferCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixBufferCreateFlagBits
      showTableBufferCreateFlagBits
      conNameBufferCreateFlagBits
      (\(BufferCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BufferCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixBufferCreateFlagBits
      showTableBufferCreateFlagBits
      conNameBufferCreateFlagBits
      BufferCreateFlagBits
