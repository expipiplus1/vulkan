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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type BufferCreateFlags = BufferCreateFlagBits

-- | VkBufferCreateFlagBits - Bitmask specifying additional parameters of a
-- buffer
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features Physical Device Features>
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- 'BufferCreateFlags'
newtype BufferCreateFlagBits = BufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUFFER_CREATE_SPARSE_BINDING_BIT' specifies that the buffer will be
-- backed using sparse memory binding.
pattern BUFFER_CREATE_SPARSE_BINDING_BIT = BufferCreateFlagBits 0x00000001
-- | 'BUFFER_CREATE_SPARSE_RESIDENCY_BIT' specifies that the buffer /can/ be
-- partially backed using sparse memory binding. Buffers created with this
-- flag /must/ also be created with the 'BUFFER_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT = BufferCreateFlagBits 0x00000002
-- | 'BUFFER_CREATE_SPARSE_ALIASED_BIT' specifies that the buffer will be
-- backed using sparse memory binding with memory ranges that might also
-- simultaneously be backing another buffer (or another portion of the same
-- buffer). Buffers created with this flag /must/ also be created with the
-- 'BUFFER_CREATE_SPARSE_BINDING_BIT' flag.
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT = BufferCreateFlagBits 0x00000004
-- | 'BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
-- buffer’s address /can/ be saved and reused on a subsequent run (e.g. for
-- trace capture and replay), see
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
-- for more detail.
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = BufferCreateFlagBits 0x00000010
-- | 'BUFFER_CREATE_PROTECTED_BIT' specifies that the buffer is a protected
-- buffer.
pattern BUFFER_CREATE_PROTECTED_BIT = BufferCreateFlagBits 0x00000008

instance Show BufferCreateFlagBits where
  showsPrec p = \case
    BUFFER_CREATE_SPARSE_BINDING_BIT -> showString "BUFFER_CREATE_SPARSE_BINDING_BIT"
    BUFFER_CREATE_SPARSE_RESIDENCY_BIT -> showString "BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
    BUFFER_CREATE_SPARSE_ALIASED_BIT -> showString "BUFFER_CREATE_SPARSE_ALIASED_BIT"
    BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT -> showString "BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
    BUFFER_CREATE_PROTECTED_BIT -> showString "BUFFER_CREATE_PROTECTED_BIT"
    BufferCreateFlagBits x -> showParen (p >= 11) (showString "BufferCreateFlagBits 0x" . showHex x)

instance Read BufferCreateFlagBits where
  readPrec = parens (choose [("BUFFER_CREATE_SPARSE_BINDING_BIT", pure BUFFER_CREATE_SPARSE_BINDING_BIT)
                            , ("BUFFER_CREATE_SPARSE_RESIDENCY_BIT", pure BUFFER_CREATE_SPARSE_RESIDENCY_BIT)
                            , ("BUFFER_CREATE_SPARSE_ALIASED_BIT", pure BUFFER_CREATE_SPARSE_ALIASED_BIT)
                            , ("BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT", pure BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT)
                            , ("BUFFER_CREATE_PROTECTED_BIT", pure BUFFER_CREATE_PROTECTED_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "BufferCreateFlagBits")
                       v <- step readPrec
                       pure (BufferCreateFlagBits v)))

