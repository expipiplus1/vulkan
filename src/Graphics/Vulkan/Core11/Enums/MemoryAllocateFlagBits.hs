{-# language CPP #-}
module Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits  ( MemoryAllocateFlagBits( MEMORY_ALLOCATE_DEVICE_MASK_BIT
                                                                                    , MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                                    , MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
                                                                                    , ..
                                                                                    )
                                                            , MemoryAllocateFlags
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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- 'MemoryAllocateFlags'
newtype MemoryAllocateFlagBits = MemoryAllocateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'MEMORY_ALLOCATE_DEVICE_MASK_BIT' specifies that memory will be
-- allocated for the devices in
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo'::@deviceMask@.
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT = MemoryAllocateFlagBits 0x00000001
-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
-- memory’s address /can/ be saved and reused on a subsequent run (e.g. for
-- trace capture and replay), see
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
-- for more detail.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = MemoryAllocateFlagBits 0x00000004
-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT' specifies that the memory /can/ be
-- attached to a buffer object created with the
-- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
-- bit set in @usage@, and that the memory handle /can/ be used to retrieve
-- an opaque address via
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress'.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT = MemoryAllocateFlagBits 0x00000002

type MemoryAllocateFlags = MemoryAllocateFlagBits

instance Show MemoryAllocateFlagBits where
  showsPrec p = \case
    MEMORY_ALLOCATE_DEVICE_MASK_BIT -> showString "MEMORY_ALLOCATE_DEVICE_MASK_BIT"
    MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT -> showString "MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
    MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT -> showString "MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT"
    MemoryAllocateFlagBits x -> showParen (p >= 11) (showString "MemoryAllocateFlagBits 0x" . showHex x)

instance Read MemoryAllocateFlagBits where
  readPrec = parens (choose [("MEMORY_ALLOCATE_DEVICE_MASK_BIT", pure MEMORY_ALLOCATE_DEVICE_MASK_BIT)
                            , ("MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT", pure MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT)
                            , ("MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT", pure MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "MemoryAllocateFlagBits")
                       v <- step readPrec
                       pure (MemoryAllocateFlagBits v)))

