{-# language CPP #-}
-- No documentation found for Chapter "MemoryAllocateFlagBits"
module Vulkan.Core11.Enums.MemoryAllocateFlagBits  ( MemoryAllocateFlags
                                                   , MemoryAllocateFlagBits( MEMORY_ALLOCATE_DEVICE_MASK_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
                                                                           , ..
                                                                           )
                                                   ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type MemoryAllocateFlags = MemoryAllocateFlagBits

-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- 'MemoryAllocateFlags'
newtype MemoryAllocateFlagBits = MemoryAllocateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_ALLOCATE_DEVICE_MASK_BIT' specifies that memory will be
-- allocated for the devices in
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo'::@deviceMask@.
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT                   = MemoryAllocateFlagBits 0x00000001
-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT' specifies that the
-- memory’s address /can/ be saved and reused on a subsequent run (e.g. for
-- trace capture and replay), see
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'
-- for more detail.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = MemoryAllocateFlagBits 0x00000004
-- | 'MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT' specifies that the memory /can/ be
-- attached to a buffer object created with the
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
-- bit set in @usage@, and that the memory handle /can/ be used to retrieve
-- an opaque address via
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress'.
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT                = MemoryAllocateFlagBits 0x00000002

conNameMemoryAllocateFlagBits :: String
conNameMemoryAllocateFlagBits = "MemoryAllocateFlagBits"

enumPrefixMemoryAllocateFlagBits :: String
enumPrefixMemoryAllocateFlagBits = "MEMORY_ALLOCATE_DEVICE_"

showTableMemoryAllocateFlagBits :: [(MemoryAllocateFlagBits, String)]
showTableMemoryAllocateFlagBits =
  [ (MEMORY_ALLOCATE_DEVICE_MASK_BIT                  , "MASK_BIT")
  , (MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT, "ADDRESS_CAPTURE_REPLAY_BIT")
  , (MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT               , "ADDRESS_BIT")
  ]

instance Show MemoryAllocateFlagBits where
  showsPrec p e = case lookup e showTableMemoryAllocateFlagBits of
    Just s -> showString enumPrefixMemoryAllocateFlagBits . showString s
    Nothing ->
      let MemoryAllocateFlagBits x = e
      in  showParen (p >= 11) (showString conNameMemoryAllocateFlagBits . showString " 0x" . showHex x)

instance Read MemoryAllocateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixMemoryAllocateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableMemoryAllocateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameMemoryAllocateFlagBits)
            v <- step readPrec
            pure (MemoryAllocateFlagBits v)
          )
    )

