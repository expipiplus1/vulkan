{-# language CPP #-}
-- No documentation found for Chapter "FramebufferCreateFlagBits"
module Vulkan.Core10.Enums.FramebufferCreateFlagBits  ( FramebufferCreateFlags
                                                      , FramebufferCreateFlagBits( FRAMEBUFFER_CREATE_IMAGELESS_BIT
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
type FramebufferCreateFlags = FramebufferCreateFlagBits

-- | VkFramebufferCreateFlagBits - Bitmask specifying framebuffer properties
--
-- = See Also
--
-- 'FramebufferCreateFlags'
newtype FramebufferCreateFlagBits = FramebufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FRAMEBUFFER_CREATE_IMAGELESS_BIT' specifies that image views are not
-- specified, and only attachment compatibility information will be
-- provided via a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentImageInfo'
-- structure.
pattern FRAMEBUFFER_CREATE_IMAGELESS_BIT = FramebufferCreateFlagBits 0x00000001

conNameFramebufferCreateFlagBits :: String
conNameFramebufferCreateFlagBits = "FramebufferCreateFlagBits"

enumPrefixFramebufferCreateFlagBits :: String
enumPrefixFramebufferCreateFlagBits = "FRAMEBUFFER_CREATE_IMAGELESS_BIT"

showTableFramebufferCreateFlagBits :: [(FramebufferCreateFlagBits, String)]
showTableFramebufferCreateFlagBits = [(FRAMEBUFFER_CREATE_IMAGELESS_BIT, "")]

instance Show FramebufferCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixFramebufferCreateFlagBits
                            showTableFramebufferCreateFlagBits
                            conNameFramebufferCreateFlagBits
                            (\(FramebufferCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read FramebufferCreateFlagBits where
  readPrec = enumReadPrec enumPrefixFramebufferCreateFlagBits
                          showTableFramebufferCreateFlagBits
                          conNameFramebufferCreateFlagBits
                          FramebufferCreateFlagBits

