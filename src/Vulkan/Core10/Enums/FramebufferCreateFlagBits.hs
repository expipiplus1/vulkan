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

-- No documentation found for TopLevel "VkFramebufferCreateFlagBits"
newtype FramebufferCreateFlagBits = FramebufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkFramebufferCreateFlagBits" "VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT"
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

