{-# language CPP #-}
-- No documentation found for Chapter "ShaderModuleCreateFlagBits"
module Vulkan.Core10.Enums.ShaderModuleCreateFlagBits  ( ShaderModuleCreateFlags
                                                       , ShaderModuleCreateFlagBits(..)
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
type ShaderModuleCreateFlags = ShaderModuleCreateFlagBits

-- No documentation found for TopLevel "VkShaderModuleCreateFlagBits"
newtype ShaderModuleCreateFlagBits = ShaderModuleCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameShaderModuleCreateFlagBits :: String
conNameShaderModuleCreateFlagBits = "ShaderModuleCreateFlagBits"

enumPrefixShaderModuleCreateFlagBits :: String
enumPrefixShaderModuleCreateFlagBits = ""

showTableShaderModuleCreateFlagBits :: [(ShaderModuleCreateFlagBits, String)]
showTableShaderModuleCreateFlagBits = []

instance Show ShaderModuleCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixShaderModuleCreateFlagBits
                            showTableShaderModuleCreateFlagBits
                            conNameShaderModuleCreateFlagBits
                            (\(ShaderModuleCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ShaderModuleCreateFlagBits where
  readPrec = enumReadPrec enumPrefixShaderModuleCreateFlagBits
                          showTableShaderModuleCreateFlagBits
                          conNameShaderModuleCreateFlagBits
                          ShaderModuleCreateFlagBits

