{-# language CPP #-}
-- No documentation found for Chapter "ShaderModuleCreateFlags"
module Vulkan.Core10.Enums.ShaderModuleCreateFlags  (ShaderModuleCreateFlags(..)) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- | VkShaderModuleCreateFlags - Reserved for future use
--
-- = Description
--
-- 'ShaderModuleCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'
newtype ShaderModuleCreateFlags = ShaderModuleCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameShaderModuleCreateFlags :: String
conNameShaderModuleCreateFlags = "ShaderModuleCreateFlags"

enumPrefixShaderModuleCreateFlags :: String
enumPrefixShaderModuleCreateFlags = ""

showTableShaderModuleCreateFlags :: [(ShaderModuleCreateFlags, String)]
showTableShaderModuleCreateFlags = []

instance Show ShaderModuleCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixShaderModuleCreateFlags
      showTableShaderModuleCreateFlags
      conNameShaderModuleCreateFlags
      (\(ShaderModuleCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ShaderModuleCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixShaderModuleCreateFlags
      showTableShaderModuleCreateFlags
      conNameShaderModuleCreateFlags
      ShaderModuleCreateFlags
