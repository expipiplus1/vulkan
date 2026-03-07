{-# language CPP #-}
-- No documentation found for Chapter "RenderPassCreateFlagBits"
module Vulkan.Core10.Enums.RenderPassCreateFlagBits  ( RenderPassCreateFlags
                                                     , RenderPassCreateFlagBits( RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
                                                                               , RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM
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
type RenderPassCreateFlags = RenderPassCreateFlagBits

-- | VkRenderPassCreateFlagBits - Bitmask specifying additional properties of
-- a render pass
--
-- = Description
--
-- -   'RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM' specifies that the created
--     render pass is compatible with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>.
--
-- -   'RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE' specifies
--     that the created render pass is usable with layered fragment density
--     maps.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'RenderPassCreateFlags'
newtype RenderPassCreateFlagBits = RenderPassCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkRenderPassCreateFlagBits" "VK_RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
pattern RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE = RenderPassCreateFlagBits 0x00000004

-- No documentation found for Nested "VkRenderPassCreateFlagBits" "VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM"
pattern RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM = RenderPassCreateFlagBits 0x00000002

conNameRenderPassCreateFlagBits :: String
conNameRenderPassCreateFlagBits = "RenderPassCreateFlagBits"

enumPrefixRenderPassCreateFlagBits :: String
enumPrefixRenderPassCreateFlagBits = "RENDER_PASS_CREATE_"

showTableRenderPassCreateFlagBits :: [(RenderPassCreateFlagBits, String)]
showTableRenderPassCreateFlagBits =
  [
    ( RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
    , "PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
    )
  ,
    ( RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM
    , "TRANSFORM_BIT_QCOM"
    )
  ]

instance Show RenderPassCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixRenderPassCreateFlagBits
      showTableRenderPassCreateFlagBits
      conNameRenderPassCreateFlagBits
      (\(RenderPassCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read RenderPassCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixRenderPassCreateFlagBits
      showTableRenderPassCreateFlagBits
      conNameRenderPassCreateFlagBits
      RenderPassCreateFlagBits
