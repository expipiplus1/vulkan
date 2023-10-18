{-# language CPP #-}
-- No documentation found for Chapter "RenderingFlagBits"
module Vulkan.Core13.Enums.RenderingFlagBits  ( pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR
                                              , pattern RENDERING_SUSPENDING_BIT_KHR
                                              , pattern RENDERING_RESUMING_BIT_KHR
                                              , RenderingFlags
                                              , RenderingFlagBits( RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT
                                                                 , RENDERING_SUSPENDING_BIT
                                                                 , RENDERING_RESUMING_BIT
                                                                 , RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT
                                                                 , RENDERING_CONTENTS_INLINE_BIT_EXT
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
-- No documentation found for TopLevel "VK_RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR"
pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR = RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT


-- No documentation found for TopLevel "VK_RENDERING_SUSPENDING_BIT_KHR"
pattern RENDERING_SUSPENDING_BIT_KHR = RENDERING_SUSPENDING_BIT


-- No documentation found for TopLevel "VK_RENDERING_RESUMING_BIT_KHR"
pattern RENDERING_RESUMING_BIT_KHR = RENDERING_RESUMING_BIT


type RenderingFlags = RenderingFlagBits

-- | VkRenderingFlagBits - Bitmask specifying additional properties of a
-- dynamic render pass instance
--
-- = Description
--
-- The contents of @pRenderingInfo@ /must/ match between suspended render
-- pass instances and the render pass instances that resume them, other
-- than the presence or absence of the 'RENDERING_RESUMING_BIT',
-- 'RENDERING_SUSPENDING_BIT', and
-- 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT' flags. No action or
-- synchronization commands, or other render pass instances, are allowed
-- between suspending and resuming render pass instances.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'RenderingFlags'
newtype RenderingFlagBits = RenderingFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT' specifies that draw
-- calls for the render pass instance will be recorded in secondary command
-- buffers. If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nestedCommandBuffer nestedCommandBuffer>
-- feature is enabled, the draw calls /can/ come from both inline and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands'.
pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT = RenderingFlagBits 0x00000001

-- | 'RENDERING_SUSPENDING_BIT' specifies that the render pass instance will
-- be suspended.
pattern RENDERING_SUSPENDING_BIT = RenderingFlagBits 0x00000002

-- | 'RENDERING_RESUMING_BIT' specifies that the render pass instance is
-- resuming an earlier suspended render pass instance.
pattern RENDERING_RESUMING_BIT = RenderingFlagBits 0x00000004

-- | 'RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT' specifies that
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-legacy-dithering Legacy Dithering>
-- is enabled for the render pass instance.
pattern RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT = RenderingFlagBits 0x00000008

-- | 'RENDERING_CONTENTS_INLINE_BIT_EXT' specifies that draw calls for the
-- render pass instance /can/ be recorded inline within the current command
-- buffer. When the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nestedCommandBuffer nestedCommandBuffer>
-- feature is enabled this /can/ be combined with the
-- 'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT' bit to allow draw
-- calls to be recorded both inline and in secondary command buffers.
pattern RENDERING_CONTENTS_INLINE_BIT_EXT = RenderingFlagBits 0x00000010

conNameRenderingFlagBits :: String
conNameRenderingFlagBits = "RenderingFlagBits"

enumPrefixRenderingFlagBits :: String
enumPrefixRenderingFlagBits = "RENDERING_"

showTableRenderingFlagBits :: [(RenderingFlagBits, String)]
showTableRenderingFlagBits =
  [
    ( RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT
    , "CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT"
    )
  , (RENDERING_SUSPENDING_BIT, "SUSPENDING_BIT")
  , (RENDERING_RESUMING_BIT, "RESUMING_BIT")
  ,
    ( RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT
    , "ENABLE_LEGACY_DITHERING_BIT_EXT"
    )
  ,
    ( RENDERING_CONTENTS_INLINE_BIT_EXT
    , "CONTENTS_INLINE_BIT_EXT"
    )
  ]

instance Show RenderingFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixRenderingFlagBits
      showTableRenderingFlagBits
      conNameRenderingFlagBits
      (\(RenderingFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read RenderingFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixRenderingFlagBits
      showTableRenderingFlagBits
      conNameRenderingFlagBits
      RenderingFlagBits
