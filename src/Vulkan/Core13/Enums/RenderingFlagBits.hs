{-# language CPP #-}
-- No documentation found for Chapter "RenderingFlagBits"
module Vulkan.Core13.Enums.RenderingFlagBits  ( RenderingFlags
                                              , RenderingFlagBits( RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT
                                                                 , RENDERING_SUSPENDING_BIT
                                                                 , RENDERING_RESUMING_BIT
                                                                 , RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR
                                                                 , RENDERING_CUSTOM_RESOLVE_BIT_EXT
                                                                 , RENDERING_FRAGMENT_REGION_BIT_EXT
                                                                 , RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
                                                                 , RENDERING_CONTENTS_INLINE_BIT_KHR
                                                                 , RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT
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
type RenderingFlags = RenderingFlagBits

-- | VkRenderingFlagBits - Bitmask specifying additional properties of a
-- dynamic render pass instance
--
-- = Description
--
-- -   'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT' specifies that
--     draw calls for the render pass instance will be recorded in
--     secondary command buffers. If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nestedCommandBuffer nestedCommandBuffer>
--     feature is enabled, the draw calls /can/ come from both inline and
--     'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands'.
--
-- -   'RENDERING_RESUMING_BIT' specifies that the render pass instance is
--     resuming an earlier suspended render pass instance.
--
-- -   'RENDERING_SUSPENDING_BIT' specifies that the render pass instance
--     will be suspended.
--
-- -   'RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT' specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-legacy-dithering Legacy Dithering>
--     is enabled for the render pass instance.
--
-- -   'RENDERING_CONTENTS_INLINE_BIT_KHR' specifies that draw calls for
--     the render pass instance /can/ be recorded inline within the current
--     command buffer. This /can/ be combined with the
--     'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT' bit to allow draw
--     calls to be recorded both inline and in secondary command buffers.
--
-- -   'RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE' specifies that the
--     render pass /can/ be used with layered fragment density maps.
--
-- -   'RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR' specifies
--     that
--     'Vulkan.Extensions.VK_KHR_maintenance10.RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR'
--     will always be specified for any attachment which invokes the
--     behavior described by
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#rendering-attachment-input-attachment-feedback that flag>.
--
-- -   'RENDERING_FRAGMENT_REGION_BIT_EXT' specifies that the render pass
--     /can/ access samples which are not covered in its
--     'Vulkan.Core10.FundamentalTypes.SampleMask'.
--
-- -   'RENDERING_CUSTOM_RESOLVE_BIT_EXT' specifies that the render pass
--     contains a custom resolve. When this bit is set,
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     /can/ be called.
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

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT"
pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT = RenderingFlagBits 0x00000001

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_SUSPENDING_BIT"
pattern RENDERING_SUSPENDING_BIT = RenderingFlagBits 0x00000002

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_RESUMING_BIT"
pattern RENDERING_RESUMING_BIT = RenderingFlagBits 0x00000004

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR"
pattern RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR = RenderingFlagBits 0x00000100

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_CUSTOM_RESOLVE_BIT_EXT"
pattern RENDERING_CUSTOM_RESOLVE_BIT_EXT = RenderingFlagBits 0x00000080

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_FRAGMENT_REGION_BIT_EXT"
pattern RENDERING_FRAGMENT_REGION_BIT_EXT = RenderingFlagBits 0x00000040

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
pattern RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE = RenderingFlagBits 0x00000020

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_CONTENTS_INLINE_BIT_KHR"
pattern RENDERING_CONTENTS_INLINE_BIT_KHR = RenderingFlagBits 0x00000010

-- No documentation found for Nested "VkRenderingFlagBits" "VK_RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT"
pattern RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT = RenderingFlagBits 0x00000008

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
    ( RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR
    , "LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR"
    )
  ,
    ( RENDERING_CUSTOM_RESOLVE_BIT_EXT
    , "CUSTOM_RESOLVE_BIT_EXT"
    )
  ,
    ( RENDERING_FRAGMENT_REGION_BIT_EXT
    , "FRAGMENT_REGION_BIT_EXT"
    )
  ,
    ( RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
    , "PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
    )
  ,
    ( RENDERING_CONTENTS_INLINE_BIT_KHR
    , "CONTENTS_INLINE_BIT_KHR"
    )
  ,
    ( RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT
    , "ENABLE_LEGACY_DITHERING_BIT_EXT"
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
