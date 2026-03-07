{-# language CPP #-}
-- No documentation found for Chapter "ToolPurposeFlagBits"
module Vulkan.Core13.Enums.ToolPurposeFlagBits  ( ToolPurposeFlags
                                                , ToolPurposeFlagBits( TOOL_PURPOSE_VALIDATION_BIT
                                                                     , TOOL_PURPOSE_PROFILING_BIT
                                                                     , TOOL_PURPOSE_TRACING_BIT
                                                                     , TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT
                                                                     , TOOL_PURPOSE_MODIFYING_FEATURES_BIT
                                                                     , TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT
                                                                     , TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT
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
type ToolPurposeFlags = ToolPurposeFlagBits

-- | VkToolPurposeFlagBits - Bitmask specifying the purposes of an active
-- tool
--
-- = Description
--
-- -   'TOOL_PURPOSE_VALIDATION_BIT' specifies that the tool provides
--     validation of API usage.
--
-- -   'TOOL_PURPOSE_PROFILING_BIT' specifies that the tool provides
--     profiling of API usage.
--
-- -   'TOOL_PURPOSE_TRACING_BIT' specifies that the tool is capturing data
--     about the application’s API usage, including anything from simple
--     logging to capturing data for later replay.
--
-- -   'TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT' specifies that the tool
--     provides additional API features\/extensions on top of the
--     underlying implementation.
--
-- -   'TOOL_PURPOSE_MODIFYING_FEATURES_BIT' specifies that the tool
--     modifies the API features\/limits\/extensions presented to the
--     application.
--
-- -   'TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT' specifies that the tool
--     reports additional information to the application via callbacks
--     specified by
--     'Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT'
--
-- -   'TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT' specifies that the tool
--     consumes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#debugging-debug-markers debug markers>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#debugging-object-debug-annotation object debug annotation>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#debugging-queue-labels queue labels>,
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#debugging-command-buffer-labels command buffer labels>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_tooling_info VK_EXT_tooling_info>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'ToolPurposeFlags'
newtype ToolPurposeFlagBits = ToolPurposeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_VALIDATION_BIT"
pattern TOOL_PURPOSE_VALIDATION_BIT = ToolPurposeFlagBits 0x00000001

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_PROFILING_BIT"
pattern TOOL_PURPOSE_PROFILING_BIT = ToolPurposeFlagBits 0x00000002

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_TRACING_BIT"
pattern TOOL_PURPOSE_TRACING_BIT = ToolPurposeFlagBits 0x00000004

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT"
pattern TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT = ToolPurposeFlagBits 0x00000008

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT"
pattern TOOL_PURPOSE_MODIFYING_FEATURES_BIT = ToolPurposeFlagBits 0x00000010

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT"
pattern TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT = ToolPurposeFlagBits 0x00000040

-- No documentation found for Nested "VkToolPurposeFlagBits" "VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT"
pattern TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT = ToolPurposeFlagBits 0x00000020

conNameToolPurposeFlagBits :: String
conNameToolPurposeFlagBits = "ToolPurposeFlagBits"

enumPrefixToolPurposeFlagBits :: String
enumPrefixToolPurposeFlagBits = "TOOL_PURPOSE_"

showTableToolPurposeFlagBits :: [(ToolPurposeFlagBits, String)]
showTableToolPurposeFlagBits =
  [ (TOOL_PURPOSE_VALIDATION_BIT, "VALIDATION_BIT")
  , (TOOL_PURPOSE_PROFILING_BIT, "PROFILING_BIT")
  , (TOOL_PURPOSE_TRACING_BIT, "TRACING_BIT")
  ,
    ( TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT
    , "ADDITIONAL_FEATURES_BIT"
    )
  ,
    ( TOOL_PURPOSE_MODIFYING_FEATURES_BIT
    , "MODIFYING_FEATURES_BIT"
    )
  ,
    ( TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT
    , "DEBUG_MARKERS_BIT_EXT"
    )
  ,
    ( TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT
    , "DEBUG_REPORTING_BIT_EXT"
    )
  ]

instance Show ToolPurposeFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixToolPurposeFlagBits
      showTableToolPurposeFlagBits
      conNameToolPurposeFlagBits
      (\(ToolPurposeFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ToolPurposeFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixToolPurposeFlagBits
      showTableToolPurposeFlagBits
      conNameToolPurposeFlagBits
      ToolPurposeFlagBits
