{-# language CPP #-}
-- No documentation found for Chapter "DependencyFlagBits"
module Vulkan.Core10.Enums.DependencyFlagBits  ( DependencyFlags
                                               , DependencyFlagBits( DEPENDENCY_BY_REGION_BIT
                                                                   , DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR
                                                                   , DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR
                                                                   , DEPENDENCY_FEEDBACK_LOOP_BIT_EXT
                                                                   , DEPENDENCY_VIEW_LOCAL_BIT
                                                                   , DEPENDENCY_DEVICE_GROUP_BIT
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
type DependencyFlags = DependencyFlagBits

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = Description
--
-- -   'DEPENDENCY_BY_REGION_BIT' specifies that dependencies will be split
--     into multiple
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local regions>
--     according to the (x,y,layer,sample) coordinates.
--
-- -   'DEPENDENCY_VIEW_LOCAL_BIT' specifies that dependencies will be
--     split into multiple
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local regions>
--     according to the view.
--
-- -   'DEPENDENCY_DEVICE_GROUP_BIT' specifies that dependencies are
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-device-local-dependencies non-device-local>.
--
-- -   'DEPENDENCY_FEEDBACK_LOOP_BIT_EXT' specifies that the render pass
--     will write to and read from the same image with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-feedbackloop feedback loop enabled>.
--
-- -   'DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR'
--     specifies that source and destination stages are not ignored when
--     performing a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
--
-- -   'DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR' specifies that
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2'
--     /must/ only include the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
--     of the first synchronization scope, and that
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'
--     /must/ specify the complete barrier.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'DependencyFlags'
newtype DependencyFlagBits = DependencyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_BY_REGION_BIT"
pattern DEPENDENCY_BY_REGION_BIT = DependencyFlagBits 0x00000001

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR"
pattern DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR = DependencyFlagBits 0x00000040

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR"
pattern DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR = DependencyFlagBits 0x00000020

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_FEEDBACK_LOOP_BIT_EXT"
pattern DEPENDENCY_FEEDBACK_LOOP_BIT_EXT = DependencyFlagBits 0x00000008

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_VIEW_LOCAL_BIT"
pattern DEPENDENCY_VIEW_LOCAL_BIT = DependencyFlagBits 0x00000002

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_DEVICE_GROUP_BIT"
pattern DEPENDENCY_DEVICE_GROUP_BIT = DependencyFlagBits 0x00000004

conNameDependencyFlagBits :: String
conNameDependencyFlagBits = "DependencyFlagBits"

enumPrefixDependencyFlagBits :: String
enumPrefixDependencyFlagBits = "DEPENDENCY_"

showTableDependencyFlagBits :: [(DependencyFlagBits, String)]
showTableDependencyFlagBits =
  [ (DEPENDENCY_BY_REGION_BIT, "BY_REGION_BIT")
  ,
    ( DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR
    , "ASYMMETRIC_EVENT_BIT_KHR"
    )
  ,
    ( DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR
    , "QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR"
    )
  ,
    ( DEPENDENCY_FEEDBACK_LOOP_BIT_EXT
    , "FEEDBACK_LOOP_BIT_EXT"
    )
  , (DEPENDENCY_VIEW_LOCAL_BIT, "VIEW_LOCAL_BIT")
  ,
    ( DEPENDENCY_DEVICE_GROUP_BIT
    , "DEVICE_GROUP_BIT"
    )
  ]

instance Show DependencyFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixDependencyFlagBits
      showTableDependencyFlagBits
      conNameDependencyFlagBits
      (\(DependencyFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DependencyFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixDependencyFlagBits
      showTableDependencyFlagBits
      conNameDependencyFlagBits
      DependencyFlagBits
