{-# language CPP #-}
-- No documentation found for Chapter "PipelineRobustnessBufferBehavior"
module Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior  (PipelineRobustnessBufferBehavior( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT
                                                                                              , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED
                                                                                              , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS
                                                                                              , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2
                                                                                              , ..
                                                                                              )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPipelineRobustnessBufferBehavior - Enum controlling the robustness of
-- buffer accesses in a pipeline stage
--
-- = Description
--
-- -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT' specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>
--     buffer accesses follow the behavior of robust buffer access features
--     enabled for the device.
--
-- -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED' specifies that buffer
--     accesses /must/ not be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>.
--
-- -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS' specifies
--     that buffer accesses conform to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-buffer-access>
--     guarantees.
--
-- -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     specifies that buffer accesses conform to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-buffer-access2>
--     guarantees.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PhysicalDevicePipelineRobustnessProperties',
-- 'Vulkan.Core14.PhysicalDeviceVulkan14Properties',
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PipelineRobustnessCreateInfo'
newtype PipelineRobustnessBufferBehavior = PipelineRobustnessBufferBehavior Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPipelineRobustnessBufferBehavior" "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT = PipelineRobustnessBufferBehavior 0

-- No documentation found for Nested "VkPipelineRobustnessBufferBehavior" "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED = PipelineRobustnessBufferBehavior 1

-- No documentation found for Nested "VkPipelineRobustnessBufferBehavior" "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS = PipelineRobustnessBufferBehavior 2

-- No documentation found for Nested "VkPipelineRobustnessBufferBehavior" "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2 = PipelineRobustnessBufferBehavior 3

{-# COMPLETE
  PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2 ::
    PipelineRobustnessBufferBehavior
  #-}

conNamePipelineRobustnessBufferBehavior :: String
conNamePipelineRobustnessBufferBehavior = "PipelineRobustnessBufferBehavior"

enumPrefixPipelineRobustnessBufferBehavior :: String
enumPrefixPipelineRobustnessBufferBehavior = "PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_"

showTablePipelineRobustnessBufferBehavior :: [(PipelineRobustnessBufferBehavior, String)]
showTablePipelineRobustnessBufferBehavior =
  [
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT
    , "DEVICE_DEFAULT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED
    , "DISABLED"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS
    , "ROBUST_BUFFER_ACCESS"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2
    , "ROBUST_BUFFER_ACCESS_2"
    )
  ]

instance Show PipelineRobustnessBufferBehavior where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRobustnessBufferBehavior
      showTablePipelineRobustnessBufferBehavior
      conNamePipelineRobustnessBufferBehavior
      (\(PipelineRobustnessBufferBehavior x) -> x)
      (showsPrec 11)

instance Read PipelineRobustnessBufferBehavior where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRobustnessBufferBehavior
      showTablePipelineRobustnessBufferBehavior
      conNamePipelineRobustnessBufferBehavior
      PipelineRobustnessBufferBehavior
