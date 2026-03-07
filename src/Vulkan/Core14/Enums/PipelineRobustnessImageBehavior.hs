{-# language CPP #-}
-- No documentation found for Chapter "PipelineRobustnessImageBehavior"
module Vulkan.Core14.Enums.PipelineRobustnessImageBehavior  (PipelineRobustnessImageBehavior( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT
                                                                                            , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED
                                                                                            , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS
                                                                                            , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2
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

-- | VkPipelineRobustnessImageBehavior - Enum controlling the robustness of
-- image accesses in a pipeline stage
--
-- = Description
--
-- -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT' specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>
--     image accesses follow the behavior of robust image access features
--     enabled for the device.
--
-- -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED' specifies that image
--     accesses /must/ not be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>.
--
-- -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS' specifies
--     that image accesses conform to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-image-access>
--     guarantees.
--
-- -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2' specifies
--     that image accesses conform to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-image-access2>
--     guarantees.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PhysicalDevicePipelineRobustnessProperties',
-- 'Vulkan.Core14.PhysicalDeviceVulkan14Properties',
-- 'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PipelineRobustnessCreateInfo'
newtype PipelineRobustnessImageBehavior = PipelineRobustnessImageBehavior Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPipelineRobustnessImageBehavior" "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT = PipelineRobustnessImageBehavior 0

-- No documentation found for Nested "VkPipelineRobustnessImageBehavior" "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED = PipelineRobustnessImageBehavior 1

-- No documentation found for Nested "VkPipelineRobustnessImageBehavior" "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS = PipelineRobustnessImageBehavior 2

-- No documentation found for Nested "VkPipelineRobustnessImageBehavior" "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2 = PipelineRobustnessImageBehavior 3

{-# COMPLETE
  PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2 ::
    PipelineRobustnessImageBehavior
  #-}

conNamePipelineRobustnessImageBehavior :: String
conNamePipelineRobustnessImageBehavior = "PipelineRobustnessImageBehavior"

enumPrefixPipelineRobustnessImageBehavior :: String
enumPrefixPipelineRobustnessImageBehavior = "PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_"

showTablePipelineRobustnessImageBehavior :: [(PipelineRobustnessImageBehavior, String)]
showTablePipelineRobustnessImageBehavior =
  [
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT
    , "DEVICE_DEFAULT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED
    , "DISABLED"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS
    , "ROBUST_IMAGE_ACCESS"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2
    , "ROBUST_IMAGE_ACCESS_2"
    )
  ]

instance Show PipelineRobustnessImageBehavior where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRobustnessImageBehavior
      showTablePipelineRobustnessImageBehavior
      conNamePipelineRobustnessImageBehavior
      (\(PipelineRobustnessImageBehavior x) -> x)
      (showsPrec 11)

instance Read PipelineRobustnessImageBehavior where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRobustnessImageBehavior
      showTablePipelineRobustnessImageBehavior
      conNamePipelineRobustnessImageBehavior
      PipelineRobustnessImageBehavior
