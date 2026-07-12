{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Extra device requirements.

Multiview + cube-map arrays for the shadow pass (a light's six cube faces in one
seam-free pass, one array element per light), and multi-draw-indirect for the
unified mesh pass (one draw call across all meshes). The sync staples the graph
driver submits with come from 'Vulkan.Utils.Frame.syncDeviceRequirements', which
both boots already merge in.
-}
module Requirements
  ( deviceRequirements
  ) where

import Vulkan.Core10 (PhysicalDeviceFeatures)
import Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import Vulkan.Requirement (DeviceRequirement)
import qualified Vulkan.Utils.Requirements.TH as U

deviceRequirements :: [DeviceRequirement]
deviceRequirements =
  [U.reqs|
    PhysicalDeviceMultiviewFeatures.multiview
    PhysicalDeviceFeatures.imageCubeArray
    PhysicalDeviceFeatures.multiDrawIndirect
  |]
