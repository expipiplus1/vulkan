{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance1 - device extension
--
-- == VK_KHR_maintenance1
--
-- [__Name String__]
--     @VK_KHR_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     70
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance1] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_maintenance1 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-03-13
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Dan Ginsburg, Valve
--
--     -   Daniel Koch, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
--     -   John Kessenich, Google
--
--     -   Michael Worcester, Imagination Technologies
--
--     -   Neil Henning, Codeplay Software Ltd.
--
--     -   Piers Daniell, NVIDIA
--
--     -   Slawomir Grajewski, Intel
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- @VK_KHR_maintenance1@ adds a collection of minor features that were
-- intentionally left out or overlooked from the original Vulkan 1.0
-- release.
--
-- The new features are as follows:
--
-- -   Allow 2D and 2D array image views to be created from 3D images,
--     which can then be used as color framebuffer attachments. This allows
--     applications to render to slices of a 3D image.
--
-- -   Support 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage' between
--     2D array layers and 3D slices. This extension allows copying from
--     layers of a 2D array image to slices of a 3D image and vice versa.
--
-- -   Allow negative height to be specified in the
--     'Vulkan.Core10.Pipeline.Viewport'::@height@ field to perform
--     y-inversion of the clip-space to framebuffer-space transform. This
--     allows apps to avoid having to use @gl_Position.y = -gl_Position.y@
--     in shaders also targeting other APIs.
--
-- -   Allow implementations to express support for doing just transfers
--     and clears of image formats that they otherwise support no other
--     format features for. This is done by adding new format feature flags
--     'FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR' and
--     'FORMAT_FEATURE_TRANSFER_DST_BIT_KHR'.
--
-- -   Support 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer' on
--     transfer-only queues. Previously
--     'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer' was defined to
--     only work on command buffers allocated from command pools which
--     support graphics or compute queues. It is now allowed on queues that
--     just support transfer operations.
--
-- -   Fix the inconsistency of how error conditions are returned between
--     the 'Vulkan.Core10.Pipeline.createGraphicsPipelines' and
--     'Vulkan.Core10.Pipeline.createComputePipelines' functions and the
--     'Vulkan.Core10.DescriptorSet.allocateDescriptorSets' and
--     'Vulkan.Core10.CommandBuffer.allocateCommandBuffers' functions.
--
-- -   Add new 'ERROR_OUT_OF_POOL_MEMORY_KHR' error so implementations can
--     give a more precise reason for
--     'Vulkan.Core10.DescriptorSet.allocateDescriptorSets' failures.
--
-- -   Add a new command 'trimCommandPoolKHR' which gives the
--     implementation an opportunity to release any unused command pool
--     memory back to the system.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'trimCommandPoolKHR'
--
-- == New Bitmasks
--
-- -   'CommandPoolTrimFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE1_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE1_SPEC_VERSION'
--
-- -   'KHR_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'FORMAT_FEATURE_TRANSFER_DST_BIT_KHR'
--
--     -   'FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_OUT_OF_POOL_MEMORY_KHR'
--
-- == Issues
--
-- 1.  Are viewports with zero height allowed?
--
--     __RESOLVED__: Yes, although they have low utility.
--
-- == Version History
--
-- -   Revision 1, 2016-10-26 (Piers Daniell)
--
--     -   Internal revisions
--
-- -   Revision 2, 2018-03-13 (Jon Leech)
--
--     -   Add issue for zero-height viewports
--
-- == See Also
--
-- 'CommandPoolTrimFlagsKHR', 'trimCommandPoolKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance1  ( pattern KHR_MAINTENANCE1_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE1_EXTENSION_NAME
                                              , pattern ERROR_OUT_OF_POOL_MEMORY_KHR
                                              , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
                                              , pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
                                              , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
                                              , trimCommandPoolKHR
                                              , CommandPoolTrimFlagsKHR
                                              , KHR_MAINTENANCE_1_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_1_SPEC_VERSION
                                              , KHR_MAINTENANCE_1_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_1_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance1 (trimCommandPool)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags)
import Vulkan.Core10.Enums.Result (Result(ERROR_OUT_OF_POOL_MEMORY))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_TRANSFER_DST_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_TRANSFER_SRC_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT))
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_SPEC_VERSION"
pattern KHR_MAINTENANCE1_SPEC_VERSION = KHR_MAINTENANCE_1_SPEC_VERSION


-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_EXTENSION_NAME"
pattern KHR_MAINTENANCE1_EXTENSION_NAME = KHR_MAINTENANCE_1_EXTENSION_NAME


-- No documentation found for TopLevel "VK_ERROR_OUT_OF_POOL_MEMORY_KHR"
pattern ERROR_OUT_OF_POOL_MEMORY_KHR = ERROR_OUT_OF_POOL_MEMORY


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR = FORMAT_FEATURE_TRANSFER_SRC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR = FORMAT_FEATURE_TRANSFER_DST_BIT


-- No documentation found for TopLevel "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR = IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT


-- No documentation found for TopLevel "vkTrimCommandPoolKHR"
trimCommandPoolKHR = trimCommandPool


-- No documentation found for TopLevel "VkCommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags


type KHR_MAINTENANCE_1_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_1_SPEC_VERSION"
pattern KHR_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_1_SPEC_VERSION = 2


type KHR_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_maintenance1"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_1_EXTENSION_NAME"
pattern KHR_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_maintenance1"

