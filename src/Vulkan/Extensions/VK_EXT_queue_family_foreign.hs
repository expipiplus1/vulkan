{-# language CPP #-}
-- | = Name
--
-- VK_EXT_queue_family_foreign - device extension
--
-- == VK_EXT_queue_family_foreign
--
-- [__Name String__]
--     @VK_EXT_queue_family_foreign@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     127
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Chad Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_queue_family_foreign] @chadversary%0A*Here describe the issue or question you have about the VK_EXT_queue_family_foreign extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-11-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Chad Versace, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
--     -   Jesse Hall, Google
--
--     -   Daniel Rakos, AMD
--
--     -   Ray Smith, ARM
--
-- == Description
--
-- This extension defines a special queue family,
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT', which can be used
-- to transfer ownership of resources backed by external memory to foreign,
-- external queues. This is similar to
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR', defined in
-- @VK_KHR_external_memory@. The key differences between the two are:
--
-- -   The queues represented by
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR' must share
--     the same physical device and the same driver version as the current
--     'Vulkan.Core10.Handles.Instance'.
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT' has no such
--     restrictions. It can represent devices and drivers from other
--     vendors, and can even represent non-Vulkan-capable devices.
--
-- -   All resources backed by external memory support
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR'. Support for
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT' is more
--     restrictive.
--
-- -   Applications should expect transitions to\/from
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT' to be more
--     expensive than transitions to\/from
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR'.
--
-- == New Enum Constants
--
-- -   'EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME'
--
-- -   'EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- == Version History
--
-- -   Revision 1, 2017-11-01 (Chad Versace)
--
--     -   Squashed internal revisions
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_queue_family_foreign Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_queue_family_foreign  ( EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
                                                      , pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
                                                      , EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
                                                      , pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
                                                      , QUEUE_FAMILY_FOREIGN_EXT
                                                      , pattern QUEUE_FAMILY_FOREIGN_EXT
                                                      ) where

import Data.String (IsString)
import Vulkan.Core10.APIConstants (QUEUE_FAMILY_FOREIGN_EXT)
import Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_FOREIGN_EXT)
type EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION"
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1


type EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = "VK_EXT_queue_family_foreign"

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME"
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = "VK_EXT_queue_family_foreign"

