{-# language CPP #-}
-- | = Name
--
-- VK_KHR_portability_enumeration - instance extension
--
-- == VK_KHR_portability_enumeration
--
-- [__Name String__]
--     @VK_KHR_portability_enumeration@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     395
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Charles Giessen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_portability_enumeration] @charles-lunarg%0A*Here describe the issue or question you have about the VK_KHR_portability_enumeration extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_portability_subset@
--
-- [__Contributors__]
--
--     -   Lenny Komow, LunarG
--
--     -   Charles Giessen, LunarG
--
-- == Description
--
-- This extension allows applications to control whether devices that
-- expose the @VK_KHR_portability_subset@ extension are included in the
-- results of physical device enumeration. Since devices which support the
-- @VK_KHR_portability_subset@ extension are not fully conformant Vulkan
-- implementations, the Vulkan loader does not report those devices unless
-- the application explicitly asks for them. This prevents applications
-- which may not be aware of non-conformant devices from accidentally using
-- them, as any device which supports the @VK_KHR_portability_subset@
-- extension mandates that the extension must be enabled if that device is
-- used.
--
-- This extension is implemented in the loader.
--
-- == New Enum Constants
--
-- -   'KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME'
--
-- -   'KHR_PORTABILITY_ENUMERATION_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.InstanceCreateFlagBits.InstanceCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.InstanceCreateFlagBits.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR'
--
-- == Version History
--
-- -   Revision 1, 2021-06-02 (Lenny Komow)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_portability_enumeration Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_portability_enumeration  ( KHR_PORTABILITY_ENUMERATION_SPEC_VERSION
                                                         , pattern KHR_PORTABILITY_ENUMERATION_SPEC_VERSION
                                                         , KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
                                                         , pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
                                                         ) where

import Data.String (IsString)

type KHR_PORTABILITY_ENUMERATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PORTABILITY_ENUMERATION_SPEC_VERSION"
pattern KHR_PORTABILITY_ENUMERATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PORTABILITY_ENUMERATION_SPEC_VERSION = 1


type KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME = "VK_KHR_portability_enumeration"

-- No documentation found for TopLevel "VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME"
pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME = "VK_KHR_portability_enumeration"

