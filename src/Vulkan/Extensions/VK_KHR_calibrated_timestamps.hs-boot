{-# language CPP #-}
-- | = Name
--
-- VK_KHR_calibrated_timestamps - device extension
--
-- = VK_KHR_calibrated_timestamps
--
-- [__Name String__]
--     @VK_KHR_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     544
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_calibrated_timestamps] @aqnuep%0A*Here describe the issue or question you have about the VK_KHR_calibrated_timestamps extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_calibrated_timestamps.adoc VK_EXT_calibrated_timestamps>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Faith Ekstrand, Intel
--
--     -   Keith Packard, Valve
--
-- == Description
--
-- This extension provides an interface to query calibrated timestamps
-- obtained quasi simultaneously from two time domains.
--
-- == New Commands
--
-- -   'getCalibratedTimestampsKHR'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoKHR'
--
-- == New Enums
--
-- -   'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR'
--
-- == New Enum Constants
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-07-12 (Daniel Rakos)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_calibrated_timestamps Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_calibrated_timestamps  (CalibratedTimestampInfoKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role CalibratedTimestampInfoKHR nominal
data CalibratedTimestampInfoKHR (es :: [Type])

instance ( Extendss CalibratedTimestampInfoKHR es
         , PokeChain es ) => ToCStruct (CalibratedTimestampInfoKHR es)
instance Show (Chain es) => Show (CalibratedTimestampInfoKHR es)

instance ( Extendss CalibratedTimestampInfoKHR es
         , PeekChain es ) => FromCStruct (CalibratedTimestampInfoKHR es)

