{-# language CPP #-}
-- | = Name
--
-- VK_NVX_binary_import - device extension
--
-- == VK_NVX_binary_import
--
-- [__Name String__]
--     @VK_NVX_binary_import@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     30
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NVX_binary_import] @ewerness-nv%0A<<Here describe the issue or question you have about the VK_NVX_binary_import extension>> >
--
--     -   Liam Middlebrook
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NVX_binary_import] @liam-middlebrook%0A<<Here describe the issue or question you have about the VK_NVX_binary_import extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-09
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
-- == Description
--
-- This extension allows applications to import CuBIN binaries and execute
-- them.
--
-- Note
--
-- There is currently no specification language written for this extension,
-- so although it appears in the Vulkan headers, its interfaces are not
-- summarized here.
--
-- == Version History
--
-- -   Revision 1, 2021-04-09 (Eric Werness)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CuFunctionCreateInfoNVX', 'Vulkan.Extensions.Handles.CuFunctionNVX',
-- 'CuLaunchInfoNVX', 'CuModuleCreateInfoNVX',
-- 'Vulkan.Extensions.Handles.CuModuleNVX', 'cmdCuLaunchKernelNVX',
-- 'createCuFunctionNVX', 'createCuModuleNVX', 'destroyCuFunctionNVX',
-- 'destroyCuModuleNVX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_binary_import Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NVX_binary_import  ( CuFunctionCreateInfoNVX
                                               , CuLaunchInfoNVX
                                               , CuModuleCreateInfoNVX
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CuFunctionCreateInfoNVX

instance ToCStruct CuFunctionCreateInfoNVX
instance Show CuFunctionCreateInfoNVX

instance FromCStruct CuFunctionCreateInfoNVX


data CuLaunchInfoNVX

instance ToCStruct CuLaunchInfoNVX
instance Show CuLaunchInfoNVX

instance FromCStruct CuLaunchInfoNVX


data CuModuleCreateInfoNVX

instance ToCStruct CuModuleCreateInfoNVX
instance Show CuModuleCreateInfoNVX

instance FromCStruct CuModuleCreateInfoNVX

