{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_info - device extension
--
-- == VK_AMD_shader_info
--
-- [__Name String__]
--     @VK_AMD_shader_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     43
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jaakko Konttinen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_info] @jaakkoamd%0A<<Here describe the issue or question you have about the VK_AMD_shader_info extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jaakko Konttinen, AMD
--
-- == Description
--
-- This extension adds a way to query certain information about a compiled
-- shader which is part of a pipeline. This information may include shader
-- disassembly, shader binary and various statistics about a shader’s
-- resource usage.
--
-- While this extension provides a mechanism for extracting this
-- information, the details regarding the contents or format of this
-- information are not specified by this extension and may be provided by
-- the vendor externally.
--
-- Furthermore, all information types are optionally supported, and users
-- should not assume every implementation supports querying every type of
-- information.
--
-- == New Commands
--
-- -   'getShaderInfoAMD'
--
-- == New Structures
--
-- -   'ShaderResourceUsageAMD'
--
-- -   'ShaderStatisticsInfoAMD'
--
-- == New Enums
--
-- -   'ShaderInfoTypeAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_INFO_EXTENSION_NAME'
--
-- -   'AMD_SHADER_INFO_SPEC_VERSION'
--
-- == Examples
--
-- This example extracts the register usage of a fragment shader within a
-- particular graphics pipeline:
--
-- > extern VkDevice device;
-- > extern VkPipeline gfxPipeline;
-- >
-- > PFN_vkGetShaderInfoAMD pfnGetShaderInfoAMD = (PFN_vkGetShaderInfoAMD)vkGetDeviceProcAddr(
-- >     device, "vkGetShaderInfoAMD");
-- >
-- > VkShaderStatisticsInfoAMD statistics = {};
-- >
-- > size_t dataSize = sizeof(statistics);
-- >
-- > if (pfnGetShaderInfoAMD(device,
-- >     gfxPipeline,
-- >     VK_SHADER_STAGE_FRAGMENT_BIT,
-- >     VK_SHADER_INFO_TYPE_STATISTICS_AMD,
-- >     &dataSize,
-- >     &statistics) == VK_SUCCESS)
-- > {
-- >     printf("VGPR usage: %d\n", statistics.resourceUsage.numUsedVgprs);
-- >     printf("SGPR usage: %d\n", statistics.resourceUsage.numUsedSgprs);
-- > }
--
-- The following example continues the previous example by subsequently
-- attempting to query and print shader disassembly about the fragment
-- shader:
--
-- > // Query disassembly size (if available)
-- > if (pfnGetShaderInfoAMD(device,
-- >     gfxPipeline,
-- >     VK_SHADER_STAGE_FRAGMENT_BIT,
-- >     VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD,
-- >     &dataSize,
-- >     nullptr) == VK_SUCCESS)
-- > {
-- >     printf("Fragment shader disassembly:\n");
-- >
-- >     void* disassembly = malloc(dataSize);
-- >
-- >     // Query disassembly and print
-- >     if (pfnGetShaderInfoAMD(device,
-- >         gfxPipeline,
-- >         VK_SHADER_STAGE_FRAGMENT_BIT,
-- >         VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD,
-- >         &dataSize,
-- >         disassembly) == VK_SUCCESS)
-- >     {
-- >         printf((char*)disassembly);
-- >     }
-- >
-- >     free(disassembly);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-10-09 (Jaakko Konttinen)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ShaderInfoTypeAMD', 'ShaderResourceUsageAMD',
-- 'ShaderStatisticsInfoAMD', 'getShaderInfoAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_info  ( ShaderResourceUsageAMD
                                             , ShaderStatisticsInfoAMD
                                             , ShaderInfoTypeAMD
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ShaderResourceUsageAMD

instance ToCStruct ShaderResourceUsageAMD
instance Show ShaderResourceUsageAMD

instance FromCStruct ShaderResourceUsageAMD


data ShaderStatisticsInfoAMD

instance ToCStruct ShaderStatisticsInfoAMD
instance Show ShaderStatisticsInfoAMD

instance FromCStruct ShaderStatisticsInfoAMD


data ShaderInfoTypeAMD

