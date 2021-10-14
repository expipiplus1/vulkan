{-# language CPP #-}
-- | = Name
--
-- VK_EXT_validation_cache - device extension
--
-- == VK_EXT_validation_cache
--
-- [__Name String__]
--     @VK_EXT_validation_cache@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     161
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
--     -   Cort Stratton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_validation_cache] @cdwfs%0A<<Here describe the issue or question you have about the VK_EXT_validation_cache extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Cort Stratton, Google
--
--     -   Chris Forbes, Google
--
-- == Description
--
-- This extension provides a mechanism for caching the results of
-- potentially expensive internal validation operations across multiple
-- runs of a Vulkan application. At the core is the
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT' object type, which is
-- managed similarly to the existing 'Vulkan.Core10.Handles.PipelineCache'.
--
-- The new struct 'ShaderModuleValidationCacheCreateInfoEXT' can be
-- included in the @pNext@ chain at
-- 'Vulkan.Core10.Shader.createShaderModule' time. It contains a
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT' to use when validating
-- the 'Vulkan.Core10.Handles.ShaderModule'.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ValidationCacheEXT'
--
-- == New Commands
--
-- -   'createValidationCacheEXT'
--
-- -   'destroyValidationCacheEXT'
--
-- -   'getValidationCacheDataEXT'
--
-- -   'mergeValidationCachesEXT'
--
-- == New Structures
--
-- -   'ValidationCacheCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Shader.ShaderModuleCreateInfo':
--
--     -   'ShaderModuleValidationCacheCreateInfoEXT'
--
-- == New Enums
--
-- -   'ValidationCacheHeaderVersionEXT'
--
-- == New Bitmasks
--
-- -   'ValidationCacheCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VALIDATION_CACHE_EXTENSION_NAME'
--
-- -   'EXT_VALIDATION_CACHE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_VALIDATION_CACHE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2017-08-29 (Cort Stratton)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ShaderModuleValidationCacheCreateInfoEXT',
-- 'ValidationCacheCreateFlagsEXT', 'ValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT',
-- 'ValidationCacheHeaderVersionEXT', 'createValidationCacheEXT',
-- 'destroyValidationCacheEXT', 'getValidationCacheDataEXT',
-- 'mergeValidationCachesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_validation_cache Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_validation_cache  ( ShaderModuleValidationCacheCreateInfoEXT
                                                  , ValidationCacheCreateInfoEXT
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ShaderModuleValidationCacheCreateInfoEXT

instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT
instance Show ShaderModuleValidationCacheCreateInfoEXT

instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT


data ValidationCacheCreateInfoEXT

instance ToCStruct ValidationCacheCreateInfoEXT
instance Show ValidationCacheCreateInfoEXT

instance FromCStruct ValidationCacheCreateInfoEXT

