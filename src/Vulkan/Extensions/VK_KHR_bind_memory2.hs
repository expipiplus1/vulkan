{-# language CPP #-}
-- | = Name
--
-- VK_KHR_bind_memory2 - device extension
--
-- == VK_KHR_bind_memory2
--
-- [__Name String__]
--     @VK_KHR_bind_memory2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     158
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_bind_memory2] @tobski%0A*Here describe the issue or question you have about the VK_KHR_bind_memory2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tobias Hector, Imagination Technologies
--
-- == Description
--
-- This extension provides versions of
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory' and
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory' that allow multiple
-- bindings to be performed at once, and are extensible.
--
-- This extension also introduces 'IMAGE_CREATE_ALIAS_BIT_KHR', which
-- allows “identical” images that alias the same memory to interpret the
-- contents consistently, even across image layout changes.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'bindBufferMemory2KHR'
--
-- -   'bindImageMemory2KHR'
--
-- == New Structures
--
-- -   'BindBufferMemoryInfoKHR'
--
-- -   'BindImageMemoryInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_BIND_MEMORY_2_EXTENSION_NAME'
--
-- -   'KHR_BIND_MEMORY_2_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'IMAGE_CREATE_ALIAS_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2017-05-19 (Tobias Hector)
--
--     -   Pulled bind memory functions into their own extension
--
-- == See Also
--
-- 'BindBufferMemoryInfoKHR', 'BindImageMemoryInfoKHR',
-- 'bindBufferMemory2KHR', 'bindImageMemory2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_bind_memory2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_bind_memory2  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
                                              , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
                                              , pattern IMAGE_CREATE_ALIAS_BIT_KHR
                                              , bindBufferMemory2KHR
                                              , bindImageMemory2KHR
                                              , BindBufferMemoryInfoKHR
                                              , BindImageMemoryInfoKHR
                                              , KHR_BIND_MEMORY_2_SPEC_VERSION
                                              , pattern KHR_BIND_MEMORY_2_SPEC_VERSION
                                              , KHR_BIND_MEMORY_2_EXTENSION_NAME
                                              , pattern KHR_BIND_MEMORY_2_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (bindBufferMemory2)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (bindImageMemory2)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindBufferMemoryInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindImageMemoryInfo)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_ALIAS_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO


-- No documentation found for TopLevel "VK_IMAGE_CREATE_ALIAS_BIT_KHR"
pattern IMAGE_CREATE_ALIAS_BIT_KHR = IMAGE_CREATE_ALIAS_BIT


-- No documentation found for TopLevel "vkBindBufferMemory2KHR"
bindBufferMemory2KHR = bindBufferMemory2


-- No documentation found for TopLevel "vkBindImageMemory2KHR"
bindImageMemory2KHR = bindImageMemory2


-- No documentation found for TopLevel "VkBindBufferMemoryInfoKHR"
type BindBufferMemoryInfoKHR = BindBufferMemoryInfo


-- No documentation found for TopLevel "VkBindImageMemoryInfoKHR"
type BindImageMemoryInfoKHR = BindImageMemoryInfo


type KHR_BIND_MEMORY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_SPEC_VERSION"
pattern KHR_BIND_MEMORY_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_BIND_MEMORY_2_SPEC_VERSION = 1


type KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_EXTENSION_NAME"
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

