{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  , ShaderModuleCreateFlags
  , withCStructShaderModuleCreateInfo
  , fromCStructShaderModuleCreateInfo
  , ShaderModuleCreateInfo(..)
  , createShaderModule
  , destroyShaderModule
  , withShaderModule
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModuleCreateInfo(..)
  , VkShaderModule
  , vkCreateShaderModule
  , vkDestroyShaderModule
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.C.Core10.Shader.vkDestroyShaderModule'
type ShaderModule = VkShaderModule

-- | VkShaderModuleCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateFlags' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'
type ShaderModuleCreateFlags = VkShaderModuleCreateFlags


-- | VkShaderModuleCreateInfo - Structure specifying parameters of a newly
-- created shader module
--
-- == Valid Usage
--
-- -   @codeSize@ /must/ be greater than 0
--
-- -   If @pCode@ points to SPIR-V code, @codeSize@ /must/ be a multiple of
--     4
--
-- -   @pCode@ /must/ point to either valid SPIR-V code, formatted and
--     packed as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--     or valid GLSL code which /must/ be written to the
--     @GL_KHR_vulkan_glsl@ extension specification
--
-- -   If @pCode@ points to SPIR-V code, that code /must/ adhere to the
--     validation rules described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-module-validation Validation Rules within a Module>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   If @pCode@ points to GLSL code, it /must/ be valid GLSL code written
--     to the @GL_KHR_vulkan_glsl@ GLSL extension specification
--
-- -   @pCode@ /must/ declare the @Shader@ capability for SPIR-V code
--
-- -   @pCode@ /must/ not declare any capability that is not supported by
--     the API, as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-module-validation Capabilities>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   If @pCode@ declares any of the capabilities listed as /optional/ in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-capabilities-table SPIR-V Environment>
--     appendix, the corresponding feature(s) /must/ be enabled.
--
-- Unresolved directive in VkShaderModuleCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkShaderModuleCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule'
data ShaderModuleCreateInfo = ShaderModuleCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ShaderModuleCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "flags"
  flags :: ShaderModuleCreateFlags
  -- Length multiple valued member elided
  , -- No documentation found for Nested "ShaderModuleCreateInfo" "pCode"
  code :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkShaderModuleCreateInfo' and
-- marshal a 'ShaderModuleCreateInfo' into it. The 'VkShaderModuleCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructShaderModuleCreateInfo :: ShaderModuleCreateInfo -> (VkShaderModuleCreateInfo -> IO a) -> IO a
withCStructShaderModuleCreateInfo marshalled cont = withVec (&) (code (marshalled :: ShaderModuleCreateInfo)) (\pPCode -> maybeWith withSomeVkStruct (next (marshalled :: ShaderModuleCreateInfo)) (\pPNext -> cont (VkShaderModuleCreateInfo VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO pPNext (flags (marshalled :: ShaderModuleCreateInfo)) (4 * fromIntegral (Data.Vector.length (code (marshalled :: ShaderModuleCreateInfo)))) pPCode)))

-- | A function to read a 'VkShaderModuleCreateInfo' and all additional
-- structures in the pointer chain into a 'ShaderModuleCreateInfo'.
fromCStructShaderModuleCreateInfo :: VkShaderModuleCreateInfo -> IO ShaderModuleCreateInfo
fromCStructShaderModuleCreateInfo c = ShaderModuleCreateInfo <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkShaderModuleCreateInfo)))
                                                             <*> pure (vkFlags (c :: VkShaderModuleCreateInfo))
                                                             -- Length multiple valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkCodeSize (c :: VkShaderModuleCreateInfo)) `quot` 4) (peekElemOff (vkPCode (c :: VkShaderModuleCreateInfo))))

instance Zero ShaderModuleCreateInfo where
  zero = ShaderModuleCreateInfo Nothing
                                zero
                                Data.Vector.empty



-- | vkCreateShaderModule - Creates a new shader module object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the shader module.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'
--     structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pShaderModule@ points to a
--     'Graphics.Vulkan.C.Core10.Shader.VkShaderModule' handle in which the
--     resulting shader module object is returned.
--
-- = Description
--
-- Once a shader module has been created, any entry points it contains
-- /can/ be used in pipeline shader stages as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-compute Compute Pipelines>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-graphics Graphics Pipelines>.
--
-- If the shader stage fails to compile
-- 'Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader.VK_ERROR_INVALID_SHADER_NV'
-- will be generated and the compile log will be reported back to the
-- application by
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_EXT_debug_report@
-- if enabled.
--
-- Unresolved directive in vkCreateShaderModule.txt -
-- include::{generated}\/validity\/protos\/vkCreateShaderModule.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModule',
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'
createShaderModule :: Device ->  ShaderModuleCreateInfo ->  Maybe AllocationCallbacks ->  IO (ShaderModule)
createShaderModule = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pShaderModule' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructShaderModuleCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateShaderModule commandTable device' pCreateInfo' pAllocator pShaderModule' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pShaderModule')))))


-- | vkDestroyShaderModule - Destroy a shader module
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the shader module.
--
-- -   @shaderModule@ is the handle of the shader module to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- A shader module /can/ be destroyed while pipelines created using its
-- shaders are still in use.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @shaderModule@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @shaderModule@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- Unresolved directive in vkDestroyShaderModule.txt -
-- include::{generated}\/validity\/protos\/vkDestroyShaderModule.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModule'
destroyShaderModule :: Device ->  ShaderModule ->  Maybe AllocationCallbacks ->  IO ()
destroyShaderModule = \(Device device' commandTable) -> \shaderModule' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyShaderModule commandTable device' shaderModule' pAllocator *> (pure ()))

-- | A safe wrapper for 'createShaderModule' and 'destroyShaderModule' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withShaderModule
  :: Device -> ShaderModuleCreateInfo -> Maybe (AllocationCallbacks) -> (ShaderModule -> IO a) -> IO a
withShaderModule device shaderModuleCreateInfo allocationCallbacks = bracket
  (createShaderModule device shaderModuleCreateInfo allocationCallbacks)
  (\o -> destroyShaderModule device o allocationCallbacks)
