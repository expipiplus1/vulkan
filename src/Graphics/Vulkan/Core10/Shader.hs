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


-- No complete pragma for ShaderModuleCreateFlags as it has no patterns


-- | VkShaderModuleCreateInfo - Structure specifying parameters of a newly
-- created shader module
--
-- == Valid Usage
--
-- -   @codeSize@ /must/ be greater than 0
--
-- -   @codeSize@ /must/ be a multiple of 4
--
-- -   @pCode@ /must/ point to valid SPIR-V code, formatted and packed as
--     described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--
-- -   @pCode@ /must/ adhere to the validation rules described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-module-validation Validation Rules within a Module>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT'
--
-- -   @flags@ /must/ be @0@
--
-- -   @pCode@ /must/ be a valid pointer to an array of
--     \(\textrm{codeSize} \over 4\) @uint32_t@ values
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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pShaderModule@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Shader.VkShaderModule' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader.VK_ERROR_INVALID_SHADER_NV'
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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @shaderModule@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @shaderModule@
--     /must/ be a valid 'Graphics.Vulkan.C.Core10.Shader.VkShaderModule'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @shaderModule@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @shaderModule@ /must/ be externally synchronized
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
