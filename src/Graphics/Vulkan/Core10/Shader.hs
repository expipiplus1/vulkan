{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModule
  , vkCreateShaderModule
  , vkDestroyShaderModule
  , VkShaderModuleCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkShaderModuleCreateFlags

-- | VkShaderModuleCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkShaderModuleCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkShaderModuleCreateInfo'
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkShaderModuleCreateFlags where
  
  showsPrec p (VkShaderModuleCreateFlags x) = showParen (p >= 11) (showString "VkShaderModuleCreateFlags " . showsPrec 11 x)

instance Read VkShaderModuleCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderModuleCreateFlags")
                        v <- step readPrec
                        pure (VkShaderModuleCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkShaderModule_T
-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'vkCreateShaderModule', 'vkDestroyShaderModule'
type VkShaderModule = Ptr VkShaderModule_T
-- | vkCreateShaderModule - Creates a new shader module object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the shader module.
--
-- -   @pCreateInfo@ parameter is a pointer to an instance of the
--     @VkShaderModuleCreateInfo@ structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pShaderModule@ points to a @VkShaderModule@ handle in which the
--     resulting shader module object is returned.
--
-- = Description
--
-- Once a shader module has been created, any entry points it contains
-- /can/ be used in pipeline shader stages as described in [Compute
-- Pipelines](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-compute)
-- and [Graphics
-- Pipelines](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-graphics).
--
-- If the shader stage fails to compile @VK_ERROR_INVALID_SHADER_NV@ will
-- be generated and the compile log will be reported back to the
-- application by @{html_spec_relative}#VK_EXT_debug_report@ if enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkShaderModuleCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pShaderModule@ /must/ be a valid pointer to a @VkShaderModule@
--     handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkShaderModule', 'VkShaderModuleCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateShaderModule" vkCreateShaderModule :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult
-- | vkDestroyShaderModule - Destroy a shader module module
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the shader module.
--
-- -   @shaderModule@ is the handle of the shader module to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- = Description
--
-- A shader module /can/ be destroyed while pipelines created using its
-- shaders are still in use.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @shaderModule@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @shaderModule@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @shaderModule@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @shaderModule@
--     /must/ be a valid @VkShaderModule@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkShaderModule'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyShaderModule" vkDestroyShaderModule :: ("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
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
--     packed as described by the [Khronos SPIR-V
--     Specification](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirv-spec)
--     or valid GLSL code which /must/ be written to the GL_KHR_vulkan_glsl
--     extension specification
--
-- -   If @pCode@ points to SPIR-V code, that code /must/ adhere to the
--     validation rules described by the [Validation Rules within a
--     Module](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirvenv-module-validation)
--     section of the [SPIR-V
--     Environment](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirvenv-capabilities)
--     appendix
--
-- -   If @pCode@ points to GLSL code, it /must/ be valid GLSL code written
--     to the GL_KHR_vulkan_glsl GLSL extension specification
--
-- -   @pCode@ /must/ declare the @Shader@ capability for SPIR-V code
--
-- -   @pCode@ /must/ not declare any capability that is not supported by
--     the API, as described by the
--     [Capabilities](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirvenv-module-validation)
--     section of the [SPIR-V
--     Environment](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirvenv-capabilities)
--     appendix
--
-- -   If @pCode@ declares any of the capabilities listed as /optional/ in
--     the [SPIR-V
--     Environment](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#spirvenv-capabilities-table)
--     appendix, the corresponding feature(s) /must/ be enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT'
--
-- -   @flags@ /must/ be @0@
--
-- -   @pCode@ /must/ be a valid pointer to an array of
--     \(codeSize \over 4\) @uint32_t@ values
--
-- = See Also
--
-- 'VkShaderModuleCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateShaderModule'
data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkShaderModuleCreateFlags
  , -- | @codeSize@ is the size, in bytes, of the code pointed to by @pCode@.
  vkCodeSize :: CSize
  , -- | @pCode@ points to code that is used to create the shader module. The
  -- type and format of the code is determined from the content of the memory
  -- addressed by @pCode@.
  vkPCode :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkShaderModuleCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkCodeSize (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPCode (poked :: VkShaderModuleCreateInfo))
