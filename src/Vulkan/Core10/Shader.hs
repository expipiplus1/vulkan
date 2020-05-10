{-# language CPP #-}
module Vulkan.Core10.Shader  ( createShaderModule
                             , withShaderModule
                             , destroyShaderModule
                             , ShaderModuleCreateInfo(..)
                             ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (ptrToWordPtr)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import qualified Data.ByteString (length)
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateShaderModule))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyShaderModule))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (ShaderModule)
import Vulkan.Core10.Handles (ShaderModule(..))
import Vulkan.Core10.Enums.ShaderModuleCreateFlagBits (ShaderModuleCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ShaderModuleValidationCacheCreateInfoEXT)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateShaderModule
  :: FunPtr (Ptr Device_T -> Ptr (ShaderModuleCreateInfo a) -> Ptr AllocationCallbacks -> Ptr ShaderModule -> IO Result) -> Ptr Device_T -> Ptr (ShaderModuleCreateInfo a) -> Ptr AllocationCallbacks -> Ptr ShaderModule -> IO Result

-- | vkCreateShaderModule - Creates a new shader module object
--
-- = Description
--
-- Once a shader module has been created, any entry points it contains
-- /can/ be used in pipeline shader stages as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-compute Compute Pipelines>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-graphics Graphics Pipelines>.
--
-- If the shader stage fails to compile
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV' will be generated
-- and the compile log will be reported back to the application by
-- @VK_EXT_debug_report@ if enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ShaderModuleCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pShaderModule@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.ShaderModule' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ShaderModule',
-- 'ShaderModuleCreateInfo'
createShaderModule :: forall a io
                    . (Extendss ShaderModuleCreateInfo a, PokeChain a, MonadIO io)
                   => -- | @device@ is the logical device that creates the shader module.
                      Device
                   -> -- | @pCreateInfo@ is a pointer to a 'ShaderModuleCreateInfo' structure.
                      (ShaderModuleCreateInfo a)
                   -> -- | @pAllocator@ controls host memory allocation as described in the
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                      -- chapter.
                      (("allocator" ::: Maybe AllocationCallbacks))
                   -> io (ShaderModule)
createShaderModule device createInfo allocator = liftIO . evalContT $ do
  let vkCreateShaderModulePtr = pVkCreateShaderModule (deviceCmds (device :: Device))
  lift $ unless (vkCreateShaderModulePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateShaderModule is null" Nothing Nothing
  let vkCreateShaderModule' = mkVkCreateShaderModule vkCreateShaderModulePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPShaderModule <- ContT $ bracket (callocBytes @ShaderModule 8) free
  r <- lift $ vkCreateShaderModule' (deviceHandle (device)) pCreateInfo pAllocator (pPShaderModule)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pShaderModule <- lift $ peek @ShaderModule pPShaderModule
  pure $ (pShaderModule)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createShaderModule' and 'destroyShaderModule'
--
-- To ensure that 'destroyShaderModule' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withShaderModule :: forall a io r . (Extendss ShaderModuleCreateInfo a, PokeChain a, MonadIO io) => Device -> ShaderModuleCreateInfo a -> Maybe AllocationCallbacks -> (io (ShaderModule) -> ((ShaderModule) -> io ()) -> r) -> r
withShaderModule device pCreateInfo pAllocator b =
  b (createShaderModule device pCreateInfo pAllocator)
    (\(o0) -> destroyShaderModule device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyShaderModule
  :: FunPtr (Ptr Device_T -> ShaderModule -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ShaderModule -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyShaderModule - Destroy a shader module
--
-- = Description
--
-- A shader module /can/ be destroyed while pipelines created using its
-- shaders are still in use.
--
-- == Valid Usage
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @shaderModule@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @shaderModule@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @shaderModule@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shaderModule@ /must/ be a valid
--     'Vulkan.Core10.Handles.ShaderModule' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ShaderModule'
destroyShaderModule :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that destroys the shader module.
                       Device
                    -> -- | @shaderModule@ is the handle of the shader module to destroy.
                       ShaderModule
                    -> -- | @pAllocator@ controls host memory allocation as described in the
                       -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                       -- chapter.
                       (("allocator" ::: Maybe AllocationCallbacks))
                    -> io ()
destroyShaderModule device shaderModule allocator = liftIO . evalContT $ do
  let vkDestroyShaderModulePtr = pVkDestroyShaderModule (deviceCmds (device :: Device))
  lift $ unless (vkDestroyShaderModulePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyShaderModule is null" Nothing Nothing
  let vkDestroyShaderModule' = mkVkDestroyShaderModule vkDestroyShaderModulePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyShaderModule' (deviceHandle (device)) (shaderModule) pAllocator
  pure $ ()


-- | VkShaderModuleCreateInfo - Structure specifying parameters of a newly
-- created shader module
--
-- == Valid Usage
--
-- -   @codeSize@ /must/ be greater than 0
--
-- -   If @pCode@ is a pointer to SPIR-V code, @codeSize@ /must/ be a
--     multiple of 4
--
-- -   @pCode@ /must/ point to either valid SPIR-V code, formatted and
--     packed as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--     or valid GLSL code which /must/ be written to the
--     @GL_KHR_vulkan_glsl@ extension specification
--
-- -   If @pCode@ is a pointer to SPIR-V code, that code /must/ adhere to
--     the validation rules described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-module-validation Validation Rules within a Module>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   If @pCode@ is a pointer to GLSL code, it /must/ be valid GLSL code
--     written to the @GL_KHR_vulkan_glsl@ GLSL extension specification
--
-- -   @pCode@ /must/ declare the @Shader@ capability for SPIR-V code
--
-- -   @pCode@ /must/ not declare any capability that is not supported by
--     the API, as described by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-module-validation Capabilities>
--     section of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities SPIR-V Environment>
--     appendix
--
-- -   If @pCode@ declares any of the capabilities listed as /optional/ in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table SPIR-V Environment>
--     appendix, the corresponding feature(s) /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_validation_cache.ShaderModuleValidationCacheCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @pCode@ /must/ be a valid pointer to an array of
--     \(\textrm{codeSize} \over 4\) @uint32_t@ values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ShaderModuleCreateFlagBits.ShaderModuleCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createShaderModule'
data ShaderModuleCreateInfo (es :: [Type]) = ShaderModuleCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: ShaderModuleCreateFlags
  , -- | @pCode@ is a pointer to code that is used to create the shader module.
    -- The type and format of the code is determined from the content of the
    -- memory addressed by @pCode@.
    code :: ByteString
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ShaderModuleCreateInfo es)

instance Extensible ShaderModuleCreateInfo where
  extensibleType = STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  setNext x next = x{next = next}
  getNext ShaderModuleCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ShaderModuleCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ShaderModuleValidationCacheCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss ShaderModuleCreateInfo es, PokeChain es) => ToCStruct (ShaderModuleCreateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderModuleCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderModuleCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr CSize)) (fromIntegral $ Data.ByteString.length (code))
    lift $ unless (Data.ByteString.length (code) .&. 3 == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "code size must be a multiple of 4" Nothing Nothing
    unalignedCode <- ContT $ unsafeUseAsCString (code)
    pCode'' <- if ptrToWordPtr unalignedCode .&. 3 == 0
      -- If this pointer is already aligned properly then use it
      then pure $ castPtr @CChar @Word32 unalignedCode
      -- Otherwise allocate and copy the bytes
      else do
        let len = Data.ByteString.length (code)
        mem <- ContT $ allocaBytesAligned @Word32 len 4
        lift $ copyBytes mem (castPtr @CChar @Word32 unalignedCode) len
        pure mem
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word32))) pCode''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss ShaderModuleCreateInfo es, PeekChain es) => FromCStruct (ShaderModuleCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ShaderModuleCreateFlags ((p `plusPtr` 16 :: Ptr ShaderModuleCreateFlags))
    codeSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pCode <- peek @(Ptr Word32) ((p `plusPtr` 32 :: Ptr (Ptr Word32)))
    code <- packCStringLen (castPtr @Word32 @CChar pCode, fromIntegral $ ((\(CSize a) -> a) codeSize) * 4)
    pure $ ShaderModuleCreateInfo
             next flags code

instance es ~ '[] => Zero (ShaderModuleCreateInfo es) where
  zero = ShaderModuleCreateInfo
           ()
           zero
           mempty

