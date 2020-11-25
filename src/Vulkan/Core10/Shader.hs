{-# language CPP #-}
-- No documentation found for Chapter "Shader"
module Vulkan.Core10.Shader  ( createShaderModule
                             , withShaderModule
                             , destroyShaderModule
                             , ShaderModuleCreateInfo(..)
                             , ShaderModule(..)
                             , ShaderModuleCreateFlagBits(..)
                             , ShaderModuleCreateFlags
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
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
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (ShaderModule(..))
import Vulkan.Core10.Enums.ShaderModuleCreateFlagBits (ShaderModuleCreateFlagBits(..))
import Vulkan.Core10.Enums.ShaderModuleCreateFlagBits (ShaderModuleCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateShaderModule
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ShaderModuleCreateInfo) -> Ptr AllocationCallbacks -> Ptr ShaderModule -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct ShaderModuleCreateInfo) -> Ptr AllocationCallbacks -> Ptr ShaderModule -> IO Result

-- No documentation found for TopLevel "vkCreateShaderModule"
createShaderModule :: forall a io
                    . (Extendss ShaderModuleCreateInfo a, PokeChain a, MonadIO io)
                   => -- No documentation found for Nested "vkCreateShaderModule" "device"
                      Device
                   -> -- No documentation found for Nested "vkCreateShaderModule" "pCreateInfo"
                      (ShaderModuleCreateInfo a)
                   -> -- No documentation found for Nested "vkCreateShaderModule" "pAllocator"
                      ("allocator" ::: Maybe AllocationCallbacks)
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
  r <- lift $ vkCreateShaderModule' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPShaderModule)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pShaderModule <- lift $ peek @ShaderModule pPShaderModule
  pure $ (pShaderModule)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createShaderModule' and 'destroyShaderModule'
--
-- To ensure that 'destroyShaderModule' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withShaderModule :: forall a io r . (Extendss ShaderModuleCreateInfo a, PokeChain a, MonadIO io) => Device -> ShaderModuleCreateInfo a -> Maybe AllocationCallbacks -> (io ShaderModule -> (ShaderModule -> io ()) -> r) -> r
withShaderModule device pCreateInfo pAllocator b =
  b (createShaderModule device pCreateInfo pAllocator)
    (\(o0) -> destroyShaderModule device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyShaderModule
  :: FunPtr (Ptr Device_T -> ShaderModule -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ShaderModule -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyShaderModule"
destroyShaderModule :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkDestroyShaderModule" "device"
                       Device
                    -> -- No documentation found for Nested "vkDestroyShaderModule" "shaderModule"
                       ShaderModule
                    -> -- No documentation found for Nested "vkDestroyShaderModule" "pAllocator"
                       ("allocator" ::: Maybe AllocationCallbacks)
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



-- No documentation found for TopLevel "VkShaderModuleCreateInfo"
data ShaderModuleCreateInfo (es :: [Type]) = ShaderModuleCreateInfo
  { -- No documentation found for Nested "VkShaderModuleCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "flags"
    flags :: ShaderModuleCreateFlags
  , -- No documentation found for Nested "VkShaderModuleCreateInfo" "pCode"
    code :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderModuleCreateInfo (es :: [Type]))
#endif
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

