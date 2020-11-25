{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_validation_cache"
module Vulkan.Extensions.VK_EXT_validation_cache  ( createValidationCacheEXT
                                                  , withValidationCacheEXT
                                                  , destroyValidationCacheEXT
                                                  , getValidationCacheDataEXT
                                                  , mergeValidationCachesEXT
                                                  , ValidationCacheCreateInfoEXT(..)
                                                  , ShaderModuleValidationCacheCreateInfoEXT(..)
                                                  , ValidationCacheCreateFlagsEXT(..)
                                                  , ValidationCacheHeaderVersionEXT( VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
                                                                                   , ..
                                                                                   )
                                                  , EXT_VALIDATION_CACHE_SPEC_VERSION
                                                  , pattern EXT_VALIDATION_CACHE_SPEC_VERSION
                                                  , EXT_VALIDATION_CACHE_EXTENSION_NAME
                                                  , pattern EXT_VALIDATION_CACHE_EXTENSION_NAME
                                                  , ValidationCacheEXT(..)
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCStringLen)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateValidationCacheEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyValidationCacheEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetValidationCacheDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkMergeValidationCachesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Extensions.Handles (ValidationCacheEXT)
import Vulkan.Extensions.Handles (ValidationCacheEXT(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (ValidationCacheEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateValidationCacheEXT
  :: FunPtr (Ptr Device_T -> Ptr ValidationCacheCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr ValidationCacheEXT -> IO Result) -> Ptr Device_T -> Ptr ValidationCacheCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr ValidationCacheEXT -> IO Result

-- No documentation found for TopLevel "vkCreateValidationCacheEXT"
createValidationCacheEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCreateValidationCacheEXT" "device"
                            Device
                         -> -- No documentation found for Nested "vkCreateValidationCacheEXT" "pCreateInfo"
                            ValidationCacheCreateInfoEXT
                         -> -- No documentation found for Nested "vkCreateValidationCacheEXT" "pAllocator"
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (ValidationCacheEXT)
createValidationCacheEXT device createInfo allocator = liftIO . evalContT $ do
  let vkCreateValidationCacheEXTPtr = pVkCreateValidationCacheEXT (deviceCmds (device :: Device))
  lift $ unless (vkCreateValidationCacheEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateValidationCacheEXT is null" Nothing Nothing
  let vkCreateValidationCacheEXT' = mkVkCreateValidationCacheEXT vkCreateValidationCacheEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPValidationCache <- ContT $ bracket (callocBytes @ValidationCacheEXT 8) free
  r <- lift $ vkCreateValidationCacheEXT' (deviceHandle (device)) pCreateInfo pAllocator (pPValidationCache)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pValidationCache <- lift $ peek @ValidationCacheEXT pPValidationCache
  pure $ (pValidationCache)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createValidationCacheEXT' and 'destroyValidationCacheEXT'
--
-- To ensure that 'destroyValidationCacheEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withValidationCacheEXT :: forall io r . MonadIO io => Device -> ValidationCacheCreateInfoEXT -> Maybe AllocationCallbacks -> (io ValidationCacheEXT -> (ValidationCacheEXT -> io ()) -> r) -> r
withValidationCacheEXT device pCreateInfo pAllocator b =
  b (createValidationCacheEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyValidationCacheEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyValidationCacheEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ValidationCacheEXT -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyValidationCacheEXT"
destroyValidationCacheEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkDestroyValidationCacheEXT" "device"
                             Device
                          -> -- No documentation found for Nested "vkDestroyValidationCacheEXT" "validationCache"
                             ValidationCacheEXT
                          -> -- No documentation found for Nested "vkDestroyValidationCacheEXT" "pAllocator"
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io ()
destroyValidationCacheEXT device validationCache allocator = liftIO . evalContT $ do
  let vkDestroyValidationCacheEXTPtr = pVkDestroyValidationCacheEXT (deviceCmds (device :: Device))
  lift $ unless (vkDestroyValidationCacheEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyValidationCacheEXT is null" Nothing Nothing
  let vkDestroyValidationCacheEXT' = mkVkDestroyValidationCacheEXT vkDestroyValidationCacheEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyValidationCacheEXT' (deviceHandle (device)) (validationCache) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetValidationCacheDataEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> ValidationCacheEXT -> Ptr CSize -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetValidationCacheDataEXT"
getValidationCacheDataEXT :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkGetValidationCacheDataEXT" "device"
                             Device
                          -> -- No documentation found for Nested "vkGetValidationCacheDataEXT" "validationCache"
                             ValidationCacheEXT
                          -> io (Result, ("data" ::: ByteString))
getValidationCacheDataEXT device validationCache = liftIO . evalContT $ do
  let vkGetValidationCacheDataEXTPtr = pVkGetValidationCacheDataEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetValidationCacheDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetValidationCacheDataEXT is null" Nothing Nothing
  let vkGetValidationCacheDataEXT' = mkVkGetValidationCacheDataEXT vkGetValidationCacheDataEXTPtr
  let device' = deviceHandle (device)
  pPDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ vkGetValidationCacheDataEXT' device' (validationCache) (pPDataSize) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDataSize <- lift $ peek @CSize pPDataSize
  pPData <- ContT $ bracket (callocBytes @(()) (fromIntegral (((\(CSize a) -> a) pDataSize)))) free
  r' <- lift $ vkGetValidationCacheDataEXT' device' (validationCache) (pPDataSize) (pPData)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDataSize'' <- lift $ peek @CSize pPDataSize
  pData' <- lift $ packCStringLen  (castPtr @() @CChar pPData, (fromIntegral (((\(CSize a) -> a) pDataSize''))))
  pure $ ((r'), pData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergeValidationCachesEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Word32 -> Ptr ValidationCacheEXT -> IO Result) -> Ptr Device_T -> ValidationCacheEXT -> Word32 -> Ptr ValidationCacheEXT -> IO Result

-- No documentation found for TopLevel "vkMergeValidationCachesEXT"
mergeValidationCachesEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkMergeValidationCachesEXT" "device"
                            Device
                         -> -- No documentation found for Nested "vkMergeValidationCachesEXT" "dstCache"
                            ("dstCache" ::: ValidationCacheEXT)
                         -> -- No documentation found for Nested "vkMergeValidationCachesEXT" "pSrcCaches"
                            ("srcCaches" ::: Vector ValidationCacheEXT)
                         -> io ()
mergeValidationCachesEXT device dstCache srcCaches = liftIO . evalContT $ do
  let vkMergeValidationCachesEXTPtr = pVkMergeValidationCachesEXT (deviceCmds (device :: Device))
  lift $ unless (vkMergeValidationCachesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMergeValidationCachesEXT is null" Nothing Nothing
  let vkMergeValidationCachesEXT' = mkVkMergeValidationCachesEXT vkMergeValidationCachesEXTPtr
  pPSrcCaches <- ContT $ allocaBytesAligned @ValidationCacheEXT ((Data.Vector.length (srcCaches)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSrcCaches `plusPtr` (8 * (i)) :: Ptr ValidationCacheEXT) (e)) (srcCaches)
  r <- lift $ vkMergeValidationCachesEXT' (deviceHandle (device)) (dstCache) ((fromIntegral (Data.Vector.length $ (srcCaches)) :: Word32)) (pPSrcCaches)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkValidationCacheCreateInfoEXT"
data ValidationCacheCreateInfoEXT = ValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "flags"
    flags :: ValidationCacheCreateFlagsEXT
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "initialDataSize"
    initialDataSize :: Word64
  , -- No documentation found for Nested "VkValidationCacheCreateInfoEXT" "pInitialData"
    initialData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ValidationCacheCreateInfoEXT)
#endif
deriving instance Show ValidationCacheCreateInfoEXT

instance ToCStruct ValidationCacheCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ValidationCacheCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (initialDataSize))
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (initialData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ValidationCacheCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @ValidationCacheCreateFlagsEXT ((p `plusPtr` 16 :: Ptr ValidationCacheCreateFlagsEXT))
    initialDataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pInitialData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ ValidationCacheCreateInfoEXT
             flags ((\(CSize a) -> a) initialDataSize) pInitialData


instance Storable ValidationCacheCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ValidationCacheCreateInfoEXT where
  zero = ValidationCacheCreateInfoEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkShaderModuleValidationCacheCreateInfoEXT"
data ShaderModuleValidationCacheCreateInfoEXT = ShaderModuleValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "VkShaderModuleValidationCacheCreateInfoEXT" "validationCache"
    validationCache :: ValidationCacheEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderModuleValidationCacheCreateInfoEXT)
#endif
deriving instance Show ShaderModuleValidationCacheCreateInfoEXT

instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderModuleValidationCacheCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheEXT)) (validationCache)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheEXT)) (zero)
    f

instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT where
  peekCStruct p = do
    validationCache <- peek @ValidationCacheEXT ((p `plusPtr` 16 :: Ptr ValidationCacheEXT))
    pure $ ShaderModuleValidationCacheCreateInfoEXT
             validationCache


instance Storable ShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderModuleValidationCacheCreateInfoEXT where
  zero = ShaderModuleValidationCacheCreateInfoEXT
           zero


-- No documentation found for TopLevel "VkValidationCacheCreateFlagsEXT"
newtype ValidationCacheCreateFlagsEXT = ValidationCacheCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameValidationCacheCreateFlagsEXT :: String
conNameValidationCacheCreateFlagsEXT = "ValidationCacheCreateFlagsEXT"

enumPrefixValidationCacheCreateFlagsEXT :: String
enumPrefixValidationCacheCreateFlagsEXT = ""

showTableValidationCacheCreateFlagsEXT :: [(ValidationCacheCreateFlagsEXT, String)]
showTableValidationCacheCreateFlagsEXT = []


instance Show ValidationCacheCreateFlagsEXT where
showsPrec = enumShowsPrec enumPrefixValidationCacheCreateFlagsEXT
                          showTableValidationCacheCreateFlagsEXT
                          conNameValidationCacheCreateFlagsEXT
                          (\(ValidationCacheCreateFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ValidationCacheCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixValidationCacheCreateFlagsEXT
                          showTableValidationCacheCreateFlagsEXT
                          conNameValidationCacheCreateFlagsEXT
                          ValidationCacheCreateFlagsEXT


-- No documentation found for TopLevel "VkValidationCacheHeaderVersionEXT"
newtype ValidationCacheHeaderVersionEXT = ValidationCacheHeaderVersionEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkValidationCacheHeaderVersionEXT" "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = ValidationCacheHeaderVersionEXT 1
{-# complete VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: ValidationCacheHeaderVersionEXT #-}

conNameValidationCacheHeaderVersionEXT :: String
conNameValidationCacheHeaderVersionEXT = "ValidationCacheHeaderVersionEXT"

enumPrefixValidationCacheHeaderVersionEXT :: String
enumPrefixValidationCacheHeaderVersionEXT = "VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"

showTableValidationCacheHeaderVersionEXT :: [(ValidationCacheHeaderVersionEXT, String)]
showTableValidationCacheHeaderVersionEXT = [(VALIDATION_CACHE_HEADER_VERSION_ONE_EXT, "")]


instance Show ValidationCacheHeaderVersionEXT where
showsPrec = enumShowsPrec enumPrefixValidationCacheHeaderVersionEXT
                          showTableValidationCacheHeaderVersionEXT
                          conNameValidationCacheHeaderVersionEXT
                          (\(ValidationCacheHeaderVersionEXT x) -> x)
                          (showsPrec 11)


instance Read ValidationCacheHeaderVersionEXT where
  readPrec = enumReadPrec enumPrefixValidationCacheHeaderVersionEXT
                          showTableValidationCacheHeaderVersionEXT
                          conNameValidationCacheHeaderVersionEXT
                          ValidationCacheHeaderVersionEXT


type EXT_VALIDATION_CACHE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_SPEC_VERSION"
pattern EXT_VALIDATION_CACHE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VALIDATION_CACHE_SPEC_VERSION = 1


type EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_EXTENSION_NAME"
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"

