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
module Vulkan.Extensions.VK_NVX_binary_import  ( createCuModuleNVX
                                               , withCuModuleNVX
                                               , createCuFunctionNVX
                                               , withCuFunctionNVX
                                               , destroyCuModuleNVX
                                               , destroyCuFunctionNVX
                                               , cmdCuLaunchKernelNVX
                                               , CuModuleCreateInfoNVX(..)
                                               , CuFunctionCreateInfoNVX(..)
                                               , CuLaunchInfoNVX(..)
                                               , NVX_BINARY_IMPORT_SPEC_VERSION
                                               , pattern NVX_BINARY_IMPORT_SPEC_VERSION
                                               , NVX_BINARY_IMPORT_EXTENSION_NAME
                                               , pattern NVX_BINARY_IMPORT_EXTENSION_NAME
                                               , CuModuleNVX(..)
                                               , CuFunctionNVX(..)
                                               , DebugReportObjectTypeEXT(..)
                                               ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (CuFunctionNVX)
import Vulkan.Extensions.Handles (CuFunctionNVX(..))
import Vulkan.Extensions.Handles (CuModuleNVX)
import Vulkan.Extensions.Handles (CuModuleNVX(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCuLaunchKernelNVX))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCuFunctionNVX))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCuModuleNVX))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCuFunctionNVX))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCuModuleNVX))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (CuFunctionNVX(..))
import Vulkan.Extensions.Handles (CuModuleNVX(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCuModuleNVX
  :: FunPtr (Ptr Device_T -> Ptr CuModuleCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr CuModuleNVX -> IO Result) -> Ptr Device_T -> Ptr CuModuleCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr CuModuleNVX -> IO Result

-- No documentation found for TopLevel "vkCreateCuModuleNVX"
createCuModuleNVX :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCreateCuModuleNVX" "device"
                     Device
                  -> -- No documentation found for Nested "vkCreateCuModuleNVX" "pCreateInfo"
                     CuModuleCreateInfoNVX
                  -> -- No documentation found for Nested "vkCreateCuModuleNVX" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (CuModuleNVX)
createCuModuleNVX device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCuModuleNVXPtr = pVkCreateCuModuleNVX (deviceCmds (device :: Device))
  lift $ unless (vkCreateCuModuleNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCuModuleNVX is null" Nothing Nothing
  let vkCreateCuModuleNVX' = mkVkCreateCuModuleNVX vkCreateCuModuleNVXPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPModule <- ContT $ bracket (callocBytes @CuModuleNVX 8) free
  r <- lift $ traceAroundEvent "vkCreateCuModuleNVX" (vkCreateCuModuleNVX' (deviceHandle (device)) pCreateInfo pAllocator (pPModule))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModule <- lift $ peek @CuModuleNVX pPModule
  pure $ (pModule)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCuModuleNVX' and 'destroyCuModuleNVX'
--
-- To ensure that 'destroyCuModuleNVX' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCuModuleNVX :: forall io r . MonadIO io => Device -> CuModuleCreateInfoNVX -> Maybe AllocationCallbacks -> (io CuModuleNVX -> (CuModuleNVX -> io ()) -> r) -> r
withCuModuleNVX device pCreateInfo pAllocator b =
  b (createCuModuleNVX device pCreateInfo pAllocator)
    (\(o0) -> destroyCuModuleNVX device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCuFunctionNVX
  :: FunPtr (Ptr Device_T -> Ptr CuFunctionCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr CuFunctionNVX -> IO Result) -> Ptr Device_T -> Ptr CuFunctionCreateInfoNVX -> Ptr AllocationCallbacks -> Ptr CuFunctionNVX -> IO Result

-- No documentation found for TopLevel "vkCreateCuFunctionNVX"
createCuFunctionNVX :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCreateCuFunctionNVX" "device"
                       Device
                    -> -- No documentation found for Nested "vkCreateCuFunctionNVX" "pCreateInfo"
                       CuFunctionCreateInfoNVX
                    -> -- No documentation found for Nested "vkCreateCuFunctionNVX" "pAllocator"
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (CuFunctionNVX)
createCuFunctionNVX device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCuFunctionNVXPtr = pVkCreateCuFunctionNVX (deviceCmds (device :: Device))
  lift $ unless (vkCreateCuFunctionNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCuFunctionNVX is null" Nothing Nothing
  let vkCreateCuFunctionNVX' = mkVkCreateCuFunctionNVX vkCreateCuFunctionNVXPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFunction <- ContT $ bracket (callocBytes @CuFunctionNVX 8) free
  r <- lift $ traceAroundEvent "vkCreateCuFunctionNVX" (vkCreateCuFunctionNVX' (deviceHandle (device)) pCreateInfo pAllocator (pPFunction))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFunction <- lift $ peek @CuFunctionNVX pPFunction
  pure $ (pFunction)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCuFunctionNVX' and 'destroyCuFunctionNVX'
--
-- To ensure that 'destroyCuFunctionNVX' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCuFunctionNVX :: forall io r . MonadIO io => Device -> CuFunctionCreateInfoNVX -> Maybe AllocationCallbacks -> (io CuFunctionNVX -> (CuFunctionNVX -> io ()) -> r) -> r
withCuFunctionNVX device pCreateInfo pAllocator b =
  b (createCuFunctionNVX device pCreateInfo pAllocator)
    (\(o0) -> destroyCuFunctionNVX device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCuModuleNVX
  :: FunPtr (Ptr Device_T -> CuModuleNVX -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CuModuleNVX -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyCuModuleNVX"
destroyCuModuleNVX :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkDestroyCuModuleNVX" "device"
                      Device
                   -> -- No documentation found for Nested "vkDestroyCuModuleNVX" "module"
                      CuModuleNVX
                   -> -- No documentation found for Nested "vkDestroyCuModuleNVX" "pAllocator"
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io ()
destroyCuModuleNVX device module' allocator = liftIO . evalContT $ do
  let vkDestroyCuModuleNVXPtr = pVkDestroyCuModuleNVX (deviceCmds (device :: Device))
  lift $ unless (vkDestroyCuModuleNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCuModuleNVX is null" Nothing Nothing
  let vkDestroyCuModuleNVX' = mkVkDestroyCuModuleNVX vkDestroyCuModuleNVXPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyCuModuleNVX" (vkDestroyCuModuleNVX' (deviceHandle (device)) (module') pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCuFunctionNVX
  :: FunPtr (Ptr Device_T -> CuFunctionNVX -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CuFunctionNVX -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyCuFunctionNVX"
destroyCuFunctionNVX :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkDestroyCuFunctionNVX" "device"
                        Device
                     -> -- No documentation found for Nested "vkDestroyCuFunctionNVX" "function"
                        CuFunctionNVX
                     -> -- No documentation found for Nested "vkDestroyCuFunctionNVX" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io ()
destroyCuFunctionNVX device function allocator = liftIO . evalContT $ do
  let vkDestroyCuFunctionNVXPtr = pVkDestroyCuFunctionNVX (deviceCmds (device :: Device))
  lift $ unless (vkDestroyCuFunctionNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCuFunctionNVX is null" Nothing Nothing
  let vkDestroyCuFunctionNVX' = mkVkDestroyCuFunctionNVX vkDestroyCuFunctionNVXPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyCuFunctionNVX" (vkDestroyCuFunctionNVX' (deviceHandle (device)) (function) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCuLaunchKernelNVX
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CuLaunchInfoNVX -> IO ()) -> Ptr CommandBuffer_T -> Ptr CuLaunchInfoNVX -> IO ()

-- No documentation found for TopLevel "vkCmdCuLaunchKernelNVX"
cmdCuLaunchKernelNVX :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdCuLaunchKernelNVX" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdCuLaunchKernelNVX" "pLaunchInfo"
                        CuLaunchInfoNVX
                     -> io ()
cmdCuLaunchKernelNVX commandBuffer launchInfo = liftIO . evalContT $ do
  let vkCmdCuLaunchKernelNVXPtr = pVkCmdCuLaunchKernelNVX (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCuLaunchKernelNVXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCuLaunchKernelNVX is null" Nothing Nothing
  let vkCmdCuLaunchKernelNVX' = mkVkCmdCuLaunchKernelNVX vkCmdCuLaunchKernelNVXPtr
  pLaunchInfo <- ContT $ withCStruct (launchInfo)
  lift $ traceAroundEvent "vkCmdCuLaunchKernelNVX" (vkCmdCuLaunchKernelNVX' (commandBufferHandle (commandBuffer)) pLaunchInfo)
  pure $ ()


-- No documentation found for TopLevel "VkCuModuleCreateInfoNVX"
data CuModuleCreateInfoNVX = CuModuleCreateInfoNVX
  { -- No documentation found for Nested "VkCuModuleCreateInfoNVX" "dataSize"
    dataSize :: Word64
  , -- No documentation found for Nested "VkCuModuleCreateInfoNVX" "pData"
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CuModuleCreateInfoNVX)
#endif
deriving instance Show CuModuleCreateInfoNVX

instance ToCStruct CuModuleCreateInfoNVX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CuModuleCreateInfoNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct CuModuleCreateInfoNVX where
  peekCStruct p = do
    dataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ CuModuleCreateInfoNVX
             (coerce @CSize @Word64 dataSize) pData

instance Storable CuModuleCreateInfoNVX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CuModuleCreateInfoNVX where
  zero = CuModuleCreateInfoNVX
           zero
           zero


-- No documentation found for TopLevel "VkCuFunctionCreateInfoNVX"
data CuFunctionCreateInfoNVX = CuFunctionCreateInfoNVX
  { -- No documentation found for Nested "VkCuFunctionCreateInfoNVX" "module"
    module' :: CuModuleNVX
  , -- No documentation found for Nested "VkCuFunctionCreateInfoNVX" "pName"
    name :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CuFunctionCreateInfoNVX)
#endif
deriving instance Show CuFunctionCreateInfoNVX

instance ToCStruct CuFunctionCreateInfoNVX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CuFunctionCreateInfoNVX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CuModuleNVX)) (module')
    pName'' <- ContT $ useAsCString (name)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CuModuleNVX)) (zero)
    pName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    lift $ f

instance FromCStruct CuFunctionCreateInfoNVX where
  peekCStruct p = do
    module' <- peek @CuModuleNVX ((p `plusPtr` 16 :: Ptr CuModuleNVX))
    pName <- packCString =<< peek ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    pure $ CuFunctionCreateInfoNVX
             module' pName

instance Zero CuFunctionCreateInfoNVX where
  zero = CuFunctionCreateInfoNVX
           zero
           mempty


-- No documentation found for TopLevel "VkCuLaunchInfoNVX"
data CuLaunchInfoNVX = CuLaunchInfoNVX
  { -- No documentation found for Nested "VkCuLaunchInfoNVX" "function"
    function :: CuFunctionNVX
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "gridDimX"
    gridDimX :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "gridDimY"
    gridDimY :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "gridDimZ"
    gridDimZ :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "blockDimX"
    blockDimX :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "blockDimY"
    blockDimY :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "blockDimZ"
    blockDimZ :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "sharedMemBytes"
    sharedMemBytes :: Word32
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "pParams"
    params :: Vector (Ptr ())
  , -- No documentation found for Nested "VkCuLaunchInfoNVX" "pExtras"
    extras :: Vector (Ptr ())
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CuLaunchInfoNVX)
#endif
deriving instance Show CuLaunchInfoNVX

instance ToCStruct CuLaunchInfoNVX where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CuLaunchInfoNVX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CuFunctionNVX)) (function)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (gridDimX)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (gridDimY)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (gridDimZ)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (blockDimX)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (blockDimY)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (blockDimZ)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (sharedMemBytes)
    lift $ poke ((p `plusPtr` 56 :: Ptr CSize)) ((fromIntegral (Data.Vector.length $ (params)) :: CSize))
    pPParams' <- ContT $ allocaBytes @(Ptr ()) ((Data.Vector.length (params)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPParams' `plusPtr` (8 * (i)) :: Ptr (Ptr ())) (e)) (params)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr ())))) (pPParams')
    lift $ poke ((p `plusPtr` 72 :: Ptr CSize)) ((fromIntegral (Data.Vector.length $ (extras)) :: CSize))
    pPExtras' <- ContT $ allocaBytes @(Ptr ()) ((Data.Vector.length (extras)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExtras' `plusPtr` (8 * (i)) :: Ptr (Ptr ())) (e)) (extras)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr (Ptr ())))) (pPExtras')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CuFunctionNVX)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct CuLaunchInfoNVX where
  peekCStruct p = do
    function <- peek @CuFunctionNVX ((p `plusPtr` 16 :: Ptr CuFunctionNVX))
    gridDimX <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    gridDimY <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    gridDimZ <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    blockDimX <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    blockDimY <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    blockDimZ <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    sharedMemBytes <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    paramCount <- peek @CSize ((p `plusPtr` 56 :: Ptr CSize))
    pParams <- peek @(Ptr (Ptr ())) ((p `plusPtr` 64 :: Ptr (Ptr (Ptr ()))))
    pParams' <- generateM (fromIntegral (coerce @CSize @Word64 paramCount)) (\i -> peek @(Ptr ()) ((pParams `advancePtrBytes` (8 * (i)) :: Ptr (Ptr ()))))
    extraCount <- peek @CSize ((p `plusPtr` 72 :: Ptr CSize))
    pExtras <- peek @(Ptr (Ptr ())) ((p `plusPtr` 80 :: Ptr (Ptr (Ptr ()))))
    pExtras' <- generateM (fromIntegral (coerce @CSize @Word64 extraCount)) (\i -> peek @(Ptr ()) ((pExtras `advancePtrBytes` (8 * (i)) :: Ptr (Ptr ()))))
    pure $ CuLaunchInfoNVX
             function gridDimX gridDimY gridDimZ blockDimX blockDimY blockDimZ sharedMemBytes pParams' pExtras'

instance Zero CuLaunchInfoNVX where
  zero = CuLaunchInfoNVX
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty


type NVX_BINARY_IMPORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NVX_BINARY_IMPORT_SPEC_VERSION"
pattern NVX_BINARY_IMPORT_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_BINARY_IMPORT_SPEC_VERSION = 1


type NVX_BINARY_IMPORT_EXTENSION_NAME = "VK_NVX_binary_import"

-- No documentation found for TopLevel "VK_NVX_BINARY_IMPORT_EXTENSION_NAME"
pattern NVX_BINARY_IMPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_BINARY_IMPORT_EXTENSION_NAME = "VK_NVX_binary_import"

