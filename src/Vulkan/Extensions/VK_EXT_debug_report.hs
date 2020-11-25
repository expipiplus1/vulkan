{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_debug_report"
module Vulkan.Extensions.VK_EXT_debug_report  ( createDebugReportCallbackEXT
                                              , withDebugReportCallbackEXT
                                              , destroyDebugReportCallbackEXT
                                              , debugReportMessageEXT
                                              , pattern STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
                                              , pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT
                                              , pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT
                                              , DebugReportCallbackCreateInfoEXT(..)
                                              , DebugReportFlagsEXT
                                              , DebugReportFlagBitsEXT( DEBUG_REPORT_INFORMATION_BIT_EXT
                                                                      , DEBUG_REPORT_WARNING_BIT_EXT
                                                                      , DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
                                                                      , DEBUG_REPORT_ERROR_BIT_EXT
                                                                      , DEBUG_REPORT_DEBUG_BIT_EXT
                                                                      , ..
                                                                      )
                                              , DebugReportObjectTypeEXT( DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
                                                                        , ..
                                                                        )
                                              , PFN_vkDebugReportCallbackEXT
                                              , FN_vkDebugReportCallbackEXT
                                              , EXT_DEBUG_REPORT_SPEC_VERSION
                                              , pattern EXT_DEBUG_REPORT_SPEC_VERSION
                                              , EXT_DEBUG_REPORT_EXTENSION_NAME
                                              , pattern EXT_DEBUG_REPORT_EXTENSION_NAME
                                              , DebugReportCallbackEXT(..)
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
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CChar(..))
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
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.Handles (DebugReportCallbackEXT)
import Vulkan.Extensions.Handles (DebugReportCallbackEXT(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDebugReportCallbackEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDebugReportMessageEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDestroyDebugReportCallbackEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DebugReportCallbackEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugReportCallbackEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugReportCallbackCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result) -> Ptr Instance_T -> Ptr DebugReportCallbackCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result

-- No documentation found for TopLevel "vkCreateDebugReportCallbackEXT"
createDebugReportCallbackEXT :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCreateDebugReportCallbackEXT" "instance"
                                Instance
                             -> -- No documentation found for Nested "vkCreateDebugReportCallbackEXT" "pCreateInfo"
                                DebugReportCallbackCreateInfoEXT
                             -> -- No documentation found for Nested "vkCreateDebugReportCallbackEXT" "pAllocator"
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (DebugReportCallbackEXT)
createDebugReportCallbackEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDebugReportCallbackEXTPtr = pVkCreateDebugReportCallbackEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDebugReportCallbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDebugReportCallbackEXT is null" Nothing Nothing
  let vkCreateDebugReportCallbackEXT' = mkVkCreateDebugReportCallbackEXT vkCreateDebugReportCallbackEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCallback <- ContT $ bracket (callocBytes @DebugReportCallbackEXT 8) free
  r <- lift $ vkCreateDebugReportCallbackEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPCallback)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCallback <- lift $ peek @DebugReportCallbackEXT pPCallback
  pure $ (pCallback)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDebugReportCallbackEXT' and 'destroyDebugReportCallbackEXT'
--
-- To ensure that 'destroyDebugReportCallbackEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDebugReportCallbackEXT :: forall io r . MonadIO io => Instance -> DebugReportCallbackCreateInfoEXT -> Maybe AllocationCallbacks -> (io DebugReportCallbackEXT -> (DebugReportCallbackEXT -> io ()) -> r) -> r
withDebugReportCallbackEXT instance' pCreateInfo pAllocator b =
  b (createDebugReportCallbackEXT instance' pCreateInfo pAllocator)
    (\(o0) -> destroyDebugReportCallbackEXT instance' o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugReportCallbackEXT
  :: FunPtr (Ptr Instance_T -> DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDebugReportCallbackEXT"
destroyDebugReportCallbackEXT :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkDestroyDebugReportCallbackEXT" "instance"
                                 Instance
                              -> -- No documentation found for Nested "vkDestroyDebugReportCallbackEXT" "callback"
                                 DebugReportCallbackEXT
                              -> -- No documentation found for Nested "vkDestroyDebugReportCallbackEXT" "pAllocator"
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroyDebugReportCallbackEXT instance' callback allocator = liftIO . evalContT $ do
  let vkDestroyDebugReportCallbackEXTPtr = pVkDestroyDebugReportCallbackEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroyDebugReportCallbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDebugReportCallbackEXT is null" Nothing Nothing
  let vkDestroyDebugReportCallbackEXT' = mkVkDestroyDebugReportCallbackEXT vkDestroyDebugReportCallbackEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDebugReportCallbackEXT' (instanceHandle (instance')) (callback) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugReportMessageEXT
  :: FunPtr (Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()) -> Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()

-- No documentation found for TopLevel "vkDebugReportMessageEXT"
debugReportMessageEXT :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkDebugReportMessageEXT" "instance"
                         Instance
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "flags"
                         DebugReportFlagsEXT
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "objectType"
                         DebugReportObjectTypeEXT
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "object"
                         ("object" ::: Word64)
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "location"
                         ("location" ::: Word64)
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "messageCode"
                         ("messageCode" ::: Int32)
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "pLayerPrefix"
                         ("layerPrefix" ::: ByteString)
                      -> -- No documentation found for Nested "vkDebugReportMessageEXT" "pMessage"
                         ("message" ::: ByteString)
                      -> io ()
debugReportMessageEXT instance' flags objectType object location messageCode layerPrefix message = liftIO . evalContT $ do
  let vkDebugReportMessageEXTPtr = pVkDebugReportMessageEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDebugReportMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugReportMessageEXT is null" Nothing Nothing
  let vkDebugReportMessageEXT' = mkVkDebugReportMessageEXT vkDebugReportMessageEXTPtr
  pLayerPrefix <- ContT $ useAsCString (layerPrefix)
  pMessage <- ContT $ useAsCString (message)
  lift $ vkDebugReportMessageEXT' (instanceHandle (instance')) (flags) (objectType) (object) (CSize (location)) (messageCode) pLayerPrefix pMessage
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT = DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT



-- No documentation found for TopLevel "VkDebugReportCallbackCreateInfoEXT"
data DebugReportCallbackCreateInfoEXT = DebugReportCallbackCreateInfoEXT
  { -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "flags"
    flags :: DebugReportFlagsEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "pfnCallback"
    pfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- No documentation found for Nested "VkDebugReportCallbackCreateInfoEXT" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugReportCallbackCreateInfoEXT)
#endif
deriving instance Show DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugReportCallbackCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT)) (pfnCallback)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT)) (zero)
    f

instance FromCStruct DebugReportCallbackCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DebugReportFlagsEXT ((p `plusPtr` 16 :: Ptr DebugReportFlagsEXT))
    pfnCallback <- peek @PFN_vkDebugReportCallbackEXT ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ DebugReportCallbackCreateInfoEXT
             flags pfnCallback pUserData


instance Storable DebugReportCallbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugReportCallbackCreateInfoEXT where
  zero = DebugReportCallbackCreateInfoEXT
           zero
           zero
           zero


type DebugReportFlagsEXT = DebugReportFlagBitsEXT

-- No documentation found for TopLevel "VkDebugReportFlagBitsEXT"
newtype DebugReportFlagBitsEXT = DebugReportFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_INFORMATION_BIT_EXT"
pattern DEBUG_REPORT_INFORMATION_BIT_EXT         = DebugReportFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_WARNING_BIT_EXT"
pattern DEBUG_REPORT_WARNING_BIT_EXT             = DebugReportFlagBitsEXT 0x00000002
-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT"
pattern DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = DebugReportFlagBitsEXT 0x00000004
-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_ERROR_BIT_EXT"
pattern DEBUG_REPORT_ERROR_BIT_EXT               = DebugReportFlagBitsEXT 0x00000008
-- No documentation found for Nested "VkDebugReportFlagBitsEXT" "VK_DEBUG_REPORT_DEBUG_BIT_EXT"
pattern DEBUG_REPORT_DEBUG_BIT_EXT               = DebugReportFlagBitsEXT 0x00000010

conNameDebugReportFlagBitsEXT :: String
conNameDebugReportFlagBitsEXT = "DebugReportFlagBitsEXT"

enumPrefixDebugReportFlagBitsEXT :: String
enumPrefixDebugReportFlagBitsEXT = "DEBUG_REPORT_"

showTableDebugReportFlagBitsEXT :: [(DebugReportFlagBitsEXT, String)]
showTableDebugReportFlagBitsEXT =
  [ (DEBUG_REPORT_INFORMATION_BIT_EXT        , "INFORMATION_BIT_EXT")
  , (DEBUG_REPORT_WARNING_BIT_EXT            , "WARNING_BIT_EXT")
  , (DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT, "PERFORMANCE_WARNING_BIT_EXT")
  , (DEBUG_REPORT_ERROR_BIT_EXT              , "ERROR_BIT_EXT")
  , (DEBUG_REPORT_DEBUG_BIT_EXT              , "DEBUG_BIT_EXT")
  ]


instance Show DebugReportFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixDebugReportFlagBitsEXT
                          showTableDebugReportFlagBitsEXT
                          conNameDebugReportFlagBitsEXT
                          (\(DebugReportFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DebugReportFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugReportFlagBitsEXT
                          showTableDebugReportFlagBitsEXT
                          conNameDebugReportFlagBitsEXT
                          DebugReportFlagBitsEXT


-- No documentation found for TopLevel "VkDebugReportObjectTypeEXT"
newtype DebugReportObjectTypeEXT = DebugReportObjectTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT                    = DebugReportObjectTypeEXT 0
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT                   = DebugReportObjectTypeEXT 1
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT            = DebugReportObjectTypeEXT 2
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                     = DebugReportObjectTypeEXT 3
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                      = DebugReportObjectTypeEXT 4
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT                  = DebugReportObjectTypeEXT 5
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT             = DebugReportObjectTypeEXT 6
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                      = DebugReportObjectTypeEXT 7
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT              = DebugReportObjectTypeEXT 8
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                     = DebugReportObjectTypeEXT 9
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                      = DebugReportObjectTypeEXT 10
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                      = DebugReportObjectTypeEXT 11
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT                 = DebugReportObjectTypeEXT 12
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT                = DebugReportObjectTypeEXT 13
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT                 = DebugReportObjectTypeEXT 14
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT              = DebugReportObjectTypeEXT 15
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT             = DebugReportObjectTypeEXT 16
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT            = DebugReportObjectTypeEXT 17
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT                = DebugReportObjectTypeEXT 18
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT                   = DebugReportObjectTypeEXT 19
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT      = DebugReportObjectTypeEXT 20
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT                    = DebugReportObjectTypeEXT 21
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT            = DebugReportObjectTypeEXT 22
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT             = DebugReportObjectTypeEXT 23
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT                = DebugReportObjectTypeEXT 24
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT               = DebugReportObjectTypeEXT 25
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT                = DebugReportObjectTypeEXT 26
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT              = DebugReportObjectTypeEXT 27
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT  = DebugReportObjectTypeEXT 28
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT                = DebugReportObjectTypeEXT 29
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT           = DebugReportObjectTypeEXT 30
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT       = DebugReportObjectTypeEXT 33
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT  = DebugReportObjectTypeEXT 1000165000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT   = DebugReportObjectTypeEXT 1000156000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT = DebugReportObjectTypeEXT 1000150000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = DebugReportObjectTypeEXT 1000085000
{-# complete DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
             DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
             DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
             DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
             DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT,
             DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT :: DebugReportObjectTypeEXT #-}

conNameDebugReportObjectTypeEXT :: String
conNameDebugReportObjectTypeEXT = "DebugReportObjectTypeEXT"

enumPrefixDebugReportObjectTypeEXT :: String
enumPrefixDebugReportObjectTypeEXT = "DEBUG_REPORT_OBJECT_TYPE_"

showTableDebugReportObjectTypeEXT :: [(DebugReportObjectTypeEXT, String)]
showTableDebugReportObjectTypeEXT =
  [ (DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT                   , "UNKNOWN_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT                  , "INSTANCE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT           , "PHYSICAL_DEVICE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                    , "DEVICE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                     , "QUEUE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT                 , "SEMAPHORE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT            , "COMMAND_BUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                     , "FENCE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT             , "DEVICE_MEMORY_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                    , "BUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                     , "IMAGE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                     , "EVENT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT                , "QUERY_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT               , "BUFFER_VIEW_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT                , "IMAGE_VIEW_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT             , "SHADER_MODULE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT            , "PIPELINE_CACHE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT           , "PIPELINE_LAYOUT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT               , "RENDER_PASS_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT                  , "PIPELINE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT     , "DESCRIPTOR_SET_LAYOUT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT                   , "SAMPLER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT           , "DESCRIPTOR_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT            , "DESCRIPTOR_SET_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT               , "FRAMEBUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT              , "COMMAND_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT               , "SURFACE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT             , "SWAPCHAIN_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT , "DEBUG_REPORT_CALLBACK_EXT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT               , "DISPLAY_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT          , "DISPLAY_MODE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT      , "VALIDATION_CACHE_EXT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT , "ACCELERATION_STRUCTURE_NV_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT  , "SAMPLER_YCBCR_CONVERSION_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT, "ACCELERATION_STRUCTURE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT, "DESCRIPTOR_UPDATE_TEMPLATE_EXT")
  ]


instance Show DebugReportObjectTypeEXT where
showsPrec = enumShowsPrec enumPrefixDebugReportObjectTypeEXT
                          showTableDebugReportObjectTypeEXT
                          conNameDebugReportObjectTypeEXT
                          (\(DebugReportObjectTypeEXT x) -> x)
                          (showsPrec 11)


instance Read DebugReportObjectTypeEXT where
  readPrec = enumReadPrec enumPrefixDebugReportObjectTypeEXT
                          showTableDebugReportObjectTypeEXT
                          conNameDebugReportObjectTypeEXT
                          DebugReportObjectTypeEXT


type FN_vkDebugReportCallbackEXT = DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO Bool32
-- No documentation found for TopLevel "PFN_vkDebugReportCallbackEXT"
type PFN_vkDebugReportCallbackEXT = FunPtr FN_vkDebugReportCallbackEXT


type EXT_DEBUG_REPORT_SPEC_VERSION = 9

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_SPEC_VERSION"
pattern EXT_DEBUG_REPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_REPORT_SPEC_VERSION = 9


type EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_EXTENSION_NAME"
pattern EXT_DEBUG_REPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

