{-# language CPP #-}
-- No documentation found for Chapter "ExtensionDiscovery"
module Vulkan.Core10.ExtensionDiscovery  ( enumerateInstanceExtensionProperties
                                         , enumerateDeviceExtensionProperties
                                         , ExtensionProperties(..)
                                         ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Foreign.C.Types (CChar(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Dynamic (getInstanceProcAddr')
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkEnumerateDeviceExtensionProperties))
import Vulkan.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceExtensionProperties
  :: FunPtr (Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result) -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- No documentation found for TopLevel "vkEnumerateInstanceExtensionProperties"
enumerateInstanceExtensionProperties :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkEnumerateInstanceExtensionProperties" "pLayerName"
                                        ("layerName" ::: Maybe ByteString)
                                     -> io (Result, ("properties" ::: Vector ExtensionProperties))
enumerateInstanceExtensionProperties layerName = liftIO . evalContT $ do
  vkEnumerateInstanceExtensionPropertiesPtr <- lift $ castFunPtr @_ @(("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr ExtensionProperties) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceExtensionProperties"#)
  lift $ unless (vkEnumerateInstanceExtensionPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateInstanceExtensionProperties is null" Nothing Nothing
  let vkEnumerateInstanceExtensionProperties' = mkVkEnumerateInstanceExtensionProperties vkEnumerateInstanceExtensionPropertiesPtr
  pLayerName <- case (layerName) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateInstanceExtensionProperties' pLayerName (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @ExtensionProperties ((fromIntegral (pPropertyCount)) * 260)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 260) :: Ptr ExtensionProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateInstanceExtensionProperties' pLayerName (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @ExtensionProperties (((pPProperties) `advancePtrBytes` (260 * (i)) :: Ptr ExtensionProperties)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceExtensionProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result) -> Ptr PhysicalDevice_T -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- No documentation found for TopLevel "vkEnumerateDeviceExtensionProperties"
enumerateDeviceExtensionProperties :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkEnumerateDeviceExtensionProperties" "physicalDevice"
                                      PhysicalDevice
                                   -> -- No documentation found for Nested "vkEnumerateDeviceExtensionProperties" "pLayerName"
                                      ("layerName" ::: Maybe ByteString)
                                   -> io (Result, ("properties" ::: Vector ExtensionProperties))
enumerateDeviceExtensionProperties physicalDevice layerName = liftIO . evalContT $ do
  let vkEnumerateDeviceExtensionPropertiesPtr = pVkEnumerateDeviceExtensionProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkEnumerateDeviceExtensionPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateDeviceExtensionProperties is null" Nothing Nothing
  let vkEnumerateDeviceExtensionProperties' = mkVkEnumerateDeviceExtensionProperties vkEnumerateDeviceExtensionPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pLayerName <- case (layerName) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateDeviceExtensionProperties' physicalDevice' pLayerName (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @ExtensionProperties ((fromIntegral (pPropertyCount)) * 260)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 260) :: Ptr ExtensionProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateDeviceExtensionProperties' physicalDevice' pLayerName (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @ExtensionProperties (((pPProperties) `advancePtrBytes` (260 * (i)) :: Ptr ExtensionProperties)))
  pure $ ((r'), pProperties')



-- No documentation found for TopLevel "VkExtensionProperties"
data ExtensionProperties = ExtensionProperties
  { -- No documentation found for Nested "VkExtensionProperties" "extensionName"
    extensionName :: ByteString
  , -- No documentation found for Nested "VkExtensionProperties" "specVersion"
    specVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExtensionProperties)
#endif
deriving instance Show ExtensionProperties

instance ToCStruct ExtensionProperties where
  withCStruct x f = allocaBytesAligned 260 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExtensionProperties{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (extensionName)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (specVersion)
    f
  cStructSize = 260
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (zero)
    f

instance FromCStruct ExtensionProperties where
  peekCStruct p = do
    extensionName <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    specVersion <- peek @Word32 ((p `plusPtr` 256 :: Ptr Word32))
    pure $ ExtensionProperties
             extensionName specVersion


instance Storable ExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExtensionProperties where
  zero = ExtensionProperties
           mempty
           zero

