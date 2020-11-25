{-# language CPP #-}
-- No documentation found for Chapter "LayerDiscovery"
module Vulkan.Core10.LayerDiscovery  ( enumerateInstanceLayerProperties
                                     , enumerateDeviceLayerProperties
                                     , LayerProperties(..)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Vulkan.Dynamic (InstanceCmds(pVkEnumerateDeviceLayerProperties))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
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
  "dynamic" mkVkEnumerateInstanceLayerProperties
  :: FunPtr (Ptr Word32 -> Ptr LayerProperties -> IO Result) -> Ptr Word32 -> Ptr LayerProperties -> IO Result

-- No documentation found for TopLevel "vkEnumerateInstanceLayerProperties"
enumerateInstanceLayerProperties :: forall io
                                  . (MonadIO io)
                                 => io (Result, ("properties" ::: Vector LayerProperties))
enumerateInstanceLayerProperties  = liftIO . evalContT $ do
  vkEnumerateInstanceLayerPropertiesPtr <- lift $ castFunPtr @_ @(("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr LayerProperties) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceLayerProperties"#)
  lift $ unless (vkEnumerateInstanceLayerPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateInstanceLayerProperties is null" Nothing Nothing
  let vkEnumerateInstanceLayerProperties' = mkVkEnumerateInstanceLayerProperties vkEnumerateInstanceLayerPropertiesPtr
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateInstanceLayerProperties' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @LayerProperties ((fromIntegral (pPropertyCount)) * 520)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 520) :: Ptr LayerProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateInstanceLayerProperties' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @LayerProperties (((pPProperties) `advancePtrBytes` (520 * (i)) :: Ptr LayerProperties)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceLayerProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr LayerProperties -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr LayerProperties -> IO Result

-- No documentation found for TopLevel "vkEnumerateDeviceLayerProperties"
enumerateDeviceLayerProperties :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkEnumerateDeviceLayerProperties" "physicalDevice"
                                  PhysicalDevice
                               -> io (Result, ("properties" ::: Vector LayerProperties))
enumerateDeviceLayerProperties physicalDevice = liftIO . evalContT $ do
  let vkEnumerateDeviceLayerPropertiesPtr = pVkEnumerateDeviceLayerProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkEnumerateDeviceLayerPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumerateDeviceLayerProperties is null" Nothing Nothing
  let vkEnumerateDeviceLayerProperties' = mkVkEnumerateDeviceLayerProperties vkEnumerateDeviceLayerPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateDeviceLayerProperties' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @LayerProperties ((fromIntegral (pPropertyCount)) * 520)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 520) :: Ptr LayerProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateDeviceLayerProperties' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @LayerProperties (((pPProperties) `advancePtrBytes` (520 * (i)) :: Ptr LayerProperties)))
  pure $ ((r'), pProperties')



-- No documentation found for TopLevel "VkLayerProperties"
data LayerProperties = LayerProperties
  { -- No documentation found for Nested "VkLayerProperties" "layerName"
    layerName :: ByteString
  , -- No documentation found for Nested "VkLayerProperties" "specVersion"
    specVersion :: Word32
  , -- No documentation found for Nested "VkLayerProperties" "implementationVersion"
    implementationVersion :: Word32
  , -- No documentation found for Nested "VkLayerProperties" "description"
    description :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LayerProperties)
#endif
deriving instance Show LayerProperties

instance ToCStruct LayerProperties where
  withCStruct x f = allocaBytesAligned 520 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LayerProperties{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (layerName)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (specVersion)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (implementationVersion)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 520
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct LayerProperties where
  peekCStruct p = do
    layerName <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    specVersion <- peek @Word32 ((p `plusPtr` 256 :: Ptr Word32))
    implementationVersion <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pure $ LayerProperties
             layerName specVersion implementationVersion description


instance Storable LayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LayerProperties where
  zero = LayerProperties
           mempty
           zero
           zero
           mempty

