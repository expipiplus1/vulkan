{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_fence_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities  ( getPhysicalDeviceExternalFenceProperties
                                                                       , PhysicalDeviceExternalFenceInfo(..)
                                                                       , ExternalFenceProperties(..)
                                                                       , StructureType(..)
                                                                       , ExternalFenceHandleTypeFlagBits(..)
                                                                       , ExternalFenceHandleTypeFlags
                                                                       , ExternalFenceFeatureFlagBits(..)
                                                                       , ExternalFenceFeatureFlags
                                                                       ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalFenceProperties))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO))
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlagBits(..))
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits(..))
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalFenceProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalFenceInfo -> Ptr ExternalFenceProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalFenceInfo -> Ptr ExternalFenceProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalFenceProperties"
getPhysicalDeviceExternalFenceProperties :: forall io
                                          . (MonadIO io)
                                         => -- No documentation found for Nested "vkGetPhysicalDeviceExternalFenceProperties" "physicalDevice"
                                            PhysicalDevice
                                         -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalFenceProperties" "pExternalFenceInfo"
                                            PhysicalDeviceExternalFenceInfo
                                         -> io (ExternalFenceProperties)
getPhysicalDeviceExternalFenceProperties physicalDevice externalFenceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalFencePropertiesPtr = pVkGetPhysicalDeviceExternalFenceProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceExternalFencePropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalFenceProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalFenceProperties' = mkVkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFencePropertiesPtr
  pExternalFenceInfo <- ContT $ withCStruct (externalFenceInfo)
  pPExternalFenceProperties <- ContT (withZeroCStruct @ExternalFenceProperties)
  lift $ vkGetPhysicalDeviceExternalFenceProperties' (physicalDeviceHandle (physicalDevice)) pExternalFenceInfo (pPExternalFenceProperties)
  pExternalFenceProperties <- lift $ peekCStruct @ExternalFenceProperties pPExternalFenceProperties
  pure $ (pExternalFenceProperties)



-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfo"
data PhysicalDeviceExternalFenceInfo = PhysicalDeviceExternalFenceInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalFenceInfo" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalFenceInfo)
#endif
deriving instance Show PhysicalDeviceExternalFenceInfo

instance ToCStruct PhysicalDeviceExternalFenceInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalFenceInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalFenceInfo where
  peekCStruct p = do
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalFenceInfo
             handleType


instance Storable PhysicalDeviceExternalFenceInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalFenceInfo where
  zero = PhysicalDeviceExternalFenceInfo
           zero



-- No documentation found for TopLevel "VkExternalFenceProperties"
data ExternalFenceProperties = ExternalFenceProperties
  { -- No documentation found for Nested "VkExternalFenceProperties" "exportFromImportedHandleTypes"
    exportFromImportedHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "VkExternalFenceProperties" "compatibleHandleTypes"
    compatibleHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "VkExternalFenceProperties" "externalFenceFeatures"
    externalFenceFeatures :: ExternalFenceFeatureFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalFenceProperties)
#endif
deriving instance Show ExternalFenceProperties

instance ToCStruct ExternalFenceProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalFenceProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlags)) (exportFromImportedHandleTypes)
    poke ((p `plusPtr` 20 :: Ptr ExternalFenceHandleTypeFlags)) (compatibleHandleTypes)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceFeatureFlags)) (externalFenceFeatures)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ExternalFenceHandleTypeFlags)) (zero)
    f

instance FromCStruct ExternalFenceProperties where
  peekCStruct p = do
    exportFromImportedHandleTypes <- peek @ExternalFenceHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalFenceHandleTypeFlags))
    compatibleHandleTypes <- peek @ExternalFenceHandleTypeFlags ((p `plusPtr` 20 :: Ptr ExternalFenceHandleTypeFlags))
    externalFenceFeatures <- peek @ExternalFenceFeatureFlags ((p `plusPtr` 24 :: Ptr ExternalFenceFeatureFlags))
    pure $ ExternalFenceProperties
             exportFromImportedHandleTypes compatibleHandleTypes externalFenceFeatures


instance Storable ExternalFenceProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalFenceProperties where
  zero = ExternalFenceProperties
           zero
           zero
           zero

