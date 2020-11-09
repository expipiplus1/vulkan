{-# language CPP #-}
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

-- | vkGetPhysicalDeviceExternalFenceProperties - Function for querying
-- external fence handle capabilities.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalFenceProperties', 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceExternalFenceInfo'
getPhysicalDeviceExternalFenceProperties :: forall io
                                          . (MonadIO io)
                                         => -- | @physicalDevice@ is the physical device from which to query the fence
                                            -- capabilities.
                                            --
                                            -- #VUID-vkGetPhysicalDeviceExternalFenceProperties-physicalDevice-parameter#
                                            -- @physicalDevice@ /must/ be a valid
                                            -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                            PhysicalDevice
                                         -> -- | @pExternalFenceInfo@ is a pointer to a 'PhysicalDeviceExternalFenceInfo'
                                            -- structure describing the parameters that would be consumed by
                                            -- 'Vulkan.Core10.Fence.createFence'.
                                            --
                                            -- #VUID-vkGetPhysicalDeviceExternalFenceProperties-pExternalFenceInfo-parameter#
                                            -- @pExternalFenceInfo@ /must/ be a valid pointer to a valid
                                            -- 'PhysicalDeviceExternalFenceInfo' structure
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


-- | VkPhysicalDeviceExternalFenceInfo - Structure specifying fence creation
-- parameters.
--
-- = Description
--
-- Note
--
-- Handles of type
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'
-- generated by the implementation may represent either Linux Sync Files or
-- Android Fences at the implementation’s discretion. Applications /should/
-- only use operations defined for both types of file descriptors, unless
-- they know via means external to Vulkan the type of the file descriptor,
-- or are prepared to deal with the system-defined operation failures
-- resulting from using the wrong type.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalFenceProperties',
-- 'Vulkan.Extensions.VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFencePropertiesKHR'
data PhysicalDeviceExternalFenceInfo = PhysicalDeviceExternalFenceInfo
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
    -- value indicating an external fence handle type for which capabilities
    -- will be returned.
    --
    -- #VUID-VkPhysicalDeviceExternalFenceInfo-handleType-parameter#
    -- @handleType@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
    -- value
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


-- | VkExternalFenceProperties - Structure describing supported external
-- fence handle features
--
-- = Description
--
-- If @handleType@ is not supported by the implementation, then
-- 'ExternalFenceProperties'::@externalFenceFeatures@ will be set to zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits.ExternalFenceFeatureFlags',
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalFenceProperties',
-- 'Vulkan.Extensions.VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFencePropertiesKHR'
data ExternalFenceProperties = ExternalFenceProperties
  { -- | @exportFromImportedHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
    -- indicating which types of imported handle @handleType@ /can/ be exported
    -- from.
    exportFromImportedHandleTypes :: ExternalFenceHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
    -- specifying handle types which /can/ be specified at the same time as
    -- @handleType@ when creating a fence.
    compatibleHandleTypes :: ExternalFenceHandleTypeFlags
  , -- | @externalFenceFeatures@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits.ExternalFenceFeatureFlagBits'
    -- indicating the features of @handleType@.
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

