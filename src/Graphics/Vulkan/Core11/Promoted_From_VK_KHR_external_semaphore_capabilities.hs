{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities  ( getPhysicalDeviceExternalSemaphoreProperties
                                                                                    , PhysicalDeviceExternalSemaphoreInfo(..)
                                                                                    , ExternalSemaphoreProperties(..)
                                                                                    , StructureType(..)
                                                                                    , ExternalSemaphoreHandleTypeFlagBits(..)
                                                                                    , ExternalSemaphoreHandleTypeFlags
                                                                                    , ExternalSemaphoreFeatureFlagBits(..)
                                                                                    , ExternalSemaphoreFeatureFlags
                                                                                    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalSemaphoreProperties))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO))
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlagBits(..))
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(..))
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalSemaphoreProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (PhysicalDeviceExternalSemaphoreInfo a) -> Ptr ExternalSemaphoreProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr (PhysicalDeviceExternalSemaphoreInfo a) -> Ptr ExternalSemaphoreProperties -> IO ()

-- | vkGetPhysicalDeviceExternalSemaphoreProperties - Function for querying
-- external semaphore handle capabilities.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     semaphore capabilities.
--
-- -   @pExternalSemaphoreInfo@ is a pointer to a
--     'PhysicalDeviceExternalSemaphoreInfo' structure describing the
--     parameters that would be consumed by
--     'Graphics.Vulkan.Core10.QueueSemaphore.createSemaphore'.
--
-- -   @pExternalSemaphoreProperties@ is a pointer to a
--     'ExternalSemaphoreProperties' structure in which capabilities are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceExternalSemaphoreInfo'
getPhysicalDeviceExternalSemaphoreProperties :: forall a io . (PokeChain a, MonadIO io) => PhysicalDevice -> PhysicalDeviceExternalSemaphoreInfo a -> io (ExternalSemaphoreProperties)
getPhysicalDeviceExternalSemaphoreProperties physicalDevice externalSemaphoreInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalSemaphoreProperties' = mkVkGetPhysicalDeviceExternalSemaphoreProperties (pVkGetPhysicalDeviceExternalSemaphoreProperties (instanceCmds (physicalDevice :: PhysicalDevice)))
  pExternalSemaphoreInfo <- ContT $ withCStruct (externalSemaphoreInfo)
  pPExternalSemaphoreProperties <- ContT (withZeroCStruct @ExternalSemaphoreProperties)
  lift $ vkGetPhysicalDeviceExternalSemaphoreProperties' (physicalDeviceHandle (physicalDevice)) pExternalSemaphoreInfo (pPExternalSemaphoreProperties)
  pExternalSemaphoreProperties <- lift $ peekCStruct @ExternalSemaphoreProperties pPExternalSemaphoreProperties
  pure $ (pExternalSemaphoreProperties)


-- | VkPhysicalDeviceExternalSemaphoreInfo - Structure specifying semaphore
-- creation parameters.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR'
data PhysicalDeviceExternalSemaphoreInfo (es :: [Type]) = PhysicalDeviceExternalSemaphoreInfo
  { -- | @pNext@ is NULL or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @handleType@ is a
    -- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- value specifying the external semaphore handle type for which
    -- capabilities will be returned.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PhysicalDeviceExternalSemaphoreInfo es)

instance Extensible PhysicalDeviceExternalSemaphoreInfo where
  extensibleType = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  setNext x next = x{next = next}
  getNext PhysicalDeviceExternalSemaphoreInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceExternalSemaphoreInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SemaphoreTypeCreateInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PhysicalDeviceExternalSemaphoreInfo es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalSemaphoreInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (PhysicalDeviceExternalSemaphoreInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalSemaphoreInfo
             next handleType

instance es ~ '[] => Zero (PhysicalDeviceExternalSemaphoreInfo es) where
  zero = PhysicalDeviceExternalSemaphoreInfo
           ()
           zero


-- | VkExternalSemaphoreProperties - Structure describing supported external
-- semaphore handle features
--
-- = Description
--
-- If @handleType@ is not supported by the implementation, then
-- 'ExternalSemaphoreProperties'::@externalSemaphoreFeatures@ will be set
-- to zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits.ExternalSemaphoreFeatureFlags',
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR'
data ExternalSemaphoreProperties = ExternalSemaphoreProperties
  { -- | @exportFromImportedHandleTypes@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying which types of imported handle @handleType@ /can/ be exported
    -- from.
    exportFromImportedHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying handle types which /can/ be specified at the same time as
    -- @handleType@ when creating a semaphore.
    compatibleHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- | @externalSemaphoreFeatures@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits.ExternalSemaphoreFeatureFlagBits'
    -- describing the features of @handleType@.
    externalSemaphoreFeatures :: ExternalSemaphoreFeatureFlags
  }
  deriving (Typeable)
deriving instance Show ExternalSemaphoreProperties

instance ToCStruct ExternalSemaphoreProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalSemaphoreProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags)) (exportFromImportedHandleTypes)
    poke ((p `plusPtr` 20 :: Ptr ExternalSemaphoreHandleTypeFlags)) (compatibleHandleTypes)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreFeatureFlags)) (externalSemaphoreFeatures)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ExternalSemaphoreHandleTypeFlags)) (zero)
    f

instance FromCStruct ExternalSemaphoreProperties where
  peekCStruct p = do
    exportFromImportedHandleTypes <- peek @ExternalSemaphoreHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags))
    compatibleHandleTypes <- peek @ExternalSemaphoreHandleTypeFlags ((p `plusPtr` 20 :: Ptr ExternalSemaphoreHandleTypeFlags))
    externalSemaphoreFeatures <- peek @ExternalSemaphoreFeatureFlags ((p `plusPtr` 24 :: Ptr ExternalSemaphoreFeatureFlags))
    pure $ ExternalSemaphoreProperties
             exportFromImportedHandleTypes compatibleHandleTypes externalSemaphoreFeatures

instance Storable ExternalSemaphoreProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalSemaphoreProperties where
  zero = ExternalSemaphoreProperties
           zero
           zero
           zero

