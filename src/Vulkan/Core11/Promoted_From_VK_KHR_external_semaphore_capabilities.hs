{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_semaphore_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities  ( getPhysicalDeviceExternalSemaphoreProperties
                                                                           , PhysicalDeviceExternalSemaphoreInfo(..)
                                                                           , ExternalSemaphoreProperties(..)
                                                                           , StructureType(..)
                                                                           , ExternalSemaphoreHandleTypeFlagBits(..)
                                                                           , ExternalSemaphoreHandleTypeFlags
                                                                           , ExternalSemaphoreFeatureFlagBits(..)
                                                                           , ExternalSemaphoreFeatureFlags
                                                                           ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalSemaphoreProperties))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO))
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlagBits(..))
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(..))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalSemaphoreProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceExternalSemaphoreInfo) -> Ptr ExternalSemaphoreProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceExternalSemaphoreInfo) -> Ptr ExternalSemaphoreProperties -> IO ()

-- | vkGetPhysicalDeviceExternalSemaphoreProperties - Function for querying
-- external semaphore handle capabilities.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalSemaphoreProperties', 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceExternalSemaphoreInfo'
getPhysicalDeviceExternalSemaphoreProperties :: forall a io
                                              . (Extendss PhysicalDeviceExternalSemaphoreInfo a, PokeChain a, MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the
                                                -- semaphore capabilities.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceExternalSemaphoreProperties-physicalDevice-parameter#
                                                -- @physicalDevice@ /must/ be a valid
                                                -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                PhysicalDevice
                                             -> -- | @pExternalSemaphoreInfo@ is a pointer to a
                                                -- 'PhysicalDeviceExternalSemaphoreInfo' structure describing the
                                                -- parameters that would be consumed by
                                                -- 'Vulkan.Core10.QueueSemaphore.createSemaphore'.
                                                --
                                                -- #VUID-vkGetPhysicalDeviceExternalSemaphoreProperties-pExternalSemaphoreInfo-parameter#
                                                -- @pExternalSemaphoreInfo@ /must/ be a valid pointer to a valid
                                                -- 'PhysicalDeviceExternalSemaphoreInfo' structure
                                                (PhysicalDeviceExternalSemaphoreInfo a)
                                             -> io (ExternalSemaphoreProperties)
getPhysicalDeviceExternalSemaphoreProperties physicalDevice externalSemaphoreInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalSemaphorePropertiesPtr = pVkGetPhysicalDeviceExternalSemaphoreProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceExternalSemaphorePropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalSemaphoreProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalSemaphoreProperties' = mkVkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphorePropertiesPtr
  pExternalSemaphoreInfo <- ContT $ withCStruct (externalSemaphoreInfo)
  pPExternalSemaphoreProperties <- ContT (withZeroCStruct @ExternalSemaphoreProperties)
  lift $ traceAroundEvent "vkGetPhysicalDeviceExternalSemaphoreProperties" (vkGetPhysicalDeviceExternalSemaphoreProperties' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pExternalSemaphoreInfo) (pPExternalSemaphoreProperties))
  pExternalSemaphoreProperties <- lift $ peekCStruct @ExternalSemaphoreProperties pPExternalSemaphoreProperties
  pure $ (pExternalSemaphoreProperties)


-- | VkPhysicalDeviceExternalSemaphoreInfo - Structure specifying semaphore
-- creation parameters.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceExternalSemaphoreInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO'
--
-- -   #VUID-VkPhysicalDeviceExternalSemaphoreInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
--
-- -   #VUID-VkPhysicalDeviceExternalSemaphoreInfo-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPhysicalDeviceExternalSemaphoreInfo-handleType-parameter#
--     @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalSemaphoreProperties',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR'
data PhysicalDeviceExternalSemaphoreInfo (es :: [Type]) = PhysicalDeviceExternalSemaphoreInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- value specifying the external semaphore handle type for which
    -- capabilities will be returned.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalSemaphoreInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceExternalSemaphoreInfo es)

instance Extensible PhysicalDeviceExternalSemaphoreInfo where
  extensibleTypeName = "PhysicalDeviceExternalSemaphoreInfo"
  setNext x next = x{next = next}
  getNext PhysicalDeviceExternalSemaphoreInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceExternalSemaphoreInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SemaphoreTypeCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceExternalSemaphoreInfo es, PokeChain es) => ToCStruct (PhysicalDeviceExternalSemaphoreInfo es) where
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

instance (Extendss PhysicalDeviceExternalSemaphoreInfo es, PeekChain es) => FromCStruct (PhysicalDeviceExternalSemaphoreInfo es) where
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
-- 'Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits.ExternalSemaphoreFeatureFlags',
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalSemaphoreProperties',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR'
data ExternalSemaphoreProperties = ExternalSemaphoreProperties
  { -- | @exportFromImportedHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying which types of imported handle @handleType@ /can/ be exported
    -- from.
    exportFromImportedHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- specifying handle types which /can/ be specified at the same time as
    -- @handleType@ when creating a semaphore.
    compatibleHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- | @externalSemaphoreFeatures@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits.ExternalSemaphoreFeatureFlagBits'
    -- describing the features of @handleType@.
    externalSemaphoreFeatures :: ExternalSemaphoreFeatureFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalSemaphoreProperties)
#endif
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

