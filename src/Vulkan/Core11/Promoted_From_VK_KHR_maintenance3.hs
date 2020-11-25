{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance3"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance3  ( getDescriptorSetLayoutSupport
                                                        , PhysicalDeviceMaintenance3Properties(..)
                                                        , DescriptorSetLayoutSupport(..)
                                                        , StructureType(..)
                                                        ) where

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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountLayoutSupport)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutSupport))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
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
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSupport
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr (SomeStruct DescriptorSetLayoutSupport) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr (SomeStruct DescriptorSetLayoutSupport) -> IO ()

-- No documentation found for TopLevel "vkGetDescriptorSetLayoutSupport"
getDescriptorSetLayoutSupport :: forall a b io
                               . (Extendss DescriptorSetLayoutCreateInfo a, Extendss DescriptorSetLayoutSupport b, PokeChain a, PokeChain b, PeekChain b, MonadIO io)
                              => -- No documentation found for Nested "vkGetDescriptorSetLayoutSupport" "device"
                                 Device
                              -> -- No documentation found for Nested "vkGetDescriptorSetLayoutSupport" "pCreateInfo"
                                 (DescriptorSetLayoutCreateInfo a)
                              -> io (DescriptorSetLayoutSupport b)
getDescriptorSetLayoutSupport device createInfo = liftIO . evalContT $ do
  let vkGetDescriptorSetLayoutSupportPtr = pVkGetDescriptorSetLayoutSupport (deviceCmds (device :: Device))
  lift $ unless (vkGetDescriptorSetLayoutSupportPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetLayoutSupport is null" Nothing Nothing
  let vkGetDescriptorSetLayoutSupport' = mkVkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupportPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPSupport <- ContT (withZeroCStruct @(DescriptorSetLayoutSupport _))
  lift $ vkGetDescriptorSetLayoutSupport' (deviceHandle (device)) (forgetExtensions pCreateInfo) (forgetExtensions (pPSupport))
  pSupport <- lift $ peekCStruct @(DescriptorSetLayoutSupport _) pPSupport
  pure $ (pSupport)



-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3Properties"
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
    maxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance3Properties)
#endif
deriving instance Show PhysicalDeviceMaintenance3Properties

instance ToCStruct PhysicalDeviceMaintenance3Properties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance3Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPerSetDescriptors)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxMemoryAllocationSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceMaintenance3Properties where
  peekCStruct p = do
    maxPerSetDescriptors <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxMemoryAllocationSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ PhysicalDeviceMaintenance3Properties
             maxPerSetDescriptors maxMemoryAllocationSize


instance Storable PhysicalDeviceMaintenance3Properties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance3Properties where
  zero = PhysicalDeviceMaintenance3Properties
           zero
           zero



-- No documentation found for TopLevel "VkDescriptorSetLayoutSupport"
data DescriptorSetLayoutSupport (es :: [Type]) = DescriptorSetLayoutSupport
  { -- No documentation found for Nested "VkDescriptorSetLayoutSupport" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDescriptorSetLayoutSupport" "supported"
    supported :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutSupport (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetLayoutSupport es)

instance Extensible DescriptorSetLayoutSupport where
  extensibleType = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  setNext x next = x{next = next}
  getNext DescriptorSetLayoutSupport{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetLayoutSupport e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetVariableDescriptorCountLayoutSupport = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetLayoutSupport es, PokeChain es) => ToCStruct (DescriptorSetLayoutSupport es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutSupport{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supported))
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss DescriptorSetLayoutSupport es, PeekChain es) => FromCStruct (DescriptorSetLayoutSupport es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    supported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DescriptorSetLayoutSupport
             next (bool32ToBool supported)

instance es ~ '[] => Zero (DescriptorSetLayoutSupport es) where
  zero = DescriptorSetLayoutSupport
           ()
           zero

