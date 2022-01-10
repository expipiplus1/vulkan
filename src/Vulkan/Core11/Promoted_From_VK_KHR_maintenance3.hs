{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance3"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance3  ( getDescriptorSetLayoutSupport
                                                        , PhysicalDeviceMaintenance3Properties(..)
                                                        , DescriptorSetLayoutSupport(..)
                                                        , StructureType(..)
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutSupport))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSupport
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr (SomeStruct DescriptorSetLayoutSupport) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct DescriptorSetLayoutCreateInfo) -> Ptr (SomeStruct DescriptorSetLayoutSupport) -> IO ()

-- | vkGetDescriptorSetLayoutSupport - Query whether a descriptor set layout
-- can be created
--
-- = Description
--
-- Some implementations have limitations on what fits in a descriptor set
-- which are not easily expressible in terms of existing limits like
-- @maxDescriptorSet@*, for example if all descriptor types share a limited
-- space in memory but each descriptor is a different size or alignment.
-- This command returns information about whether a descriptor set
-- satisfies this limit. If the descriptor set layout satisfies the
-- 'PhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@ limit,
-- this command is guaranteed to return
-- 'Vulkan.Core10.FundamentalTypes.TRUE' in
-- 'DescriptorSetLayoutSupport'::@supported@. If the descriptor set layout
-- exceeds the
-- 'PhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@ limit,
-- whether the descriptor set layout is supported is
-- implementation-dependent and /may/ depend on whether the descriptor
-- sizes and alignments cause the layout to exceed an internal limit.
--
-- This command does not consider other limits such as
-- @maxPerStageDescriptor@*, and so a descriptor set layout that is
-- supported according to this command /must/ still satisfy the pipeline
-- layout limits such as @maxPerStageDescriptor@* in order to be used in a
-- pipeline layout.
--
-- Note
--
-- This is a 'Vulkan.Core10.Handles.Device' query rather than
-- 'Vulkan.Core10.Handles.PhysicalDevice' because the answer /may/ depend
-- on enabled features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
-- 'DescriptorSetLayoutSupport', 'Vulkan.Core10.Handles.Device'
getDescriptorSetLayoutSupport :: forall a b io
                               . (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, Extendss DescriptorSetLayoutSupport b, PokeChain b, PeekChain b, MonadIO io)
                              => -- | @device@ is the logical device that would create the descriptor set
                                 -- layout.
                                 --
                                 -- #VUID-vkGetDescriptorSetLayoutSupport-device-parameter# @device@ /must/
                                 -- be a valid 'Vulkan.Core10.Handles.Device' handle
                                 Device
                              -> -- | @pCreateInfo@ is a pointer to a
                                 -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo' structure
                                 -- specifying the state of the descriptor set layout object.
                                 --
                                 -- #VUID-vkGetDescriptorSetLayoutSupport-pCreateInfo-parameter#
                                 -- @pCreateInfo@ /must/ be a valid pointer to a valid
                                 -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo' structure
                                 (DescriptorSetLayoutCreateInfo a)
                              -> io (DescriptorSetLayoutSupport b)
getDescriptorSetLayoutSupport device createInfo = liftIO . evalContT $ do
  let vkGetDescriptorSetLayoutSupportPtr = pVkGetDescriptorSetLayoutSupport (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorSetLayoutSupportPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetLayoutSupport is null" Nothing Nothing
  let vkGetDescriptorSetLayoutSupport' = mkVkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupportPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPSupport <- ContT (withZeroCStruct @(DescriptorSetLayoutSupport _))
  lift $ traceAroundEvent "vkGetDescriptorSetLayoutSupport" (vkGetDescriptorSetLayoutSupport' (deviceHandle (device)) (forgetExtensions pCreateInfo) (forgetExtensions (pPSupport)))
  pSupport <- lift $ peekCStruct @(DescriptorSetLayoutSupport _) pPSupport
  pure $ (pSupport)


-- | VkPhysicalDeviceMaintenance3Properties - Structure describing descriptor
-- set properties
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance3Properties' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- | #extension-limits-maxPerSetDescriptors# @maxPerSetDescriptors@ is a
    -- maximum number of descriptors (summed over all descriptor types) in a
    -- single descriptor set that is guaranteed to satisfy any
    -- implementation-dependent constraints on the size of a descriptor set
    -- itself. Applications /can/ query whether a descriptor set that goes
    -- beyond this limit is supported using 'getDescriptorSetLayoutSupport'.
    maxPerSetDescriptors :: Word32
  , -- | #extension-limits-maxMemoryAllocationSize# @maxMemoryAllocationSize@ is
    -- the maximum size of a memory allocation that /can/ be created, even if
    -- there is more space available in the heap.
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance3Properties)
#endif
deriving instance Show PhysicalDeviceMaintenance3Properties

instance ToCStruct PhysicalDeviceMaintenance3Properties where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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


-- | VkDescriptorSetLayoutSupport - Structure returning information about
-- whether a descriptor set layout can be supported
--
-- = Description
--
-- @supported@ is set to 'Vulkan.Core10.FundamentalTypes.TRUE' if the
-- descriptor set /can/ be created, or else is set to
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetLayoutSupport-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT'
--
-- -   #VUID-VkDescriptorSetLayoutSupport-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountLayoutSupport'
--
-- -   #VUID-VkDescriptorSetLayoutSupport-sType-unique# The @sType@ value
--     of each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDescriptorSetLayoutSupport',
-- 'Vulkan.Extensions.VK_KHR_maintenance3.getDescriptorSetLayoutSupportKHR'
data DescriptorSetLayoutSupport (es :: [Type]) = DescriptorSetLayoutSupport
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @supported@ specifies whether the descriptor set layout /can/ be
    -- created.
    supported :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutSupport (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetLayoutSupport es)

instance Extensible DescriptorSetLayoutSupport where
  extensibleTypeName = "DescriptorSetLayoutSupport"
  setNext DescriptorSetLayoutSupport{..} next' = DescriptorSetLayoutSupport{next = next', ..}
  getNext DescriptorSetLayoutSupport{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetLayoutSupport e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetVariableDescriptorCountLayoutSupport = Just f
    | otherwise = Nothing

instance (Extendss DescriptorSetLayoutSupport es, PokeChain es) => ToCStruct (DescriptorSetLayoutSupport es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

