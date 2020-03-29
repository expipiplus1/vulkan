{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3  ( getDescriptorSetLayoutSupport
                                                                 , PhysicalDeviceMaintenance3Properties(..)
                                                                 , DescriptorSetLayoutSupport(..)
                                                                 , StructureType(..)
                                                                 ) where

import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountLayoutSupport)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutSupport))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSupport
  :: FunPtr (Ptr Device_T -> Ptr (DescriptorSetLayoutCreateInfo a) -> Ptr (DescriptorSetLayoutSupport b) -> IO ()) -> Ptr Device_T -> Ptr (DescriptorSetLayoutCreateInfo a) -> Ptr (DescriptorSetLayoutSupport b) -> IO ()

-- | vkGetDescriptorSetLayoutSupport - Query whether a descriptor set layout
-- can be created
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     would create the descriptor set layout.
--
-- -   @pCreateInfo@ is a pointer to a
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'
--     structure specifying the state of the descriptor set layout object.
--
-- -   @pSupport@ is a pointer to a 'DescriptorSetLayoutSupport' structure,
--     in which information about support for the descriptor set layout
--     object is returned.
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
-- 'Graphics.Vulkan.Core10.BaseType.TRUE' in
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
-- This is a 'Graphics.Vulkan.Core10.Handles.Device' query rather than
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice' because the answer /may/
-- depend on enabled features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
-- 'DescriptorSetLayoutSupport', 'Graphics.Vulkan.Core10.Handles.Device'
getDescriptorSetLayoutSupport :: (PokeChain a, PokeChain b, PeekChain b) => Device -> DescriptorSetLayoutCreateInfo a -> IO (DescriptorSetLayoutSupport b)
getDescriptorSetLayoutSupport device createInfo = evalContT $ do
  let vkGetDescriptorSetLayoutSupport' = mkVkGetDescriptorSetLayoutSupport (pVkGetDescriptorSetLayoutSupport (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPSupport <- ContT (withZeroCStruct @(DescriptorSetLayoutSupport _))
  lift $ vkGetDescriptorSetLayoutSupport' (deviceHandle (device)) pCreateInfo (pPSupport)
  pSupport <- lift $ peekCStruct @(DescriptorSetLayoutSupport _) pPSupport
  pure $ (pSupport)


-- | VkPhysicalDeviceMaintenance3Properties - Structure describing descriptor
-- set properties
--
-- = Members
--
-- The members of the 'PhysicalDeviceMaintenance3Properties' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance3Properties' structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- | @maxPerSetDescriptors@ is a maximum number of descriptors (summed over
    -- all descriptor types) in a single descriptor set that is guaranteed to
    -- satisfy any implementation-dependent constraints on the size of a
    -- descriptor set itself. Applications /can/ query whether a descriptor set
    -- that goes beyond this limit is supported using
    -- 'getDescriptorSetLayoutSupport'.
    maxPerSetDescriptors :: Word32
  , -- | @maxMemoryAllocationSize@ is the maximum size of a memory allocation
    -- that /can/ be created, even if there is more space available in the
    -- heap.
    maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Typeable)
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


-- | VkDescriptorSetLayoutSupport - Structure returning information about
-- whether a descriptor set layout can be supported
--
-- = Description
--
-- @supported@ is set to 'Graphics.Vulkan.Core10.BaseType.TRUE' if the
-- descriptor set /can/ be created, or else is set to
-- 'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountLayoutSupport'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance3.getDescriptorSetLayoutSupportKHR'
data DescriptorSetLayoutSupport (es :: [Type]) = DescriptorSetLayoutSupport
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @supported@ specifies whether the descriptor set layout /can/ be
    -- created.
    supported :: Bool
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (DescriptorSetLayoutSupport es)

instance Extensible DescriptorSetLayoutSupport where
  extensibleType = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  setNext x next = x{next = next}
  getNext DescriptorSetLayoutSupport{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetLayoutSupport e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DescriptorSetVariableDescriptorCountLayoutSupport = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (DescriptorSetLayoutSupport es) where
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

instance PeekChain es => FromCStruct (DescriptorSetLayoutSupport es) where
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

