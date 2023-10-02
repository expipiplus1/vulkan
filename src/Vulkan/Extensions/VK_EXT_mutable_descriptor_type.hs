{-# language CPP #-}
-- | = Name
--
-- VK_EXT_mutable_descriptor_type - device extension
--
-- == VK_EXT_mutable_descriptor_type
--
-- [__Name String__]
--     @VK_EXT_mutable_descriptor_type@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     495
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance3 VK_KHR_maintenance3>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_mutable_descriptor_type] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_mutable_descriptor_type extension* >
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_mutable_descriptor_type] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_EXT_mutable_descriptor_type extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_mutable_descriptor_type.adoc VK_EXT_mutable_descriptor_type>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-08-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to reduce descriptor memory footprint
-- by allowing a descriptor to be able to mutate to a given list of
-- descriptor types depending on which descriptor types are written into,
-- or copied into a descriptor set.
--
-- The main use case this extension intends to address is descriptor
-- indexing with
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
-- where the descriptor types are completely generic, as this means
-- applications can allocate one large descriptor set, rather than having
-- one large descriptor set per descriptor type, which significantly bloats
-- descriptor memory usage and causes performance issues.
--
-- This extension also adds a mechanism to declare that a descriptor pool,
-- and therefore the descriptor sets that are allocated from it, reside
-- only in host memory; as such these descriptors can only be
-- updated\/copied, but not bound.
--
-- These features together allow much more efficient emulation of the raw
-- D3D12 binding model. This extension is primarily intended to be useful
-- for API layering efforts.
--
-- == New Structures
--
-- -   'MutableDescriptorTypeListEXT'
--
-- -   Extending
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'MutableDescriptorTypeCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMutableDescriptorTypeFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME'
--
-- -   'EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-08-22 (Jon Leech)
--
--     -   Initial version, promoted from
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type VK_VALVE_mutable_descriptor_type>.
--
-- == See Also
--
-- 'MutableDescriptorTypeCreateInfoEXT', 'MutableDescriptorTypeListEXT',
-- 'PhysicalDeviceMutableDescriptorTypeFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_mutable_descriptor_type  ( PhysicalDeviceMutableDescriptorTypeFeaturesEXT(..)
                                                         , MutableDescriptorTypeListEXT(..)
                                                         , MutableDescriptorTypeCreateInfoEXT(..)
                                                         , EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                         , pattern EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                         , EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                         , pattern EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT))
-- | VkPhysicalDeviceMutableDescriptorTypeFeaturesEXT - Structure describing
-- whether the mutable descriptor type is supported
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #features-mutableDescriptorType# @mutableDescriptorType@ indicates
--     that the implementation /must/ support using the
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--     with at least the following descriptor types, where any combination
--     of the types /must/ be supported:
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--
-- -   Additionally, @mutableDescriptorType@ indicates that:
--
--     -   Non-uniform descriptor indexing /must/ be supported if all
--         descriptor types in a 'MutableDescriptorTypeListEXT' for
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--         have the corresponding non-uniform indexing features enabled in
--         'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures'.
--
--     -   'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--         with @descriptorType@ of
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--         relaxes the list of required descriptor types to the descriptor
--         types which have the corresponding update-after-bind feature
--         enabled in
--         'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures'.
--
--     -   Dynamically uniform descriptor indexing /must/ be supported if
--         all descriptor types in a 'MutableDescriptorTypeListEXT' for
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--         have the corresponding dynamic indexing features enabled.
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_EXT'
--         /must/ be supported.
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--         /must/ be supported.
--
-- If the 'PhysicalDeviceMutableDescriptorTypeFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMutableDescriptorTypeFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceMutableDescriptorTypeFeaturesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mutable_descriptor_type VK_EXT_mutable_descriptor_type>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type VK_VALVE_mutable_descriptor_type>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMutableDescriptorTypeFeaturesEXT = PhysicalDeviceMutableDescriptorTypeFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceMutableDescriptorTypeFeaturesEXT" "mutableDescriptorType"
    mutableDescriptorType :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMutableDescriptorTypeFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceMutableDescriptorTypeFeaturesEXT

instance ToCStruct PhysicalDeviceMutableDescriptorTypeFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMutableDescriptorTypeFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (mutableDescriptorType))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMutableDescriptorTypeFeaturesEXT where
  peekCStruct p = do
    mutableDescriptorType <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMutableDescriptorTypeFeaturesEXT
             (bool32ToBool mutableDescriptorType)

instance Storable PhysicalDeviceMutableDescriptorTypeFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMutableDescriptorTypeFeaturesEXT where
  zero = PhysicalDeviceMutableDescriptorTypeFeaturesEXT
           zero


-- | VkMutableDescriptorTypeListEXT - Structure describing descriptor types
-- that a given descriptor may mutate to
--
-- == Valid Usage
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-descriptorTypeCount-04597#
--     @descriptorTypeCount@ /must/ not be @0@ if the corresponding binding
--     is of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-04598#
--     @pDescriptorTypes@ /must/ be a valid pointer to an array of
--     @descriptorTypeCount@ valid, unique
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values if the
--     given binding is of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--     type
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-descriptorTypeCount-04599#
--     @descriptorTypeCount@ /must/ be @0@ if the corresponding binding is
--     not of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-04600#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-04601#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-04602#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-04603#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMutableDescriptorTypeListEXT-pDescriptorTypes-parameter# If
--     @descriptorTypeCount@ is not @0@, @pDescriptorTypes@ /must/ be a
--     valid pointer to an array of @descriptorTypeCount@ valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mutable_descriptor_type VK_EXT_mutable_descriptor_type>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type VK_VALVE_mutable_descriptor_type>,
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'MutableDescriptorTypeCreateInfoEXT'
data MutableDescriptorTypeListEXT = MutableDescriptorTypeListEXT
  { -- | @pDescriptorTypes@ is @NULL@ or a pointer to an array of
    -- @descriptorTypeCount@
    -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values defining
    -- which descriptor types a given binding may mutate to.
    descriptorTypes :: Vector DescriptorType }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MutableDescriptorTypeListEXT)
#endif
deriving instance Show MutableDescriptorTypeListEXT

instance ToCStruct MutableDescriptorTypeListEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MutableDescriptorTypeListEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorTypes)) :: Word32))
    pPDescriptorTypes' <- ContT $ allocaBytes @DescriptorType ((Data.Vector.length (descriptorTypes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorTypes' `plusPtr` (4 * (i)) :: Ptr DescriptorType) (e)) (descriptorTypes)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr DescriptorType))) (pPDescriptorTypes')
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct MutableDescriptorTypeListEXT where
  peekCStruct p = do
    descriptorTypeCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pDescriptorTypes <- peek @(Ptr DescriptorType) ((p `plusPtr` 8 :: Ptr (Ptr DescriptorType)))
    pDescriptorTypes' <- generateM (fromIntegral descriptorTypeCount) (\i -> peek @DescriptorType ((pDescriptorTypes `advancePtrBytes` (4 * (i)) :: Ptr DescriptorType)))
    pure $ MutableDescriptorTypeListEXT
             pDescriptorTypes'

instance Zero MutableDescriptorTypeListEXT where
  zero = MutableDescriptorTypeListEXT
           mempty


-- | VkMutableDescriptorTypeCreateInfoEXT - Structure describing the list of
-- possible active descriptor types for mutable type descriptors
--
-- = Description
--
-- If @mutableDescriptorTypeListCount@ is zero or if this structure is not
-- included in the @pNext@ chain, the 'MutableDescriptorTypeListEXT' for
-- each element is considered to be zero or @NULL@ for each member.
-- Otherwise, the descriptor set layout binding at
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the descriptor type lists in
-- 'MutableDescriptorTypeCreateInfoEXT'::@pMutableDescriptorTypeLists@[i].
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMutableDescriptorTypeCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT'
--
-- -   #VUID-VkMutableDescriptorTypeCreateInfoEXT-pMutableDescriptorTypeLists-parameter#
--     If @mutableDescriptorTypeListCount@ is not @0@,
--     @pMutableDescriptorTypeLists@ /must/ be a valid pointer to an array
--     of @mutableDescriptorTypeListCount@ valid
--     'MutableDescriptorTypeListEXT' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mutable_descriptor_type VK_EXT_mutable_descriptor_type>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type VK_VALVE_mutable_descriptor_type>,
-- 'MutableDescriptorTypeListEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MutableDescriptorTypeCreateInfoEXT = MutableDescriptorTypeCreateInfoEXT
  { -- | @pMutableDescriptorTypeLists@ is a pointer to an array of
    -- 'MutableDescriptorTypeListEXT' structures.
    mutableDescriptorTypeLists :: Vector MutableDescriptorTypeListEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MutableDescriptorTypeCreateInfoEXT)
#endif
deriving instance Show MutableDescriptorTypeCreateInfoEXT

instance ToCStruct MutableDescriptorTypeCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MutableDescriptorTypeCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (mutableDescriptorTypeLists)) :: Word32))
    pPMutableDescriptorTypeLists' <- ContT $ allocaBytes @MutableDescriptorTypeListEXT ((Data.Vector.length (mutableDescriptorTypeLists)) * 16)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMutableDescriptorTypeLists' `plusPtr` (16 * (i)) :: Ptr MutableDescriptorTypeListEXT) (e) . ($ ())) (mutableDescriptorTypeLists)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr MutableDescriptorTypeListEXT))) (pPMutableDescriptorTypeLists')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MutableDescriptorTypeCreateInfoEXT where
  peekCStruct p = do
    mutableDescriptorTypeListCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pMutableDescriptorTypeLists <- peek @(Ptr MutableDescriptorTypeListEXT) ((p `plusPtr` 24 :: Ptr (Ptr MutableDescriptorTypeListEXT)))
    pMutableDescriptorTypeLists' <- generateM (fromIntegral mutableDescriptorTypeListCount) (\i -> peekCStruct @MutableDescriptorTypeListEXT ((pMutableDescriptorTypeLists `advancePtrBytes` (16 * (i)) :: Ptr MutableDescriptorTypeListEXT)))
    pure $ MutableDescriptorTypeCreateInfoEXT
             pMutableDescriptorTypeLists'

instance Zero MutableDescriptorTypeCreateInfoEXT where
  zero = MutableDescriptorTypeCreateInfoEXT
           mempty


type EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION"
pattern EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1


type EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_EXT_mutable_descriptor_type"

-- No documentation found for TopLevel "VK_EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME"
pattern EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_EXT_mutable_descriptor_type"

