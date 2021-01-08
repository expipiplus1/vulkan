{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_mutable_descriptor_type - device extension
--
-- == VK_VALVE_mutable_descriptor_type
--
-- [__Name String__]
--     @VK_VALVE_mutable_descriptor_type@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     352
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_maintenance3@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_VALVE_mutable_descriptor_type:%20&body=@Joshua-Ashton%20 >
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_VALVE_mutable_descriptor_type:%20&body=@HansKristian-Work%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-12-02
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
-- -   'MutableDescriptorTypeListVALVE'
--
-- -   Extending
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'MutableDescriptorTypeCreateInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME'
--
-- -   'VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE'
--
-- == Version History
--
-- -   Revision 1, 2020-12-01 (Joshua Ashton, Hans-Kristian Arntzen)
--
--     -   Initial specification, squashed from public draft.
--
-- = See Also
--
-- 'MutableDescriptorTypeCreateInfoVALVE',
-- 'MutableDescriptorTypeListVALVE',
-- 'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_mutable_descriptor_type  ( PhysicalDeviceMutableDescriptorTypeFeaturesVALVE(..)
                                                           , MutableDescriptorTypeListVALVE(..)
                                                           , MutableDescriptorTypeCreateInfoVALVE(..)
                                                           , VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                           , pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                           , VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                           , pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                           ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE))
-- | VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE - Structure
-- describing whether the mutable descriptor type is supported
--
-- = Members
--
-- The members of the 'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
-- structure describe the following features:
--
-- = Description
--
-- -   #features-mutableDescriptorType# @mutableDescriptorType@ indicates
--     that the implementation /must/ support using the
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
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
--         descriptor types in a 'MutableDescriptorTypeListVALVE' for
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--         have the corresponding non-uniform indexing features enabled in
--         'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures'.
--
--     -   'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--         with @descriptorType@ of
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--         relaxes the list of required descriptor types to the descriptor
--         types which have the corresponding update-after-bind feature
--         enabled in
--         'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures'.
--
--     -   Dynamically uniform descriptor indexing /must/ be supported if
--         all descriptor types in a 'MutableDescriptorTypeListVALVE' for
--         'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--         have the corresponding dynamic indexing features enabled.
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--         /must/ be supported.
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--         /must/ be supported.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMutableDescriptorTypeFeaturesVALVE = PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
  { -- No documentation found for Nested "VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE" "mutableDescriptorType"
    mutableDescriptorType :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMutableDescriptorTypeFeaturesVALVE)
#endif
deriving instance Show PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

instance ToCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMutableDescriptorTypeFeaturesVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (mutableDescriptorType))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE where
  peekCStruct p = do
    mutableDescriptorType <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
             (bool32ToBool mutableDescriptorType)

instance Storable PhysicalDeviceMutableDescriptorTypeFeaturesVALVE where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMutableDescriptorTypeFeaturesVALVE where
  zero = PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
           zero


-- | VkMutableDescriptorTypeListVALVE - Structure describing descriptor types
-- that a given descriptor may mutate to
--
-- == Valid Usage
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-descriptorTypeCount-04597#
--     @descriptorTypeCount@ /must/ not be @0@ if the corresponding binding
--     is of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-04598#
--     @pDescriptorTypes@ /must/ be a valid pointer to an array of
--     @descriptorTypeCount@ valid, unique
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values if the
--     given binding is of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--     type
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-descriptorTypeCount-04599#
--     @descriptorTypeCount@ /must/ be @0@ if the corresponding binding is
--     not of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-04600#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-04601#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-04602#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-04603#
--     @pDescriptorTypes@ /must/ not contain
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMutableDescriptorTypeListVALVE-pDescriptorTypes-parameter#
--     If @descriptorTypeCount@ is not @0@, @pDescriptorTypes@ /must/ be a
--     valid pointer to an array of @descriptorTypeCount@ valid
--     'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'MutableDescriptorTypeCreateInfoVALVE'
data MutableDescriptorTypeListVALVE = MutableDescriptorTypeListVALVE
  { -- | @pDescriptorTypes@ is @NULL@ or a pointer to an array of
    -- @descriptorTypeCount@
    -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' values which define
    -- which descriptor types a given binding may mutate to.
    descriptorTypes :: Vector DescriptorType }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MutableDescriptorTypeListVALVE)
#endif
deriving instance Show MutableDescriptorTypeListVALVE

instance ToCStruct MutableDescriptorTypeListVALVE where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MutableDescriptorTypeListVALVE{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorTypes)) :: Word32))
    pPDescriptorTypes' <- ContT $ allocaBytesAligned @DescriptorType ((Data.Vector.length (descriptorTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorTypes' `plusPtr` (4 * (i)) :: Ptr DescriptorType) (e)) (descriptorTypes)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr DescriptorType))) (pPDescriptorTypes')
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct MutableDescriptorTypeListVALVE where
  peekCStruct p = do
    descriptorTypeCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pDescriptorTypes <- peek @(Ptr DescriptorType) ((p `plusPtr` 8 :: Ptr (Ptr DescriptorType)))
    pDescriptorTypes' <- generateM (fromIntegral descriptorTypeCount) (\i -> peek @DescriptorType ((pDescriptorTypes `advancePtrBytes` (4 * (i)) :: Ptr DescriptorType)))
    pure $ MutableDescriptorTypeListVALVE
             pDescriptorTypes'

instance Zero MutableDescriptorTypeListVALVE where
  zero = MutableDescriptorTypeListVALVE
           mempty


-- | VkMutableDescriptorTypeCreateInfoVALVE - Structure describing the list
-- of possible active descriptor types for mutable type descriptors
--
-- = Description
--
-- If @mutableDescriptorTypeListCount@ is zero or if this structure is not
-- included in the @pNext@ chain, the 'MutableDescriptorTypeListVALVE' for
-- each element is considered to be zero or @NULL@ for each member.
-- Otherwise, the descriptor set layout binding at
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the descriptor type lists in
-- 'MutableDescriptorTypeCreateInfoVALVE'::@pMutableDescriptorTypeLists@[i].
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMutableDescriptorTypeCreateInfoVALVE-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE'
--
-- -   #VUID-VkMutableDescriptorTypeCreateInfoVALVE-pMutableDescriptorTypeLists-parameter#
--     If @mutableDescriptorTypeListCount@ is not @0@,
--     @pMutableDescriptorTypeLists@ /must/ be a valid pointer to an array
--     of @mutableDescriptorTypeListCount@ valid
--     'MutableDescriptorTypeListVALVE' structures
--
-- = See Also
--
-- 'MutableDescriptorTypeListVALVE',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MutableDescriptorTypeCreateInfoVALVE = MutableDescriptorTypeCreateInfoVALVE
  { -- | @pMutableDescriptorTypeLists@ is a pointer to an array of
    -- 'MutableDescriptorTypeListVALVE' structures.
    mutableDescriptorTypeLists :: Vector MutableDescriptorTypeListVALVE }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MutableDescriptorTypeCreateInfoVALVE)
#endif
deriving instance Show MutableDescriptorTypeCreateInfoVALVE

instance ToCStruct MutableDescriptorTypeCreateInfoVALVE where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MutableDescriptorTypeCreateInfoVALVE{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (mutableDescriptorTypeLists)) :: Word32))
    pPMutableDescriptorTypeLists' <- ContT $ allocaBytesAligned @MutableDescriptorTypeListVALVE ((Data.Vector.length (mutableDescriptorTypeLists)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMutableDescriptorTypeLists' `plusPtr` (16 * (i)) :: Ptr MutableDescriptorTypeListVALVE) (e) . ($ ())) (mutableDescriptorTypeLists)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr MutableDescriptorTypeListVALVE))) (pPMutableDescriptorTypeLists')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MutableDescriptorTypeCreateInfoVALVE where
  peekCStruct p = do
    mutableDescriptorTypeListCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pMutableDescriptorTypeLists <- peek @(Ptr MutableDescriptorTypeListVALVE) ((p `plusPtr` 24 :: Ptr (Ptr MutableDescriptorTypeListVALVE)))
    pMutableDescriptorTypeLists' <- generateM (fromIntegral mutableDescriptorTypeListCount) (\i -> peekCStruct @MutableDescriptorTypeListVALVE ((pMutableDescriptorTypeLists `advancePtrBytes` (16 * (i)) :: Ptr MutableDescriptorTypeListVALVE)))
    pure $ MutableDescriptorTypeCreateInfoVALVE
             pMutableDescriptorTypeLists'

instance Zero MutableDescriptorTypeCreateInfoVALVE where
  zero = MutableDescriptorTypeCreateInfoVALVE
           mempty


type VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION"
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1


type VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_VALVE_mutable_descriptor_type"

-- No documentation found for TopLevel "VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME"
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_VALVE_mutable_descriptor_type"

