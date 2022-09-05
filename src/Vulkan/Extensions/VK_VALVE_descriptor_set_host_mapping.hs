{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_descriptor_set_host_mapping - device extension
--
-- == VK_VALVE_descriptor_set_host_mapping
--
-- [__Name String__]
--     @VK_VALVE_descriptor_set_host_mapping@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     421
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_descriptor_set_host_mapping] @HansKristian-Work%0A<<Here describe the issue or question you have about the VK_VALVE_descriptor_set_host_mapping extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to directly query a host pointer for
-- a 'Vulkan.Core10.Handles.DescriptorSet' which /can/ be used to copy
-- descriptors between descriptor sets without the use of an API command.
-- Memory offsets and sizes for descriptors /can/ be queried from a
-- 'Vulkan.Core10.Handles.DescriptorSetLayout' as well.
--
-- Note
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- Note
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Commands
--
-- -   'getDescriptorSetHostMappingVALVE'
--
-- -   'getDescriptorSetLayoutHostMappingInfoVALVE'
--
-- == New Structures
--
-- -   'DescriptorSetBindingReferenceVALVE'
--
-- -   'DescriptorSetLayoutHostMappingInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME'
--
-- -   'VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE'
--
-- == Stub API References
--
-- There is currently no specification language written for this command.
-- This section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > void vkGetDescriptorSetLayoutHostMappingInfoVALVE(
-- >     VkDevice                                    device,
-- >     const VkDescriptorSetBindingReferenceVALVE* pBindingReference,
-- >     VkDescriptorSetLayoutHostMappingInfoVALVE*  pHostMapping);
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-pBindingReference-parameter#
--     @pBindingReference@ /must/ be a valid pointer to a valid
--     'DescriptorSetBindingReferenceVALVE' structure
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-pHostMapping-parameter#
--     @pHostMapping@ /must/ be a valid pointer to a
--     'DescriptorSetLayoutHostMappingInfoVALVE' structure
--
-- There is currently no specification language written for this command.
-- This section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > void vkGetDescriptorSetHostMappingVALVE(
-- >     VkDevice                                    device,
-- >     VkDescriptorSet                             descriptorSet,
-- >     void**                                      ppData);
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-descriptorSet-parameter#
--     @descriptorSet@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-ppData-parameter# @ppData@
--     /must/ be a valid pointer to a pointer value
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           descriptorSetHostMapping;
-- > } VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkDescriptorSetBindingReferenceVALVE {
-- >     VkStructureType          sType;
-- >     const void*              pNext;
-- >     VkDescriptorSetLayout    descriptorSetLayout;
-- >     uint32_t                 binding;
-- > } VkDescriptorSetBindingReferenceVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE'
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-descriptorSetLayout-parameter#
--     @descriptorSetLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkDescriptorSetLayoutHostMappingInfoVALVE {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     size_t             descriptorOffset;
-- >     uint32_t           descriptorSize;
-- > } VkDescriptorSetLayoutHostMappingInfoVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetLayoutHostMappingInfoVALVE-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutHostMappingInfoVALVE-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- == Version History
--
-- -   Revision 1, 2022-02-22 (Hans-Kristian Arntzen)
--
--     -   Initial specification
--
-- == See Also
--
-- 'DescriptorSetBindingReferenceVALVE',
-- 'DescriptorSetLayoutHostMappingInfoVALVE',
-- 'PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE',
-- 'getDescriptorSetHostMappingVALVE',
-- 'getDescriptorSetLayoutHostMappingInfoVALVE'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping  ( getDescriptorSetLayoutHostMappingInfoVALVE
                                                               , getDescriptorSetHostMappingVALVE
                                                               , PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE(..)
                                                               , DescriptorSetBindingReferenceVALVE(..)
                                                               , DescriptorSetLayoutHostMappingInfoVALVE(..)
                                                               , VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION
                                                               , pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION
                                                               , VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME
                                                               , pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME
                                                               ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetHostMappingVALVE))
import Vulkan.Dynamic (DeviceCmds(pVkGetDescriptorSetLayoutHostMappingInfoVALVE))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutHostMappingInfoVALVE
  :: FunPtr (Ptr Device_T -> Ptr DescriptorSetBindingReferenceVALVE -> Ptr DescriptorSetLayoutHostMappingInfoVALVE -> IO ()) -> Ptr Device_T -> Ptr DescriptorSetBindingReferenceVALVE -> Ptr DescriptorSetLayoutHostMappingInfoVALVE -> IO ()

-- | vkGetDescriptorSetLayoutHostMappingInfoVALVE - Stub description of
-- vkGetDescriptorSetLayoutHostMappingInfoVALVE
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping VK_VALVE_descriptor_set_host_mapping>,
-- 'DescriptorSetBindingReferenceVALVE',
-- 'DescriptorSetLayoutHostMappingInfoVALVE',
-- 'Vulkan.Core10.Handles.Device'
getDescriptorSetLayoutHostMappingInfoVALVE :: forall io
                                            . (MonadIO io)
                                           => -- | #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-device-parameter#
                                              -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                              Device
                                           -> -- | #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-pBindingReference-parameter#
                                              -- @pBindingReference@ /must/ be a valid pointer to a valid
                                              -- 'DescriptorSetBindingReferenceVALVE' structure
                                              DescriptorSetBindingReferenceVALVE
                                           -> io (DescriptorSetLayoutHostMappingInfoVALVE)
getDescriptorSetLayoutHostMappingInfoVALVE device bindingReference = liftIO . evalContT $ do
  let vkGetDescriptorSetLayoutHostMappingInfoVALVEPtr = pVkGetDescriptorSetLayoutHostMappingInfoVALVE (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorSetLayoutHostMappingInfoVALVEPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetLayoutHostMappingInfoVALVE is null" Nothing Nothing
  let vkGetDescriptorSetLayoutHostMappingInfoVALVE' = mkVkGetDescriptorSetLayoutHostMappingInfoVALVE vkGetDescriptorSetLayoutHostMappingInfoVALVEPtr
  pBindingReference <- ContT $ withCStruct (bindingReference)
  pPHostMapping <- ContT (withZeroCStruct @DescriptorSetLayoutHostMappingInfoVALVE)
  lift $ traceAroundEvent "vkGetDescriptorSetLayoutHostMappingInfoVALVE" (vkGetDescriptorSetLayoutHostMappingInfoVALVE' (deviceHandle (device)) pBindingReference (pPHostMapping))
  pHostMapping <- lift $ peekCStruct @DescriptorSetLayoutHostMappingInfoVALVE pPHostMapping
  pure $ (pHostMapping)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetHostMappingVALVE
  :: FunPtr (Ptr Device_T -> DescriptorSet -> Ptr (Ptr ()) -> IO ()) -> Ptr Device_T -> DescriptorSet -> Ptr (Ptr ()) -> IO ()

-- | vkGetDescriptorSetHostMappingVALVE - Stub description of
-- vkGetDescriptorSetHostMappingVALVE
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping VK_VALVE_descriptor_set_host_mapping>,
-- 'Vulkan.Core10.Handles.DescriptorSet', 'Vulkan.Core10.Handles.Device'
getDescriptorSetHostMappingVALVE :: forall io
                                  . (MonadIO io)
                                 => -- | #VUID-vkGetDescriptorSetHostMappingVALVE-device-parameter# @device@
                                    -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                    Device
                                 -> -- | #VUID-vkGetDescriptorSetHostMappingVALVE-descriptorSet-parameter#
                                    -- @descriptorSet@ /must/ be a valid 'Vulkan.Core10.Handles.DescriptorSet'
                                    -- handle
                                    DescriptorSet
                                 -> io (("data" ::: Ptr ()))
getDescriptorSetHostMappingVALVE device descriptorSet = liftIO . evalContT $ do
  let vkGetDescriptorSetHostMappingVALVEPtr = pVkGetDescriptorSetHostMappingVALVE (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDescriptorSetHostMappingVALVEPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDescriptorSetHostMappingVALVE is null" Nothing Nothing
  let vkGetDescriptorSetHostMappingVALVE' = mkVkGetDescriptorSetHostMappingVALVE vkGetDescriptorSetHostMappingVALVEPtr
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  lift $ traceAroundEvent "vkGetDescriptorSetHostMappingVALVE" (vkGetDescriptorSetHostMappingVALVE' (deviceHandle (device)) (descriptorSet) (pPpData))
  ppData <- lift $ peek @(Ptr ()) pPpData
  pure $ (ppData)


-- | VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE - Stub description
-- of VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping VK_VALVE_descriptor_set_host_mapping>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE = PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE" "descriptorSetHostMapping"
    descriptorSetHostMapping :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE)
#endif
deriving instance Show PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE

instance ToCStruct PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (descriptorSetHostMapping))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE where
  peekCStruct p = do
    descriptorSetHostMapping <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
             (bool32ToBool descriptorSetHostMapping)

instance Storable PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE where
  zero = PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
           zero


-- | VkDescriptorSetBindingReferenceVALVE - Stub description of
-- VkDescriptorSetBindingReferenceVALVE
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping VK_VALVE_descriptor_set_host_mapping>,
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDescriptorSetLayoutHostMappingInfoVALVE'
data DescriptorSetBindingReferenceVALVE = DescriptorSetBindingReferenceVALVE
  { -- | #VUID-VkDescriptorSetBindingReferenceVALVE-descriptorSetLayout-parameter#
    -- @descriptorSetLayout@ /must/ be a valid
    -- 'Vulkan.Core10.Handles.DescriptorSetLayout' handle
    descriptorSetLayout :: DescriptorSetLayout
  , -- No documentation found for Nested "VkDescriptorSetBindingReferenceVALVE" "binding"
    binding :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetBindingReferenceVALVE)
#endif
deriving instance Show DescriptorSetBindingReferenceVALVE

instance ToCStruct DescriptorSetBindingReferenceVALVE where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetBindingReferenceVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSetLayout)) (descriptorSetLayout)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (binding)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DescriptorSetLayout)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorSetBindingReferenceVALVE where
  peekCStruct p = do
    descriptorSetLayout <- peek @DescriptorSetLayout ((p `plusPtr` 16 :: Ptr DescriptorSetLayout))
    binding <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DescriptorSetBindingReferenceVALVE
             descriptorSetLayout binding

instance Storable DescriptorSetBindingReferenceVALVE where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorSetBindingReferenceVALVE where
  zero = DescriptorSetBindingReferenceVALVE
           zero
           zero


-- | VkDescriptorSetLayoutHostMappingInfoVALVE - Stub description of
-- VkDescriptorSetLayoutHostMappingInfoVALVE
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping VK_VALVE_descriptor_set_host_mapping>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDescriptorSetLayoutHostMappingInfoVALVE'
data DescriptorSetLayoutHostMappingInfoVALVE = DescriptorSetLayoutHostMappingInfoVALVE
  { -- No documentation found for Nested "VkDescriptorSetLayoutHostMappingInfoVALVE" "descriptorOffset"
    descriptorOffset :: Word64
  , -- No documentation found for Nested "VkDescriptorSetLayoutHostMappingInfoVALVE" "descriptorSize"
    descriptorSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutHostMappingInfoVALVE)
#endif
deriving instance Show DescriptorSetLayoutHostMappingInfoVALVE

instance ToCStruct DescriptorSetLayoutHostMappingInfoVALVE where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutHostMappingInfoVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (descriptorOffset))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (descriptorSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorSetLayoutHostMappingInfoVALVE where
  peekCStruct p = do
    descriptorOffset <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    descriptorSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DescriptorSetLayoutHostMappingInfoVALVE
             (coerce @CSize @Word64 descriptorOffset) descriptorSize

instance Storable DescriptorSetLayoutHostMappingInfoVALVE where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorSetLayoutHostMappingInfoVALVE where
  zero = DescriptorSetLayoutHostMappingInfoVALVE
           zero
           zero


type VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION"
pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION = 1


type VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME = "VK_VALVE_descriptor_set_host_mapping"

-- No documentation found for TopLevel "VK_VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME"
pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME = "VK_VALVE_descriptor_set_host_mapping"

