{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_matrix - device extension
--
-- == VK_NV_cooperative_matrix
--
-- [__Name String__]
--     @VK_NV_cooperative_matrix@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     250
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix.html SPV_NV_cooperative_matrix>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_matrix] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_matrix extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-02-05
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_cooperative_matrix.txt GL_NV_cooperative_matrix>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Markus Tavenrath, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for using cooperative matrix types in
-- SPIR-V. Cooperative matrix types are medium-sized matrices that are
-- primarily supported in compute shaders, where the storage for the matrix
-- is spread across all invocations in some scope (usually a subgroup) and
-- those invocations cooperate to efficiently perform matrix multiplies.
--
-- Cooperative matrix types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix.html SPV_NV_cooperative_matrix>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_cooperative_matrix.txt GL_NV_cooperative_matrix>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == New Structures
--
-- -   'CooperativeMatrixPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == New Enums
--
-- -   'ComponentTypeNV'
--
-- -   'ScopeNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_MATRIX_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_MATRIX_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR':
--
--     -   'COMPONENT_TYPE_FLOAT16_NV'
--
--     -   'COMPONENT_TYPE_FLOAT32_NV'
--
--     -   'COMPONENT_TYPE_FLOAT64_NV'
--
--     -   'COMPONENT_TYPE_SINT16_NV'
--
--     -   'COMPONENT_TYPE_SINT32_NV'
--
--     -   'COMPONENT_TYPE_SINT64_NV'
--
--     -   'COMPONENT_TYPE_SINT8_NV'
--
--     -   'COMPONENT_TYPE_UINT16_NV'
--
--     -   'COMPONENT_TYPE_UINT32_NV'
--
--     -   'COMPONENT_TYPE_UINT64_NV'
--
--     -   'COMPONENT_TYPE_UINT8_NV'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ScopeKHR':
--
--     -   'SCOPE_DEVICE_NV'
--
--     -   'SCOPE_QUEUE_FAMILY_NV'
--
--     -   'SCOPE_SUBGROUP_NV'
--
--     -   'SCOPE_WORKGROUP_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixNV CooperativeMatrixNV>
--
-- == Issues
--
-- (1) What matrix properties will be supported in practice?
--
-- __RESOLVED__: In NVIDIA’s initial implementation, we will support:
--
-- -   AType = BType = fp16 CType = DType = fp16 MxNxK = 16x8x16 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp16 MxNxK = 16x8x8 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp32 MxNxK = 16x8x16 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp32 MxNxK = 16x8x8 scope =
--     Subgroup
--
-- == Version History
--
-- -   Revision 1, 2019-02-05 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ComponentTypeNV', 'CooperativeMatrixPropertiesNV',
-- 'PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'PhysicalDeviceCooperativeMatrixPropertiesNV', 'ScopeNV',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_cooperative_matrix Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_matrix  ( getPhysicalDeviceCooperativeMatrixPropertiesNV
                                                   , pattern SCOPE_DEVICE_NV
                                                   , pattern SCOPE_WORKGROUP_NV
                                                   , pattern SCOPE_SUBGROUP_NV
                                                   , pattern SCOPE_QUEUE_FAMILY_NV
                                                   , pattern COMPONENT_TYPE_FLOAT16_NV
                                                   , pattern COMPONENT_TYPE_FLOAT32_NV
                                                   , pattern COMPONENT_TYPE_FLOAT64_NV
                                                   , pattern COMPONENT_TYPE_SINT8_NV
                                                   , pattern COMPONENT_TYPE_SINT16_NV
                                                   , pattern COMPONENT_TYPE_SINT32_NV
                                                   , pattern COMPONENT_TYPE_SINT64_NV
                                                   , pattern COMPONENT_TYPE_UINT8_NV
                                                   , pattern COMPONENT_TYPE_UINT16_NV
                                                   , pattern COMPONENT_TYPE_UINT32_NV
                                                   , pattern COMPONENT_TYPE_UINT64_NV
                                                   , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
                                                   , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
                                                   , CooperativeMatrixPropertiesNV(..)
                                                   , ScopeNV
                                                   , ComponentTypeNV
                                                   , NV_COOPERATIVE_MATRIX_SPEC_VERSION
                                                   , pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION
                                                   , NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                   , pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                   , ScopeKHR(..)
                                                   , ComponentTypeKHR(..)
                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_FLOAT16_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_FLOAT32_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_FLOAT64_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_SINT16_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_SINT32_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_SINT64_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_SINT8_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_UINT16_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_UINT32_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_UINT64_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(COMPONENT_TYPE_UINT8_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(SCOPE_DEVICE_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(SCOPE_QUEUE_FAMILY_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(SCOPE_SUBGROUP_KHR))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(SCOPE_WORKGROUP_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(..))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceCooperativeMatrixPropertiesNV - Returns properties
-- describing what cooperative matrix types are supported
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of cooperative matrix
-- properties available is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pPropertyCount@ is less than the number of
-- cooperative matrix properties available, at most @pPropertyCount@
-- structures will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available cooperative matrix properties were
-- returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesNV-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesNV-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesNV-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'CooperativeMatrixPropertiesNV'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix VK_NV_cooperative_matrix>,
-- 'CooperativeMatrixPropertiesNV', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceCooperativeMatrixPropertiesNV :: forall io
                                                . (MonadIO io)
                                               => -- | @physicalDevice@ is the physical device.
                                                  PhysicalDevice
                                               -> io (Result, ("properties" ::: Vector CooperativeMatrixPropertiesNV))
getPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr = pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCooperativeMatrixPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesNV' = mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV" (vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'
                                                                                     physicalDevice'
                                                                                     (pPPropertyCount)
                                                                                     (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @CooperativeMatrixPropertiesNV ((fromIntegral (pPropertyCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 48) :: Ptr CooperativeMatrixPropertiesNV) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV" (vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'
                                                                                      physicalDevice'
                                                                                      (pPPropertyCount)
                                                                                      ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @CooperativeMatrixPropertiesNV (((pPProperties) `advancePtrBytes` (48 * (i)) :: Ptr CooperativeMatrixPropertiesNV)))
  pure $ ((r'), pProperties')


-- No documentation found for TopLevel "VK_SCOPE_DEVICE_NV"
pattern SCOPE_DEVICE_NV = SCOPE_DEVICE_KHR


-- No documentation found for TopLevel "VK_SCOPE_WORKGROUP_NV"
pattern SCOPE_WORKGROUP_NV = SCOPE_WORKGROUP_KHR


-- No documentation found for TopLevel "VK_SCOPE_SUBGROUP_NV"
pattern SCOPE_SUBGROUP_NV = SCOPE_SUBGROUP_KHR


-- No documentation found for TopLevel "VK_SCOPE_QUEUE_FAMILY_NV"
pattern SCOPE_QUEUE_FAMILY_NV = SCOPE_QUEUE_FAMILY_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_FLOAT16_NV"
pattern COMPONENT_TYPE_FLOAT16_NV = COMPONENT_TYPE_FLOAT16_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_FLOAT32_NV"
pattern COMPONENT_TYPE_FLOAT32_NV = COMPONENT_TYPE_FLOAT32_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_FLOAT64_NV"
pattern COMPONENT_TYPE_FLOAT64_NV = COMPONENT_TYPE_FLOAT64_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_SINT8_NV"
pattern COMPONENT_TYPE_SINT8_NV = COMPONENT_TYPE_SINT8_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_SINT16_NV"
pattern COMPONENT_TYPE_SINT16_NV = COMPONENT_TYPE_SINT16_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_SINT32_NV"
pattern COMPONENT_TYPE_SINT32_NV = COMPONENT_TYPE_SINT32_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_SINT64_NV"
pattern COMPONENT_TYPE_SINT64_NV = COMPONENT_TYPE_SINT64_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_UINT8_NV"
pattern COMPONENT_TYPE_UINT8_NV = COMPONENT_TYPE_UINT8_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_UINT16_NV"
pattern COMPONENT_TYPE_UINT16_NV = COMPONENT_TYPE_UINT16_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_UINT32_NV"
pattern COMPONENT_TYPE_UINT32_NV = COMPONENT_TYPE_UINT32_KHR


-- No documentation found for TopLevel "VK_COMPONENT_TYPE_UINT64_NV"
pattern COMPONENT_TYPE_UINT64_NV = COMPONENT_TYPE_UINT64_KHR


-- | VkPhysicalDeviceCooperativeMatrixFeaturesNV - Structure describing
-- cooperative matrix features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrixFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCooperativeMatrixFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix VK_NV_cooperative_matrix>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrixFeaturesNV = PhysicalDeviceCooperativeMatrixFeaturesNV
  { -- | #features-cooperativeMatrix-NV# @cooperativeMatrix@ indicates that the
    -- implementation supports the @CooperativeMatrixNV@ SPIR-V capability.
    cooperativeMatrix :: Bool
  , -- | #features-cooperativeMatrixRobustBufferAccess-NV#
    -- @cooperativeMatrixRobustBufferAccess@ indicates that the implementation
    -- supports robust buffer access for SPIR-V @OpCooperativeMatrixLoadNV@ and
    -- @OpCooperativeMatrixStoreNV@ instructions.
    cooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrix))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixRobustBufferAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesNV where
  peekCStruct p = do
    cooperativeMatrix <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    cooperativeMatrixRobustBufferAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeMatrixFeaturesNV
             (bool32ToBool cooperativeMatrix)
             (bool32ToBool cooperativeMatrixRobustBufferAccess)

instance Storable PhysicalDeviceCooperativeMatrixFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixFeaturesNV where
  zero = PhysicalDeviceCooperativeMatrixFeaturesNV
           zero
           zero


-- | VkPhysicalDeviceCooperativeMatrixPropertiesNV - Structure describing
-- cooperative matrix properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrixPropertiesNV' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix VK_NV_cooperative_matrix>,
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrixPropertiesNV = PhysicalDeviceCooperativeMatrixPropertiesNV
  { -- | #limits-cooperativeMatrixSupportedStages-NV#
    -- @cooperativeMatrixSupportedStages@ is a bitfield of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' describing
    -- the shader stages that cooperative matrix instructions are supported in.
    -- @cooperativeMatrixSupportedStages@ will have the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' bit
    -- set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    cooperativeMatrixSupportedStages :: ShaderStageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixPropertiesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (cooperativeMatrixSupportedStages)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesNV where
  peekCStruct p = do
    cooperativeMatrixSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceCooperativeMatrixPropertiesNV
             cooperativeMatrixSupportedStages

instance Storable PhysicalDeviceCooperativeMatrixPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixPropertiesNV where
  zero = PhysicalDeviceCooperativeMatrixPropertiesNV
           zero


-- | VkCooperativeMatrixPropertiesNV - Structure specifying cooperative
-- matrix properties
--
-- = Description
--
-- If some types are preferred over other types (e.g. for performance),
-- they /should/ appear earlier in the list enumerated by
-- 'getPhysicalDeviceCooperativeMatrixPropertiesNV'.
--
-- At least one entry in the list /must/ have power of two values for all
-- of @MSize@, @KSize@, and @NSize@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix VK_NV_cooperative_matrix>,
-- 'ComponentTypeNV', 'ScopeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesNV'
data CooperativeMatrixPropertiesNV = CooperativeMatrixPropertiesNV
  { -- | @MSize@ is the number of rows in matrices A, C, and D.
    mSize :: Word32
  , -- | @NSize@ is the number of columns in matrices B, C, D.
    nSize :: Word32
  , -- | @KSize@ is the number of columns in matrix A and rows in matrix B.
    kSize :: Word32
  , -- | @AType@ is the component type of matrix A, of type 'ComponentTypeNV'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesNV-AType-parameter# @AType@ /must/ be
    -- a valid 'ComponentTypeNV' value
    aType :: ComponentTypeNV
  , -- | @BType@ is the component type of matrix B, of type 'ComponentTypeNV'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesNV-BType-parameter# @BType@ /must/ be
    -- a valid 'ComponentTypeNV' value
    bType :: ComponentTypeNV
  , -- | @CType@ is the component type of matrix C, of type 'ComponentTypeNV'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesNV-CType-parameter# @CType@ /must/ be
    -- a valid 'ComponentTypeNV' value
    cType :: ComponentTypeNV
  , -- | @DType@ is the component type of matrix D, of type 'ComponentTypeNV'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesNV-DType-parameter# @DType@ /must/ be
    -- a valid 'ComponentTypeNV' value
    dType :: ComponentTypeNV
  , -- | @scope@ is the scope of all the matrix types, of type 'ScopeNV'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesNV-scope-parameter# @scope@ /must/ be
    -- a valid 'ScopeNV' value
    scope :: ScopeNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CooperativeMatrixPropertiesNV)
#endif
deriving instance Show CooperativeMatrixPropertiesNV

instance ToCStruct CooperativeMatrixPropertiesNV where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CooperativeMatrixPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (mSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (kSize)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeNV)) (aType)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeNV)) (bType)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeNV)) (cType)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeNV)) (dType)
    poke ((p `plusPtr` 44 :: Ptr ScopeNV)) (scope)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ScopeNV)) (zero)
    f

instance FromCStruct CooperativeMatrixPropertiesNV where
  peekCStruct p = do
    mSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    kSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    aType <- peek @ComponentTypeNV ((p `plusPtr` 28 :: Ptr ComponentTypeNV))
    bType <- peek @ComponentTypeNV ((p `plusPtr` 32 :: Ptr ComponentTypeNV))
    cType <- peek @ComponentTypeNV ((p `plusPtr` 36 :: Ptr ComponentTypeNV))
    dType <- peek @ComponentTypeNV ((p `plusPtr` 40 :: Ptr ComponentTypeNV))
    scope <- peek @ScopeNV ((p `plusPtr` 44 :: Ptr ScopeNV))
    pure $ CooperativeMatrixPropertiesNV
             mSize nSize kSize aType bType cType dType scope

instance Storable CooperativeMatrixPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CooperativeMatrixPropertiesNV where
  zero = CooperativeMatrixPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkScopeNV"
type ScopeNV = ScopeKHR


-- No documentation found for TopLevel "VkComponentTypeNV"
type ComponentTypeNV = ComponentTypeKHR


type NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1


type NV_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_NV_cooperative_matrix"

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_NV_cooperative_matrix"

