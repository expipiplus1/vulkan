{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_matrix2 - device extension
--
-- = VK_NV_cooperative_matrix2
--
-- [__Name String__]
--     @VK_NV_cooperative_matrix2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     594
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix2.html SPV_NV_cooperative_matrix2>
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_tensor_addressing.html SPV_NV_tensor_addressing>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_matrix2] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_matrix2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_cooperative_matrix2.adoc VK_NV_cooperative_matrix2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-08-01
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_matrix2.txt GLSL_NV_cooperative_matrix2>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Karthik Vaidyanathan, NVIDIA
--
-- == Description
--
-- This extension adds several new features building on the cooperative
-- matrix types added in VK_KHR_cooperative_matrix. The goal is to add and
-- accelerate features beyond just simple GEMM kernels, including adding
-- support for type\/use conversions, reductions, per-element operations,
-- and tensor addressing, and also to improve usability and out-of-the-box
-- performance by adding support for more flexible matrix sizes, and
-- workgroup scope matrices with compiler-managed staging through shared
-- memory.
--
-- The new functionality is defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_tensor_addressing.html SPV_NV_tensor_addressing>
-- and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix2.html SPV_NV_cooperative_matrix2>
-- SPIR-V extensions and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_matrix2.txt GLSL_NV_cooperative_matrix2>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation, and which specific
-- features are supported.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV'
--
-- == New Structures
--
-- -   'CooperativeMatrixFlexibleDimensionsPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrix2FeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrix2PropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_MATRIX_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-TensorAddressingNV TensorAddressingNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixReductionsNV CooperativeMatrixReductionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixConversionsNV CooperativeMatrixConversionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixPerElementOperationsNV CooperativeMatrixPerElementOperationsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixTensorAddressingNV CooperativeMatrixTensorAddressingNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixBlockLoadsNV CooperativeMatrixBlockLoadsNV>
--
-- == Version History
--
-- -   Revision 1, 2024-08-01 (Jeff Bolz)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_cooperative_matrix2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_matrix2  ( getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV
                                                    , PhysicalDeviceCooperativeMatrix2FeaturesNV(..)
                                                    , PhysicalDeviceCooperativeMatrix2PropertiesNV(..)
                                                    , CooperativeMatrixFlexibleDimensionsPropertiesNV(..)
                                                    , NV_COOPERATIVE_MATRIX_2_SPEC_VERSION
                                                    , pattern NV_COOPERATIVE_MATRIX_2_SPEC_VERSION
                                                    , NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME
                                                    , pattern NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME
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
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ComponentTypeKHR(..))
import Vulkan.Extensions.VK_KHR_cooperative_matrix (ScopeKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixFlexibleDimensionsPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixFlexibleDimensionsPropertiesNV -> IO Result

-- | vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV -
-- Returns properties describing what cooperative matrix types are
-- supported
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of flexible dimensions
-- properties available is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the application to
-- the number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pPropertyCount@ is less than the number flexible
-- dimensions properties available, at most @pPropertyCount@ structures
-- will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available flexible dimensions properties were returned.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-cooperativeMatrixFlexibleDimensions cooperativeMatrixFlexibleDimensions>
-- feature is not supported, the implementation /must/ advertise zero
-- properties.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'CooperativeMatrixFlexibleDimensionsPropertiesNV' structures
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix2 VK_NV_cooperative_matrix2>,
-- 'CooperativeMatrixFlexibleDimensionsPropertiesNV',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV :: forall io
                                                                  . (MonadIO io)
                                                                 => -- | @physicalDevice@ is the physical device.
                                                                    PhysicalDevice
                                                                 -> io (Result, ("properties" ::: Vector CooperativeMatrixFlexibleDimensionsPropertiesNV))
getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNVPtr = pVkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV' = mkVkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV" (vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV'
                                                                                                       physicalDevice'
                                                                                                       (pPPropertyCount)
                                                                                                       (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @CooperativeMatrixFlexibleDimensionsPropertiesNV ((fromIntegral (pPropertyCount)) * 56)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 56) :: Ptr CooperativeMatrixFlexibleDimensionsPropertiesNV) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV" (vkGetPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV'
                                                                                                        physicalDevice'
                                                                                                        (pPPropertyCount)
                                                                                                        ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @CooperativeMatrixFlexibleDimensionsPropertiesNV (((pPProperties) `advancePtrBytes` (56 * (i)) :: Ptr CooperativeMatrixFlexibleDimensionsPropertiesNV)))
  pure $ ((r'), pProperties')


-- | VkPhysicalDeviceCooperativeMatrix2FeaturesNV - Structure describing
-- cooperative matrix features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrix2FeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCooperativeMatrix2FeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix2 VK_NV_cooperative_matrix2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrix2FeaturesNV = PhysicalDeviceCooperativeMatrix2FeaturesNV
  { -- | #features-cooperativeMatrixWorkgroupScope#
    -- @cooperativeMatrixWorkgroupScope@ indicates that the implementation
    -- supports workgroup scope cooperative matrices.
    cooperativeMatrixWorkgroupScope :: Bool
  , -- | #features-cooperativeMatrixFlexibleDimensions#
    -- @cooperativeMatrixFlexibleDimensions@ indicates that the implementation
    -- supports cooperative matrix sizes that are a multiple of the granularity
    -- advertised in 'CooperativeMatrixFlexibleDimensionsPropertiesNV'.
    cooperativeMatrixFlexibleDimensions :: Bool
  , -- | #features-cooperativeMatrixReductions# @cooperativeMatrixReductions@
    -- indicates that the implementation supports the
    -- @CooperativeMatrixReductionsNV@ SPIR-V capability. This allows
    -- performing (row, column, 2x2, or all element) reductions on matrices.
    cooperativeMatrixReductions :: Bool
  , -- | #features-cooperativeMatrixConversions# @cooperativeMatrixConversions@
    -- indicates that the implementation supports the
    -- @CooperativeMatrixConversionsNV@ SPIR-V capability. This allows
    -- converting accumulator matrices to A or B matrices.
    cooperativeMatrixConversions :: Bool
  , -- | #features-cooperativeMatrixPerElementOperations#
    -- @cooperativeMatrixPerElementOperations@ indicates that the
    -- implementation supports the @CooperativeMatrixPerElementOperationsNV@
    -- SPIR-V capability. This allows performing element-wise operations on
    -- matrix elements using a callback function.
    cooperativeMatrixPerElementOperations :: Bool
  , -- | #features-cooperativeMatrixTensorAddressing#
    -- @cooperativeMatrixTensorAddressing@ indicates that the implementation
    -- supports the @TensorAddressingNV@ and
    -- @CooperativeMatrixTensorAddressingNV@ SPIR-V capabilities. This allows
    -- using tensor layout and tensor view types for matrix loads and stores.
    cooperativeMatrixTensorAddressing :: Bool
  , -- | #features-cooperativeMatrixBlockLoads# @cooperativeMatrixBlockLoads@
    -- indicates that the implementation supports the
    -- @CooperativeMatrixBlockLoadsNV@ SPIR-V capability. This allows setting
    -- block size for loads and using a callback function to decode block
    -- elements.
    cooperativeMatrixBlockLoads :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrix2FeaturesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrix2FeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrix2FeaturesNV where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrix2FeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixWorkgroupScope))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixFlexibleDimensions))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixReductions))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixConversions))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixPerElementOperations))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixTensorAddressing))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixBlockLoads))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeMatrix2FeaturesNV where
  peekCStruct p = do
    cooperativeMatrixWorkgroupScope <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    cooperativeMatrixFlexibleDimensions <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    cooperativeMatrixReductions <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    cooperativeMatrixConversions <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    cooperativeMatrixPerElementOperations <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    cooperativeMatrixTensorAddressing <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    cooperativeMatrixBlockLoads <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeMatrix2FeaturesNV
             (bool32ToBool cooperativeMatrixWorkgroupScope)
             (bool32ToBool cooperativeMatrixFlexibleDimensions)
             (bool32ToBool cooperativeMatrixReductions)
             (bool32ToBool cooperativeMatrixConversions)
             (bool32ToBool cooperativeMatrixPerElementOperations)
             (bool32ToBool cooperativeMatrixTensorAddressing)
             (bool32ToBool cooperativeMatrixBlockLoads)

instance Storable PhysicalDeviceCooperativeMatrix2FeaturesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrix2FeaturesNV where
  zero = PhysicalDeviceCooperativeMatrix2FeaturesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceCooperativeMatrix2PropertiesNV - Structure describing
-- cooperative matrix properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrix2PropertiesNV' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix2 VK_NV_cooperative_matrix2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrix2PropertiesNV = PhysicalDeviceCooperativeMatrix2PropertiesNV
  { -- | #limits-cooperativeMatrixWorkgroupScopeMaxWorkgroupSize#
    -- @cooperativeMatrixWorkgroupScopeMaxWorkgroupSize@ is the maximum number
    -- of invocations in a workgroup when the module uses
    -- @OpTypeCooperativeMatrixKHR@ with @Scope@ equal to @Workgroup@.
    cooperativeMatrixWorkgroupScopeMaxWorkgroupSize :: Word32
  , -- | #limits-cooperativeMatrixFlexibleDimensionsMaxDimension#
    -- @cooperativeMatrixFlexibleDimensionsMaxDimension@ is the maximum
    -- supported dimension for cooperative matrix types when the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-cooperativeMatrixFlexibleDimensions cooperativeMatrixFlexibleDimensions>
    -- feature is enabled.
    cooperativeMatrixFlexibleDimensionsMaxDimension :: Word32
  , -- | #limits-cooperativeMatrixWorkgroupScopeReservedSharedMemory#
    -- @cooperativeMatrixWorkgroupScopeReservedSharedMemory@ is the number of
    -- bytes of shared memory reserved for the implementation when the module
    -- uses @OpTypeCooperativeMatrixKHR@ with @Scope@ equal to @Workgroup@.
    cooperativeMatrixWorkgroupScopeReservedSharedMemory :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrix2PropertiesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrix2PropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrix2PropertiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrix2PropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (cooperativeMatrixWorkgroupScopeMaxWorkgroupSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (cooperativeMatrixFlexibleDimensionsMaxDimension)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (cooperativeMatrixWorkgroupScopeReservedSharedMemory)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceCooperativeMatrix2PropertiesNV where
  peekCStruct p = do
    cooperativeMatrixWorkgroupScopeMaxWorkgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    cooperativeMatrixFlexibleDimensionsMaxDimension <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    cooperativeMatrixWorkgroupScopeReservedSharedMemory <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PhysicalDeviceCooperativeMatrix2PropertiesNV
             cooperativeMatrixWorkgroupScopeMaxWorkgroupSize
             cooperativeMatrixFlexibleDimensionsMaxDimension
             cooperativeMatrixWorkgroupScopeReservedSharedMemory

instance Storable PhysicalDeviceCooperativeMatrix2PropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrix2PropertiesNV where
  zero = PhysicalDeviceCooperativeMatrix2PropertiesNV
           zero
           zero
           zero


-- | VkCooperativeMatrixFlexibleDimensionsPropertiesNV - Structure specifying
-- cooperative matrix properties
--
-- = Description
--
-- Rather than explicitly enumerating a list of supported sizes,
-- 'CooperativeMatrixFlexibleDimensionsPropertiesNV' advertises size
-- granularities, where the matrix /must/ be a multiple of the advertised
-- size. The M and K granularities apply to rows and columns of matrices
-- with @Use@ of @MatrixA@, K, and N apply to rows and columns of matrices
-- with @Use@ of @MatrixB@, M, and N apply to rows and columns of matrices
-- with @Use@ of @MatrixAccumulator@.
--
-- For a given type combination, if multiple workgroup sizes are supported
-- there /may/ be multiple
-- 'CooperativeMatrixFlexibleDimensionsPropertiesNV' structures with
-- different granularities.
--
-- All granularity values /must/ be powers of two.
--
-- Different A\/B types may require different granularities but share the
-- same accumulator type. In such a case, the supported granularity for a
-- matrix with the accumulator type would be the smallest advertised
-- granularity.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix2 VK_NV_cooperative_matrix2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR',
-- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ScopeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV'
data CooperativeMatrixFlexibleDimensionsPropertiesNV = CooperativeMatrixFlexibleDimensionsPropertiesNV
  { -- | @MGranularity@ is the granularity of the number of rows in matrices @A@,
    -- @C@, and 'Vulkan.Core10.Enums.Result.Result'. The rows /must/ be an
    -- integer multiple of this value.
    mGranularity :: Word32
  , -- | @NGranularity@ is the granularity of columns in matrices @B@, @C@,
    -- 'Vulkan.Core10.Enums.Result.Result'. The columns /must/ be an integer
    -- multiple of this value.
    nGranularity :: Word32
  , -- | @KGranularity@ is the granularity of columns in matrix @A@ and rows in
    -- matrix @B@. The columns\/rows /must/ be an integer multiple of this
    -- value.
    kGranularity :: Word32
  , -- | @AType@ is the component type of matrix @A@, of type
    -- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR'.
    aType :: ComponentTypeKHR
  , -- | @BType@ is the component type of matrix @B@, of type
    -- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR'.
    bType :: ComponentTypeKHR
  , -- | @CType@ is the component type of matrix @C@, of type
    -- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR'.
    cType :: ComponentTypeKHR
  , -- | @ResultType@ is the component type of matrix
    -- 'Vulkan.Core10.Enums.Result.Result', of type
    -- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR'.
    resultType :: ComponentTypeKHR
  , -- | @saturatingAccumulation@ indicates whether the @SaturatingAccumulation@
    -- operand to @OpCooperativeMatrixMulAddKHR@ /must/ be present or not. If
    -- it is 'Vulkan.Core10.FundamentalTypes.TRUE', the
    -- @SaturatingAccumulation@ operand /must/ be present. If it is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the @SaturatingAccumulation@
    -- operand /must/ not be present.
    saturatingAccumulation :: Bool
  , -- | @scope@ is the scope of all the matrix types, of type
    -- 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ScopeKHR'.
    scope :: ScopeKHR
  , -- | @workgroupInvocations@ is the number of invocations in the local
    -- workgroup when this combination of values is supported.
    workgroupInvocations :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CooperativeMatrixFlexibleDimensionsPropertiesNV)
#endif
deriving instance Show CooperativeMatrixFlexibleDimensionsPropertiesNV

instance ToCStruct CooperativeMatrixFlexibleDimensionsPropertiesNV where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CooperativeMatrixFlexibleDimensionsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (mGranularity)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nGranularity)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (kGranularity)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeKHR)) (aType)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeKHR)) (bType)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeKHR)) (cType)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeKHR)) (resultType)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (saturatingAccumulation))
    poke ((p `plusPtr` 48 :: Ptr ScopeKHR)) (scope)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (workgroupInvocations)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeKHR)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr ScopeKHR)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    f

instance FromCStruct CooperativeMatrixFlexibleDimensionsPropertiesNV where
  peekCStruct p = do
    mGranularity <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nGranularity <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    kGranularity <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    aType <- peek @ComponentTypeKHR ((p `plusPtr` 28 :: Ptr ComponentTypeKHR))
    bType <- peek @ComponentTypeKHR ((p `plusPtr` 32 :: Ptr ComponentTypeKHR))
    cType <- peek @ComponentTypeKHR ((p `plusPtr` 36 :: Ptr ComponentTypeKHR))
    resultType <- peek @ComponentTypeKHR ((p `plusPtr` 40 :: Ptr ComponentTypeKHR))
    saturatingAccumulation <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    scope <- peek @ScopeKHR ((p `plusPtr` 48 :: Ptr ScopeKHR))
    workgroupInvocations <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pure $ CooperativeMatrixFlexibleDimensionsPropertiesNV
             mGranularity
             nGranularity
             kGranularity
             aType
             bType
             cType
             resultType
             (bool32ToBool saturatingAccumulation)
             scope
             workgroupInvocations

instance Storable CooperativeMatrixFlexibleDimensionsPropertiesNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CooperativeMatrixFlexibleDimensionsPropertiesNV where
  zero = CooperativeMatrixFlexibleDimensionsPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type NV_COOPERATIVE_MATRIX_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_2_SPEC_VERSION"
pattern NV_COOPERATIVE_MATRIX_2_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COOPERATIVE_MATRIX_2_SPEC_VERSION = 1


type NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME = "VK_NV_cooperative_matrix2"

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME"
pattern NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME = "VK_NV_cooperative_matrix2"

