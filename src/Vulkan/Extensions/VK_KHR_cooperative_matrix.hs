{-# language CPP #-}
-- | = Name
--
-- VK_KHR_cooperative_matrix - device extension
--
-- == VK_KHR_cooperative_matrix
--
-- [__Name String__]
--     @VK_KHR_cooperative_matrix@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     507
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_cooperative_matrix.html SPV_KHR_cooperative_matrix>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_cooperative_matrix] @kpet%0A*Here describe the issue or question you have about the VK_KHR_cooperative_matrix extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-03
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GLSL_KHR_cooperative_matrix.txt GLSL_KHR_cooperative_matrix>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Markus Tavenrath, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Boris Zanin, AMD
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
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_cooperative_matrix.html SPV_KHR_cooperative_matrix>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GLSL_KHR_cooperative_matrix.txt GLSL_KHR_cooperative_matrix>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == New Structures
--
-- -   'CooperativeMatrixPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == New Enums
--
-- -   'ComponentTypeKHR'
--
-- -   'ScopeKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COOPERATIVE_MATRIX_EXTENSION_NAME'
--
-- -   'KHR_COOPERATIVE_MATRIX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixKHR CooperativeMatrixKHR>
--
-- == Version History
--
-- -   Revision 2, 2023-05-03 (Kevin Petit)
--
--     -   First KHR revision
--
-- -   Revision 1, 2019-02-05 (Jeff Bolz)
--
--     -   NVIDIA vendor extension
--
-- == See Also
--
-- 'ComponentTypeKHR', 'CooperativeMatrixPropertiesKHR',
-- 'PhysicalDeviceCooperativeMatrixFeaturesKHR',
-- 'PhysicalDeviceCooperativeMatrixPropertiesKHR', 'ScopeKHR',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_cooperative_matrix Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_cooperative_matrix  ( getPhysicalDeviceCooperativeMatrixPropertiesKHR
                                                    , PhysicalDeviceCooperativeMatrixFeaturesKHR(..)
                                                    , CooperativeMatrixPropertiesKHR(..)
                                                    , PhysicalDeviceCooperativeMatrixPropertiesKHR(..)
                                                    , ScopeKHR( SCOPE_DEVICE_KHR
                                                              , SCOPE_WORKGROUP_KHR
                                                              , SCOPE_SUBGROUP_KHR
                                                              , SCOPE_QUEUE_FAMILY_KHR
                                                              , ..
                                                              )
                                                    , ComponentTypeKHR( COMPONENT_TYPE_FLOAT16_KHR
                                                                      , COMPONENT_TYPE_FLOAT32_KHR
                                                                      , COMPONENT_TYPE_FLOAT64_KHR
                                                                      , COMPONENT_TYPE_SINT8_KHR
                                                                      , COMPONENT_TYPE_SINT16_KHR
                                                                      , COMPONENT_TYPE_SINT32_KHR
                                                                      , COMPONENT_TYPE_SINT64_KHR
                                                                      , COMPONENT_TYPE_UINT8_KHR
                                                                      , COMPONENT_TYPE_UINT16_KHR
                                                                      , COMPONENT_TYPE_UINT32_KHR
                                                                      , COMPONENT_TYPE_UINT64_KHR
                                                                      , ..
                                                                      )
                                                    , KHR_COOPERATIVE_MATRIX_SPEC_VERSION
                                                    , pattern KHR_COOPERATIVE_MATRIX_SPEC_VERSION
                                                    , KHR_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                    , pattern KHR_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCooperativeMatrixPropertiesKHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeMatrixPropertiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesKHR -> IO Result

-- | vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR - Returns properties
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
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'CooperativeMatrixPropertiesKHR'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'CooperativeMatrixPropertiesKHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceCooperativeMatrixPropertiesKHR :: forall io
                                                 . (MonadIO io)
                                                => -- | @physicalDevice@ is the physical device.
                                                   PhysicalDevice
                                                -> io (Result, ("properties" ::: Vector CooperativeMatrixPropertiesKHR))
getPhysicalDeviceCooperativeMatrixPropertiesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesKHRPtr = pVkGetPhysicalDeviceCooperativeMatrixPropertiesKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCooperativeMatrixPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR' = mkVkGetPhysicalDeviceCooperativeMatrixPropertiesKHR vkGetPhysicalDeviceCooperativeMatrixPropertiesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR" (vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR'
                                                                                      physicalDevice'
                                                                                      (pPPropertyCount)
                                                                                      (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @CooperativeMatrixPropertiesKHR ((fromIntegral (pPropertyCount)) * 56)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 56) :: Ptr CooperativeMatrixPropertiesKHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR" (vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR'
                                                                                       physicalDevice'
                                                                                       (pPPropertyCount)
                                                                                       ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @CooperativeMatrixPropertiesKHR (((pPProperties) `advancePtrBytes` (56 * (i)) :: Ptr CooperativeMatrixPropertiesKHR)))
  pure $ ((r'), pProperties')


-- | VkPhysicalDeviceCooperativeMatrixFeaturesKHR - Structure describing
-- cooperative matrix features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCooperativeMatrixFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCooperativeMatrixFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrixFeaturesKHR = PhysicalDeviceCooperativeMatrixFeaturesKHR
  { -- | #features-cooperativeMatrix# @cooperativeMatrix@ indicates that the
    -- implementation supports the @CooperativeMatrixKHR@ SPIR-V capability.
    cooperativeMatrix :: Bool
  , -- | #features-cooperativeMatrixRobustBufferAccess#
    -- @cooperativeMatrixRobustBufferAccess@ indicates that the implementation
    -- supports robust buffer access for SPIR-V @OpCooperativeMatrixLoadKHR@
    -- and @OpCooperativeMatrixStoreKHR@ instructions.
    cooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixFeaturesKHR

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrix))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixRobustBufferAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesKHR where
  peekCStruct p = do
    cooperativeMatrix <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    cooperativeMatrixRobustBufferAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeMatrixFeaturesKHR
             (bool32ToBool cooperativeMatrix)
             (bool32ToBool cooperativeMatrixRobustBufferAccess)

instance Storable PhysicalDeviceCooperativeMatrixFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixFeaturesKHR where
  zero = PhysicalDeviceCooperativeMatrixFeaturesKHR
           zero
           zero


-- | VkCooperativeMatrixPropertiesKHR - Structure specifying cooperative
-- matrix properties
--
-- = Description
--
-- If some types are preferred over other types (e.g. for performance),
-- they /should/ appear earlier in the list enumerated by
-- 'getPhysicalDeviceCooperativeMatrixPropertiesKHR'.
--
-- At least one entry in the list /must/ have power of two values for all
-- of @MSize@, @KSize@, and @NSize@.
--
-- @scope@ /must/ be 'SCOPE_SUBGROUP_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'ComponentTypeKHR', 'ScopeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesKHR'
data CooperativeMatrixPropertiesKHR = CooperativeMatrixPropertiesKHR
  { -- | @MSize@ is the number of rows in matrices @A@, @C@, and
    -- 'Vulkan.Core10.Enums.Result.Result'.
    mSize :: Word32
  , -- | @NSize@ is the number of columns in matrices @B@, @C@,
    -- 'Vulkan.Core10.Enums.Result.Result'.
    nSize :: Word32
  , -- | @KSize@ is the number of columns in matrix @A@ and rows in matrix @B@.
    kSize :: Word32
  , -- | @AType@ is the component type of matrix @A@, of type 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesKHR-AType-parameter# @AType@ /must/
    -- be a valid 'ComponentTypeKHR' value
    aType :: ComponentTypeKHR
  , -- | @BType@ is the component type of matrix @B@, of type 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesKHR-BType-parameter# @BType@ /must/
    -- be a valid 'ComponentTypeKHR' value
    bType :: ComponentTypeKHR
  , -- | @CType@ is the component type of matrix @C@, of type 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesKHR-CType-parameter# @CType@ /must/
    -- be a valid 'ComponentTypeKHR' value
    cType :: ComponentTypeKHR
  , -- | @ResultType@ is the component type of matrix
    -- 'Vulkan.Core10.Enums.Result.Result', of type 'ComponentTypeKHR'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesKHR-ResultType-parameter#
    -- @ResultType@ /must/ be a valid 'ComponentTypeKHR' value
    resultType :: ComponentTypeKHR
  , -- | @saturatingAccumulation@ indicates whether the @SaturatingAccumulation@
    -- operand to @OpCooperativeMatrixMulAddKHR@ /must/ be present.
    saturatingAccumulation :: Bool
  , -- | @scope@ is the scope of all the matrix types, of type 'ScopeKHR'.
    --
    -- #VUID-VkCooperativeMatrixPropertiesKHR-scope-parameter# @scope@ /must/
    -- be a valid 'ScopeKHR' value
    scope :: ScopeKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CooperativeMatrixPropertiesKHR)
#endif
deriving instance Show CooperativeMatrixPropertiesKHR

instance ToCStruct CooperativeMatrixPropertiesKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CooperativeMatrixPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (mSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (kSize)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeKHR)) (aType)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeKHR)) (bType)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeKHR)) (cType)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeKHR)) (resultType)
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (saturatingAccumulation))
    poke ((p `plusPtr` 48 :: Ptr ScopeKHR)) (scope)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR)
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
    f

instance FromCStruct CooperativeMatrixPropertiesKHR where
  peekCStruct p = do
    mSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    kSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    aType <- peek @ComponentTypeKHR ((p `plusPtr` 28 :: Ptr ComponentTypeKHR))
    bType <- peek @ComponentTypeKHR ((p `plusPtr` 32 :: Ptr ComponentTypeKHR))
    cType <- peek @ComponentTypeKHR ((p `plusPtr` 36 :: Ptr ComponentTypeKHR))
    resultType <- peek @ComponentTypeKHR ((p `plusPtr` 40 :: Ptr ComponentTypeKHR))
    saturatingAccumulation <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    scope <- peek @ScopeKHR ((p `plusPtr` 48 :: Ptr ScopeKHR))
    pure $ CooperativeMatrixPropertiesKHR
             mSize
             nSize
             kSize
             aType
             bType
             cType
             resultType
             (bool32ToBool saturatingAccumulation)
             scope

instance Storable CooperativeMatrixPropertiesKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CooperativeMatrixPropertiesKHR where
  zero = CooperativeMatrixPropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceCooperativeMatrixPropertiesKHR - Structure describing
-- cooperative matrix properties supported by an implementation
--
-- = Description
--
-- @cooperativeMatrixSupportedStages@ /must/ not have any bits other than
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' set.
--
-- If the 'PhysicalDeviceCooperativeMatrixPropertiesKHR' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCooperativeMatrixPropertiesKHR = PhysicalDeviceCooperativeMatrixPropertiesKHR
  { -- | #limits-cooperativeMatrixSupportedStages#
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
deriving instance Generic (PhysicalDeviceCooperativeMatrixPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixPropertiesKHR

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (cooperativeMatrixSupportedStages)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesKHR where
  peekCStruct p = do
    cooperativeMatrixSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceCooperativeMatrixPropertiesKHR
             cooperativeMatrixSupportedStages

instance Storable PhysicalDeviceCooperativeMatrixPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixPropertiesKHR where
  zero = PhysicalDeviceCooperativeMatrixPropertiesKHR
           zero


-- | VkScopeKHR - Specify SPIR-V scope
--
-- = Description
--
-- All enum values match the corresponding SPIR-V value.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'CooperativeMatrixPropertiesKHR'
newtype ScopeKHR = ScopeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'SCOPE_DEVICE_KHR' corresponds to SPIR-V 'Vulkan.Core10.Handles.Device'
-- scope.
pattern SCOPE_DEVICE_KHR = ScopeKHR 1

-- | 'SCOPE_WORKGROUP_KHR' corresponds to SPIR-V @Workgroup@ scope.
pattern SCOPE_WORKGROUP_KHR = ScopeKHR 2

-- | 'SCOPE_SUBGROUP_KHR' corresponds to SPIR-V @Subgroup@ scope.
pattern SCOPE_SUBGROUP_KHR = ScopeKHR 3

-- | 'SCOPE_QUEUE_FAMILY_KHR' corresponds to SPIR-V @QueueFamily@ scope.
pattern SCOPE_QUEUE_FAMILY_KHR = ScopeKHR 5

{-# COMPLETE
  SCOPE_DEVICE_KHR
  , SCOPE_WORKGROUP_KHR
  , SCOPE_SUBGROUP_KHR
  , SCOPE_QUEUE_FAMILY_KHR ::
    ScopeKHR
  #-}

conNameScopeKHR :: String
conNameScopeKHR = "ScopeKHR"

enumPrefixScopeKHR :: String
enumPrefixScopeKHR = "SCOPE_"

showTableScopeKHR :: [(ScopeKHR, String)]
showTableScopeKHR =
  [ (SCOPE_DEVICE_KHR, "DEVICE_KHR")
  , (SCOPE_WORKGROUP_KHR, "WORKGROUP_KHR")
  , (SCOPE_SUBGROUP_KHR, "SUBGROUP_KHR")
  , (SCOPE_QUEUE_FAMILY_KHR, "QUEUE_FAMILY_KHR")
  ]

instance Show ScopeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixScopeKHR
      showTableScopeKHR
      conNameScopeKHR
      (\(ScopeKHR x) -> x)
      (showsPrec 11)

instance Read ScopeKHR where
  readPrec =
    enumReadPrec
      enumPrefixScopeKHR
      showTableScopeKHR
      conNameScopeKHR
      ScopeKHR

-- | VkComponentTypeKHR - Specify SPIR-V cooperative matrix component type
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>,
-- 'CooperativeMatrixPropertiesKHR'
newtype ComponentTypeKHR = ComponentTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMPONENT_TYPE_FLOAT16_KHR' corresponds to SPIR-V @OpTypeFloat@ 16.
pattern COMPONENT_TYPE_FLOAT16_KHR = ComponentTypeKHR 0

-- | 'COMPONENT_TYPE_FLOAT32_KHR' corresponds to SPIR-V @OpTypeFloat@ 32.
pattern COMPONENT_TYPE_FLOAT32_KHR = ComponentTypeKHR 1

-- | 'COMPONENT_TYPE_FLOAT64_KHR' corresponds to SPIR-V @OpTypeFloat@ 64.
pattern COMPONENT_TYPE_FLOAT64_KHR = ComponentTypeKHR 2

-- | 'COMPONENT_TYPE_SINT8_KHR' corresponds to SPIR-V @OpTypeInt@ 8 1.
pattern COMPONENT_TYPE_SINT8_KHR = ComponentTypeKHR 3

-- | 'COMPONENT_TYPE_SINT16_KHR' corresponds to SPIR-V @OpTypeInt@ 16 1.
pattern COMPONENT_TYPE_SINT16_KHR = ComponentTypeKHR 4

-- | 'COMPONENT_TYPE_SINT32_KHR' corresponds to SPIR-V @OpTypeInt@ 32 1.
pattern COMPONENT_TYPE_SINT32_KHR = ComponentTypeKHR 5

-- | 'COMPONENT_TYPE_SINT64_KHR' corresponds to SPIR-V @OpTypeInt@ 64 1.
pattern COMPONENT_TYPE_SINT64_KHR = ComponentTypeKHR 6

-- | 'COMPONENT_TYPE_UINT8_KHR' corresponds to SPIR-V @OpTypeInt@ 8 0.
pattern COMPONENT_TYPE_UINT8_KHR = ComponentTypeKHR 7

-- | 'COMPONENT_TYPE_UINT16_KHR' corresponds to SPIR-V @OpTypeInt@ 16 0.
pattern COMPONENT_TYPE_UINT16_KHR = ComponentTypeKHR 8

-- | 'COMPONENT_TYPE_UINT32_KHR' corresponds to SPIR-V @OpTypeInt@ 32 0.
pattern COMPONENT_TYPE_UINT32_KHR = ComponentTypeKHR 9

-- | 'COMPONENT_TYPE_UINT64_KHR' corresponds to SPIR-V @OpTypeInt@ 64 0.
pattern COMPONENT_TYPE_UINT64_KHR = ComponentTypeKHR 10

{-# COMPLETE
  COMPONENT_TYPE_FLOAT16_KHR
  , COMPONENT_TYPE_FLOAT32_KHR
  , COMPONENT_TYPE_FLOAT64_KHR
  , COMPONENT_TYPE_SINT8_KHR
  , COMPONENT_TYPE_SINT16_KHR
  , COMPONENT_TYPE_SINT32_KHR
  , COMPONENT_TYPE_SINT64_KHR
  , COMPONENT_TYPE_UINT8_KHR
  , COMPONENT_TYPE_UINT16_KHR
  , COMPONENT_TYPE_UINT32_KHR
  , COMPONENT_TYPE_UINT64_KHR ::
    ComponentTypeKHR
  #-}

conNameComponentTypeKHR :: String
conNameComponentTypeKHR = "ComponentTypeKHR"

enumPrefixComponentTypeKHR :: String
enumPrefixComponentTypeKHR = "COMPONENT_TYPE_"

showTableComponentTypeKHR :: [(ComponentTypeKHR, String)]
showTableComponentTypeKHR =
  [ (COMPONENT_TYPE_FLOAT16_KHR, "FLOAT16_KHR")
  , (COMPONENT_TYPE_FLOAT32_KHR, "FLOAT32_KHR")
  , (COMPONENT_TYPE_FLOAT64_KHR, "FLOAT64_KHR")
  , (COMPONENT_TYPE_SINT8_KHR, "SINT8_KHR")
  , (COMPONENT_TYPE_SINT16_KHR, "SINT16_KHR")
  , (COMPONENT_TYPE_SINT32_KHR, "SINT32_KHR")
  , (COMPONENT_TYPE_SINT64_KHR, "SINT64_KHR")
  , (COMPONENT_TYPE_UINT8_KHR, "UINT8_KHR")
  , (COMPONENT_TYPE_UINT16_KHR, "UINT16_KHR")
  , (COMPONENT_TYPE_UINT32_KHR, "UINT32_KHR")
  , (COMPONENT_TYPE_UINT64_KHR, "UINT64_KHR")
  ]

instance Show ComponentTypeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixComponentTypeKHR
      showTableComponentTypeKHR
      conNameComponentTypeKHR
      (\(ComponentTypeKHR x) -> x)
      (showsPrec 11)

instance Read ComponentTypeKHR where
  readPrec =
    enumReadPrec
      enumPrefixComponentTypeKHR
      showTableComponentTypeKHR
      conNameComponentTypeKHR
      ComponentTypeKHR

type KHR_COOPERATIVE_MATRIX_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern KHR_COOPERATIVE_MATRIX_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COOPERATIVE_MATRIX_SPEC_VERSION = 2


type KHR_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_KHR_cooperative_matrix"

-- No documentation found for TopLevel "VK_KHR_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern KHR_COOPERATIVE_MATRIX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_KHR_cooperative_matrix"

