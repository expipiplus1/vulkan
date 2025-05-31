{-# language CPP #-}
-- | = Name
--
-- VK_KHR_vertex_attribute_divisor - device extension
--
-- == VK_KHR_vertex_attribute_divisor
--
-- [__Name String__]
--     @VK_KHR_vertex_attribute_divisor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     526
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_vertex_attribute_divisor] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_vertex_attribute_divisor extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_vertex_attribute_divisor.adoc VK_KHR_vertex_attribute_divisor>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-20
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Contributors to @VK_EXT_vertex_attribute_divisor@
--
-- == Description
--
-- This extension is based on the @VK_EXT_vertex_attribute_divisor@
-- extension. The only difference is the new property
-- @supportsNonZeroFirstInstance@, which indicates support for non-zero
-- values in @firstInstance@. This allows the extension to be supported on
-- implementations that have traditionally only supported OpenGL ES.
--
-- == New Structures
--
-- -   'VertexInputBindingDivisorDescriptionKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeDivisorFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceVertexAttributeDivisorPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo':
--
--     -   'PipelineVertexInputDivisorStateCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME'
--
-- -   'KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-09-20 (Shahbaz Youssefi)
--
--     -   First Version, based on @VK_EXT_vertex_attribute_divisor@
--
-- == See Also
--
-- 'PhysicalDeviceVertexAttributeDivisorFeaturesKHR',
-- 'PhysicalDeviceVertexAttributeDivisorPropertiesKHR',
-- 'PipelineVertexInputDivisorStateCreateInfoKHR',
-- 'VertexInputBindingDivisorDescriptionKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_vertex_attribute_divisor  ( VertexInputBindingDivisorDescriptionKHR(..)
                                                          , PipelineVertexInputDivisorStateCreateInfoKHR(..)
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesKHR(..)
                                                          , PhysicalDeviceVertexAttributeDivisorFeaturesKHR(..)
                                                          , KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          , pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR))
-- | VkVertexInputBindingDivisorDescriptionKHR - Structure specifying a
-- divisor used in instanced rendering
--
-- = Description
--
-- If this structure is not used to define a divisor value for an
-- attribute, then the divisor has a logical default value of 1.
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionKHR-binding-01869#
--     @binding@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionKHR-vertexAttributeInstanceRateZeroDivisor-02228#
--     If the @vertexAttributeInstanceRateZeroDivisor@ feature is not
--     enabled, @divisor@ /must/ not be @0@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionKHR-vertexAttributeInstanceRateDivisor-02229#
--     If the @vertexAttributeInstanceRateDivisor@ feature is not enabled,
--     @divisor@ /must/ be @1@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionKHR-divisor-01870#
--     @divisor@ /must/ be a value between @0@ and
--     'PhysicalDeviceVertexAttributeDivisorPropertiesKHR'::@maxVertexAttribDivisor@,
--     inclusive
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionKHR-inputRate-01871#
--     'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@inputRate@
--     /must/ be of type
--     'Vulkan.Core10.Enums.VertexInputRate.VERTEX_INPUT_RATE_INSTANCE' for
--     this @binding@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- 'PipelineVertexInputDivisorStateCreateInfoKHR'
data VertexInputBindingDivisorDescriptionKHR = VertexInputBindingDivisorDescriptionKHR
  { -- | @binding@ is the binding number for which the divisor is specified.
    binding :: Word32
  , -- | @divisor@ is the number of successive instances that will use the same
    -- value of the vertex attribute when instanced rendering is enabled. For
    -- example, if the divisor is N, the same vertex attribute will be applied
    -- to N successive instances before moving on to the next vertex attribute.
    -- The maximum value of @divisor@ is implementation-dependent and can be
    -- queried using
    -- 'PhysicalDeviceVertexAttributeDivisorPropertiesKHR'::@maxVertexAttribDivisor@.
    -- A value of @0@ /can/ be used for the divisor if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
    -- feature is enabled. In this case, the same vertex attribute will be
    -- applied to all instances.
    divisor :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDivisorDescriptionKHR)
#endif
deriving instance Show VertexInputBindingDivisorDescriptionKHR

instance ToCStruct VertexInputBindingDivisorDescriptionKHR where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputBindingDivisorDescriptionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (divisor)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputBindingDivisorDescriptionKHR where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    divisor <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ VertexInputBindingDivisorDescriptionKHR
             binding divisor

instance Storable VertexInputBindingDivisorDescriptionKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputBindingDivisorDescriptionKHR where
  zero = VertexInputBindingDivisorDescriptionKHR
           zero
           zero


-- | VkPipelineVertexInputDivisorStateCreateInfoKHR - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputBindingDivisorDescriptionKHR'
data PipelineVertexInputDivisorStateCreateInfoKHR = PipelineVertexInputDivisorStateCreateInfoKHR
  { -- | @pVertexBindingDivisors@ is a pointer to an array of
    -- 'VertexInputBindingDivisorDescriptionKHR' structures specifying the
    -- divisor value for each binding.
    --
    -- #VUID-VkPipelineVertexInputDivisorStateCreateInfoKHR-pVertexBindingDivisors-parameter#
    -- @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
    -- @vertexBindingDivisorCount@ 'VertexInputBindingDivisorDescriptionKHR'
    -- structures
    vertexBindingDivisors :: Vector VertexInputBindingDivisorDescriptionKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineVertexInputDivisorStateCreateInfoKHR)
#endif
deriving instance Show PipelineVertexInputDivisorStateCreateInfoKHR

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineVertexInputDivisorStateCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexBindingDivisors)) :: Word32))
    pPVertexBindingDivisors' <- ContT $ allocaBytes @VertexInputBindingDivisorDescriptionKHR ((Data.Vector.length (vertexBindingDivisors)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDivisors' `plusPtr` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionKHR) (e)) (vertexBindingDivisors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionKHR))) (pPVertexBindingDivisors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineVertexInputDivisorStateCreateInfoKHR where
  peekCStruct p = do
    vertexBindingDivisorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pVertexBindingDivisors <- peek @(Ptr VertexInputBindingDivisorDescriptionKHR) ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionKHR)))
    pVertexBindingDivisors' <- generateM (fromIntegral vertexBindingDivisorCount) (\i -> peekCStruct @VertexInputBindingDivisorDescriptionKHR ((pVertexBindingDivisors `advancePtrBytes` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionKHR)))
    pure $ PipelineVertexInputDivisorStateCreateInfoKHR
             pVertexBindingDivisors'

instance Zero PipelineVertexInputDivisorStateCreateInfoKHR where
  zero = PipelineVertexInputDivisorStateCreateInfoKHR
           mempty


-- | VkPhysicalDeviceVertexAttributeDivisorPropertiesKHR - Structure
-- describing max value of vertex attribute divisor that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorPropertiesKHR' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorPropertiesKHR = PhysicalDeviceVertexAttributeDivisorPropertiesKHR
  { -- | #limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is the maximum
    -- value of the number of instances that will repeat the value of vertex
    -- attribute data when instanced rendering is enabled.
    maxVertexAttribDivisor :: Word32
  , -- | #limits-supportsNonZeroFirstInstance# @supportsNonZeroFirstInstance@
    -- specifies whether a non-zero value for the @firstInstance@ parameter of
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing drawing commands>
    -- is supported when 'VertexInputBindingDivisorDescriptionKHR'::@divisor@
    -- is not @1@.
    supportsNonZeroFirstInstance :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorPropertiesKHR

instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVertexAttribDivisor)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (supportsNonZeroFirstInstance))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesKHR where
  peekCStruct p = do
    maxVertexAttribDivisor <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    supportsNonZeroFirstInstance <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeDivisorPropertiesKHR
             maxVertexAttribDivisor (bool32ToBool supportsNonZeroFirstInstance)

instance Storable PhysicalDeviceVertexAttributeDivisorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesKHR where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesKHR
           zero
           zero


-- | VkPhysicalDeviceVertexAttributeDivisorFeaturesKHR - Structure describing
-- if fetching of vertex attribute may be repeated for instanced rendering
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVertexAttributeDivisorFeaturesKHR' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorFeaturesKHR = PhysicalDeviceVertexAttributeDivisorFeaturesKHR
  { -- | #features-vertexAttributeInstanceRateDivisor#
    -- @vertexAttributeInstanceRateDivisor@ specifies whether vertex attribute
    -- fetching may be repeated in the case of instanced rendering.
    vertexAttributeInstanceRateDivisor :: Bool
  , -- | #features-vertexAttributeInstanceRateZeroDivisor#
    -- @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero value
    -- for
    -- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VertexInputBindingDivisorDescriptionEXT'::@divisor@
    -- is supported.
    vertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorFeaturesKHR

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateDivisor))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateZeroDivisor))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeaturesKHR where
  peekCStruct p = do
    vertexAttributeInstanceRateDivisor <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    vertexAttributeInstanceRateZeroDivisor <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeDivisorFeaturesKHR
             (bool32ToBool vertexAttributeInstanceRateDivisor)
             (bool32ToBool vertexAttributeInstanceRateZeroDivisor)

instance Storable PhysicalDeviceVertexAttributeDivisorFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorFeaturesKHR where
  zero = PhysicalDeviceVertexAttributeDivisorFeaturesKHR
           zero
           zero


type KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1


type KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_KHR_vertex_attribute_divisor"

-- No documentation found for TopLevel "VK_KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_KHR_vertex_attribute_divisor"

