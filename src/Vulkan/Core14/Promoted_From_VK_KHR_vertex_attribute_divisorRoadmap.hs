{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap  ( VertexInputBindingDivisorDescription(..)
                                                                           , PipelineVertexInputDivisorStateCreateInfo(..)
                                                                           , PhysicalDeviceVertexAttributeDivisorProperties(..)
                                                                           , PhysicalDeviceVertexAttributeDivisorFeatures(..)
                                                                           , StructureType(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkVertexInputBindingDivisorDescription - Structure specifying a divisor
-- used in instanced rendering
--
-- = Description
--
-- If this structure is not used to define a divisor value for an
-- attribute, then the divisor has a logical default value of 1.
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputBindingDivisorDescription-binding-01869#
--     @binding@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputBindingDivisorDescription-vertexAttributeInstanceRateZeroDivisor-02228#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
--     feature is not enabled, @divisor@ /must/ not be @0@
--
-- -   #VUID-VkVertexInputBindingDivisorDescription-vertexAttributeInstanceRateDivisor-02229#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-vertexAttributeInstanceRateDivisor vertexAttributeInstanceRateDivisor>
--     feature is not enabled, @divisor@ /must/ be @1@
--
-- -   #VUID-VkVertexInputBindingDivisorDescription-divisor-01870#
--     @divisor@ /must/ be a value between @0@ and
--     'PhysicalDeviceVertexAttributeDivisorProperties'::@maxVertexAttribDivisor@,
--     inclusive
--
-- -   #VUID-VkVertexInputBindingDivisorDescription-inputRate-01871#
--     'Vulkan.Core10.GraphicsPipeline.VertexInputBindingDescription'::@inputRate@
--     /must/ be of type
--     'Vulkan.Core10.Enums.VertexInputRate.VERTEX_INPUT_RATE_INSTANCE' for
--     this @binding@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'PipelineVertexInputDivisorStateCreateInfo'
data VertexInputBindingDivisorDescription = VertexInputBindingDivisorDescription
  { -- | @binding@ is the binding number for which the divisor is specified.
    binding :: Word32
  , -- | @divisor@ is the number of successive instances that will use the same
    -- value of the vertex attribute when instanced rendering is enabled. For
    -- example, if the divisor is N, the same vertex attribute will be applied
    -- to N successive instances before moving on to the next vertex attribute.
    -- The maximum value of @divisor@ is implementation-dependent and can be
    -- queried using
    -- 'PhysicalDeviceVertexAttributeDivisorProperties'::@maxVertexAttribDivisor@.
    -- A value of @0@ /can/ be used for the divisor if the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
    -- feature is enabled. In this case, the same vertex attribute will be
    -- applied to all instances.
    divisor :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDivisorDescription)
#endif
deriving instance Show VertexInputBindingDivisorDescription

instance ToCStruct VertexInputBindingDivisorDescription where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputBindingDivisorDescription{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (divisor)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputBindingDivisorDescription where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    divisor <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ VertexInputBindingDivisorDescription
             binding divisor

instance Storable VertexInputBindingDivisorDescription where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputBindingDivisorDescription where
  zero = VertexInputBindingDivisorDescription
           zero
           zero


-- | VkPipelineVertexInputDivisorStateCreateInfo - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputBindingDivisorDescription'
data PipelineVertexInputDivisorStateCreateInfo = PipelineVertexInputDivisorStateCreateInfo
  { -- | @pVertexBindingDivisors@ is a pointer to an array of
    -- 'VertexInputBindingDivisorDescription' structures specifying the divisor
    -- value for each binding.
    --
    -- #VUID-VkPipelineVertexInputDivisorStateCreateInfo-pVertexBindingDivisors-parameter#
    -- @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
    -- @vertexBindingDivisorCount@ 'VertexInputBindingDivisorDescription'
    -- structures
    vertexBindingDivisors :: Vector VertexInputBindingDivisorDescription }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineVertexInputDivisorStateCreateInfo)
#endif
deriving instance Show PipelineVertexInputDivisorStateCreateInfo

instance ToCStruct PipelineVertexInputDivisorStateCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineVertexInputDivisorStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexBindingDivisors)) :: Word32))
    pPVertexBindingDivisors' <- ContT $ allocaBytes @VertexInputBindingDivisorDescription ((Data.Vector.length (vertexBindingDivisors)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDivisors' `plusPtr` (8 * (i)) :: Ptr VertexInputBindingDivisorDescription) (e)) (vertexBindingDivisors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescription))) (pPVertexBindingDivisors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineVertexInputDivisorStateCreateInfo where
  peekCStruct p = do
    vertexBindingDivisorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pVertexBindingDivisors <- peek @(Ptr VertexInputBindingDivisorDescription) ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescription)))
    pVertexBindingDivisors' <- generateM (fromIntegral vertexBindingDivisorCount) (\i -> peekCStruct @VertexInputBindingDivisorDescription ((pVertexBindingDivisors `advancePtrBytes` (8 * (i)) :: Ptr VertexInputBindingDivisorDescription)))
    pure $ PipelineVertexInputDivisorStateCreateInfo
             pVertexBindingDivisors'

instance Zero PipelineVertexInputDivisorStateCreateInfo where
  zero = PipelineVertexInputDivisorStateCreateInfo
           mempty


-- | VkPhysicalDeviceVertexAttributeDivisorProperties - Structure describing
-- max value of vertex attribute divisor that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorProperties' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorProperties = PhysicalDeviceVertexAttributeDivisorProperties
  { -- | #extension-limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is
    -- the maximum value of the number of instances that will repeat the value
    -- of vertex attribute data when instanced rendering is enabled.
    maxVertexAttribDivisor :: Word32
  , -- | #extension-limits-supportsNonZeroFirstInstance#
    -- @supportsNonZeroFirstInstance@ specifies whether a non-zero value for
    -- the @firstInstance@ parameter of
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#drawing drawing commands>
    -- is supported when 'VertexInputBindingDivisorDescription'::@divisor@ is
    -- not @1@.
    supportsNonZeroFirstInstance :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorProperties)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorProperties

instance ToCStruct PhysicalDeviceVertexAttributeDivisorProperties where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVertexAttribDivisor)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (supportsNonZeroFirstInstance))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorProperties where
  peekCStruct p = do
    maxVertexAttribDivisor <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    supportsNonZeroFirstInstance <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeDivisorProperties
             maxVertexAttribDivisor (bool32ToBool supportsNonZeroFirstInstance)

instance Storable PhysicalDeviceVertexAttributeDivisorProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorProperties where
  zero = PhysicalDeviceVertexAttributeDivisorProperties
           zero
           zero


-- | VkPhysicalDeviceVertexAttributeDivisorFeatures - Structure describing if
-- fetching of vertex attribute may be repeated for instanced rendering
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceVertexAttributeDivisorFeatures', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorFeatures = PhysicalDeviceVertexAttributeDivisorFeatures
  { -- | #extension-features-vertexAttributeInstanceRateDivisor#
    -- @vertexAttributeInstanceRateDivisor@ specifies whether vertex attribute
    -- fetching may be repeated in the case of instanced rendering.
    vertexAttributeInstanceRateDivisor :: Bool
  , -- | #extension-features-vertexAttributeInstanceRateZeroDivisor#
    -- @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero value
    -- for
    -- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VertexInputBindingDivisorDescriptionEXT'::@divisor@
    -- is supported.
    vertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorFeatures)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorFeatures

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateDivisor))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateZeroDivisor))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeatures where
  peekCStruct p = do
    vertexAttributeInstanceRateDivisor <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    vertexAttributeInstanceRateZeroDivisor <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeDivisorFeatures
             (bool32ToBool vertexAttributeInstanceRateDivisor)
             (bool32ToBool vertexAttributeInstanceRateZeroDivisor)

instance Storable PhysicalDeviceVertexAttributeDivisorFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorFeatures where
  zero = PhysicalDeviceVertexAttributeDivisorFeatures
           zero
           zero

