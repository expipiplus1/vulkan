{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_vertex_attribute_divisor  ( VertexInputBindingDivisorDescriptionEXT(..)
                                                          , PipelineVertexInputDivisorStateCreateInfoEXT(..)
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
                                                          , PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
                                                          , EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT))
-- | VkVertexInputBindingDivisorDescriptionEXT - Structure specifying a
-- divisor used in instanced rendering
--
-- = Description
--
-- If this structure is not used to define a divisor value for an attribute
-- then the divisor has a logical default value of 1.
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionEXT-binding-01869#
--     @binding@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionEXT-vertexAttributeInstanceRateZeroDivisor-02228#
--     If the @vertexAttributeInstanceRateZeroDivisor@ feature is not
--     enabled, @divisor@ /must/ not be @0@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionEXT-vertexAttributeInstanceRateDivisor-02229#
--     If the @vertexAttributeInstanceRateDivisor@ feature is not enabled,
--     @divisor@ /must/ be @1@
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionEXT-divisor-01870#
--     @divisor@ /must/ be a value between @0@ and
--     'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'::@maxVertexAttribDivisor@,
--     inclusive
--
-- -   #VUID-VkVertexInputBindingDivisorDescriptionEXT-inputRate-01871#
--     'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@inputRate@
--     /must/ be of type
--     'Vulkan.Core10.Enums.VertexInputRate.VERTEX_INPUT_RATE_INSTANCE' for
--     this @binding@
--
-- = See Also
--
-- 'PipelineVertexInputDivisorStateCreateInfoEXT'
data VertexInputBindingDivisorDescriptionEXT = VertexInputBindingDivisorDescriptionEXT
  { -- | @binding@ is the binding number for which the divisor is specified.
    binding :: Word32
  , -- | @divisor@ is the number of successive instances that will use the same
    -- value of the vertex attribute when instanced rendering is enabled. For
    -- example, if the divisor is N, the same vertex attribute will be applied
    -- to N successive instances before moving on to the next vertex attribute.
    -- The maximum value of divisor is implementation dependent and can be
    -- queried using
    -- 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'::@maxVertexAttribDivisor@.
    -- A value of @0@ /can/ be used for the divisor if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
    -- feature is enabled. In this case, the same vertex attribute will be
    -- applied to all instances.
    divisor :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDivisorDescriptionEXT)
#endif
deriving instance Show VertexInputBindingDivisorDescriptionEXT

instance ToCStruct VertexInputBindingDivisorDescriptionEXT where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputBindingDivisorDescriptionEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (divisor)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputBindingDivisorDescriptionEXT where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    divisor <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ VertexInputBindingDivisorDescriptionEXT
             binding divisor

instance Storable VertexInputBindingDivisorDescriptionEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputBindingDivisorDescriptionEXT where
  zero = VertexInputBindingDivisorDescriptionEXT
           zero
           zero


-- | VkPipelineVertexInputDivisorStateCreateInfoEXT - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineVertexInputDivisorStateCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineVertexInputDivisorStateCreateInfoEXT-pVertexBindingDivisors-parameter#
--     @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
--     @vertexBindingDivisorCount@
--     'VertexInputBindingDivisorDescriptionEXT' structures
--
-- -   #VUID-VkPipelineVertexInputDivisorStateCreateInfoEXT-vertexBindingDivisorCount-arraylength#
--     @vertexBindingDivisorCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputBindingDivisorDescriptionEXT'
data PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoEXT
  { -- | @pVertexBindingDivisors@ is a pointer to an array of
    -- 'VertexInputBindingDivisorDescriptionEXT' structures, which specifies
    -- the divisor value for each binding.
    vertexBindingDivisors :: Vector VertexInputBindingDivisorDescriptionEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineVertexInputDivisorStateCreateInfoEXT)
#endif
deriving instance Show PipelineVertexInputDivisorStateCreateInfoEXT

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineVertexInputDivisorStateCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexBindingDivisors)) :: Word32))
    pPVertexBindingDivisors' <- ContT $ allocaBytesAligned @VertexInputBindingDivisorDescriptionEXT ((Data.Vector.length (vertexBindingDivisors)) * 8) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexBindingDivisors' `plusPtr` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionEXT) (e) . ($ ())) (vertexBindingDivisors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionEXT))) (pPVertexBindingDivisors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPVertexBindingDivisors' <- ContT $ allocaBytesAligned @VertexInputBindingDivisorDescriptionEXT ((Data.Vector.length (mempty)) * 8) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexBindingDivisors' `plusPtr` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionEXT))) (pPVertexBindingDivisors')
    lift $ f

instance FromCStruct PipelineVertexInputDivisorStateCreateInfoEXT where
  peekCStruct p = do
    vertexBindingDivisorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pVertexBindingDivisors <- peek @(Ptr VertexInputBindingDivisorDescriptionEXT) ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionEXT)))
    pVertexBindingDivisors' <- generateM (fromIntegral vertexBindingDivisorCount) (\i -> peekCStruct @VertexInputBindingDivisorDescriptionEXT ((pVertexBindingDivisors `advancePtrBytes` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionEXT)))
    pure $ PipelineVertexInputDivisorStateCreateInfoEXT
             pVertexBindingDivisors'

instance Zero PipelineVertexInputDivisorStateCreateInfoEXT where
  zero = PipelineVertexInputDivisorStateCreateInfoEXT
           mempty


-- | VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT - Structure
-- describing max value of vertex attribute divisor that can be supported
-- by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is the
--     maximum value of the number of instances that will repeat the value
--     of vertex attribute data when instanced rendering is enabled.
--
-- If the 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT" "maxVertexAttribDivisor"
    maxVertexAttribDivisor :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorPropertiesEXT

instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVertexAttribDivisor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  peekCStruct p = do
    maxVertexAttribDivisor <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceVertexAttributeDivisorPropertiesEXT
             maxVertexAttribDivisor

instance Storable PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
           zero


-- | VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT - Structure describing
-- if fetching of vertex attribute may be repeated for instanced rendering
--
-- = Members
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #features-vertexAttributeInstanceRateDivisor#
--     @vertexAttributeInstanceRateDivisor@ specifies whether vertex
--     attribute fetching may be repeated in case of instanced rendering.
--
-- -   #features-vertexAttributeInstanceRateZeroDivisor#
--     @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero
--     value for 'VertexInputBindingDivisorDescriptionEXT'::@divisor@ is
--     supported.
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT' /can/ also
-- be included in @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable the feature.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateDivisor"
    vertexAttributeInstanceRateDivisor :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateZeroDivisor"
    vertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorFeaturesEXT

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateDivisor))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (vertexAttributeInstanceRateZeroDivisor))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  peekCStruct p = do
    vertexAttributeInstanceRateDivisor <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    vertexAttributeInstanceRateZeroDivisor <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeDivisorFeaturesEXT
             (bool32ToBool vertexAttributeInstanceRateDivisor) (bool32ToBool vertexAttributeInstanceRateZeroDivisor)

instance Storable PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
           zero
           zero


type EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3


type EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"

