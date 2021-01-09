{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_attribute_divisor - device extension
--
-- == VK_EXT_vertex_attribute_divisor
--
-- [__Name String__]
--     @VK_EXT_vertex_attribute_divisor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     191
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_vertex_attribute_divisor:%20&body=@vkushwaha%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension allows instance-rate vertex attributes to be repeated for
-- certain number of instances instead of advancing for every instance when
-- instanced rendering is enabled.
--
-- == New Structures
--
-- -   'VertexInputBindingDivisorDescriptionEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeDivisorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo':
--
--     -   'PipelineVertexInputDivisorStateCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) What is the effect of a non-zero value for @firstInstance@?
--
-- __RESOLVED__: The Vulkan API should follow the OpenGL convention and
-- offset attribute fetching by @firstInstance@ while computing vertex
-- attribute offsets.
--
-- 2) Should zero be an allowed divisor?
--
-- __RESOLVED__: Yes. A zero divisor means the vertex attribute is repeated
-- for all instances.
--
-- == Examples
--
-- To create a vertex binding such that the first binding uses instanced
-- rendering and the same attribute is used for every 4 draw instances, an
-- application could use the following set of structures:
--
-- >     const VkVertexInputBindingDivisorDescriptionEXT divisorDesc =
-- >     {
-- >         0,
-- >         4
-- >     };
-- >
-- >     const VkPipelineVertexInputDivisorStateCreateInfoEXT divisorInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT, // sType
-- >         NULL,                                                             // pNext
-- >         1,                                                                // vertexBindingDivisorCount
-- >         &divisorDesc                                                      // pVertexBindingDivisors
-- >     }
-- >
-- >     const VkVertexInputBindingDescription binding =
-- >     {
-- >         0,                                                                // binding
-- >         sizeof(Vertex),                                                   // stride
-- >         VK_VERTEX_INPUT_RATE_INSTANCE                                     // inputRate
-- >     };
-- >
-- >     const VkPipelineVertexInputStateCreateInfo viInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_CREATE_INFO,              // sType
-- >         &divisorInfo,                                                     // pNext
-- >         ...
-- >     };
-- >     //...
--
-- == Version History
--
-- -   Revision 1, 2017-12-04 (Vikram Kushwaha)
--
--     -   First Version
--
-- -   Revision 2, 2018-07-16 (Jason Ekstrand)
--
--     -   Adjust the interaction between @divisor@ and @firstInstance@ to
--         match the OpenGL convention.
--
--     -   Disallow divisors of zero.
--
-- -   Revision 3, 2018-08-03 (Vikram Kushwaha)
--
--     -   Allow a zero divisor.
--
--     -   Add a physical device features structure to query\/enable this
--         feature.
--
-- = See Also
--
-- 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
-- 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT',
-- 'PipelineVertexInputDivisorStateCreateInfoEXT',
-- 'VertexInputBindingDivisorDescriptionEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputBindingDivisorDescriptionEXT'
data PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoEXT
  { -- | @pVertexBindingDivisors@ is a pointer to an array of
    -- 'VertexInputBindingDivisorDescriptionEXT' structures, which specifies
    -- the divisor value for each binding.
    --
    -- #VUID-VkPipelineVertexInputDivisorStateCreateInfoEXT-pVertexBindingDivisors-parameter#
    -- @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
    -- @vertexBindingDivisorCount@ 'VertexInputBindingDivisorDescriptionEXT'
    -- structures
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDivisors' `plusPtr` (8 * (i)) :: Ptr VertexInputBindingDivisorDescriptionEXT) (e)) (vertexBindingDivisors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDivisorDescriptionEXT))) (pPVertexBindingDivisors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

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
-- If the 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- | #limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is the maximum
    -- value of the number of instances that will repeat the value of vertex
    -- attribute data when instanced rendering is enabled.
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
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- | #features-vertexAttributeInstanceRateDivisor#
    -- @vertexAttributeInstanceRateDivisor@ specifies whether vertex attribute
    -- fetching may be repeated in case of instanced rendering.
    vertexAttributeInstanceRateDivisor :: Bool
  , -- | #features-vertexAttributeInstanceRateZeroDivisor#
    -- @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero value
    -- for 'VertexInputBindingDivisorDescriptionEXT'::@divisor@ is supported.
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

