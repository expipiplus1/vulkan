{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
                                                                           , PhysicalDeviceShaderDrawParametersFeatures(..)
                                                                           , PhysicalDeviceShaderDrawParameterFeatures
                                                                           , StructureType(..)
                                                                           ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES


-- | VkPhysicalDeviceShaderDrawParametersFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderDrawParametersFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderDrawParametersFeatures = PhysicalDeviceShaderDrawParametersFeatures
  { -- | @shaderDrawParameters@ specifies whether shader draw parameters are
    -- supported.
    shaderDrawParameters :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderDrawParametersFeatures

instance ToCStruct PhysicalDeviceShaderDrawParametersFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderDrawParametersFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderDrawParameters))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderDrawParametersFeatures where
  peekCStruct p = do
    shaderDrawParameters <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderDrawParametersFeatures
             (bool32ToBool shaderDrawParameters)

instance Storable PhysicalDeviceShaderDrawParametersFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderDrawParametersFeatures where
  zero = PhysicalDeviceShaderDrawParametersFeatures
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceShaderDrawParameterFeatures"
type PhysicalDeviceShaderDrawParameterFeatures = PhysicalDeviceShaderDrawParametersFeatures

