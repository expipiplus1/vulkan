{-# language CPP #-}
-- No documentation found for Chapter "VK_QCOM_render_pass_transform"
module Vulkan.Extensions.VK_QCOM_render_pass_transform  ( RenderPassTransformBeginInfoQCOM(..)
                                                        , CommandBufferInheritanceRenderPassTransformInfoQCOM(..)
                                                        , QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION
                                                        , pattern QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION
                                                        , QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME
                                                        , pattern QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME
                                                        , SurfaceTransformFlagBitsKHR(..)
                                                        , SurfaceTransformFlagsKHR
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)

-- No documentation found for TopLevel "VkRenderPassTransformBeginInfoQCOM"
data RenderPassTransformBeginInfoQCOM = RenderPassTransformBeginInfoQCOM
  { -- No documentation found for Nested "VkRenderPassTransformBeginInfoQCOM" "transform"
    transform :: SurfaceTransformFlagBitsKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassTransformBeginInfoQCOM)
#endif
deriving instance Show RenderPassTransformBeginInfoQCOM

instance ToCStruct RenderPassTransformBeginInfoQCOM where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassTransformBeginInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    f

instance FromCStruct RenderPassTransformBeginInfoQCOM where
  peekCStruct p = do
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR))
    pure $ RenderPassTransformBeginInfoQCOM
             transform


instance Storable RenderPassTransformBeginInfoQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassTransformBeginInfoQCOM where
  zero = RenderPassTransformBeginInfoQCOM
           zero



-- No documentation found for TopLevel "VkCommandBufferInheritanceRenderPassTransformInfoQCOM"
data CommandBufferInheritanceRenderPassTransformInfoQCOM = CommandBufferInheritanceRenderPassTransformInfoQCOM
  { -- No documentation found for Nested "VkCommandBufferInheritanceRenderPassTransformInfoQCOM" "transform"
    transform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkCommandBufferInheritanceRenderPassTransformInfoQCOM" "renderArea"
    renderArea :: Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceRenderPassTransformInfoQCOM)
#endif
deriving instance Show CommandBufferInheritanceRenderPassTransformInfoQCOM

instance ToCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceRenderPassTransformInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    poke ((p `plusPtr` 20 :: Ptr Rect2D)) (renderArea)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Rect2D)) (zero)
    f

instance FromCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM where
  peekCStruct p = do
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR))
    renderArea <- peekCStruct @Rect2D ((p `plusPtr` 20 :: Ptr Rect2D))
    pure $ CommandBufferInheritanceRenderPassTransformInfoQCOM
             transform renderArea


instance Storable CommandBufferInheritanceRenderPassTransformInfoQCOM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferInheritanceRenderPassTransformInfoQCOM where
  zero = CommandBufferInheritanceRenderPassTransformInfoQCOM
           zero
           zero


type QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION"
pattern QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION = 1


type QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME = "VK_QCOM_render_pass_transform"

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME"
pattern QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME = "VK_QCOM_render_pass_transform"

