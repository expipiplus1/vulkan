{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform  ( RenderPassTransformBeginInfoQCOM(..)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.CommandBufferBuilding (Rect2D)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagBitsKHR(..))
import Graphics.Vulkan.Extensions.VK_KHR_display (SurfaceTransformFlagsKHR)
-- | VkRenderPassTransformBeginInfoQCOM - Structure describing transform
-- parameters of a render pass instance
--
-- == Valid Usage
--
-- -   @transform@ /must/ be VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
--     VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR,
--     VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR, or
--     VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR.
--
-- -   The renderpass must have been created with
--     'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo'::@flags@
--     containing VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR'
data RenderPassTransformBeginInfoQCOM = RenderPassTransformBeginInfoQCOM
  { -- | @transform@ is a VkSurfaceTransformFlagBitsKHR value describing the
    -- transform to be applied applied to rasterization.
    transform :: SurfaceTransformFlagBitsKHR }
  deriving (Typeable)
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


-- | VkCommandBufferInheritanceRenderPassTransformInfoQCOM - Structure
-- describing transformed render pass parameters command buffer
--
-- = Description
--
-- When the secondary is recorded to execute within a render pass instance
-- using 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands',
-- the render pass transform parameters of the secondary command buffer
-- /must/ be consistent with the render pass transform parameters specified
-- for the render pass instance. In particular, the @transform@ and
-- @renderArea@ for command buffer /must/ be identical to the @transform@
-- and @renderArea@ of the render pass instance.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.SurfaceTransformFlagBitsKHR'
data CommandBufferInheritanceRenderPassTransformInfoQCOM = CommandBufferInheritanceRenderPassTransformInfoQCOM
  { -- | @transform@ /must/ be VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR,
    -- VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR,
    -- VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR, or
    -- VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR.
    transform :: SurfaceTransformFlagBitsKHR
  , -- | @renderArea@ is the render area that is affected by the command buffer.
    renderArea :: Rect2D
  }
  deriving (Typeable)
deriving instance Show CommandBufferInheritanceRenderPassTransformInfoQCOM

instance ToCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceRenderPassTransformInfoQCOM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (transform)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Rect2D)) (renderArea) . ($ ())
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 20 :: Ptr Rect2D)) (zero) . ($ ())
    lift $ f

instance FromCStruct CommandBufferInheritanceRenderPassTransformInfoQCOM where
  peekCStruct p = do
    transform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 16 :: Ptr SurfaceTransformFlagBitsKHR))
    renderArea <- peekCStruct @Rect2D ((p `plusPtr` 20 :: Ptr Rect2D))
    pure $ CommandBufferInheritanceRenderPassTransformInfoQCOM
             transform renderArea

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

