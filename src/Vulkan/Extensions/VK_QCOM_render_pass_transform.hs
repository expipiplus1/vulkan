{-# language CPP #-}
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
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
-- | VkRenderPassTransformBeginInfoQCOM - Structure describing transform
-- parameters of a render pass instance
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassTransformBeginInfoQCOM-transform-02871#
--     @transform@ /must/ be
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--
-- -   #VUID-VkRenderPassTransformBeginInfoQCOM-flags-02872# The
--     @renderpass@ /must/ have been created with
--     'Vulkan.Core10.Pass.RenderPassCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassTransformBeginInfoQCOM-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR'
data RenderPassTransformBeginInfoQCOM = RenderPassTransformBeginInfoQCOM
  { -- | @transform@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
    -- describing the transform to be applied to rasterization.
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


-- | VkCommandBufferInheritanceRenderPassTransformInfoQCOM - Structure
-- describing transformed render pass parameters command buffer
--
-- = Description
--
-- When the secondary is recorded to execute within a render pass instance
-- using 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands', the
-- render pass transform parameters of the secondary command buffer /must/
-- be consistent with the render pass transform parameters specified for
-- the render pass instance. In particular, the @transform@ and
-- @renderArea@ for command buffer /must/ be identical to the @transform@
-- and @renderArea@ of the render pass instance.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceRenderPassTransformInfoQCOM-transform-02864#
--     @transform@ /must/ be
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
--     or
--     'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceRenderPassTransformInfoQCOM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR'
data CommandBufferInheritanceRenderPassTransformInfoQCOM = CommandBufferInheritanceRenderPassTransformInfoQCOM
  { -- | @transform@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceTransformFlagBitsKHR' value
    -- describing the transform to be applied to the render pass.
    transform :: SurfaceTransformFlagBitsKHR
  , -- | @renderArea@ is the render area that is affected by the command buffer.
    renderArea :: Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceRenderPassTransformInfoQCOM)
#endif
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

