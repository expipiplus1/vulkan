{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_render_pass_transform - device extension
--
-- == VK_QCOM_render_pass_transform
--
-- [__Name String__]
--     @VK_QCOM_render_pass_transform@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     283
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_QCOM_render_pass_transform:%20&body=@jackohound%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-10-15
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires @VK_KHR_swapchain@
--
--     -   This extension interacts with @VK_EXT_fragment_density_map@
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Brandon Light, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension provides a mechanism for applications to enable driver
-- support for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>.
--
-- Mobile devices can be rotated and mobile applications need to render
-- properly when a device is held in a landscape or portrait orientation.
-- When the current orientation differs from the device’s native
-- orientation, a rotation is required so that the \"up\" direction of the
-- rendered scene matches the current orientation.
--
-- If the Display Processing Unit (DPU) doesnt natively support rotation,
-- the Vulkan presentation engine can handle this rotation in a separate
-- composition pass. Alternatively, the application can render frames
-- \"pre-rotated\" to avoid this extra pass. The latter is preferred to
-- reduce power consumption and achieve the best performance because it
-- avoids tasking the GPU with extra work to perform the copy\/rotate
-- operation.
--
-- Unlike OpenGL ES, the burden of pre-rotation in Vulkan falls on the
-- application. To implement pre-rotation, applications render into
-- swapchain images matching the device native aspect ratio of the display
-- and \"pre-rotate\" the rendering content to match the device’s current
-- orientation. The burden is more than adjusting the Model View Projection
-- (MVP) matrix in the vertex shader to account for rotation and aspect
-- ratio. The coordinate systems of scissors, viewports, derivatives and
-- several shader built-ins may need to be adapted to produce the correct
-- result.
--
-- It is difficult for some game engines to manage this burden; many chose
-- to simply accept the performance\/power overhead of performing rotation
-- in the presentation engine.
--
-- This extension allows applications to achieve the performance benefits
-- of pre-rotated rendering by moving much of the above-mentioned burden to
-- the graphics driver. The following is unchanged with this extension:
--
-- -   Applications create a swapchain matching the native orientation of
--     the display. Applications must also set the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@preTransform@
--     equal to the @currentTransform@ as returned by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR'.
--
-- The following is changed with this extension:
--
-- -   At 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass', the
--     application provides extension struct
--     'RenderPassTransformBeginInfoQCOM' specifying the render pass
--     transform parameters.
--
-- -   At 'Vulkan.Core10.CommandBuffer.beginCommandBuffer' for secondary
--     command buffers, the application provides extension struct
--     'CommandBufferInheritanceRenderPassTransformInfoQCOM' specifying the
--     render pass transform parameters.
--
-- -   The @renderArea@, viewPorts and scissors are all provided in the
--     current (non-rotated) coordinate system. The implementation will
--     transform those into the native (rotated) coordinate system.
--
-- -   The implementation is responsible for transforming shader built-ins
--     (@FragCoord@, @PointCoord@, @SamplePosition@, interpolateAt(), dFdx,
--     dFdy, fWidth) into the rotated coordinate system.
--
-- -   The implementation is responsible for transforming @position@ to the
--     rotated coordinate system.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceRenderPassTransformInfoQCOM'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'RenderPassTransformBeginInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME'
--
-- -   'QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM'
--
-- == Issues
--
-- 1) Some early Adreno drivers (October 2019 through March 2020)
-- advertised support for this extension but expected VK_STRUCTURE_TYPE
-- values different from those in the vukan headers. To cover all Adreno
-- devices on the market, applications need to detect the driver version
-- and use the appropriate VK_STRUCTURE_TYPE values from the table below.
--
-- The driver version reported in VkPhysicalDeviceProperties.driverVersion
-- is a @uint32_t@ type. You can decode the @uint32_t@ value into a
-- major.minor.patch version as shown below:
--
-- > uint32_t  major = ((driverVersion) >> 22);
-- > uint32_t  minor = ((driverVersion) >> 12) & 0x3ff);
-- > uint32_t  patch = ((driverVersion) & 0xfff);
--
-- If the Adreno major.minor.patch version is greater than or equal to to
-- 512.469.0, then simply use the VK_STRUCTURE_TYPE values as defined in
-- vulkan_core.h. If the version is less than or equal to to 512.468.0,
-- then use the alternate values for the two VK_STRUCTURE_TYPEs in the
-- table below.
--
-- +------------------------------------------------------------+------------------+------------------+
-- |                                                            | Adreno Driver    |                  |
-- |                                                            | Version          |                  |
-- +============================================================+==================+==================+
-- |                                                            | 512.468.0 and    | 512.469.0 and    |
-- |                                                            | earlier          | later            |
-- +------------------------------------------------------------+------------------+------------------+
-- | VK_STRUCTURE_TYPE_ RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM   | 1000282000       | 1000282001       |
-- +------------------------------------------------------------+------------------+------------------+
-- | VK_STRUCTURE_TYPE_                                         | 1000282001       | 1000282000       |
-- | COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM |                  |                  |
-- +------------------------------------------------------------+------------------+------------------+
--
-- @Adreno@ Driver Requirements
--
-- 2) Should the extension support only rotations (e.g. 90, 180,
-- 270-degrees), or also mirror transforms (e.g. vertical flips)? Mobile
-- use-cases only require rotation. Other display systems such as
-- projectors might require a flipped transform.
--
-- __RESOLVED__: In this version of the extension, the functionality is
-- restricted to 90, 180, and 270-degree rotations to address mobile
-- use-cases.
--
-- 3) How does this extension interact with VK_EXT_fragment_density_map?
--
-- __RESOLVED__ Some implementations may not be able to support a render
-- pass that enables both renderpass transform and fragment density maps.
-- For simplicity, this extension disallows enabling both features within a
-- single render pass.
--
-- 4) What should this extension be named?
--
-- We considered names such as \"rotated_rendering\", \"pre_rotation\" and
-- others. Since the functionality is limited to a render pass, it seemed
-- the name should include \"render_pass\". While the current extension is
-- limited to rotations, it could be extended to other transforms (like
-- mirror) in the future.
--
-- __RESOLVED__ The name \"render_pass_transform\" seems like the most
-- accurate description of the introduced functionality.
--
-- == Version History
--
-- -   Revision 1, 2020-02-05 (Jeff Leger)
--
-- = See Also
--
-- 'CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'RenderPassTransformBeginInfoQCOM'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_render_pass_transform Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR)
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
-- == Valid Usage (Implicit)
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
    --
    -- #VUID-VkCommandBufferInheritanceRenderPassTransformInfoQCOM-transform-02864#
    -- @transform@ /must/ be
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_90_BIT_KHR',
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_180_BIT_KHR',
    -- or
    -- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
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

