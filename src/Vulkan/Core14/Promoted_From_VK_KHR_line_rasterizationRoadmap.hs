{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_line_rasterizationRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap  ( cmdSetLineStipple
                                                                     , PhysicalDeviceLineRasterizationFeatures(..)
                                                                     , PhysicalDeviceLineRasterizationProperties(..)
                                                                     , PipelineRasterizationLineStateCreateInfo(..)
                                                                     , StructureType(..)
                                                                     , DynamicState(..)
                                                                     , LineRasterizationMode(..)
                                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineStipple))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.DynamicState (DynamicState(..))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineStipple
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()

-- | vkCmdSetLineStipple - Set line stipple dynamically for a command buffer
--
-- = Description
--
-- This command sets the line stipple state for subsequent drawing commands
-- when drawing using
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE' set in
-- 'Vulkan.Core10.GraphicsPipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'PipelineRasterizationLineStateCreateInfo'::@lineStippleFactor@ and
-- 'PipelineRasterizationLineStateCreateInfo'::@lineStipplePattern@ values
-- used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineStipple-lineStippleFactor-02776#
--     @lineStippleFactor@ /must/ be in the range [1,256]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineStipple-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineStipple-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineStipple-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSetLineStipple-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetLineStipple is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineStipple :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @lineStippleFactor@ is the repeat factor used in stippled line
                     -- rasterization.
                     ("lineStippleFactor" ::: Word32)
                  -> -- | @lineStipplePattern@ is the bit pattern used in stippled line
                     -- rasterization.
                     ("lineStipplePattern" ::: Word16)
                  -> io ()
cmdSetLineStipple commandBuffer
                    lineStippleFactor
                    lineStipplePattern = liftIO $ do
  let vkCmdSetLineStipplePtr = pVkCmdSetLineStipple (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLineStipplePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineStipple is null" Nothing Nothing
  let vkCmdSetLineStipple' = mkVkCmdSetLineStipple vkCmdSetLineStipplePtr
  traceAroundEvent "vkCmdSetLineStipple" (vkCmdSetLineStipple'
                                            (commandBufferHandle (commandBuffer))
                                            (lineStippleFactor)
                                            (lineStipplePattern))
  pure $ ()


-- | VkPhysicalDeviceLineRasterizationFeatures - Structure describing the
-- line rasterization features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationFeatures' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceLineRasterizationFeatures', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationFeatures = PhysicalDeviceLineRasterizationFeatures
  { -- | #extension-features-rectangularLines# @rectangularLines@ indicates
    -- whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines rectangular line rasterization>.
    rectangularLines :: Bool
  , -- | #extension-features-bresenhamLines# @bresenhamLines@ indicates whether
    -- the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-bresenham Bresenham-style line rasterization>.
    bresenhamLines :: Bool
  , -- | #extension-features-smoothLines# @smoothLines@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-smooth smooth line rasterization>.
    smoothLines :: Bool
  , -- | #extension-features-stippledRectangularLines# @stippledRectangularLines@
    -- indicates whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR'
    -- lines.
    stippledRectangularLines :: Bool
  , -- | #extension-features-stippledBresenhamLines# @stippledBresenhamLines@
    -- indicates whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM'
    -- lines.
    stippledBresenhamLines :: Bool
  , -- | #extension-features-stippledSmoothLines# @stippledSmoothLines@ indicates
    -- whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH'
    -- lines.
    stippledSmoothLines :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationFeatures)
#endif
deriving instance Show PhysicalDeviceLineRasterizationFeatures

instance ToCStruct PhysicalDeviceLineRasterizationFeatures where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rectangularLines))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (bresenhamLines))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (smoothLines))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (stippledRectangularLines))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (stippledBresenhamLines))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (stippledSmoothLines))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLineRasterizationFeatures where
  peekCStruct p = do
    rectangularLines <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bresenhamLines <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    smoothLines <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    stippledRectangularLines <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    stippledBresenhamLines <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    stippledSmoothLines <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceLineRasterizationFeatures
             (bool32ToBool rectangularLines)
             (bool32ToBool bresenhamLines)
             (bool32ToBool smoothLines)
             (bool32ToBool stippledRectangularLines)
             (bool32ToBool stippledBresenhamLines)
             (bool32ToBool stippledSmoothLines)

instance Storable PhysicalDeviceLineRasterizationFeatures where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationFeatures where
  zero = PhysicalDeviceLineRasterizationFeatures
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceLineRasterizationProperties - Structure describing line
-- rasterization properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationProperties' structure is included
-- in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationProperties = PhysicalDeviceLineRasterizationProperties
  { -- | #extension-limits-lineSubPixelPrecisionBits# @lineSubPixelPrecisionBits@
    -- is the number of bits of subpixel precision in framebuffer coordinates
    -- xf and yf when rasterizing
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines line segments>.
    lineSubPixelPrecisionBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationProperties)
#endif
deriving instance Show PhysicalDeviceLineRasterizationProperties

instance ToCStruct PhysicalDeviceLineRasterizationProperties where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (lineSubPixelPrecisionBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceLineRasterizationProperties where
  peekCStruct p = do
    lineSubPixelPrecisionBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceLineRasterizationProperties
             lineSubPixelPrecisionBits

instance Storable PhysicalDeviceLineRasterizationProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationProperties where
  zero = PhysicalDeviceLineRasterizationProperties
           zero


-- | VkPipelineRasterizationLineStateCreateInfo - Structure specifying
-- parameters of a newly created pipeline line rasterization state
--
-- = Description
--
-- If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the
-- values of @lineStippleFactor@ and @lineStipplePattern@ are ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-lineRasterizationMode-02768#
--     If @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-rectangularLines rectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-lineRasterizationMode-02769#
--     If @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bresenhamLines bresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-lineRasterizationMode-02770#
--     If @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-smoothLines smoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-stippledLineEnable-02771#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-stippledLineEnable-02772#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-stippledLineEnable-02773#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-stippledLineEnable-02774#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_DEFAULT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfo-lineRasterizationMode-parameter#
--     @lineRasterizationMode@ /must/ be a valid
--     'Vulkan.Core14.Enums.LineRasterizationMode.LineRasterizationMode'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core14.Enums.LineRasterizationMode.LineRasterizationMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationLineStateCreateInfo = PipelineRasterizationLineStateCreateInfo
  { -- | @lineRasterizationMode@ is a
    -- 'Vulkan.Core14.Enums.LineRasterizationMode.LineRasterizationMode' value
    -- selecting the style of line rasterization.
    lineRasterizationMode :: LineRasterizationMode
  , -- | @stippledLineEnable@ enables
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>.
    stippledLineEnable :: Bool
  , -- | @lineStippleFactor@ is the repeat factor used in stippled line
    -- rasterization.
    lineStippleFactor :: Word32
  , -- | @lineStipplePattern@ is the bit pattern used in stippled line
    -- rasterization.
    lineStipplePattern :: Word16
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationLineStateCreateInfo)
#endif
deriving instance Show PipelineRasterizationLineStateCreateInfo

instance ToCStruct PipelineRasterizationLineStateCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationLineStateCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationMode)) (lineRasterizationMode)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (stippledLineEnable))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (lineStippleFactor)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (lineStipplePattern)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationMode)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (zero)
    f

instance FromCStruct PipelineRasterizationLineStateCreateInfo where
  peekCStruct p = do
    lineRasterizationMode <- peek @LineRasterizationMode ((p `plusPtr` 16 :: Ptr LineRasterizationMode))
    stippledLineEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    lineStippleFactor <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    lineStipplePattern <- peek @Word16 ((p `plusPtr` 28 :: Ptr Word16))
    pure $ PipelineRasterizationLineStateCreateInfo
             lineRasterizationMode
             (bool32ToBool stippledLineEnable)
             lineStippleFactor
             lineStipplePattern

instance Storable PipelineRasterizationLineStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationLineStateCreateInfo where
  zero = PipelineRasterizationLineStateCreateInfo
           zero
           zero
           zero
           zero

