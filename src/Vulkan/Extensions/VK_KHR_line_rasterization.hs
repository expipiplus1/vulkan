{-# language CPP #-}
-- | = Name
--
-- VK_KHR_line_rasterization - device extension
--
-- == VK_KHR_line_rasterization
--
-- [__Name String__]
--     @VK_KHR_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     535
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_line_rasterization] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_line_rasterization extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Allen Jensen, NVIDIA
--
--     -   Faith Ekstrand, Intel
--
-- == Description
--
-- This extension adds some line rasterization features that are commonly
-- used in CAD applications and supported in other APIs like OpenGL.
-- Bresenham-style line rasterization is supported, smooth rectangular
-- lines (coverage to alpha) are supported, and stippled lines are
-- supported for all three line rasterization modes.
--
-- == New Commands
--
-- -   'cmdSetLineStippleKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLineRasterizationFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLineRasterizationPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationLineStateCreateInfoKHR'
--
-- == New Enums
--
-- -   'LineRasterizationModeKHR'
--
-- == New Enum Constants
--
-- -   'KHR_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'KHR_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Do we need to support Bresenham-style and smooth lines with more than
-- one rasterization sample? i.e. the equivalent of
-- glDisable(GL_MULTISAMPLE) in OpenGL when the framebuffer has more than
-- one sample?
--
-- __RESOLVED__: Yes. For simplicity, Bresenham line rasterization carries
-- forward a few restrictions from OpenGL, such as not supporting
-- per-sample shading, alpha to coverage, or alpha to one.
--
-- == Version History
--
-- -   Revision 1, 2019-05-09 (Jeff Bolz)
--
--     -   Initial draft
--
-- == See Also
--
-- 'LineRasterizationModeKHR',
-- 'PhysicalDeviceLineRasterizationFeaturesKHR',
-- 'PhysicalDeviceLineRasterizationPropertiesKHR',
-- 'PipelineRasterizationLineStateCreateInfoKHR', 'cmdSetLineStippleKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_line_rasterization  ( cmdSetLineStippleKHR
                                                    , pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT
                                                    , PhysicalDeviceLineRasterizationFeaturesKHR(..)
                                                    , PhysicalDeviceLineRasterizationPropertiesKHR(..)
                                                    , PipelineRasterizationLineStateCreateInfoKHR(..)
                                                    , LineRasterizationModeKHR( LINE_RASTERIZATION_MODE_DEFAULT_KHR
                                                                              , LINE_RASTERIZATION_MODE_RECTANGULAR_KHR
                                                                              , LINE_RASTERIZATION_MODE_BRESENHAM_KHR
                                                                              , LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR
                                                                              , ..
                                                                              )
                                                    , KHR_LINE_RASTERIZATION_SPEC_VERSION
                                                    , pattern KHR_LINE_RASTERIZATION_SPEC_VERSION
                                                    , KHR_LINE_RASTERIZATION_EXTENSION_NAME
                                                    , pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineStippleKHR))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineStippleKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()

-- | vkCmdSetLineStippleKHR - Set line stipple dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the line stipple state for subsequent drawing commands
-- when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.DYNAMIC_STATE_LINE_STIPPLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'PipelineRasterizationLineStateCreateInfoKHR'::@lineStippleFactor@ and
-- 'PipelineRasterizationLineStateCreateInfoKHR'::@lineStipplePattern@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineStippleKHR-lineStippleFactor-02776#
--     @lineStippleFactor@ /must/ be in the range [1,256]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineStippleKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineStippleKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineStippleKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetLineStippleKHR-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineStippleKHR :: forall io
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
cmdSetLineStippleKHR commandBuffer
                       lineStippleFactor
                       lineStipplePattern = liftIO $ do
  let vkCmdSetLineStippleKHRPtr = pVkCmdSetLineStippleKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLineStippleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineStippleKHR is null" Nothing Nothing
  let vkCmdSetLineStippleKHR' = mkVkCmdSetLineStippleKHR vkCmdSetLineStippleKHRPtr
  traceAroundEvent "vkCmdSetLineStippleKHR" (vkCmdSetLineStippleKHR'
                                               (commandBufferHandle (commandBuffer))
                                               (lineStippleFactor)
                                               (lineStipplePattern))
  pure $ ()


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT"
pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT = LINE_RASTERIZATION_MODE_DEFAULT_KHR


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT = LINE_RASTERIZATION_MODE_RECTANGULAR_KHR


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT"
pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT = LINE_RASTERIZATION_MODE_BRESENHAM_KHR


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT = LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR


-- | VkPhysicalDeviceLineRasterizationFeaturesKHR - Structure describing the
-- line rasterization features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceLineRasterizationFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationFeaturesKHR = PhysicalDeviceLineRasterizationFeaturesKHR
  { -- | #features-rectangularLines# @rectangularLines@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines rectangular line rasterization>.
    rectangularLines :: Bool
  , -- | #features-bresenhamLines# @bresenhamLines@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham-style line rasterization>.
    bresenhamLines :: Bool
  , -- | #features-smoothLines# @smoothLines@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-smooth smooth line rasterization>.
    smoothLines :: Bool
  , -- | #features-stippledRectangularLines# @stippledRectangularLines@ indicates
    -- whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR' lines.
    stippledRectangularLines :: Bool
  , -- | #features-stippledBresenhamLines# @stippledBresenhamLines@ indicates
    -- whether the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_BRESENHAM_KHR' lines.
    stippledBresenhamLines :: Bool
  , -- | #features-stippledSmoothLines# @stippledSmoothLines@ indicates whether
    -- the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR' lines.
    stippledSmoothLines :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceLineRasterizationFeaturesKHR

instance ToCStruct PhysicalDeviceLineRasterizationFeaturesKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLineRasterizationFeaturesKHR where
  peekCStruct p = do
    rectangularLines <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bresenhamLines <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    smoothLines <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    stippledRectangularLines <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    stippledBresenhamLines <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    stippledSmoothLines <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceLineRasterizationFeaturesKHR
             (bool32ToBool rectangularLines)
             (bool32ToBool bresenhamLines)
             (bool32ToBool smoothLines)
             (bool32ToBool stippledRectangularLines)
             (bool32ToBool stippledBresenhamLines)
             (bool32ToBool stippledSmoothLines)

instance Storable PhysicalDeviceLineRasterizationFeaturesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationFeaturesKHR where
  zero = PhysicalDeviceLineRasterizationFeaturesKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceLineRasterizationPropertiesKHR - Structure describing
-- line rasterization properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationPropertiesKHR' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationPropertiesKHR = PhysicalDeviceLineRasterizationPropertiesKHR
  { -- | #limits-lineSubPixelPrecisionBits# @lineSubPixelPrecisionBits@ is the
    -- number of bits of subpixel precision in framebuffer coordinates xf and
    -- yf when rasterizing
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines line segments>.
    lineSubPixelPrecisionBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceLineRasterizationPropertiesKHR

instance ToCStruct PhysicalDeviceLineRasterizationPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (lineSubPixelPrecisionBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceLineRasterizationPropertiesKHR where
  peekCStruct p = do
    lineSubPixelPrecisionBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceLineRasterizationPropertiesKHR
             lineSubPixelPrecisionBits

instance Storable PhysicalDeviceLineRasterizationPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationPropertiesKHR where
  zero = PhysicalDeviceLineRasterizationPropertiesKHR
           zero


-- | VkPipelineRasterizationLineStateCreateInfoKHR - Structure specifying
-- parameters of a newly created pipeline line rasterization state
--
-- = Description
--
-- If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the
-- values of @lineStippleFactor@ and @lineStipplePattern@ are ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-lineRasterizationMode-02768#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR', then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-rectangularLines rectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-lineRasterizationMode-02769#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_BRESENHAM_KHR', then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bresenhamLines bresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-lineRasterizationMode-02770#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR', then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-smoothLines smoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-stippledLineEnable-02771#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR', then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-stippledLineEnable-02772#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_BRESENHAM_KHR',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-stippledLineEnable-02773#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR', then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-stippledLineEnable-02774#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_DEFAULT_KHR',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoKHR-lineRasterizationMode-parameter#
--     @lineRasterizationMode@ /must/ be a valid 'LineRasterizationModeKHR'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'LineRasterizationModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationLineStateCreateInfoKHR = PipelineRasterizationLineStateCreateInfoKHR
  { -- | @lineRasterizationMode@ is a 'LineRasterizationModeKHR' value selecting
    -- the style of line rasterization.
    lineRasterizationMode :: LineRasterizationModeKHR
  , -- | @stippledLineEnable@ enables
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>.
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
deriving instance Generic (PipelineRasterizationLineStateCreateInfoKHR)
#endif
deriving instance Show PipelineRasterizationLineStateCreateInfoKHR

instance ToCStruct PipelineRasterizationLineStateCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationLineStateCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationModeKHR)) (lineRasterizationMode)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (stippledLineEnable))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (lineStippleFactor)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (lineStipplePattern)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationModeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (zero)
    f

instance FromCStruct PipelineRasterizationLineStateCreateInfoKHR where
  peekCStruct p = do
    lineRasterizationMode <- peek @LineRasterizationModeKHR ((p `plusPtr` 16 :: Ptr LineRasterizationModeKHR))
    stippledLineEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    lineStippleFactor <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    lineStipplePattern <- peek @Word16 ((p `plusPtr` 28 :: Ptr Word16))
    pure $ PipelineRasterizationLineStateCreateInfoKHR
             lineRasterizationMode
             (bool32ToBool stippledLineEnable)
             lineStippleFactor
             lineStipplePattern

instance Storable PipelineRasterizationLineStateCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationLineStateCreateInfoKHR where
  zero = PipelineRasterizationLineStateCreateInfoKHR
           zero
           zero
           zero
           zero


-- | VkLineRasterizationModeKHR - Line rasterization modes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- 'PipelineRasterizationLineStateCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
newtype LineRasterizationModeKHR = LineRasterizationModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'LINE_RASTERIZATION_MODE_DEFAULT_KHR' is equivalent to
-- 'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR' if
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
-- is 'Vulkan.Core10.FundamentalTypes.TRUE', otherwise lines are drawn as
-- non-@strictLines@ parallelograms. Both of these modes are defined in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_DEFAULT_KHR = LineRasterizationModeKHR 0

-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR' specifies lines drawn as if
-- they were rectangles extruded from the line
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_KHR = LineRasterizationModeKHR 1

-- | 'LINE_RASTERIZATION_MODE_BRESENHAM_KHR' specifies lines drawn by
-- determining which pixel diamonds the line intersects and exits, as
-- defined in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_BRESENHAM_KHR = LineRasterizationModeKHR 2

-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR' specifies lines drawn
-- if they were rectangles extruded from the line, with alpha falloff, as
-- defined in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-lines-smooth Smooth Lines>.
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR = LineRasterizationModeKHR 3

{-# COMPLETE
  LINE_RASTERIZATION_MODE_DEFAULT_KHR
  , LINE_RASTERIZATION_MODE_RECTANGULAR_KHR
  , LINE_RASTERIZATION_MODE_BRESENHAM_KHR
  , LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR ::
    LineRasterizationModeKHR
  #-}

conNameLineRasterizationModeKHR :: String
conNameLineRasterizationModeKHR = "LineRasterizationModeKHR"

enumPrefixLineRasterizationModeKHR :: String
enumPrefixLineRasterizationModeKHR = "LINE_RASTERIZATION_MODE_"

showTableLineRasterizationModeKHR :: [(LineRasterizationModeKHR, String)]
showTableLineRasterizationModeKHR =
  [
    ( LINE_RASTERIZATION_MODE_DEFAULT_KHR
    , "DEFAULT_KHR"
    )
  ,
    ( LINE_RASTERIZATION_MODE_RECTANGULAR_KHR
    , "RECTANGULAR_KHR"
    )
  ,
    ( LINE_RASTERIZATION_MODE_BRESENHAM_KHR
    , "BRESENHAM_KHR"
    )
  ,
    ( LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR
    , "RECTANGULAR_SMOOTH_KHR"
    )
  ]

instance Show LineRasterizationModeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixLineRasterizationModeKHR
      showTableLineRasterizationModeKHR
      conNameLineRasterizationModeKHR
      (\(LineRasterizationModeKHR x) -> x)
      (showsPrec 11)

instance Read LineRasterizationModeKHR where
  readPrec =
    enumReadPrec
      enumPrefixLineRasterizationModeKHR
      showTableLineRasterizationModeKHR
      conNameLineRasterizationModeKHR
      LineRasterizationModeKHR

type KHR_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_LINE_RASTERIZATION_SPEC_VERSION"
pattern KHR_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_LINE_RASTERIZATION_SPEC_VERSION = 1


type KHR_LINE_RASTERIZATION_EXTENSION_NAME = "VK_KHR_line_rasterization"

-- No documentation found for TopLevel "VK_KHR_LINE_RASTERIZATION_EXTENSION_NAME"
pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME = "VK_KHR_line_rasterization"

