{-# language CPP #-}
-- | = Name
--
-- VK_EXT_line_rasterization - device extension
--
-- == VK_EXT_line_rasterization
--
-- [__Name String__]
--     @VK_EXT_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     260
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse CAD support>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_line_rasterization:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-09
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
--     -   Jason Ekstrand, Intel
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
-- -   'cmdSetLineStippleEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLineRasterizationFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLineRasterizationPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationLineStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'LineRasterizationModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'EXT_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- > (1) Do we need to support Bresenham-style and smooth lines with more than
-- >     one rasterization sample? i.e. the equivalent of
-- >     glDisable(GL_MULTISAMPLE) in OpenGL when the framebuffer has more than
-- >     one sample?
--
-- > RESOLVED: Yes.
-- > For simplicity, Bresenham line rasterization carries forward a few
-- > restrictions from OpenGL, such as not supporting per-sample shading, alpha
-- > to coverage, or alpha to one.
--
-- == Version History
--
-- -   Revision 1, 2019-05-09 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'LineRasterizationModeEXT',
-- 'PhysicalDeviceLineRasterizationFeaturesEXT',
-- 'PhysicalDeviceLineRasterizationPropertiesEXT',
-- 'PipelineRasterizationLineStateCreateInfoEXT', 'cmdSetLineStippleEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_line_rasterization  ( cmdSetLineStippleEXT
                                                    , PhysicalDeviceLineRasterizationFeaturesEXT(..)
                                                    , PhysicalDeviceLineRasterizationPropertiesEXT(..)
                                                    , PipelineRasterizationLineStateCreateInfoEXT(..)
                                                    , LineRasterizationModeEXT( LINE_RASTERIZATION_MODE_DEFAULT_EXT
                                                                              , LINE_RASTERIZATION_MODE_RECTANGULAR_EXT
                                                                              , LINE_RASTERIZATION_MODE_BRESENHAM_EXT
                                                                              , LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT
                                                                              , ..
                                                                              )
                                                    , EXT_LINE_RASTERIZATION_SPEC_VERSION
                                                    , pattern EXT_LINE_RASTERIZATION_SPEC_VERSION
                                                    , EXT_LINE_RASTERIZATION_EXTENSION_NAME
                                                    , pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineStippleEXT))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineStippleEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word16 -> IO ()

-- | vkCmdSetLineStippleEXT - Set the dynamic line width state
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineStippleEXT-lineStippleFactor-02776#
--     @lineStippleFactor@ /must/ be in the range [1,256]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineStippleEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineStippleEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineStippleEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineStippleEXT :: forall io
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
cmdSetLineStippleEXT commandBuffer lineStippleFactor lineStipplePattern = liftIO $ do
  let vkCmdSetLineStippleEXTPtr = pVkCmdSetLineStippleEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetLineStippleEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineStippleEXT is null" Nothing Nothing
  let vkCmdSetLineStippleEXT' = mkVkCmdSetLineStippleEXT vkCmdSetLineStippleEXTPtr
  traceAroundEvent "vkCmdSetLineStippleEXT" (vkCmdSetLineStippleEXT' (commandBufferHandle (commandBuffer)) (lineStippleFactor) (lineStipplePattern))
  pure $ ()


-- | VkPhysicalDeviceLineRasterizationFeaturesEXT - Structure describing the
-- line rasterization features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceLineRasterizationFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceLineRasterizationFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- the feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationFeaturesEXT = PhysicalDeviceLineRasterizationFeaturesEXT
  { -- | #features-rectangularLines# @rectangularLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines rectangular line rasterization>.
    rectangularLines :: Bool
  , -- | #features-bresenhamLines# @bresenhamLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham-style line rasterization>.
    bresenhamLines :: Bool
  , -- | #features-smoothLines# @smoothLines@ indicates whether the
    -- implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-smooth smooth line rasterization>.
    smoothLines :: Bool
  , -- | #features-stippledRectangularLines# @stippledRectangularLines@ indicates
    -- whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT' lines, or with
    -- 'LINE_RASTERIZATION_MODE_DEFAULT_EXT' lines if
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
    -- is 'Vulkan.Core10.FundamentalTypes.TRUE'.
    stippledRectangularLines :: Bool
  , -- | #features-stippledBresenhamLines# @stippledBresenhamLines@ indicates
    -- whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT' lines.
    stippledBresenhamLines :: Bool
  , -- | #features-stippledSmoothLines# @stippledSmoothLines@ indicates whether
    -- the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT' lines.
    stippledSmoothLines :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceLineRasterizationFeaturesEXT

instance ToCStruct PhysicalDeviceLineRasterizationFeaturesEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLineRasterizationFeaturesEXT where
  peekCStruct p = do
    rectangularLines <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bresenhamLines <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    smoothLines <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    stippledRectangularLines <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    stippledBresenhamLines <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    stippledSmoothLines <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceLineRasterizationFeaturesEXT
             (bool32ToBool rectangularLines) (bool32ToBool bresenhamLines) (bool32ToBool smoothLines) (bool32ToBool stippledRectangularLines) (bool32ToBool stippledBresenhamLines) (bool32ToBool stippledSmoothLines)

instance Storable PhysicalDeviceLineRasterizationFeaturesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationFeaturesEXT where
  zero = PhysicalDeviceLineRasterizationFeaturesEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceLineRasterizationPropertiesEXT - Structure describing
-- line rasterization properties supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceLineRasterizationPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceLineRasterizationPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationPropertiesEXT = PhysicalDeviceLineRasterizationPropertiesEXT
  { -- | #limits-lineSubPixelPrecisionBits# @lineSubPixelPrecisionBits@ is the
    -- number of bits of subpixel precision in framebuffer coordinates xf and
    -- yf when rasterizing
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines line segments>.
    lineSubPixelPrecisionBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLineRasterizationPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceLineRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceLineRasterizationPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLineRasterizationPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (lineSubPixelPrecisionBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceLineRasterizationPropertiesEXT where
  peekCStruct p = do
    lineSubPixelPrecisionBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceLineRasterizationPropertiesEXT
             lineSubPixelPrecisionBits

instance Storable PhysicalDeviceLineRasterizationPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLineRasterizationPropertiesEXT where
  zero = PhysicalDeviceLineRasterizationPropertiesEXT
           zero


-- | VkPipelineRasterizationLineStateCreateInfoEXT - Structure specifying
-- parameters of a newly created pipeline line rasterization state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-lineRasterizationMode-02768#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rectangularLines rectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-lineRasterizationMode-02769#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_BRESENHAM_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bresenhamLines bresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-lineRasterizationMode-02770#
--     If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bresenhamLines smoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-stippledLineEnable-02771#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-stippledLineEnable-02772#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-stippledLineEnable-02773#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-stippledLineEnable-02774#
--     If @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_DEFAULT_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineRasterizationLineStateCreateInfoEXT-lineRasterizationMode-parameter#
--     @lineRasterizationMode@ /must/ be a valid 'LineRasterizationModeEXT'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'LineRasterizationModeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationLineStateCreateInfoEXT = PipelineRasterizationLineStateCreateInfoEXT
  { -- | @lineRasterizationMode@ is a 'LineRasterizationModeEXT' value selecting
    -- the style of line rasterization.
    lineRasterizationMode :: LineRasterizationModeEXT
  , -- | @stippledLineEnable@ enables
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>.
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
deriving instance Generic (PipelineRasterizationLineStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationLineStateCreateInfoEXT

instance ToCStruct PipelineRasterizationLineStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationLineStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationModeEXT)) (lineRasterizationMode)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (stippledLineEnable))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (lineStippleFactor)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (lineStipplePattern)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr LineRasterizationModeEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word16)) (zero)
    f

instance FromCStruct PipelineRasterizationLineStateCreateInfoEXT where
  peekCStruct p = do
    lineRasterizationMode <- peek @LineRasterizationModeEXT ((p `plusPtr` 16 :: Ptr LineRasterizationModeEXT))
    stippledLineEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    lineStippleFactor <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    lineStipplePattern <- peek @Word16 ((p `plusPtr` 28 :: Ptr Word16))
    pure $ PipelineRasterizationLineStateCreateInfoEXT
             lineRasterizationMode (bool32ToBool stippledLineEnable) lineStippleFactor lineStipplePattern

instance Storable PipelineRasterizationLineStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationLineStateCreateInfoEXT where
  zero = PipelineRasterizationLineStateCreateInfoEXT
           zero
           zero
           zero
           zero


-- | VkLineRasterizationModeEXT - Line rasterization modes
--
-- = See Also
--
-- 'PipelineRasterizationLineStateCreateInfoEXT'
newtype LineRasterizationModeEXT = LineRasterizationModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'LINE_RASTERIZATION_MODE_DEFAULT_EXT' is equivalent to
-- 'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT' if
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
-- is 'Vulkan.Core10.FundamentalTypes.TRUE', otherwise lines are drawn as
-- non-@strictLines@ parallelograms. Both of these modes are defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT            = LineRasterizationModeEXT 0
-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT' specifies lines drawn as if
-- they were rectangles extruded from the line
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT        = LineRasterizationModeEXT 1
-- | 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT' specifies lines drawn by
-- determining which pixel diamonds the line intersects and exits, as
-- defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT          = LineRasterizationModeEXT 2
-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT' specifies lines drawn
-- if they were rectangles extruded from the line, with alpha falloff, as
-- defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-smooth Smooth Lines>.
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT = LineRasterizationModeEXT 3
{-# complete LINE_RASTERIZATION_MODE_DEFAULT_EXT,
             LINE_RASTERIZATION_MODE_RECTANGULAR_EXT,
             LINE_RASTERIZATION_MODE_BRESENHAM_EXT,
             LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT :: LineRasterizationModeEXT #-}

conNameLineRasterizationModeEXT :: String
conNameLineRasterizationModeEXT = "LineRasterizationModeEXT"

enumPrefixLineRasterizationModeEXT :: String
enumPrefixLineRasterizationModeEXT = "LINE_RASTERIZATION_MODE_"

showTableLineRasterizationModeEXT :: [(LineRasterizationModeEXT, String)]
showTableLineRasterizationModeEXT =
  [ (LINE_RASTERIZATION_MODE_DEFAULT_EXT           , "DEFAULT_EXT")
  , (LINE_RASTERIZATION_MODE_RECTANGULAR_EXT       , "RECTANGULAR_EXT")
  , (LINE_RASTERIZATION_MODE_BRESENHAM_EXT         , "BRESENHAM_EXT")
  , (LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT, "RECTANGULAR_SMOOTH_EXT")
  ]

instance Show LineRasterizationModeEXT where
  showsPrec = enumShowsPrec enumPrefixLineRasterizationModeEXT
                            showTableLineRasterizationModeEXT
                            conNameLineRasterizationModeEXT
                            (\(LineRasterizationModeEXT x) -> x)
                            (showsPrec 11)

instance Read LineRasterizationModeEXT where
  readPrec = enumReadPrec enumPrefixLineRasterizationModeEXT
                          showTableLineRasterizationModeEXT
                          conNameLineRasterizationModeEXT
                          LineRasterizationModeEXT


type EXT_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_SPEC_VERSION"
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION = 1


type EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

