{-# language CPP #-}
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

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
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
import Data.Word (Word16)
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineStippleEXT))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
-- -   @lineStippleFactor@ /must/ be in the range [1,256]
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
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
  vkCmdSetLineStippleEXT' (commandBufferHandle (commandBuffer)) (lineStippleFactor) (lineStipplePattern)
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
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLineRasterizationFeaturesEXT = PhysicalDeviceLineRasterizationFeaturesEXT
  { -- | @rectangularLines@ indicates whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines rectangular line rasterization>.
    rectangularLines :: Bool
  , -- | @bresenhamLines@ indicates whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham-style line rasterization>.
    bresenhamLines :: Bool
  , -- | @smoothLines@ indicates whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-smooth smooth line rasterization>.
    smoothLines :: Bool
  , -- | @stippledRectangularLines@ indicates whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT' lines, or with
    -- 'LINE_RASTERIZATION_MODE_DEFAULT_EXT' lines if
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
    -- is 'Vulkan.Core10.BaseType.TRUE'.
    stippledRectangularLines :: Bool
  , -- | @stippledBresenhamLines@ indicates whether the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-stipple stippled line rasterization>
    -- with 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT' lines.
    stippledBresenhamLines :: Bool
  , -- | @stippledSmoothLines@ indicates whether the implementation supports
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
  { -- | @lineSubPixelPrecisionBits@ is the number of bits of subpixel precision
    -- in framebuffer coordinates xf and yf when rasterizing
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
-- -   If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rectangularLines rectangularLines>
--     feature /must/ be enabled
--
-- -   If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_BRESENHAM_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bresenhamLines bresenhamLines>
--     feature /must/ be enabled
--
-- -   If @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bresenhamLines smoothLines>
--     feature /must/ be enabled
--
-- -   If @stippledLineEnable@ is 'Vulkan.Core10.BaseType.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   If @stippledLineEnable@ is 'Vulkan.Core10.BaseType.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   If @stippledLineEnable@ is 'Vulkan.Core10.BaseType.TRUE' and
--     @lineRasterizationMode@ is
--     'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   If @stippledLineEnable@ is 'Vulkan.Core10.BaseType.TRUE' and
--     @lineRasterizationMode@ is 'LINE_RASTERIZATION_MODE_DEFAULT_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.BaseType.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT'
--
-- -   @lineRasterizationMode@ /must/ be a valid 'LineRasterizationModeEXT'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32', 'LineRasterizationModeEXT',
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
-- is 'Vulkan.Core10.BaseType.TRUE', otherwise lines are drawn as
-- non-@strictLines@ parallelograms. Both of these modes are defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT = LineRasterizationModeEXT 0
-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT' specifies lines drawn as if
-- they were rectangles extruded from the line
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT = LineRasterizationModeEXT 1
-- | 'LINE_RASTERIZATION_MODE_BRESENHAM_EXT' specifies lines drawn by
-- determining which pixel diamonds the line intersects and exits, as
-- defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-bresenham Bresenham Line Segment Rasterization>.
pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT = LineRasterizationModeEXT 2
-- | 'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT' specifies lines drawn
-- if they were rectangles extruded from the line, with alpha falloff, as
-- defined in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-smooth Smooth Lines>.
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT = LineRasterizationModeEXT 3
{-# complete LINE_RASTERIZATION_MODE_DEFAULT_EXT,
             LINE_RASTERIZATION_MODE_RECTANGULAR_EXT,
             LINE_RASTERIZATION_MODE_BRESENHAM_EXT,
             LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT :: LineRasterizationModeEXT #-}

instance Show LineRasterizationModeEXT where
  showsPrec p = \case
    LINE_RASTERIZATION_MODE_DEFAULT_EXT -> showString "LINE_RASTERIZATION_MODE_DEFAULT_EXT"
    LINE_RASTERIZATION_MODE_RECTANGULAR_EXT -> showString "LINE_RASTERIZATION_MODE_RECTANGULAR_EXT"
    LINE_RASTERIZATION_MODE_BRESENHAM_EXT -> showString "LINE_RASTERIZATION_MODE_BRESENHAM_EXT"
    LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT -> showString "LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT"
    LineRasterizationModeEXT x -> showParen (p >= 11) (showString "LineRasterizationModeEXT " . showsPrec 11 x)

instance Read LineRasterizationModeEXT where
  readPrec = parens (choose [("LINE_RASTERIZATION_MODE_DEFAULT_EXT", pure LINE_RASTERIZATION_MODE_DEFAULT_EXT)
                            , ("LINE_RASTERIZATION_MODE_RECTANGULAR_EXT", pure LINE_RASTERIZATION_MODE_RECTANGULAR_EXT)
                            , ("LINE_RASTERIZATION_MODE_BRESENHAM_EXT", pure LINE_RASTERIZATION_MODE_BRESENHAM_EXT)
                            , ("LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT", pure LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "LineRasterizationModeEXT")
                       v <- step readPrec
                       pure (LineRasterizationModeEXT v)))


type EXT_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_SPEC_VERSION"
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION = 1


type EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

