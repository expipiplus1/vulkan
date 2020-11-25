{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_line_rasterization"
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
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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

-- No documentation found for TopLevel "vkCmdSetLineStippleEXT"
cmdSetLineStippleEXT :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdSetLineStippleEXT" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdSetLineStippleEXT" "lineStippleFactor"
                        ("lineStippleFactor" ::: Word32)
                     -> -- No documentation found for Nested "vkCmdSetLineStippleEXT" "lineStipplePattern"
                        ("lineStipplePattern" ::: Word16)
                     -> io ()
cmdSetLineStippleEXT commandBuffer lineStippleFactor lineStipplePattern = liftIO $ do
  let vkCmdSetLineStippleEXTPtr = pVkCmdSetLineStippleEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetLineStippleEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineStippleEXT is null" Nothing Nothing
  let vkCmdSetLineStippleEXT' = mkVkCmdSetLineStippleEXT vkCmdSetLineStippleEXTPtr
  vkCmdSetLineStippleEXT' (commandBufferHandle (commandBuffer)) (lineStippleFactor) (lineStipplePattern)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationFeaturesEXT"
data PhysicalDeviceLineRasterizationFeaturesEXT = PhysicalDeviceLineRasterizationFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "rectangularLines"
    rectangularLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "bresenhamLines"
    bresenhamLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "smoothLines"
    smoothLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "stippledRectangularLines"
    stippledRectangularLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "stippledBresenhamLines"
    stippledBresenhamLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationFeaturesEXT" "stippledSmoothLines"
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



-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationPropertiesEXT"
data PhysicalDeviceLineRasterizationPropertiesEXT = PhysicalDeviceLineRasterizationPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceLineRasterizationPropertiesEXT" "lineSubPixelPrecisionBits"
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



-- No documentation found for TopLevel "VkPipelineRasterizationLineStateCreateInfoEXT"
data PipelineRasterizationLineStateCreateInfoEXT = PipelineRasterizationLineStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineRasterizationLineStateCreateInfoEXT" "lineRasterizationMode"
    lineRasterizationMode :: LineRasterizationModeEXT
  , -- No documentation found for Nested "VkPipelineRasterizationLineStateCreateInfoEXT" "stippledLineEnable"
    stippledLineEnable :: Bool
  , -- No documentation found for Nested "VkPipelineRasterizationLineStateCreateInfoEXT" "lineStippleFactor"
    lineStippleFactor :: Word32
  , -- No documentation found for Nested "VkPipelineRasterizationLineStateCreateInfoEXT" "lineStipplePattern"
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


-- No documentation found for TopLevel "VkLineRasterizationModeEXT"
newtype LineRasterizationModeEXT = LineRasterizationModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkLineRasterizationModeEXT" "VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT"
pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT            = LineRasterizationModeEXT 0
-- No documentation found for Nested "VkLineRasterizationModeEXT" "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT        = LineRasterizationModeEXT 1
-- No documentation found for Nested "VkLineRasterizationModeEXT" "VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT"
pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT          = LineRasterizationModeEXT 2
-- No documentation found for Nested "VkLineRasterizationModeEXT" "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT"
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

