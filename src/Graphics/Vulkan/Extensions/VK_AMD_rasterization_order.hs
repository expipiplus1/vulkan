{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order  ( PipelineRasterizationStateRasterizationOrderAMD(..)
                                                              , RasterizationOrderAMD( RASTERIZATION_ORDER_STRICT_AMD
                                                                                     , RASTERIZATION_ORDER_RELAXED_AMD
                                                                                     , ..
                                                                                     )
                                                              , AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                              , pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                              , AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                              , pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD))
-- | VkPipelineRasterizationStateRasterizationOrderAMD - Structure defining
-- rasterization order for a graphics pipeline
--
-- == Valid Usage (Implicit)
--
-- If the @VK_AMD_rasterization_order@ device extension is not enabled or
-- the application does not request a particular rasterization order
-- through specifying a 'PipelineRasterizationStateRasterizationOrderAMD'
-- structure then the rasterization order used by the graphics pipeline
-- defaults to 'RASTERIZATION_ORDER_STRICT_AMD'.
--
-- = See Also
--
-- 'RasterizationOrderAMD',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- | @rasterizationOrder@ /must/ be a valid 'RasterizationOrderAMD' value
    rasterizationOrder :: RasterizationOrderAMD }
  deriving (Typeable)
deriving instance Show PipelineRasterizationStateRasterizationOrderAMD

instance ToCStruct PipelineRasterizationStateRasterizationOrderAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateRasterizationOrderAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD)) (rasterizationOrder)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD)) (zero)
    f

instance FromCStruct PipelineRasterizationStateRasterizationOrderAMD where
  peekCStruct p = do
    rasterizationOrder <- peek @RasterizationOrderAMD ((p `plusPtr` 16 :: Ptr RasterizationOrderAMD))
    pure $ PipelineRasterizationStateRasterizationOrderAMD
             rasterizationOrder

instance Storable PipelineRasterizationStateRasterizationOrderAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationStateRasterizationOrderAMD where
  zero = PipelineRasterizationStateRasterizationOrderAMD
           zero


-- | VkRasterizationOrderAMD - Specify rasterization order for a graphics
-- pipeline
--
-- = See Also
--
-- 'PipelineRasterizationStateRasterizationOrderAMD'
newtype RasterizationOrderAMD = RasterizationOrderAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RASTERIZATION_ORDER_STRICT_AMD' specifies that operations for each
-- primitive in a subpass /must/ occur in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_STRICT_AMD = RasterizationOrderAMD 0
-- | 'RASTERIZATION_ORDER_RELAXED_AMD' specifies that operations for each
-- primitive in a subpass /may/ not occur in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-order primitive order>.
pattern RASTERIZATION_ORDER_RELAXED_AMD = RasterizationOrderAMD 1
{-# complete RASTERIZATION_ORDER_STRICT_AMD,
             RASTERIZATION_ORDER_RELAXED_AMD :: RasterizationOrderAMD #-}

instance Show RasterizationOrderAMD where
  showsPrec p = \case
    RASTERIZATION_ORDER_STRICT_AMD -> showString "RASTERIZATION_ORDER_STRICT_AMD"
    RASTERIZATION_ORDER_RELAXED_AMD -> showString "RASTERIZATION_ORDER_RELAXED_AMD"
    RasterizationOrderAMD x -> showParen (p >= 11) (showString "RasterizationOrderAMD " . showsPrec 11 x)

instance Read RasterizationOrderAMD where
  readPrec = parens (choose [("RASTERIZATION_ORDER_STRICT_AMD", pure RASTERIZATION_ORDER_STRICT_AMD)
                            , ("RASTERIZATION_ORDER_RELAXED_AMD", pure RASTERIZATION_ORDER_RELAXED_AMD)]
                     +++
                     prec 10 (do
                       expectP (Ident "RasterizationOrderAMD")
                       v <- step readPrec
                       pure (RasterizationOrderAMD v)))


type AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION"
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1


type AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME"
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

