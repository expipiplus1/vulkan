{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color  ( PipelineCoverageToColorStateCreateInfoNV(..)
                                                                    , PipelineCoverageToColorStateCreateFlagsNV(..)
                                                                    , NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                                    , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                                    , NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                                    , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                                    ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV))
-- | VkPipelineCoverageToColorStateCreateInfoNV - Structure specifying
-- whether fragment coverage replaces a color
--
-- = Description
--
-- If @coverageToColorEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE',
-- the fragment coverage information is treated as a bitmask with one bit
-- for each sample (as in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplemask Sample Mask>
-- section), and this bitmask replaces the first component of the color
-- value corresponding to the fragment shader output location with
-- @Location@ equal to @coverageToColorLocation@ and @Index@ equal to zero.
-- If the color attachment format has fewer bits than the sample coverage,
-- the low bits of the sample coverage bitmask are taken without any
-- clamping. If the color attachment format has more bits than the sample
-- coverage, the high bits of the sample coverage bitmask are filled with
-- zeros.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>
-- is in use, the coverage bitmask only has bits set for samples that
-- correspond to the fragment shader invocation that shades those samples.
--
-- This pipeline stage occurs after sample counting and before blending,
-- and is always performed after fragment shading regardless of the setting
-- of @EarlyFragmentTests@.
--
-- If @coverageToColorEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE',
-- these operations are skipped. If this structure is not present, it is as
-- if @coverageToColorEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE'.
--
-- == Valid Usage
--
-- -   If @coverageToColorEnable@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', then the render pass subpass
--     indicated by
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::'Graphics.Vulkan.Core10.Handles.RenderPass'
--     and
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@subpass@
--     /must/ have a color attachment at the location selected by
--     @coverageToColorLocation@, with a
--     'Graphics.Vulkan.Core10.Enums.Format.Format' of
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV'
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'PipelineCoverageToColorStateCreateFlagsNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is reserved for future use.
    flags :: PipelineCoverageToColorStateCreateFlagsNV
  , -- | @coverageToColorEnable@ controls whether the fragment coverage value
    -- replaces a fragment color output.
    coverageToColorEnable :: Bool
  , -- | @coverageToColorLocation@ controls which fragment shader color output
    -- value is replaced.
    coverageToColorLocation :: Word32
  }
  deriving (Typeable)
deriving instance Show PipelineCoverageToColorStateCreateInfoNV

instance ToCStruct PipelineCoverageToColorStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCoverageToColorStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCoverageToColorStateCreateFlagsNV)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (coverageToColorEnable))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (coverageToColorLocation)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineCoverageToColorStateCreateInfoNV where
  peekCStruct p = do
    flags <- peek @PipelineCoverageToColorStateCreateFlagsNV ((p `plusPtr` 16 :: Ptr PipelineCoverageToColorStateCreateFlagsNV))
    coverageToColorEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    coverageToColorLocation <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PipelineCoverageToColorStateCreateInfoNV
             flags (bool32ToBool coverageToColorEnable) coverageToColorLocation

instance Storable PipelineCoverageToColorStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCoverageToColorStateCreateInfoNV where
  zero = PipelineCoverageToColorStateCreateInfoNV
           zero
           zero
           zero


-- | VkPipelineCoverageToColorStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'PipelineCoverageToColorStateCreateFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineCoverageToColorStateCreateInfoNV'
newtype PipelineCoverageToColorStateCreateFlagsNV = PipelineCoverageToColorStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineCoverageToColorStateCreateFlagsNV where
  showsPrec p = \case
    PipelineCoverageToColorStateCreateFlagsNV x -> showParen (p >= 11) (showString "PipelineCoverageToColorStateCreateFlagsNV 0x" . showHex x)

instance Read PipelineCoverageToColorStateCreateFlagsNV where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCoverageToColorStateCreateFlagsNV")
                       v <- step readPrec
                       pure (PipelineCoverageToColorStateCreateFlagsNV v)))


type NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1


type NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"

