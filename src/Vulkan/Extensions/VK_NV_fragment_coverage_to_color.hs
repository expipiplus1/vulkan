{-# language CPP #-}
-- | = Name
--
-- VK_NV_fragment_coverage_to_color - device extension
--
-- == VK_NV_fragment_coverage_to_color
--
-- [__Name String__]
--     @VK_NV_fragment_coverage_to_color@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     150
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_fragment_coverage_to_color] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_fragment_coverage_to_color extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-21
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows the fragment coverage value, represented as an
-- integer bitmask, to be substituted for a color output being written to a
-- single-component color attachment with integer components (e.g.
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'). The functionality provided
-- by this extension is different from simply writing the
-- 'Vulkan.Core10.FundamentalTypes.SampleMask' fragment shader output, in
-- that the coverage value written to the framebuffer is taken after
-- stencil test and depth test, as well as after fragment operations such
-- as alpha-to-coverage.
--
-- This functionality may be useful for deferred rendering algorithms,
-- where the second pass needs to know which samples belong to which
-- original fragments.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo':
--
--     -   'PipelineCoverageToColorStateCreateInfoNV'
--
-- == New Bitmasks
--
-- -   'PipelineCoverageToColorStateCreateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME'
--
-- -   'NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2017-05-21 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PipelineCoverageToColorStateCreateFlagsNV',
-- 'PipelineCoverageToColorStateCreateInfoNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fragment_coverage_to_color  ( PipelineCoverageToColorStateCreateInfoNV(..)
                                                           , PipelineCoverageToColorStateCreateFlagsNV(..)
                                                           , NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                           , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                           , NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                           , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                           ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV))
-- | VkPipelineCoverageToColorStateCreateInfoNV - Structure specifying
-- whether fragment coverage replaces a color
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo' includes a
-- 'PipelineCoverageToColorStateCreateInfoNV' structure, then that
-- structure controls whether the fragment coverage is substituted for a
-- fragment color output and, if so, which output is replaced.
--
-- If @coverageToColorEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask coverage mask>
-- replaces the first component of the color value corresponding to the
-- fragment shader output location with @Location@ equal to
-- @coverageToColorLocation@ and @Index@ equal to zero. If the color
-- attachment format has fewer bits than the coverage mask, the low bits of
-- the sample coverage mask are taken without any clamping. If the color
-- attachment format has more bits than the coverage mask, the high bits of
-- the sample coverage mask are filled with zeros.
--
-- If @coverageToColorEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
-- these operations are skipped. If this structure is not included in the
-- @pNext@ chain, it is as if @coverageToColorEnable@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineCoverageToColorStateCreateInfoNV-coverageToColorEnable-01404#
--     If @coverageToColorEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then the render pass subpass indicated by
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     and 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@subpass@
--     /must/ have a color attachment at the location selected by
--     @coverageToColorLocation@, with a
--     'Vulkan.Core10.Enums.Format.Format' of
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCoverageToColorStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV'
--
-- -   #VUID-VkPipelineCoverageToColorStateCreateInfoNV-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'PipelineCoverageToColorStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    flags :: PipelineCoverageToColorStateCreateFlagsNV
  , -- | @coverageToColorEnable@ controls whether the fragment coverage value
    -- replaces a fragment color output.
    coverageToColorEnable :: Bool
  , -- | @coverageToColorLocation@ controls which fragment shader color output
    -- value is replaced.
    coverageToColorLocation :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCoverageToColorStateCreateInfoNV)
#endif
deriving instance Show PipelineCoverageToColorStateCreateInfoNV

instance ToCStruct PipelineCoverageToColorStateCreateInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>,
-- 'PipelineCoverageToColorStateCreateInfoNV'
newtype PipelineCoverageToColorStateCreateFlagsNV = PipelineCoverageToColorStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePipelineCoverageToColorStateCreateFlagsNV :: String
conNamePipelineCoverageToColorStateCreateFlagsNV = "PipelineCoverageToColorStateCreateFlagsNV"

enumPrefixPipelineCoverageToColorStateCreateFlagsNV :: String
enumPrefixPipelineCoverageToColorStateCreateFlagsNV = ""

showTablePipelineCoverageToColorStateCreateFlagsNV :: [(PipelineCoverageToColorStateCreateFlagsNV, String)]
showTablePipelineCoverageToColorStateCreateFlagsNV = []

instance Show PipelineCoverageToColorStateCreateFlagsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCoverageToColorStateCreateFlagsNV
      showTablePipelineCoverageToColorStateCreateFlagsNV
      conNamePipelineCoverageToColorStateCreateFlagsNV
      (\(PipelineCoverageToColorStateCreateFlagsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCoverageToColorStateCreateFlagsNV where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCoverageToColorStateCreateFlagsNV
      showTablePipelineCoverageToColorStateCreateFlagsNV
      conNamePipelineCoverageToColorStateCreateFlagsNV
      PipelineCoverageToColorStateCreateFlagsNV

type NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1


type NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME"
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = "VK_NV_fragment_coverage_to_color"

