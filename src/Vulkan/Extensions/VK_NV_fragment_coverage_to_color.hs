{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_fragment_coverage_to_color"
module Vulkan.Extensions.VK_NV_fragment_coverage_to_color  ( PipelineCoverageToColorStateCreateInfoNV(..)
                                                           , PipelineCoverageToColorStateCreateFlagsNV(..)
                                                           , NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                           , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION
                                                           , NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                           , pattern NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
                                                           ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV))

-- No documentation found for TopLevel "VkPipelineCoverageToColorStateCreateInfoNV"
data PipelineCoverageToColorStateCreateInfoNV = PipelineCoverageToColorStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "flags"
    flags :: PipelineCoverageToColorStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "coverageToColorEnable"
    coverageToColorEnable :: Bool
  , -- No documentation found for Nested "VkPipelineCoverageToColorStateCreateInfoNV" "coverageToColorLocation"
    coverageToColorLocation :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCoverageToColorStateCreateInfoNV)
#endif
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


-- No documentation found for TopLevel "VkPipelineCoverageToColorStateCreateFlagsNV"
newtype PipelineCoverageToColorStateCreateFlagsNV = PipelineCoverageToColorStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineCoverageToColorStateCreateFlagsNV :: String
conNamePipelineCoverageToColorStateCreateFlagsNV = "PipelineCoverageToColorStateCreateFlagsNV"

enumPrefixPipelineCoverageToColorStateCreateFlagsNV :: String
enumPrefixPipelineCoverageToColorStateCreateFlagsNV = ""

showTablePipelineCoverageToColorStateCreateFlagsNV :: [(PipelineCoverageToColorStateCreateFlagsNV, String)]
showTablePipelineCoverageToColorStateCreateFlagsNV = []


instance Show PipelineCoverageToColorStateCreateFlagsNV where
showsPrec = enumShowsPrec enumPrefixPipelineCoverageToColorStateCreateFlagsNV
                          showTablePipelineCoverageToColorStateCreateFlagsNV
                          conNamePipelineCoverageToColorStateCreateFlagsNV
                          (\(PipelineCoverageToColorStateCreateFlagsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCoverageToColorStateCreateFlagsNV where
  readPrec = enumReadPrec enumPrefixPipelineCoverageToColorStateCreateFlagsNV
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

