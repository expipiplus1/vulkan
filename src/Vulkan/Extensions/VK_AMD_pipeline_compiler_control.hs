{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_pipeline_compiler_control"
module Vulkan.Extensions.VK_AMD_pipeline_compiler_control  ( PipelineCompilerControlCreateInfoAMD(..)
                                                           , PipelineCompilerControlFlagsAMD
                                                           , PipelineCompilerControlFlagBitsAMD(..)
                                                           , AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD))

-- No documentation found for TopLevel "VkPipelineCompilerControlCreateInfoAMD"
data PipelineCompilerControlCreateInfoAMD = PipelineCompilerControlCreateInfoAMD
  { -- No documentation found for Nested "VkPipelineCompilerControlCreateInfoAMD" "compilerControlFlags"
    compilerControlFlags :: PipelineCompilerControlFlagsAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCompilerControlCreateInfoAMD)
#endif
deriving instance Show PipelineCompilerControlCreateInfoAMD

instance ToCStruct PipelineCompilerControlCreateInfoAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCompilerControlCreateInfoAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCompilerControlFlagsAMD)) (compilerControlFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineCompilerControlCreateInfoAMD where
  peekCStruct p = do
    compilerControlFlags <- peek @PipelineCompilerControlFlagsAMD ((p `plusPtr` 16 :: Ptr PipelineCompilerControlFlagsAMD))
    pure $ PipelineCompilerControlCreateInfoAMD
             compilerControlFlags


instance Storable PipelineCompilerControlCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCompilerControlCreateInfoAMD where
  zero = PipelineCompilerControlCreateInfoAMD
           zero


type PipelineCompilerControlFlagsAMD = PipelineCompilerControlFlagBitsAMD

-- No documentation found for TopLevel "VkPipelineCompilerControlFlagBitsAMD"
newtype PipelineCompilerControlFlagBitsAMD = PipelineCompilerControlFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineCompilerControlFlagBitsAMD :: String
conNamePipelineCompilerControlFlagBitsAMD = "PipelineCompilerControlFlagBitsAMD"

enumPrefixPipelineCompilerControlFlagBitsAMD :: String
enumPrefixPipelineCompilerControlFlagBitsAMD = ""

showTablePipelineCompilerControlFlagBitsAMD :: [(PipelineCompilerControlFlagBitsAMD, String)]
showTablePipelineCompilerControlFlagBitsAMD = []


instance Show PipelineCompilerControlFlagBitsAMD where
showsPrec = enumShowsPrec enumPrefixPipelineCompilerControlFlagBitsAMD
                          showTablePipelineCompilerControlFlagBitsAMD
                          conNamePipelineCompilerControlFlagBitsAMD
                          (\(PipelineCompilerControlFlagBitsAMD x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCompilerControlFlagBitsAMD where
  readPrec = enumReadPrec enumPrefixPipelineCompilerControlFlagBitsAMD
                          showTablePipelineCompilerControlFlagBitsAMD
                          conNamePipelineCompilerControlFlagBitsAMD
                          PipelineCompilerControlFlagBitsAMD


type AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION"
pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1


type AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME = "VK_AMD_pipeline_compiler_control"

-- No documentation found for TopLevel "VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME"
pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME = "VK_AMD_pipeline_compiler_control"

