{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_pipeline_compiler_control  ( PipelineCompilerControlCreateInfoAMD(..)
                                                           , PipelineCompilerControlFlagBitsAMD(..)
                                                           , PipelineCompilerControlFlagsAMD
                                                           , AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD))
-- | VkPipelineCompilerControlCreateInfoAMD - Structure used to pass
-- compilation control flags to a pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineCompilerControlFlagsAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCompilerControlCreateInfoAMD = PipelineCompilerControlCreateInfoAMD
  { -- | @compilerControlFlags@ is a bitmask of
    -- 'PipelineCompilerControlFlagBitsAMD' affecting how the pipeline will be
    -- compiled.
    --
    -- @compilerControlFlags@ /must/ be @0@
    compilerControlFlags :: PipelineCompilerControlFlagsAMD }
  deriving (Typeable, Eq)
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


-- | VkPipelineCompilerControlFlagBitsAMD - Enum specifying available
-- compilation control flags
--
-- = See Also
--
-- 'PipelineCompilerControlFlagsAMD'
newtype PipelineCompilerControlFlagBitsAMD = PipelineCompilerControlFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



type PipelineCompilerControlFlagsAMD = PipelineCompilerControlFlagBitsAMD

instance Show PipelineCompilerControlFlagBitsAMD where
  showsPrec p = \case
    PipelineCompilerControlFlagBitsAMD x -> showParen (p >= 11) (showString "PipelineCompilerControlFlagBitsAMD 0x" . showHex x)

instance Read PipelineCompilerControlFlagBitsAMD where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCompilerControlFlagBitsAMD")
                       v <- step readPrec
                       pure (PipelineCompilerControlFlagBitsAMD v)))


type AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION"
pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1


type AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME = "VK_AMD_pipeline_compiler_control"

-- No documentation found for TopLevel "VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME"
pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME = "VK_AMD_pipeline_compiler_control"

