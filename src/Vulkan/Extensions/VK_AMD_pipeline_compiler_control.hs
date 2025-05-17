{-# language CPP #-}
-- | = Name
--
-- VK_AMD_pipeline_compiler_control - device extension
--
-- == VK_AMD_pipeline_compiler_control
--
-- [__Name String__]
--     @VK_AMD_pipeline_compiler_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     184
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_pipeline_compiler_control] @anteru%0A*Here describe the issue or question you have about the VK_AMD_pipeline_compiler_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension introduces 'PipelineCompilerControlCreateInfoAMD'
-- structure that can be chained to a pipeline’s creation information to
-- specify additional flags that affect pipeline compilation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.ExecutionGraphPipelineCreateInfoAMDX':
--
--     -   'PipelineCompilerControlCreateInfoAMD'
--
-- == New Enums
--
-- -   'PipelineCompilerControlFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'PipelineCompilerControlFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME'
--
-- -   'AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-07-26 (Tobias Hector)
--
--     -   Initial revision.
--
-- == See Also
--
-- 'PipelineCompilerControlCreateInfoAMD',
-- 'PipelineCompilerControlFlagBitsAMD', 'PipelineCompilerControlFlagsAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_pipeline_compiler_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_pipeline_compiler_control  ( PipelineCompilerControlCreateInfoAMD(..)
                                                           , PipelineCompilerControlFlagsAMD
                                                           , PipelineCompilerControlFlagBitsAMD(..)
                                                           , AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION
                                                           , AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
                                                           , pattern AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD))
-- | VkPipelineCompilerControlCreateInfoAMD - Structure used to pass
-- compilation control flags to a pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_pipeline_compiler_control VK_AMD_pipeline_compiler_control>,
-- 'PipelineCompilerControlFlagsAMD',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCompilerControlCreateInfoAMD = PipelineCompilerControlCreateInfoAMD
  { -- | @compilerControlFlags@ is a bitmask of
    -- 'PipelineCompilerControlFlagBitsAMD' affecting how the pipeline will be
    -- compiled.
    --
    -- #VUID-VkPipelineCompilerControlCreateInfoAMD-compilerControlFlags-zerobitmask#
    -- @compilerControlFlags@ /must/ be @0@
    compilerControlFlags :: PipelineCompilerControlFlagsAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCompilerControlCreateInfoAMD)
#endif
deriving instance Show PipelineCompilerControlCreateInfoAMD

instance ToCStruct PipelineCompilerControlCreateInfoAMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

-- | VkPipelineCompilerControlFlagBitsAMD - Enum specifying available
-- compilation control flags
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_pipeline_compiler_control VK_AMD_pipeline_compiler_control>,
-- 'PipelineCompilerControlFlagsAMD'
newtype PipelineCompilerControlFlagBitsAMD = PipelineCompilerControlFlagBitsAMD Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePipelineCompilerControlFlagBitsAMD :: String
conNamePipelineCompilerControlFlagBitsAMD = "PipelineCompilerControlFlagBitsAMD"

enumPrefixPipelineCompilerControlFlagBitsAMD :: String
enumPrefixPipelineCompilerControlFlagBitsAMD = ""

showTablePipelineCompilerControlFlagBitsAMD :: [(PipelineCompilerControlFlagBitsAMD, String)]
showTablePipelineCompilerControlFlagBitsAMD = []

instance Show PipelineCompilerControlFlagBitsAMD where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCompilerControlFlagBitsAMD
      showTablePipelineCompilerControlFlagBitsAMD
      conNamePipelineCompilerControlFlagBitsAMD
      (\(PipelineCompilerControlFlagBitsAMD x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCompilerControlFlagBitsAMD where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCompilerControlFlagBitsAMD
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

