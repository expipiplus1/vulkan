{-# language CPP #-}
module Vulkan.Extensions.VK_NV_framebuffer_mixed_samples  ( PipelineCoverageModulationStateCreateInfoNV(..)
                                                          , PipelineCoverageModulationStateCreateFlagsNV(..)
                                                          , CoverageModulationModeNV( COVERAGE_MODULATION_MODE_NONE_NV
                                                                                    , COVERAGE_MODULATION_MODE_RGB_NV
                                                                                    , COVERAGE_MODULATION_MODE_ALPHA_NV
                                                                                    , COVERAGE_MODULATION_MODE_RGBA_NV
                                                                                    , ..
                                                                                    )
                                                          , NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
                                                          , pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION
                                                          , NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
                                                          , pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
                                                          ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV))
-- | VkPipelineCoverageModulationStateCreateInfoNV - Structure specifying
-- parameters controlling coverage modulation
--
-- = Description
--
-- If @coverageModulationTableEnable@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', then for each color sample the
-- associated bits of the pixel coverage are counted and divided by the
-- number of associated bits to produce a modulation factor R in the range
-- (0,1] (a value of zero would have been killed due to a color coverage of
-- 0). Specifically:
--
-- -   N = value of @rasterizationSamples@
--
-- -   M = value of 'Vulkan.Core10.Pass.AttachmentDescription'::@samples@
--     for any color attachments
--
-- -   R = popcount(associated coverage bits) \/ (N \/ M)
--
-- If @coverageModulationTableEnable@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', the value R is computed using a
-- programmable lookup table. The lookup table has N \/ M elements, and the
-- element of the table is selected by:
--
-- -   R = @pCoverageModulationTable@[popcount(associated coverage bits)-1]
--
-- Note that the table does not have an entry for popcount(associated
-- coverage bits) = 0, because such samples would have been killed.
--
-- The values of @pCoverageModulationTable@ /may/ be rounded to an
-- implementation-dependent precision, which is at least as fine as 1 \/ N,
-- and clamped to [0,1].
--
-- For each color attachment with a floating point or normalized color
-- format, each fragment output color value is replicated to M values which
-- /can/ each be modulated (multiplied) by that color sample’s associated
-- value of R. Which components are modulated is controlled by
-- @coverageModulationMode@.
--
-- If this structure is not present, it is as if @coverageModulationMode@
-- is 'COVERAGE_MODULATION_MODE_NONE_NV'.
--
-- If the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-coverage-reduction coverage reduction mode>
-- is
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.COVERAGE_REDUCTION_MODE_TRUNCATE_NV',
-- each color sample is associated with only a single coverage sample. In
-- this case, it is as if @coverageModulationMode@ is
-- 'COVERAGE_MODULATION_MODE_NONE_NV'.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineCoverageModulationStateCreateInfoNV-coverageModulationTableEnable-01405#
--     If @coverageModulationTableEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @coverageModulationTableCount@ /must/ be equal to the number of
--     rasterization samples divided by the number of color samples in the
--     subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCoverageModulationStateCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV'
--
-- -   #VUID-VkPipelineCoverageModulationStateCreateInfoNV-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineCoverageModulationStateCreateInfoNV-coverageModulationMode-parameter#
--     @coverageModulationMode@ /must/ be a valid
--     'CoverageModulationModeNV' value
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'CoverageModulationModeNV',
-- 'PipelineCoverageModulationStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCoverageModulationStateCreateInfoNV = PipelineCoverageModulationStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    flags :: PipelineCoverageModulationStateCreateFlagsNV
  , -- | @coverageModulationMode@ is a 'CoverageModulationModeNV' value
    -- controlling which color components are modulated.
    coverageModulationMode :: CoverageModulationModeNV
  , -- | @coverageModulationTableEnable@ controls whether the modulation factor
    -- is looked up from a table in @pCoverageModulationTable@.
    coverageModulationTableEnable :: Bool
  , -- | @coverageModulationTableCount@ is the number of elements in
    -- @pCoverageModulationTable@.
    coverageModulationTableCount :: Word32
  , -- | @pCoverageModulationTable@ is a table of modulation factors containing a
    -- value for each number of covered samples.
    coverageModulationTable :: Vector Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCoverageModulationStateCreateInfoNV)
#endif
deriving instance Show PipelineCoverageModulationStateCreateInfoNV

instance ToCStruct PipelineCoverageModulationStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCoverageModulationStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCoverageModulationStateCreateFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr CoverageModulationModeNV)) (coverageModulationMode)
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (coverageModulationTableEnable))
    let pCoverageModulationTableLength = Data.Vector.length $ (coverageModulationTable)
    coverageModulationTableCount'' <- lift $ if (coverageModulationTableCount) == 0
      then pure $ fromIntegral pCoverageModulationTableLength
      else do
        unless (fromIntegral pCoverageModulationTableLength == (coverageModulationTableCount) || pCoverageModulationTableLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pCoverageModulationTable must be empty or have 'coverageModulationTableCount' elements" Nothing Nothing
        pure (coverageModulationTableCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (coverageModulationTableCount'')
    pCoverageModulationTable'' <- if Data.Vector.null (coverageModulationTable)
      then pure nullPtr
      else do
        pPCoverageModulationTable <- ContT $ allocaBytesAligned @CFloat (((Data.Vector.length (coverageModulationTable))) * 4) 4
        lift $ Data.Vector.imapM_ (\i e -> poke (pPCoverageModulationTable `plusPtr` (4 * (i)) :: Ptr CFloat) (CFloat (e))) ((coverageModulationTable))
        pure $ pPCoverageModulationTable
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CFloat))) pCoverageModulationTable''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr CoverageModulationModeNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineCoverageModulationStateCreateInfoNV where
  peekCStruct p = do
    flags <- peek @PipelineCoverageModulationStateCreateFlagsNV ((p `plusPtr` 16 :: Ptr PipelineCoverageModulationStateCreateFlagsNV))
    coverageModulationMode <- peek @CoverageModulationModeNV ((p `plusPtr` 20 :: Ptr CoverageModulationModeNV))
    coverageModulationTableEnable <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    coverageModulationTableCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pCoverageModulationTable <- peek @(Ptr CFloat) ((p `plusPtr` 32 :: Ptr (Ptr CFloat)))
    let pCoverageModulationTableLength = if pCoverageModulationTable == nullPtr then 0 else (fromIntegral coverageModulationTableCount)
    pCoverageModulationTable' <- generateM pCoverageModulationTableLength (\i -> do
      pCoverageModulationTableElem <- peek @CFloat ((pCoverageModulationTable `advancePtrBytes` (4 * (i)) :: Ptr CFloat))
      pure $ (\(CFloat a) -> a) pCoverageModulationTableElem)
    pure $ PipelineCoverageModulationStateCreateInfoNV
             flags coverageModulationMode (bool32ToBool coverageModulationTableEnable) coverageModulationTableCount pCoverageModulationTable'

instance Zero PipelineCoverageModulationStateCreateInfoNV where
  zero = PipelineCoverageModulationStateCreateInfoNV
           zero
           zero
           zero
           zero
           mempty


-- | VkPipelineCoverageModulationStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'PipelineCoverageModulationStateCreateFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineCoverageModulationStateCreateInfoNV'
newtype PipelineCoverageModulationStateCreateFlagsNV = PipelineCoverageModulationStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineCoverageModulationStateCreateFlagsNV where
  showsPrec p = \case
    PipelineCoverageModulationStateCreateFlagsNV x -> showParen (p >= 11) (showString "PipelineCoverageModulationStateCreateFlagsNV 0x" . showHex x)

instance Read PipelineCoverageModulationStateCreateFlagsNV where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCoverageModulationStateCreateFlagsNV")
                       v <- step readPrec
                       pure (PipelineCoverageModulationStateCreateFlagsNV v)))


-- | VkCoverageModulationModeNV - Specify the coverage modulation mode
--
-- = See Also
--
-- 'PipelineCoverageModulationStateCreateInfoNV'
newtype CoverageModulationModeNV = CoverageModulationModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COVERAGE_MODULATION_MODE_NONE_NV' specifies that no components are
-- multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_NONE_NV = CoverageModulationModeNV 0
-- | 'COVERAGE_MODULATION_MODE_RGB_NV' specifies that the red, green, and
-- blue components are multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_RGB_NV = CoverageModulationModeNV 1
-- | 'COVERAGE_MODULATION_MODE_ALPHA_NV' specifies that the alpha component
-- is multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_ALPHA_NV = CoverageModulationModeNV 2
-- | 'COVERAGE_MODULATION_MODE_RGBA_NV' specifies that all components are
-- multiplied by the modulation factor.
pattern COVERAGE_MODULATION_MODE_RGBA_NV = CoverageModulationModeNV 3
{-# complete COVERAGE_MODULATION_MODE_NONE_NV,
             COVERAGE_MODULATION_MODE_RGB_NV,
             COVERAGE_MODULATION_MODE_ALPHA_NV,
             COVERAGE_MODULATION_MODE_RGBA_NV :: CoverageModulationModeNV #-}

instance Show CoverageModulationModeNV where
  showsPrec p = \case
    COVERAGE_MODULATION_MODE_NONE_NV -> showString "COVERAGE_MODULATION_MODE_NONE_NV"
    COVERAGE_MODULATION_MODE_RGB_NV -> showString "COVERAGE_MODULATION_MODE_RGB_NV"
    COVERAGE_MODULATION_MODE_ALPHA_NV -> showString "COVERAGE_MODULATION_MODE_ALPHA_NV"
    COVERAGE_MODULATION_MODE_RGBA_NV -> showString "COVERAGE_MODULATION_MODE_RGBA_NV"
    CoverageModulationModeNV x -> showParen (p >= 11) (showString "CoverageModulationModeNV " . showsPrec 11 x)

instance Read CoverageModulationModeNV where
  readPrec = parens (choose [("COVERAGE_MODULATION_MODE_NONE_NV", pure COVERAGE_MODULATION_MODE_NONE_NV)
                            , ("COVERAGE_MODULATION_MODE_RGB_NV", pure COVERAGE_MODULATION_MODE_RGB_NV)
                            , ("COVERAGE_MODULATION_MODE_ALPHA_NV", pure COVERAGE_MODULATION_MODE_ALPHA_NV)
                            , ("COVERAGE_MODULATION_MODE_RGBA_NV", pure COVERAGE_MODULATION_MODE_RGBA_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "CoverageModulationModeNV")
                       v <- step readPrec
                       pure (CoverageModulationModeNV v)))


type NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1


type NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"

