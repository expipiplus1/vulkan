{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_framebuffer_mixed_samples"
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
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

-- No documentation found for TopLevel "VkPipelineCoverageModulationStateCreateInfoNV"
data PipelineCoverageModulationStateCreateInfoNV = PipelineCoverageModulationStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "flags"
    flags :: PipelineCoverageModulationStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationMode"
    coverageModulationMode :: CoverageModulationModeNV
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableEnable"
    coverageModulationTableEnable :: Bool
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "coverageModulationTableCount"
    coverageModulationTableCount :: Word32
  , -- No documentation found for Nested "VkPipelineCoverageModulationStateCreateInfoNV" "pCoverageModulationTable"
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


-- No documentation found for TopLevel "VkPipelineCoverageModulationStateCreateFlagsNV"
newtype PipelineCoverageModulationStateCreateFlagsNV = PipelineCoverageModulationStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineCoverageModulationStateCreateFlagsNV :: String
conNamePipelineCoverageModulationStateCreateFlagsNV = "PipelineCoverageModulationStateCreateFlagsNV"

enumPrefixPipelineCoverageModulationStateCreateFlagsNV :: String
enumPrefixPipelineCoverageModulationStateCreateFlagsNV = ""

showTablePipelineCoverageModulationStateCreateFlagsNV :: [(PipelineCoverageModulationStateCreateFlagsNV, String)]
showTablePipelineCoverageModulationStateCreateFlagsNV = []


instance Show PipelineCoverageModulationStateCreateFlagsNV where
showsPrec = enumShowsPrec enumPrefixPipelineCoverageModulationStateCreateFlagsNV
                          showTablePipelineCoverageModulationStateCreateFlagsNV
                          conNamePipelineCoverageModulationStateCreateFlagsNV
                          (\(PipelineCoverageModulationStateCreateFlagsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCoverageModulationStateCreateFlagsNV where
  readPrec = enumReadPrec enumPrefixPipelineCoverageModulationStateCreateFlagsNV
                          showTablePipelineCoverageModulationStateCreateFlagsNV
                          conNamePipelineCoverageModulationStateCreateFlagsNV
                          PipelineCoverageModulationStateCreateFlagsNV


-- No documentation found for TopLevel "VkCoverageModulationModeNV"
newtype CoverageModulationModeNV = CoverageModulationModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_NONE_NV"
pattern COVERAGE_MODULATION_MODE_NONE_NV  = CoverageModulationModeNV 0
-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_RGB_NV"
pattern COVERAGE_MODULATION_MODE_RGB_NV   = CoverageModulationModeNV 1
-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
pattern COVERAGE_MODULATION_MODE_ALPHA_NV = CoverageModulationModeNV 2
-- No documentation found for Nested "VkCoverageModulationModeNV" "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
pattern COVERAGE_MODULATION_MODE_RGBA_NV  = CoverageModulationModeNV 3
{-# complete COVERAGE_MODULATION_MODE_NONE_NV,
             COVERAGE_MODULATION_MODE_RGB_NV,
             COVERAGE_MODULATION_MODE_ALPHA_NV,
             COVERAGE_MODULATION_MODE_RGBA_NV :: CoverageModulationModeNV #-}

conNameCoverageModulationModeNV :: String
conNameCoverageModulationModeNV = "CoverageModulationModeNV"

enumPrefixCoverageModulationModeNV :: String
enumPrefixCoverageModulationModeNV = "COVERAGE_MODULATION_MODE_"

showTableCoverageModulationModeNV :: [(CoverageModulationModeNV, String)]
showTableCoverageModulationModeNV =
  [ (COVERAGE_MODULATION_MODE_NONE_NV , "NONE_NV")
  , (COVERAGE_MODULATION_MODE_RGB_NV  , "RGB_NV")
  , (COVERAGE_MODULATION_MODE_ALPHA_NV, "ALPHA_NV")
  , (COVERAGE_MODULATION_MODE_RGBA_NV , "RGBA_NV")
  ]


instance Show CoverageModulationModeNV where
showsPrec = enumShowsPrec enumPrefixCoverageModulationModeNV
                          showTableCoverageModulationModeNV
                          conNameCoverageModulationModeNV
                          (\(CoverageModulationModeNV x) -> x)
                          (showsPrec 11)


instance Read CoverageModulationModeNV where
  readPrec = enumReadPrec enumPrefixCoverageModulationModeNV
                          showTableCoverageModulationModeNV
                          conNameCoverageModulationModeNV
                          CoverageModulationModeNV


type NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1


type NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"

-- No documentation found for TopLevel "VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME"
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = "VK_NV_framebuffer_mixed_samples"

