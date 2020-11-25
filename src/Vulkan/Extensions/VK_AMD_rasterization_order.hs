{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_rasterization_order"
module Vulkan.Extensions.VK_AMD_rasterization_order  ( PipelineRasterizationStateRasterizationOrderAMD(..)
                                                     , RasterizationOrderAMD( RASTERIZATION_ORDER_STRICT_AMD
                                                                            , RASTERIZATION_ORDER_RELAXED_AMD
                                                                            , ..
                                                                            )
                                                     , AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                     , pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION
                                                     , AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                     , pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD))

-- No documentation found for TopLevel "VkPipelineRasterizationStateRasterizationOrderAMD"
data PipelineRasterizationStateRasterizationOrderAMD = PipelineRasterizationStateRasterizationOrderAMD
  { -- No documentation found for Nested "VkPipelineRasterizationStateRasterizationOrderAMD" "rasterizationOrder"
    rasterizationOrder :: RasterizationOrderAMD }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationStateRasterizationOrderAMD)
#endif
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


-- No documentation found for TopLevel "VkRasterizationOrderAMD"
newtype RasterizationOrderAMD = RasterizationOrderAMD Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkRasterizationOrderAMD" "VK_RASTERIZATION_ORDER_STRICT_AMD"
pattern RASTERIZATION_ORDER_STRICT_AMD  = RasterizationOrderAMD 0
-- No documentation found for Nested "VkRasterizationOrderAMD" "VK_RASTERIZATION_ORDER_RELAXED_AMD"
pattern RASTERIZATION_ORDER_RELAXED_AMD = RasterizationOrderAMD 1
{-# complete RASTERIZATION_ORDER_STRICT_AMD,
             RASTERIZATION_ORDER_RELAXED_AMD :: RasterizationOrderAMD #-}

conNameRasterizationOrderAMD :: String
conNameRasterizationOrderAMD = "RasterizationOrderAMD"

enumPrefixRasterizationOrderAMD :: String
enumPrefixRasterizationOrderAMD = "RASTERIZATION_ORDER_"

showTableRasterizationOrderAMD :: [(RasterizationOrderAMD, String)]
showTableRasterizationOrderAMD =
  [(RASTERIZATION_ORDER_STRICT_AMD, "STRICT_AMD"), (RASTERIZATION_ORDER_RELAXED_AMD, "RELAXED_AMD")]


instance Show RasterizationOrderAMD where
showsPrec = enumShowsPrec enumPrefixRasterizationOrderAMD
                          showTableRasterizationOrderAMD
                          conNameRasterizationOrderAMD
                          (\(RasterizationOrderAMD x) -> x)
                          (showsPrec 11)


instance Read RasterizationOrderAMD where
  readPrec = enumReadPrec enumPrefixRasterizationOrderAMD
                          showTableRasterizationOrderAMD
                          conNameRasterizationOrderAMD
                          RasterizationOrderAMD


type AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION"
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1


type AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

-- No documentation found for TopLevel "VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME"
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"

