{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_viewport_swizzle"
module Vulkan.Extensions.VK_NV_viewport_swizzle  ( ViewportSwizzleNV(..)
                                                 , PipelineViewportSwizzleStateCreateInfoNV(..)
                                                 , PipelineViewportSwizzleStateCreateFlagsNV(..)
                                                 , ViewportCoordinateSwizzleNV( VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV
                                                                              , VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV
                                                                              , ..
                                                                              )
                                                 , NV_VIEWPORT_SWIZZLE_SPEC_VERSION
                                                 , pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION
                                                 , NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
                                                 , pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
                                                 ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV))

-- No documentation found for TopLevel "VkViewportSwizzleNV"
data ViewportSwizzleNV = ViewportSwizzleNV
  { -- No documentation found for Nested "VkViewportSwizzleNV" "x"
    x :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "VkViewportSwizzleNV" "y"
    y :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "VkViewportSwizzleNV" "z"
    z :: ViewportCoordinateSwizzleNV
  , -- No documentation found for Nested "VkViewportSwizzleNV" "w"
    w :: ViewportCoordinateSwizzleNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewportSwizzleNV)
#endif
deriving instance Show ViewportSwizzleNV

instance ToCStruct ViewportSwizzleNV where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewportSwizzleNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV)) (x)
    poke ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV)) (y)
    poke ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV)) (z)
    poke ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV)) (w)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV)) (zero)
    f

instance FromCStruct ViewportSwizzleNV where
  peekCStruct p = do
    x <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 0 :: Ptr ViewportCoordinateSwizzleNV))
    y <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 4 :: Ptr ViewportCoordinateSwizzleNV))
    z <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 8 :: Ptr ViewportCoordinateSwizzleNV))
    w <- peek @ViewportCoordinateSwizzleNV ((p `plusPtr` 12 :: Ptr ViewportCoordinateSwizzleNV))
    pure $ ViewportSwizzleNV
             x y z w


instance Storable ViewportSwizzleNV where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewportSwizzleNV where
  zero = ViewportSwizzleNV
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPipelineViewportSwizzleStateCreateInfoNV"
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportSwizzleStateCreateInfoNV" "flags"
    flags :: PipelineViewportSwizzleStateCreateFlagsNV
  , -- No documentation found for Nested "VkPipelineViewportSwizzleStateCreateInfoNV" "pViewportSwizzles"
    viewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportSwizzleStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportSwizzleStateCreateInfoNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportSwizzleStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineViewportSwizzleStateCreateFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewportSwizzles)) :: Word32))
    pPViewportSwizzles' <- ContT $ allocaBytesAligned @ViewportSwizzleNV ((Data.Vector.length (viewportSwizzles)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportSwizzles' `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e)) (viewportSwizzles)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV))) (pPViewportSwizzles')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPViewportSwizzles' <- ContT $ allocaBytesAligned @ViewportSwizzleNV ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportSwizzles' `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV))) (pPViewportSwizzles')
    lift $ f

instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV where
  peekCStruct p = do
    flags <- peek @PipelineViewportSwizzleStateCreateFlagsNV ((p `plusPtr` 16 :: Ptr PipelineViewportSwizzleStateCreateFlagsNV))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewportSwizzles <- peek @(Ptr ViewportSwizzleNV) ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV)))
    pViewportSwizzles' <- generateM (fromIntegral viewportCount) (\i -> peekCStruct @ViewportSwizzleNV ((pViewportSwizzles `advancePtrBytes` (16 * (i)) :: Ptr ViewportSwizzleNV)))
    pure $ PipelineViewportSwizzleStateCreateInfoNV
             flags pViewportSwizzles'

instance Zero PipelineViewportSwizzleStateCreateInfoNV where
  zero = PipelineViewportSwizzleStateCreateInfoNV
           zero
           mempty


-- No documentation found for TopLevel "VkPipelineViewportSwizzleStateCreateFlagsNV"
newtype PipelineViewportSwizzleStateCreateFlagsNV = PipelineViewportSwizzleStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineViewportSwizzleStateCreateFlagsNV :: String
conNamePipelineViewportSwizzleStateCreateFlagsNV = "PipelineViewportSwizzleStateCreateFlagsNV"

enumPrefixPipelineViewportSwizzleStateCreateFlagsNV :: String
enumPrefixPipelineViewportSwizzleStateCreateFlagsNV = ""

showTablePipelineViewportSwizzleStateCreateFlagsNV :: [(PipelineViewportSwizzleStateCreateFlagsNV, String)]
showTablePipelineViewportSwizzleStateCreateFlagsNV = []


instance Show PipelineViewportSwizzleStateCreateFlagsNV where
showsPrec = enumShowsPrec enumPrefixPipelineViewportSwizzleStateCreateFlagsNV
                          showTablePipelineViewportSwizzleStateCreateFlagsNV
                          conNamePipelineViewportSwizzleStateCreateFlagsNV
                          (\(PipelineViewportSwizzleStateCreateFlagsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineViewportSwizzleStateCreateFlagsNV where
  readPrec = enumReadPrec enumPrefixPipelineViewportSwizzleStateCreateFlagsNV
                          showTablePipelineViewportSwizzleStateCreateFlagsNV
                          conNamePipelineViewportSwizzleStateCreateFlagsNV
                          PipelineViewportSwizzleStateCreateFlagsNV


-- No documentation found for TopLevel "VkViewportCoordinateSwizzleNV"
newtype ViewportCoordinateSwizzleNV = ViewportCoordinateSwizzleNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = ViewportCoordinateSwizzleNV 0
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = ViewportCoordinateSwizzleNV 1
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = ViewportCoordinateSwizzleNV 2
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = ViewportCoordinateSwizzleNV 3
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = ViewportCoordinateSwizzleNV 4
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = ViewportCoordinateSwizzleNV 5
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = ViewportCoordinateSwizzleNV 6
-- No documentation found for Nested "VkViewportCoordinateSwizzleNV" "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
pattern VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = ViewportCoordinateSwizzleNV 7
{-# complete VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV,
             VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV,
             VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV :: ViewportCoordinateSwizzleNV #-}

conNameViewportCoordinateSwizzleNV :: String
conNameViewportCoordinateSwizzleNV = "ViewportCoordinateSwizzleNV"

enumPrefixViewportCoordinateSwizzleNV :: String
enumPrefixViewportCoordinateSwizzleNV = "VIEWPORT_COORDINATE_SWIZZLE_"

showTableViewportCoordinateSwizzleNV :: [(ViewportCoordinateSwizzleNV, String)]
showTableViewportCoordinateSwizzleNV =
  [ (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV, "POSITIVE_X_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV, "NEGATIVE_X_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV, "POSITIVE_Y_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV, "NEGATIVE_Y_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV, "POSITIVE_Z_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV, "NEGATIVE_Z_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV, "POSITIVE_W_NV")
  , (VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV, "NEGATIVE_W_NV")
  ]


instance Show ViewportCoordinateSwizzleNV where
showsPrec = enumShowsPrec enumPrefixViewportCoordinateSwizzleNV
                          showTableViewportCoordinateSwizzleNV
                          conNameViewportCoordinateSwizzleNV
                          (\(ViewportCoordinateSwizzleNV x) -> x)
                          (showsPrec 11)


instance Read ViewportCoordinateSwizzleNV where
  readPrec = enumReadPrec enumPrefixViewportCoordinateSwizzleNV
                          showTableViewportCoordinateSwizzleNV
                          conNameViewportCoordinateSwizzleNV
                          ViewportCoordinateSwizzleNV


type NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION"
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1


type NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME"
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

