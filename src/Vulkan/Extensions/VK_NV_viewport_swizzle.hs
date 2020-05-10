{-# language CPP #-}
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.BaseType (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV))
-- | VkViewportSwizzleNV - Structure specifying a viewport swizzle
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateInfoNV',
-- 'ViewportCoordinateSwizzleNV'
data ViewportSwizzleNV = ViewportSwizzleNV
  { -- | @x@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the x component of the primitive
    --
    -- @x@ /must/ be a valid 'ViewportCoordinateSwizzleNV' value
    x :: ViewportCoordinateSwizzleNV
  , -- | @y@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the y component of the primitive
    --
    -- @y@ /must/ be a valid 'ViewportCoordinateSwizzleNV' value
    y :: ViewportCoordinateSwizzleNV
  , -- | @z@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the z component of the primitive
    --
    -- @z@ /must/ be a valid 'ViewportCoordinateSwizzleNV' value
    z :: ViewportCoordinateSwizzleNV
  , -- | @w@ is a 'ViewportCoordinateSwizzleNV' value specifying the swizzle
    -- operation to apply to the w component of the primitive
    --
    -- @w@ /must/ be a valid 'ViewportCoordinateSwizzleNV' value
    w :: ViewportCoordinateSwizzleNV
  }
  deriving (Typeable, Eq)
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


-- | VkPipelineViewportSwizzleStateCreateInfoNV - Structure specifying
-- swizzle applied to primitive clip coordinates
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'ViewportSwizzleNV'
data PipelineViewportSwizzleStateCreateInfoNV = PipelineViewportSwizzleStateCreateInfoNV
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: PipelineViewportSwizzleStateCreateFlagsNV
  , -- | @pViewportSwizzles@ is a pointer to an array of 'ViewportSwizzleNV'
    -- structures, defining the viewport swizzles.
    --
    -- @pViewportSwizzles@ /must/ be a valid pointer to an array of
    -- @viewportCount@ valid 'ViewportSwizzleNV' structures
    viewportSwizzles :: Vector ViewportSwizzleNV
  }
  deriving (Typeable)
deriving instance Show PipelineViewportSwizzleStateCreateInfoNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportSwizzleStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineViewportSwizzleStateCreateFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewportSwizzles)) :: Word32))
    pPViewportSwizzles' <- ContT $ allocaBytesAligned @ViewportSwizzleNV ((Data.Vector.length (viewportSwizzles)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewportSwizzles' `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e) . ($ ())) (viewportSwizzles)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ViewportSwizzleNV))) (pPViewportSwizzles')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPViewportSwizzles' <- ContT $ allocaBytesAligned @ViewportSwizzleNV ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewportSwizzles' `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e) . ($ ())) (mempty)
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


-- | VkPipelineViewportSwizzleStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'PipelineViewportSwizzleStateCreateFlagsNV' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'PipelineViewportSwizzleStateCreateInfoNV'
newtype PipelineViewportSwizzleStateCreateFlagsNV = PipelineViewportSwizzleStateCreateFlagsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineViewportSwizzleStateCreateFlagsNV where
  showsPrec p = \case
    PipelineViewportSwizzleStateCreateFlagsNV x -> showParen (p >= 11) (showString "PipelineViewportSwizzleStateCreateFlagsNV 0x" . showHex x)

instance Read PipelineViewportSwizzleStateCreateFlagsNV where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineViewportSwizzleStateCreateFlagsNV")
                       v <- step readPrec
                       pure (PipelineViewportSwizzleStateCreateFlagsNV v)))


-- | VkViewportCoordinateSwizzleNV - Specify how a viewport coordinate is
-- swizzled
--
-- = Description
--
-- These values are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-viewport-swizzle Viewport Swizzle>.
--
-- = See Also
--
-- 'ViewportSwizzleNV'
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

instance Show ViewportCoordinateSwizzleNV where
  showsPrec p = \case
    VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
    VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
    VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
    VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
    VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
    VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
    VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
    VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV -> showString "VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
    ViewportCoordinateSwizzleNV x -> showParen (p >= 11) (showString "ViewportCoordinateSwizzleNV " . showsPrec 11 x)

instance Read ViewportCoordinateSwizzleNV where
  readPrec = parens (choose [("VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV", pure VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV", pure VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV", pure VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV", pure VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV", pure VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV", pure VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV", pure VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV)
                            , ("VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV", pure VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "ViewportCoordinateSwizzleNV")
                       v <- step readPrec
                       pure (ViewportCoordinateSwizzleNV v)))


type NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION"
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1


type NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

-- No documentation found for TopLevel "VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME"
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = "VK_NV_viewport_swizzle"

