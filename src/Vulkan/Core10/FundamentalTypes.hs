{-# language CPP #-}
-- No documentation found for Chapter "FundamentalTypes"
module Vulkan.Core10.FundamentalTypes  ( boolToBool32
                                       , bool32ToBool
                                       , Offset2D(..)
                                       , Offset3D(..)
                                       , Extent2D(..)
                                       , Extent3D(..)
                                       , Rect2D(..)
                                       , Bool32( FALSE
                                               , TRUE
                                               , ..
                                               )
                                       , SampleMask
                                       , Flags
                                       , DeviceSize
                                       , DeviceAddress
                                       , StructureType(..)
                                       , Result(..)
                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Data.Bool (bool)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

boolToBool32 :: Bool -> Bool32
boolToBool32 = bool FALSE TRUE

bool32ToBool :: Bool32 -> Bool
bool32ToBool = \case
  FALSE -> False
  TRUE  -> True



-- No documentation found for TopLevel "VkOffset2D"
data Offset2D = Offset2D
  { -- No documentation found for Nested "VkOffset2D" "x"
    x :: Int32
  , -- No documentation found for Nested "VkOffset2D" "y"
    y :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset2D)
#endif
deriving instance Show Offset2D

instance ToCStruct Offset2D where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset2D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    pure $ Offset2D
             x y


instance Storable Offset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset2D where
  zero = Offset2D
           zero
           zero



-- No documentation found for TopLevel "VkOffset3D"
data Offset3D = Offset3D
  { -- No documentation found for Nested "VkOffset3D" "x"
    x :: Int32
  , -- No documentation found for Nested "VkOffset3D" "y"
    y :: Int32
  , -- No documentation found for Nested "VkOffset3D" "z"
    z :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset3D)
#endif
deriving instance Show Offset3D

instance ToCStruct Offset3D where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset3D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    z <- peek @Int32 ((p `plusPtr` 8 :: Ptr Int32))
    pure $ Offset3D
             x y z


instance Storable Offset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset3D where
  zero = Offset3D
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExtent2D"
data Extent2D = Extent2D
  { -- No documentation found for Nested "VkExtent2D" "width"
    width :: Word32
  , -- No documentation found for Nested "VkExtent2D" "height"
    height :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent2D)
#endif
deriving instance Show Extent2D

instance ToCStruct Extent2D where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent2D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ Extent2D
             width height


instance Storable Extent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent2D where
  zero = Extent2D
           zero
           zero



-- No documentation found for TopLevel "VkExtent3D"
data Extent3D = Extent3D
  { -- No documentation found for Nested "VkExtent3D" "width"
    width :: Word32
  , -- No documentation found for Nested "VkExtent3D" "height"
    height :: Word32
  , -- No documentation found for Nested "VkExtent3D" "depth"
    depth :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent3D)
#endif
deriving instance Show Extent3D

instance ToCStruct Extent3D where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (depth)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent3D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ Extent3D
             width height depth


instance Storable Extent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent3D where
  zero = Extent3D
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRect2D"
data Rect2D = Rect2D
  { -- No documentation found for Nested "VkRect2D" "offset"
    offset :: Offset2D
  , -- No documentation found for Nested "VkRect2D" "extent"
    extent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Rect2D)
#endif
deriving instance Show Rect2D

instance ToCStruct Rect2D where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Rect2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (extent)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct Rect2D where
  peekCStruct p = do
    offset <- peekCStruct @Offset2D ((p `plusPtr` 0 :: Ptr Offset2D))
    extent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    pure $ Rect2D
             offset extent


instance Storable Rect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Rect2D where
  zero = Rect2D
           zero
           zero


-- No documentation found for TopLevel "VkBool32"
newtype Bool32 = Bool32 Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern FALSE = Bool32 0
-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern TRUE  = Bool32 1
{-# complete FALSE,
             TRUE :: Bool32 #-}

conNameBool32 :: String
conNameBool32 = "Bool32"

enumPrefixBool32 :: String
enumPrefixBool32 = ""

showTableBool32 :: [(Bool32, String)]
showTableBool32 = [(FALSE, "FALSE"), (TRUE, "TRUE")]


instance Show Bool32 where
showsPrec = enumShowsPrec enumPrefixBool32 showTableBool32 conNameBool32 (\(Bool32 x) -> x) (showsPrec 11)


instance Read Bool32 where
  readPrec = enumReadPrec enumPrefixBool32 showTableBool32 conNameBool32 Bool32


-- No documentation found for TopLevel "VkSampleMask"
type SampleMask = Word32


-- No documentation found for TopLevel "VkFlags"
type Flags = Word32


-- No documentation found for TopLevel "VkDeviceSize"
type DeviceSize = Word64


-- No documentation found for TopLevel "VkDeviceAddress"
type DeviceAddress = Word64

