{-# language QuasiQuotes #-}
module Bespoke.Utils
  ( marshalUtils
  , zeroClass
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                )
import           Polysemy
import           Foreign.Ptr
import           Foreign.C.Types
import           Text.InterpolatedString.Perl6.Unindented

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BS
import qualified Data.Vector                   as V
import qualified Data.Vector.Sized             as VS
import qualified Data.Vector.Storable.Sized    as VSS
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Sized     as VGS
import qualified Data.Vector.Generic.Sized.Internal
                                               as VGSI
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           GHC.TypeNats

import           Render.Element
import           Error
import           Haskell.Name

zeroClass :: (HasErr r, HasRenderParams r) => Sem r RenderElement
zeroClass = genRe "zero class" $ do
  traverseV_
    tellImport
    [ 'nullPtr
    , 'nullFunPtr
    , ''CFloat
    , ''CChar
    , ''CSize
    , ''CInt
    , ''Int8
    , ''Int16
    , ''Int32
    , ''Int64
    , ''Word8
    , ''Word16
    , ''Word32
    , ''Word64
    , ''KnownNat
    , ''Storable
    , ''FunPtr
    , ''Ptr
    ]

  traverseV_ tellQualImport [''VSS.Vector, 'VSS.replicate]

  tellExport (EClass (TyConName "Zero"))
  tellExplicitModule (ModName "Graphics.Vulkan.CStruct")

  tellDoc [qi|
    -- | A class for initializing things with all zero data
    --
    -- Any instance should satisfy the following law:
    --
    -- @ new zero = calloc @ or @ with zero = withZeroCStruct @
    --
    -- i.e. Marshaling @zero@ to memory yeilds only zero-valued bytes, except
    -- for structs which require a "type" tag
    --
    class Zero a where
      zero :: a

    instance (KnownNat n, Storable a, Zero a) => Zero (Data.Vector.Storable.Sized.Vector n a) where
      zero = Data.Vector.Storable.Sized.replicate zero

    instance Zero (FunPtr a) where
      zero = nullFunPtr

    instance Zero (Ptr a) where
      zero = nullPtr

    instance Zero Int8 where
      zero = 0

    instance Zero Int16 where
      zero = 0

    instance Zero Int32 where
      zero = 0

    instance Zero Int64 where
      zero = 0

    instance Zero Word8 where
      zero = 0

    instance Zero Word16 where
      zero = 0

    instance Zero Word32 where
      zero = 0

    instance Zero Word64 where
      zero = 0

    instance Zero Float where
      zero = 0

    instance Zero CFloat where
      zero = 0

    instance Zero CChar where
      zero = 0

    instance Zero CSize where
      zero = 0

    instance Zero CInt where
      zero = 0
    |]

marshalUtils :: (HasErr r, HasRenderParams r) => Sem r RenderElement
marshalUtils = genRe "marshal utils" $ do
  tellExplicitModule (ModName "Graphics.Vulkan.CStruct.Utils")
  traverseV_ tellImportWithAll [''Proxy, ''CChar]
  traverseV_
    tellImport
    [ ''Word8
    , ''BS.ByteString
    , 'BS.take
    , 'BS.unpack
    , 'BS.packCString
    , 'BS.packCStringLen
    , 'BS.unsafeUseAsCString
    , ''V.Vector
    , 'V.ifoldr
    , 'allocaArray
    , 'copyBytes
    , ''Ptr
    , 'castPtr
    , ''Storable
    , 'pokeElemOff
    , 'peekElemOff
    , 'natVal
    , ''KnownNat
    , ''(<=)
    , 'natVal
    ]

  traverseV_
    tellQualImport
    [ 'V.length
    , 'VG.length
    , 'VG.take
    , 'VG.replicate
    , 'VG.fromList
    , ''VG.Vector
    , '(VG.++)
    , 'VG.snoc
    , 'VG.empty
    , ''VGS.Vector
    , 'VGS.fromSized
    , ''VS.Vector
    , 'BS.length
    , ''VSS.Vector
    ]

  tellQualImportWithAll ''VGSI.Vector

  traverseV_
    (tellExport . ETerm . TermName)
    [ "withVec"
    , "withArray"
    , "withSizedArray"
    , "byteStringToSizedVector"
    , "byteStringToNullTerminatedSizedVector"
    , "padSized"
    , "padVector"
    , "packCStringElemOff"
    , "pokeFixedLengthByteString"
    , "pokeFixedLengthNullTerminatedByteString"
    , "peekByteStringFromSizedVectorPtr"
    , "lowerArrayPtr"
    ]

  tellDoc [qi|
    packCStringElemOff :: Ptr (Ptr CChar) -> Int -> IO ByteString
    packCStringElemOff p o = packCString =<< peekElemOff p o

    withArray
      :: forall a b d
       . (a -> (b -> IO d) -> IO d)
      -> Vector a
      -> (Vector b -> IO d)
      -> IO d
    withArray alloc v cont =
      let go :: a -> (Vector b -> IO d) -> (Vector b -> IO d)
          go x complete bs = alloc x (\b -> complete (Data.Vector.Generic.snoc bs b))
      in  foldr go cont v (Data.Vector.Generic.empty)

    withSizedArray
      :: forall a b d n
       . (a -> (b -> IO d) -> IO d)
      -> Data.Vector.Sized.Vector n a
      -> (Data.Vector.Sized.Vector n b -> IO d)
      -> IO d
    withSizedArray alloc v cont = withArray
      alloc
      (Data.Vector.Generic.Sized.fromSized v)
      (cont . Data.Vector.Generic.Sized.Internal.Vector)

    withVec
      :: forall a b d
       . Storable b
      => (a -> (b -> IO d) -> IO d)
      -> Vector a
      -> (Ptr b -> IO d)
      -> IO d
    withVec alloc v cont = allocaArray (Data.Vector.length v) $ \p ->
      let go :: Int -> a -> IO d -> IO d
          go index x complete = alloc x (\b -> pokeElemOff p index b *> complete)
      in  ifoldr go (cont p) v

    -- | Pad or truncate a vector so that it has the required size
    padSized
      :: forall n a v
       . (KnownNat n, Data.Vector.Generic.Vector v a)
      => a
      -- ^ The value with which to pad if the given vector is too short
      -> v a
      -- ^ The vector to pad or truncate
      -> Data.Vector.Generic.Sized.Internal.Vector v n a
    padSized p v = Data.Vector.Generic.Sized.Internal.Vector padded
      where
        padded :: v a
        padded = let n = fromIntegral (natVal (Proxy @n))
                 in padVector p n v

    -- | Make sure a vector is at least a certain length
    padVector
      :: (Data.Vector.Generic.Vector v a)
      => a
      -> Int
      -> v a
      -> v a
    padVector p n v =
      let m = Data.Vector.Generic.length v
      in case m `compare` n of
           LT -> v Data.Vector.Generic.++ (Data.Vector.Generic.replicate (n - m) p)
           EQ -> v
           GT -> Data.Vector.Generic.take n v

    -- | Convert a bytestring to a null terminated sized vector. If the bytestring
    -- is too long it will be truncated.
    byteStringToNullTerminatedSizedVector
      :: forall n v
       . (KnownNat n, 1 <= n, Data.Vector.Generic.Vector v CChar)
      => ByteString
      -> Data.Vector.Generic.Sized.Internal.Vector v n CChar
    byteStringToNullTerminatedSizedVector bs = padSized
      (CChar 0)
      (byteStringToVector (Data.ByteString.take predN bs))
      where
        predN = pred (fromIntegral (natVal (Proxy @n)))
        byteStringToVector =
          Data.Vector.Generic.fromList . fmap fromIntegral . Data.ByteString.unpack

    -- | Convert a bytestring to a sized vector. If the bytestring is too
    -- long it will be truncated. If it is too short it will be zero padded
    byteStringToSizedVector
      :: forall n v
       . (KnownNat n, Data.Vector.Generic.Vector v Word8)
      => ByteString
      -> Data.Vector.Generic.Sized.Internal.Vector v n Word8
    byteStringToSizedVector bs = padSized
      0
      (byteStringToVector (Data.ByteString.take n bs))
      where
        n                  = fromIntegral (natVal (Proxy @n))
        byteStringToVector = Data.Vector.Generic.fromList . Data.ByteString.unpack

    -- | Store a 'ByteString' in a fixed amount of space inserting a null
    -- character at the end and truncating if necessary.
    --
    -- If the 'ByteString' is not long enough to fill the space the remaining
    -- bytes are unchanged
    --
    -- Note that if the 'ByteString' is exactly long enough the last byte will
    -- still be replaced with 0
    pokeFixedLengthNullTerminatedByteString
      :: forall n
       . KnownNat n
      => Ptr (Data.Vector.Storable.Sized.Vector n CChar)
      -> ByteString
      -> IO ()
    pokeFixedLengthNullTerminatedByteString to bs =
      unsafeUseAsCString bs $ \from -> do
        let maxLength = fromIntegral (natVal (Proxy @n))
            len       = min maxLength (Data.ByteString.length bs)
            end       = min (maxLength - 1) len
        -- Copy the entire string into the buffer
        copyBytes (lowerArrayPtr to) from len
        -- Make the last byte (the one following the string, or the
        -- one at the end of the buffer)
        pokeElemOff (lowerArrayPtr to) end 0

    -- | Store a 'ByteString' in a fixed amount of space, truncating if necessary.
    --
    -- If the 'ByteString' is not long enough to fill the space the remaining
    -- bytes are unchanged
    pokeFixedLengthByteString
      :: forall n
       . KnownNat n
      => Ptr (Data.Vector.Storable.Sized.Vector n Word8)
      -> ByteString
      -> IO ()
    pokeFixedLengthByteString to bs = unsafeUseAsCString bs $ \from -> do
      let maxLength = fromIntegral (natVal (Proxy @n))
          len       = min maxLength (Data.ByteString.length bs)
      copyBytes (lowerArrayPtr to) (castPtr @CChar @Word8 from) len

    peekByteStringFromSizedVectorPtr
      :: forall n
       . KnownNat n
      => Ptr (Data.Vector.Storable.Sized.Vector n Word8)
      -> IO ByteString
    peekByteStringFromSizedVectorPtr p = packCStringLen (castPtr p, fromIntegral (natVal (Proxy @n)))

    -- | Get the pointer to the first element in the array
    lowerArrayPtr
      :: forall a n
       . Ptr (Data.Vector.Storable.Sized.Vector n a)
      -> Ptr a
    lowerArrayPtr = castPtr
  |]

