{-# language CPP #-}
module Graphics.Vulkan.CStruct.Utils  ( withVec
                                      , withArray
                                      , withSizedArray
                                      , byteStringToSizedVector
                                      , byteStringToNullTerminatedSizedVector
                                      , padSized
                                      , padVector
                                      , packCStringElemOff
                                      , pokeFixedLengthByteString
                                      , pokeFixedLengthNullTerminatedByteString
                                      , peekByteStringFromSizedVectorPtr
                                      , lowerArrayPtr
                                      , advancePtrBytes
                                      ) where

import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff)
import Foreign.Storable (pokeElemOff)
import GHC.Ptr (castPtr)
import Foreign.Ptr (plusPtr)
import GHC.TypeNats (natVal)
import qualified Data.ByteString (length)
import Data.ByteString (packCString)
import Data.ByteString (packCStringLen)
import Data.ByteString (take)
import Data.ByteString (unpack)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Vector (ifoldr)
import qualified Data.Vector (length)
import qualified Data.Vector.Generic ((++))
import qualified Data.Vector.Generic (empty)
import qualified Data.Vector.Generic (fromList)
import qualified Data.Vector.Generic (length)
import qualified Data.Vector.Generic (replicate)
import qualified Data.Vector.Generic (snoc)
import qualified Data.Vector.Generic (take)
import qualified Data.Vector.Generic.Sized (fromSized)
import Data.Proxy (Proxy(..))
import Foreign.C.Types (CChar(..))
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (type(<=))
import GHC.TypeNats (KnownNat)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic.Sized.Internal (Vector)
import qualified Data.Vector.Generic.Sized.Internal (Vector(..))
import qualified Data.Vector.Sized (Vector)
import qualified Data.Vector.Storable.Sized (Vector)

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

-- | A type restricted 'plusPtr'
advancePtrBytes :: Ptr a -> Int -> Ptr a
advancePtrBytes = plusPtr

