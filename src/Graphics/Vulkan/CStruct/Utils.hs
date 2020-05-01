{-# language CPP #-}
module Graphics.Vulkan.CStruct.Utils  ( pokeFixedLengthByteString
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

-- | An unpopulated type intended to be used as in @'Ptr' (FixedArray n a)@ to
-- indicate that the pointer points to an array of @n@ @a@s
-- data FixedArray (n :: Nat) (a :: Type)

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

