{-# language CPP #-}
-- No documentation found for Chapter "Utils"
module Vulkan.CStruct.Utils  ( pokeFixedLengthByteString
                             , pokeFixedLengthNullTerminatedByteString
                             , peekByteStringFromSizedVectorPtr
                             , callocFixedArray
                             , lowerArrayPtr
                             , advancePtrBytes
                             , FixedArray
                             ) where

import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff)
import Foreign.Storable (pokeElemOff)
import Foreign.Storable (sizeOf)
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
import Data.Proxy (Proxy(..))
import Foreign.C.Types (CChar(..))
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (type(<=))
import GHC.TypeNats (KnownNat)
import GHC.TypeNats (Nat)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector.Generic (Vector)

-- | An unpopulated type intended to be used as in @'Ptr' (FixedArray n a)@ to
-- indicate that the pointer points to an array of @n@ @a@s
data FixedArray (n :: Nat) (a :: Type)

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
  => Ptr (FixedArray n CChar)
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
  => Ptr (FixedArray n Word8)
  -> ByteString
  -> IO ()
pokeFixedLengthByteString to bs = unsafeUseAsCString bs $ \from -> do
  let maxLength = fromIntegral (natVal (Proxy @n))
      len       = min maxLength (Data.ByteString.length bs)
  copyBytes (lowerArrayPtr to) (castPtr @CChar @Word8 from) len

-- | Peek a 'ByteString' from a fixed sized array of bytes
peekByteStringFromSizedVectorPtr
  :: forall n
   . KnownNat n
  => Ptr (FixedArray n Word8)
  -> IO ByteString
peekByteStringFromSizedVectorPtr p = packCStringLen (castPtr p, fromIntegral (natVal (Proxy @n)))

-- | Allocate a zero array with the size specified by the 'FixedArray'
-- return type. Make sure to release the memory with 'free'
callocFixedArray
  :: forall n a . (KnownNat n, Storable a) => IO (Ptr (FixedArray n a))
callocFixedArray = callocBytes
  ( sizeOf (error "sizeOf evaluated its argument" :: a)
  * fromIntegral (natVal (Proxy @n))
  )

-- | Get the pointer to the first element in the array
lowerArrayPtr
  :: forall a n
   . Ptr (FixedArray n a)
  -> Ptr a
lowerArrayPtr = castPtr

-- | A type restricted 'plusPtr'
advancePtrBytes :: Ptr a -> Int -> Ptr a
advancePtrBytes = plusPtr

