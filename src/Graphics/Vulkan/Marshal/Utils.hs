{-# language Strict #-}
{-# language CPP #-}
{-# language FunctionalDependencies #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Marshal.Utils
  ( withVec
  , withArray
  , withSizedArray
  , byteStringToSizedVector
  , byteStringToNullTerminatedSizedVector
  , padSized
  , padVector
  , packCStringElemOff
  , peekVkStruct
  ) where

import Data.ByteString
  ( ByteString
  , packCString
  , take
  , unpack
  )
import Data.Proxy
  ( Proxy(Proxy)
  )
import Data.Vector
  ( Vector
  , ifoldr
  )
import qualified Data.Vector
  ( length
  )
import qualified Data.Vector.Generic
  ( (++)
  , Vector
  , empty
  , fromList
  , length
  , replicate
  , snoc
  , take
  )
import qualified Data.Vector.Generic.Sized
  ( Vector
  , fromSized
  )
import qualified Data.Vector.Generic.Sized.Internal
  ( Vector(Vector)
  )
import qualified Data.Vector.Sized
  ( Vector
  )
import Data.Word
  ( Word8
  )
import Foreign.C.Types
  ( CChar(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Ptr
  ( Ptr
  )
import Foreign.Storable
  ( Storable
  , peekElemOff
  , pokeElemOff
  )
import GHC.TypeNats
  ( KnownNat
  , natVal
  , type (<=)
  )


import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( peekVkStruct
  )


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
  -> Data.Vector.Generic.Sized.Vector v n a
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
  -> Data.Vector.Generic.Sized.Vector v n CChar
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
  -> Data.Vector.Generic.Sized.Vector v n Word8
byteStringToSizedVector bs = padSized
  0
  (byteStringToVector (Data.ByteString.take n bs))
  where
    n                  = fromIntegral (natVal (Proxy @n))
    byteStringToVector = Data.Vector.Generic.fromList . Data.ByteString.unpack

