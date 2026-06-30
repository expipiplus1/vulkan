-- These extensions are not in the package-wide default-extensions.
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| A fixed-length array suitable as a std140\/std430 block /field/.

@'Array' n a@ is @n@ elements of @a@ laid out as an array member: each element
occupies a layout-dependent stride (std140 rounds the element stride up to a
multiple of 16; std430 packs to the element's own alignment). The element type's
'Block' instance does the per-element encoding, so anything that is a 'Block' (a
scalar, a vector, or a generated struct record) can be the element.

The backing 'VS.Vector' should hold exactly @n@ elements; 'write140' \/ 'write430'
write the first @n@ and ignore any extra (so a buffer is never overrun).
-}
module Vulkan.Utils.SpirV.Array
  ( Array (..)
  , unsafeFromList
  , toList

    -- * Runtime-sized arrays
    -- $runtime
  , std140Stride
  , std430Stride
  , pokeStd140
  , pokeStd430
  , peekStd140
  , peekStd430
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Storable qualified as VS
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Ptr.Diff (Diff (..))
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import GHC.TypeNats qualified as TN
import Graphics.Gl.Block (Block (..), roundUp)

-- | @n@ elements of @a@, laid out as an array block member.
newtype Array (n :: Nat) a = Array (VS.Vector a)

deriving instance (Storable a, Eq a) => Eq (Array n a)
deriving instance (Storable a, Show a) => Show (Array n a)

-- | Build an 'Array' from a list (should be @n@ elements long).
{-# INLINE unsafeFromList #-}
unsafeFromList :: forall n a. (KnownNat n, Storable a) => [a] -> Array n a
unsafeFromList = Array . VS.fromListN (count (Proxy @n))

-- | The elements of an 'Array' as a list.
toList :: (Storable a) => Array n a -> [a]
toList (Array v) = VS.toList v

{- | A tight, host-side 'Storable' (n contiguous elements). This is unrelated to
the gl-block layout below — it only lets an 'Array' nest as the element of
another 'Array' (multi-dimensional arrays: @'Array' h ('Array' w a)@).
-}
instance (KnownNat n, Storable a) => Storable (Array n a) where
  sizeOf _ = count (Proxy @n) * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = Array <$> VS.generateM (count (Proxy @n)) (peekElemOff (castPtr ptr))
  poke ptr (Array v) = VS.imapM_ (pokeElemOff (castPtr ptr)) (VS.take (count (Proxy @n)) v)

instance (KnownNat n, Block a, Storable a) => Block (Array n a) where
  type PackedSize (Array n a) = n TN.* PackedSize a

  isStruct _ = True

  alignment140 _ = lcm 16 (alignment140 (Proxy @a))
  alignment430 _ = alignment430 (Proxy @a)

  sizeOf140 _ = count (Proxy @n) * std140Stride (Proxy @a)
  sizeOf430 _ = count (Proxy @n) * std430Stride (Proxy @a)
  sizeOfPacked _ = count (Proxy @n) * sizeOfPacked (Proxy @a)

  read140 = readArrayWith (count (Proxy @n)) (std140Stride (Proxy @a)) read140
  read430 = readArrayWith (count (Proxy @n)) (std430Stride (Proxy @a)) read430
  readPacked = readArrayWith (count (Proxy @n)) (sizeOfPacked (Proxy @a)) readPacked

  write140 = writeArrayWith (count (Proxy @n)) (std140Stride (Proxy @a)) write140
  write430 = writeArrayWith (count (Proxy @n)) (std430Stride (Proxy @a)) write430
  writePacked = writeArrayWith (count (Proxy @n)) (sizeOfPacked (Proxy @a)) writePacked
  {-# INLINE alignment140 #-}
  {-# INLINE alignment430 #-}
  {-# INLINE isStruct #-}
  {-# INLINE read140 #-}
  {-# INLINE read430 #-}
  {-# INLINE readPacked #-}
  {-# INLINE sizeOf140 #-}
  {-# INLINE sizeOf430 #-}
  {-# INLINE write140 #-}
  {-# INLINE write430 #-}
  {-# INLINE writePacked #-}

{-# INLINE count #-}
count :: (KnownNat n) => Proxy n -> Int
count = fromIntegral . natVal

{- | Per-element stride for an array member: std140 rounds the element size up to
a multiple of 16.
-}
std140Stride :: (Block a) => Proxy a -> Int
std140Stride p = roundUp (sizeOf140 p) (lcm 16 (alignment140 p))

{- | Per-element stride for an array member: std430 packs to the element's own
base alignment.
-}
std430Stride :: (Block a) => Proxy a -> Int
std430Stride p = roundUp (sizeOf430 p) (alignment430 p)

{- $runtime
A runtime-sized array (the trailing @T[]@ of a storage buffer) has no
compile-time length, so it is /not/ a fixed-size 'Block' and is represented
directly as a @'VS.Vector' a@. These helpers (de)serialize such a vector at the
layout's element stride, using the element's own 'Block' instance — so an element
whose array stride differs from its packed size (e.g. a @vec3@, or a struct
padded up to its alignment) is still placed correctly. The 'Ptr' should point at
the start of the array (for a buffer whose only contents are the array, that is
the mapped base pointer).
-}

-- | Write a runtime-length array of std140 elements starting at @ptr@.
{-# INLINE pokeStd140 #-}
pokeStd140 :: forall a x m. (Block a, Storable a, MonadIO m) => Ptr x -> VS.Vector a -> m ()
pokeStd140 = pokeArrayAt (std140Stride (Proxy @a)) write140

-- | Write a runtime-length array of std430 elements starting at @ptr@.
{-# INLINE pokeStd430 #-}
pokeStd430 :: forall a x m. (Block a, Storable a, MonadIO m) => Ptr x -> VS.Vector a -> m ()
pokeStd430 = pokeArrayAt (std430Stride (Proxy @a)) write430

-- | Read @n@ std140 elements starting at @ptr@.
{-# INLINE peekStd140 #-}
peekStd140 :: forall a x m. (Block a, Storable a, MonadIO m) => Int -> Ptr x -> m (VS.Vector a)
peekStd140 = peekArrayAt (std140Stride (Proxy @a)) read140

-- | Read @n@ std430 elements starting at @ptr@.
{-# INLINE peekStd430 #-}
peekStd430 :: forall a x m. (Block a, Storable a, MonadIO m) => Int -> Ptr x -> m (VS.Vector a)
peekStd430 = peekArrayAt (std430Stride (Proxy @a)) read430

{-# INLINE pokeArrayAt #-}
pokeArrayAt
  :: (Storable a, MonadIO m)
  => Int
  -> (Ptr x -> Diff x a -> a -> IO ())
  -> Ptr x
  -> VS.Vector a
  -> m ()
pokeArrayAt stride wr ptr v =
  liftIO $ VS.imapM_ (\i x -> wr ptr (Diff (i * stride)) x) v

{-# INLINE peekArrayAt #-}
peekArrayAt
  :: (Storable a, MonadIO m)
  => Int
  -> (Ptr x -> Diff x a -> IO a)
  -> Int
  -> Ptr x
  -> m (VS.Vector a)
peekArrayAt stride rd n ptr =
  liftIO $ VS.generateM n (\i -> rd ptr (Diff (i * stride)))

{-# INLINE readArrayWith #-}
readArrayWith
  :: (Storable a, MonadIO m)
  => Int
  -- ^ element count
  -> Int
  -- ^ element stride
  -> (Ptr x -> Diff x a -> IO a)
  -- ^ element reader
  -> Ptr x
  -> Diff x (Array n a)
  -> m (Array n a)
readArrayWith n stride rd ptr (Diff o) =
  liftIO $ Array <$> VS.generateM n (\i -> rd ptr (Diff (o + i * stride)))

{-# INLINE writeArrayWith #-}
writeArrayWith
  :: (Storable a, MonadIO m)
  => Int
  -- ^ element count
  -> Int
  -- ^ element stride
  -> (Ptr x -> Diff x a -> a -> IO ())
  -- ^ element writer
  -> Ptr x
  -> Diff x (Array n a)
  -> Array n a
  -> m ()
writeArrayWith n stride wr ptr (Diff o) (Array v) =
  liftIO $ VS.imapM_ (\i x -> wr ptr (Diff (o + i * stride)) x) (VS.take n v)
