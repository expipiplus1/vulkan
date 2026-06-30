{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-| A block of memory tagged with a type-level layout signature.

A @'Buffer' sig@ is host memory whose byte layout is described by @sig@ (a
'SigOffsetMap' from "Vulkan.Utils.SpirV.Signature"). 'readBuffer' \/ 'writeBuffer'
move whole records in and out, but only for a record whose own layout 'Fits' the
buffer's signature — so a layout mismatch is a /compile/ error, and because
layout-equivalent records share a signature (and 'Fits' accepts a compatible one),
a value written as one record can be read back through any layout-compatible record
(a structural view).

@
buf <- 'newBuffer' (MyUbo …)         -- Buffer (Sig MyUbo)
view <- 'readBuffer' buf             -- any r with Fits (Sig r) (Sig MyUbo)
@

The signature is the soundness boundary: 'newBuffer' derives it from the record
it writes (sound by construction); 'unsafeAsBuffer' tags a foreign pointer you
already hold (e.g. VMA @mappedData@) with a signature /you/ assert it satisfies —
that assertion is exactly the reflected buffer's layout.
-}
module Vulkan.Utils.SpirV.Buffer
  ( Buffer
  , newBuffer
  , allocBuffer
  , allocArrayBuffer
  , readBuffer
  , writeBuffer
  , readBufferElem
  , writeBufferElem
  , bufferSize
  , withBufferPtr
  , unsafeAsBuffer
  , unsafeAsBufferPtr
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, mallocForeignPtrBytes, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))

import Vulkan.Utils.SpirV.Layout qualified -- brings OffsetMap's fields into scope for HasField/ORD
import Vulkan.Utils.SpirV.Signature (Fits, FitsTail, KnownArrayTail, KnownLayout, KnownSigOffsetMap, Sig, SigOffsetMap, arrayTailBaseStride, sigOffsetMapVal)

-- | Host memory laid out as @sig@.
newtype Buffer (sig :: SigOffsetMap) = Buffer (ForeignPtr Word8)

-- | Allocate and initialize a buffer from a record, taking its layout signature.
newBuffer :: forall r io. (KnownLayout r, Storable r, MonadIO io) => r -> io (Buffer (Sig r))
newBuffer r = liftIO $ do
  fp <- mallocForeignPtrBytes (sizeOf r)
  withForeignPtr fp $ \p -> poke (castPtr p) r
  pure (Buffer fp)

{- | Allocate an uninitialized buffer sized for the (statically-sized) signature
@sig@; use with a type application, e.g. @allocBuffer \@(Sig MyUbo)@.
-}
allocBuffer :: forall sig io. (KnownSigOffsetMap sig, MonadIO io) => io (Buffer sig)
allocBuffer = liftIO $ case sigSize @sig of
  Just n -> Buffer <$> mallocForeignPtrBytes n
  Nothing ->
    ioError (userError "allocBuffer: runtime-sized layout has no static size")

-- | Read a record out of a buffer, if its layout fits the buffer's signature.
readBuffer :: forall r sig io. (Storable r, Fits (Sig r) sig, MonadIO io) => Buffer sig -> io r
readBuffer (Buffer fp) = liftIO (withForeignPtr fp (peek . castPtr))

-- | Write a record into a buffer, if its layout fits the buffer's signature.
writeBuffer :: forall r sig io. (Storable r, Fits (Sig r) sig, MonadIO io) => Buffer sig -> r -> io ()
writeBuffer (Buffer fp) r = liftIO (withForeignPtr fp (\p -> poke (castPtr p) r))

{- | Allocate a runtime-array buffer (an 'ArrayOf' signature) sized for @n@
elements: @base + n * stride@ bytes, both taken from the signature's tail.
-}
allocArrayBuffer :: forall sig io. (KnownArrayTail sig, MonadIO io) => Int -> io (Buffer sig)
allocArrayBuffer n = liftIO (Buffer <$> mallocForeignPtrBytes (base + n * stride))
  where
    (base, stride) = arrayTailBaseStride @sig

{- | Read the @i@-th element of a runtime-array buffer (an SSBO @T[]@), if the
record's layout fits the buffer's array tail. The element is at byte offset
@base + i * stride@ taken from the buffer's signature.
-}
readBufferElem :: forall r sig io. (Storable r, FitsTail (Sig r) sig, KnownArrayTail sig, MonadIO io) => Buffer sig -> Int -> io r
readBufferElem (Buffer fp) i =
  liftIO (withForeignPtr fp (\p -> peek (castPtr (p `plusPtr` elemOffset @sig i))))

{- | Write the @i@-th element of a runtime-array buffer, if the record's layout
fits the buffer's array tail.
-}
writeBufferElem :: forall r sig io. (Storable r, FitsTail (Sig r) sig, KnownArrayTail sig, MonadIO io) => Buffer sig -> Int -> r -> io ()
writeBufferElem (Buffer fp) i r =
  liftIO (withForeignPtr fp (\p -> poke (castPtr (p `plusPtr` elemOffset @sig i)) r))

{- | The byte offset of array element @i@ from the buffer signature's tail.

'KnownArrayTail' provides the @(base, stride)@ from the type level.
-}
elemOffset :: forall sig. (KnownArrayTail sig) => Int -> Int
elemOffset i = base + i * stride
  where
    (base, stride) = arrayTailBaseStride @sig

-- | The buffer's size in bytes, from its signature ('Nothing' if open-ended).
bufferSize :: forall sig. (KnownSigOffsetMap sig) => Buffer sig -> Maybe Int
bufferSize _ = sigSize @sig

-- | The static byte size of a signature, if it is not runtime-sized.
sigSize :: forall sig. (KnownSigOffsetMap sig) => Maybe Int
sigSize = (sigOffsetMapVal (Proxy @sig)).size

-- | Run an action on the raw buffer pointer (e.g. to upload to a GPU buffer).
withBufferPtr :: (MonadIO io) => Buffer sig -> (Ptr Word8 -> IO a) -> io a
withBufferPtr (Buffer fp) k = liftIO (withForeignPtr fp k)

{- | Tag a foreign pointer with a layout signature you assert it satisfies. The
safety obligation is the caller's: @sig@ must match the memory's real layout
(for a reflected GPU buffer, its reflected block layout).
-}
unsafeAsBuffer :: ForeignPtr a -> Buffer sig
unsafeAsBuffer = Buffer . castForeignPtr

{- | Tag a raw pointer (e.g. VMA @mappedData@) with an asserted layout signature.
The pointer's memory is owned elsewhere — no finalizer is attached, so the
caller must keep it alive for the buffer's lifetime. Same obligation as
'unsafeAsBuffer'.
-}
unsafeAsBufferPtr :: (MonadIO io) => Ptr a -> io (Buffer sig)
unsafeAsBufferPtr p = liftIO (Buffer . castForeignPtr <$> newForeignPtr_ p)
