{-# language CPP #-}
module Graphics.Vulkan.CStruct  ( ToCStruct(..)
                                , FromCStruct(..)
                                ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)

-- | A class for types which can be marshalled into a C style
-- structure.
class ToCStruct a where
  -- | Allocates a C type structure and all dependencies and passes
  -- it to a continuation. The space is deallocated when this
  -- continuation returns and the C type structure must not be
  -- returned out of it.
  withCStruct :: a -> (Ptr a -> IO b) -> IO b
  withCStruct x f = allocaBytesAligned (cStructSize @a) (cStructAlignment @a)
    $ \p -> pokeCStruct p x (f p)

  -- | Write a C type struct into some existing memory and run a
  -- continuation. The pointed to structure is not necessarily valid
  -- outside the continuation as additional allocations may have been
  -- made.
  pokeCStruct :: Ptr a -> a -> IO b -> IO b

  -- | Allocate space for an "empty" @a@ and populate any univalued
  -- members with their value.
  withZeroCStruct :: (Ptr a -> IO b) -> IO b
  withZeroCStruct f =
    bracket (callocBytes @a (cStructSize @a)) free $ \p -> pokeZeroCStruct p (f p)

  -- | And populate any univalued members with their value, run a
  -- function and then clean up any allocated resources.
  pokeZeroCStruct :: Ptr a -> IO b -> IO b

  -- | The size of this struct, note that this doesn't account for any
  -- extra pointed-to data
  cStructSize :: Int

  -- | The required memory alignment for this type
  cStructAlignment :: Int


-- | A class for types which can be marshalled from a C style
-- structure.
class FromCStruct a where
  -- | Read an @a@ and any other pointed to data from memory
  peekCStruct :: Ptr a -> IO a

