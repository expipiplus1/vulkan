{-# language Strict #-}
{-# language CPP #-}
{-# language FunctionalDependencies #-}
{-# language DefaultSignatures #-}

module Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , withSomeVkStruct
  , FromCStruct(..)
  , HasNext(..)
  , peekVkStruct
  ) where

import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Ptr
  ( Ptr
  )
import Foreign.Storable
  ( Storable
  )





data SomeVkStruct
instance Show SomeVkStruct
instance Eq SomeVkStruct

withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a

class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
  fromCStruct :: c -> IO marshalled

class HasNext a where
  getNext :: a -> Maybe SomeVkStruct

peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
