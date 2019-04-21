{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , withSomeVkStruct
  , peekVkStruct
  ) where

import Foreign.Ptr
  ( Ptr
  )





data SomeVkStruct
instance Show SomeVkStruct
instance Eq SomeVkStruct

withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a

peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
