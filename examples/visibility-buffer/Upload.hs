{-# LANGUAGE ScopedTypeVariables #-}

{-| Small-buffer uploads via 'Vk.cmdUpdateBuffer'.

The stride comes from the element's 'Storable' instance, so it can't drift from the
struct it writes.
-}
module Upload
  ( slice
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import UnliftIO.Foreign (withArrayLen)
import qualified Vulkan.Core10 as Vk

{- | Write @xs@ into @buffer@ starting at element @firstIndex@.

Vulkan rejects a zero @dataSize@, so an empty list is a no-op. @dataSize@ also caps at
65536 bytes; bulk data needs a staging copy instead.
-}
slice :: forall m a. (MonadIO m, Storable a) => Vk.CommandBuffer -> Vk.Buffer -> Word32 -> [a] -> m ()
slice cb buffer firstIndex xs =
  unless (null xs) $
    liftIO $ withArrayLen xs \n p ->
      Vk.cmdUpdateBuffer cb buffer (fromIntegral firstIndex * stride) (fromIntegral n * stride) (castPtr p)
  where
    stride = fromIntegral (sizeOf (undefined :: a))
