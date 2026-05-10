{-| Helpers for the headless examples: turn a CPU-mapped VMA allocation
into a 'JP.Image' and write it as PNG.
-}
module ImageReadback
  ( captureImageRGBA8
  , savePng
  ) where

import qualified Codec.Picture as JP
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Say (sayErr)
import qualified Vulkan.Core10 as Vk
import qualified VulkanMemoryAllocator as VMA

{- | Invalidate the allocation (for non-HOST_COHERENT memory) then materialize
a JuicyPixels image from the caller's per-pixel reader.
-}
captureImageRGBA8
  :: (MonadIO m)
  => VMA.Allocator
  -> VMA.Allocation
  -> Int
  -> Int
  -> (Int -> Int -> IO JP.PixelRGBA8)
  -> m (JP.Image JP.PixelRGBA8)
captureImageRGBA8 allocator allocation width height reader = do
  VMA.invalidateAllocation allocator allocation 0 Vk.WHOLE_SIZE
  liftIO (JP.withImage width height reader)

-- | @sayErr "Writing PATH"@ then write the image as PNG.
savePng :: (MonadIO m) => FilePath -> JP.Image JP.PixelRGBA8 -> m ()
savePng path img = do
  sayErr $ "Writing " <> Text.pack path
  liftIO $ BSL.writeFile path (JP.encodePng img)
