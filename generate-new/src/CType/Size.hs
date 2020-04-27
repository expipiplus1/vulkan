module CType.Size
  ( scanOffsets
  , roundToAlignment
  )
where

import           Polysemy
import           Polysemy.State
import           Relude                  hiding ( get
                                                , put
                                                , runState
                                                )

scanOffsets
  :: (Traversable f, Monad m)
  => (Int -> Int -> Int)
  -- ^ Member offset given member alignment and current size
  -> (Int -> Int -> Int -> Int)
  -- ^ Next size given current size, member size and offset
  -> (a -> m (Int, Int))
  -- ^ Size and alignment
  -> f a
  -> m (Int, Int, f Int)
  -- Total size, alignment and offsets for the elements
scanOffsets getOffset getSize getTypeSize ts = do
  ((unalignedSize, align), offsets) <- runM . runState (0, 1) . for ts $ \t ->
    do
      (memberSize, memberAlign) <- embed $ getTypeSize t
      (totalSize , maxAlign   ) <- get
      let newOffset = getOffset memberAlign totalSize
      put (getSize totalSize memberSize newOffset, max maxAlign memberAlign)
      pure newOffset
  pure (roundToAlignment align unalignedSize, align, offsets)

-- | Find the next multiple of an alignment
roundToAlignment
  :: Int
  -- ^ The alignment
  -> Int
  -- ^ The value to align
  -> Int
  -- ^ The next multiple of alignment
roundToAlignment alignment value =
  alignment * ((value + (alignment - 1)) `quot` alignment)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = flip traverse
