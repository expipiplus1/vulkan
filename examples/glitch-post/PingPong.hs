{-| The two frames in flight: one recording on the CPU while the other runs on
the GPU.

Whatever a frame writes and the next frame must not stomp lives in a 'PingPong'
— the camera slots, the particle state buffers, the descriptor sets binding them
— and is picked with the 'Slot' of the frame being recorded.
-}
module PingPong
  ( PingPong (..)
  , twice
  , Slot
  , slot
  , slots
  , current
  , previous
  ) where

data PingPong a = PingPong a a
  deriving (Functor, Foldable, Traversable)

-- | Zips the halves.
instance Applicative PingPong where
  pure a = PingPong a a
  PingPong f g <*> PingPong a b = PingPong (f a) (g b)

-- | Run an allocation once per slot.
twice :: (Applicative f) => f a -> f (PingPong a)
twice action = PingPong <$> action <*> action

data Slot = Ping | Pong

-- | The slot of the @n@th submitted frame.
slot :: Int -> Slot
slot n = if even n then Ping else Pong

-- | Each slot in its own half, for building a 'PingPong' slot by slot.
slots :: PingPong Slot
slots = PingPong Ping Pong

-- | The half this frame writes.
current :: Slot -> PingPong a -> a
current Ping (PingPong a _) = a
current Pong (PingPong _ b) = b

-- | The half the previous frame wrote.
previous :: Slot -> PingPong a -> a
previous Ping = current Pong
previous Pong = current Ping
