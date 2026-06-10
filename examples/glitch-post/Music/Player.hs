{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Music.Player
  ( open
  , close
  , load
  , delete
  , setLooping
  , play
  , playTime
  ) where

import Control.Monad
import qualified Foreign

import Foreign.C.Types
import qualified Sound.OpenAL.FFI.AL as AL
import qualified Sound.OpenAL.FFI.ALC as ALC

open :: IO (ALC.Device, ALC.Context)
open = do
  device <- ALC.alcOpenDevice Foreign.nullPtr
  context <- ALC.alcCreateContext device Foreign.nullPtr

  ok <- fmap (== 1) $ ALC.alcMakeContextCurrent context
  unless ok $
    error "OpenAL setup failed"
  putStrLn $ "Context OK: " <> show ok
  pure (device, context)

close :: (ALC.Device, ALC.Context) -> IO CChar
close (device, context) = do
  ALC.alcDestroyContext context
  ALC.alcCloseDevice device

load :: CInt -> CInt -> Foreign.Ptr a -> Int -> IO (AL.Buffer, AL.Source)
load fmt rate bufData bufSize = do
  buffer@(AL.Buffer bufId) <- Foreign.alloca \bufPtr -> do
    AL.alGenBuffers 1 bufPtr
    Foreign.peek bufPtr
  AL.alGetError >>= print . (buffer,)

  AL.alBufferData
    buffer
    fmt -- AL.FORMAT_MONO16
    (Foreign.castPtr bufData)
    (fromIntegral bufSize)
    rate -- 48000
  AL.alGetError >>= print . (buffer,)

  source <- Foreign.alloca \sourcePtr -> do
    AL.alGenSources 1 sourcePtr
    Foreign.peek sourcePtr
  AL.alGetError >>= print . (source,)

  AL.alSourcei source AL.BUFFER (fromIntegral bufId) -- XXX: unsigned to signed conversion
  AL.alGetError >>= print . (buffer,)
  pure (buffer, source)

delete :: (AL.Buffer, AL.Source) -> IO ()
delete (buffer, source) = do
  Foreign.with source $ AL.alDeleteSources 1
  Foreign.with buffer $ AL.alDeleteBuffers 1

setLooping :: AL.Source -> Bool -> IO ()
setLooping source loop = AL.alSourcei source AL.LOOPING (if loop then 1 else 0)

play :: AL.Source -> IO ()
play source = Foreign.with source $ AL.alSourcePlayv 1

{- | The source's playhead in buffer seconds; wraps with a looping source.
Advances in mixer-update steps (typically ~20 ms), not per sample.
-}
playTime :: AL.Source -> IO Float
playTime source = Foreign.alloca \ptr -> do
  AL.alGetSourcefv source AL.SEC_OFFSET ptr
  realToFrac <$> Foreign.peek ptr
