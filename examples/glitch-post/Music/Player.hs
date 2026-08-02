{-# LANGUAGE BlockArguments #-}

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

{- | Open the default device and make a context current.

Checked handle by handle: a NULL device slips through the later calls —
'ALC.alcMakeContextCurrent' on a NULL context is a valid "clear current"
and still returns true.
-}
open :: IO (ALC.Device, ALC.Context)
open = do
  device <- ALC.alcOpenDevice Foreign.nullPtr
  when (device == ALC.nullDevice) $
    error "OpenAL: no audio device"
  context <- ALC.alcCreateContext device Foreign.nullPtr
  when (context == ALC.Context Foreign.nullPtr) $
    error "OpenAL: context creation failed"
  ok <- ALC.alcMakeContextCurrent context
  when (ok /= 1) $
    error "OpenAL: making the context current failed"
  pure (device, context)

close :: (ALC.Device, ALC.Context) -> IO ()
close (device, context) = do
  -- A current context refuses destruction; detach it first.
  void $ ALC.alcMakeContextCurrent (ALC.Context Foreign.nullPtr)
  ALC.alcDestroyContext context
  void $ ALC.alcCloseDevice device

load :: CInt -> CInt -> Foreign.Ptr a -> Int -> IO (AL.Buffer, AL.Source)
load fmt rate bufData bufSize = do
  buffer@(AL.Buffer bufId) <- Foreign.alloca \bufPtr -> do
    AL.alGenBuffers 1 bufPtr
    Foreign.peek bufPtr
  checkError "alGenBuffers"

  AL.alBufferData
    buffer
    fmt
    (Foreign.castPtr bufData)
    (fromIntegral bufSize)
    rate
  checkError "alBufferData"

  source <- Foreign.alloca \sourcePtr -> do
    AL.alGenSources 1 sourcePtr
    Foreign.peek sourcePtr
  checkError "alGenSources"

  AL.alSourcei source AL.BUFFER (fromIntegral bufId) -- XXX: unsigned to signed conversion
  checkError "alSourcei"
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

checkError :: String -> IO ()
checkError ctx = do
  err <- AL.alGetError
  when (err /= 0) $
    error $
      "OpenAL: " <> ctx <> " failed: " <> show err
