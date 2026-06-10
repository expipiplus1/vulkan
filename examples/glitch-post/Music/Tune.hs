{-| A few-minute looping cracktro tune in C minor.

Timing: row_len 4900 samples = 16th notes at ~135 BPM.
A pattern is 32 rows = 2 bars. 48 sequence steps ~ 2:51 per loop.

Harmony: main loop Cm-Ab-Eb-Bb (i-VI-III-VII) over two patterns
(2 chords per pattern, 16 rows each), bridge Fm-Cm-Ab-G (iv-i-VI-V).

Arrangement (sequence steps):
  1-4   intro: pad + arp + echo blips
  5-8   bass and kick enter
  9-16  melody A, full kit
  17-20 break: lead rests, blips return, snare build
  21-28 melody B, busy hats
  29-32 bridge: blips over bridge chords, drums build back
  33-40 melody A reprise
  41-44 melody B reprise
  45-48 outro: strip back down to pad + arp + blips, loops into intro
-}
module Music.Tune (withTune) where

import Sound.PlSynth
  ( PlSynthT (..)
  , emptyPlSynthT
  , withPlSynth
  , withSong
  , withSongTracks
  , pattern SAW
  , pattern SIN
  , pattern SQR
  , pattern TRI
  )

import Control.Monad (forM_, unless, when)
import Data.Int (Int16)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, peekElemOff, pokeElemOff, sizeOf)

withTune :: (Ptr Word8 -> Int -> IO r) -> IO r
withTune action = do
  forM_ tracks \(name, (sq, pats, _synth)) -> do
    unless (length sq == steps) $
      error $
        name <> ": sequence length " <> show (length sq) <> " /= " <> show steps
    unless (all ((<= 32) . length) pats) $
      error $
        name <> ": pattern longer than 32 rows"
    unless (all ((<= length pats) . fromIntegral) sq) $
      error $
        name <> ": sequence refers to a missing pattern"
  -- withPlSynth fills the generator's static wavetables; rendering
  -- without it dereferences NULL.
  withPlSynth $ withSongTracks 4900 (map snd tracks) \song ->
    withSong song \ptr len -> do
      -- The render runs past the sequence grid by the last notes' release
      -- and delay tails (pl_synth_song_len adds pl_synth_instrument_len),
      -- but the loop point is the grid end — looping the raw buffer plays
      -- a few beats long per pass. Fold the tail back onto the start,
      -- which is exactly what a looping tracker would play, and hand the
      -- action one grid-exact loop.
      let
        loopBytes = steps * 32 * 4900 * 2 * sizeOf (0 :: Int16)
        samples = castPtr ptr :: Ptr Int16
        grid = loopBytes `div` 2
      when (len > loopBytes) $
        forM_ [0 .. min grid ((len - loopBytes) `div` 2) - 1] \i -> do
          a <- peekElemOff samples i
          b <- peekElemOff samples (grid + i)
          let s = fromIntegral a + fromIntegral b :: Int
          pokeElemOff samples i (fromIntegral (max (-32768) (min 32767 s)))
      action ptr (min len loopBytes)

steps :: Int
steps = 48

tracks :: [(String, ([Word8], [[Word8]], PlSynthT))]
tracks =
  [ ("bass", bass)
  , ("arp", arp)
  , ("lead", lead)
  , ("pad", pad)
  , ("kick", kick)
  , ("snare", snare)
  , ("hats", hats)
  , ("blips", blips)
  ]

-- * Note names

-- Relative to each instrument's octave setting: 135 = C (~C3 at oct 7).
fN1, gN1, afN1, bbN1, bN1 :: Word8
fN1 = 128
gN1 = 130
afN1 = 131
bbN1 = 133
bN1 = 134

c0, d0, ef0, f0, g0, af0, bb0, b0 :: Word8
c0 = 135
d0 = 137
ef0 = 138
f0 = 140
g0 = 142
af0 = 143
bb0 = 145
b0 = 146

c1, d1, ef1, f1, g1, af1, bb1, c2 :: Word8
c1 = 147
d1 = 149
ef1 = 150
f1 = 152
g1 = 154
af1 = 155
bb1 = 157
c2 = 159

-- * Pattern helpers

-- | Two 16-row bars make one 32-row pattern.
bars :: [Word8] -> [Word8] -> [Word8]
bars a b
  | length a == 16 && length b == 16 = a <> b
  | otherwise = error "bar must be exactly 16 rows"

-- * Tracks

{- ORMOLU_DISABLE -}

-- | Driving octave bass on a square + slightly chorused saw, lowpassed.
bass :: ([Word8], [[Word8]], PlSynthT)
bass =
  ( concat
      [ [0,0,0,0]         -- intro
      , [1,2,1,2]         -- groove enters
      , [1,2,1,2,1,2,1,5] -- melody A
      , [1,2,1,5]         -- break
      , [1,2,1,2,1,2,1,5] -- melody B
      , [3,4,3,4]         -- bridge
      , [1,2,1,2,1,2,1,5] -- melody A reprise
      , [1,2,1,2]         -- melody B reprise
      , [1,2,0,0]         -- outro
      ]
  , [ bars (bar c0)   (bar afN1) -- 1: Cm | Ab
    , bars (bar ef0)  (bar bbN1) -- 2: Eb | Bb
    , bars (bar fN1)  (bar c0)   -- 3: Fm | Cm (bridge)
    , bars (bar afN1) (bar gN1)  -- 4: Ab | G  (bridge)
    , bars (bar ef0)  fill       -- 5: Eb | Bb with leading-tone fill
    ]
  , emptyPlSynthT
      { osc0_oct = 6, osc0_vol = 192, osc0_waveform = SQR
      , osc1_oct = 6, osc1_detune = 9, osc1_vol = 110, osc1_waveform = SAW
      , env_attack = 60, env_sustain = 1200, env_release = 3500, env_master = 100
      , fx_filter = 2, fx_freq = 800, fx_resonance = 120
      }
  )
  where
    bar r = [r,0,r+12,0, r,0,r+12,0, r,0,r+12,0, r,r,r+12,0]
    fill  = [bbN1,0,bb0,0, bbN1,0,bb0,0, bbN1,0,bbN1,0, bN1,0,bN1,bN1]

-- | The signature chip arpeggio: saw + octave square, dotted-8th delay,
-- LFO sweeping a lowpass, ping-pong panning.
arp :: ([Word8], [[Word8]], PlSynthT)
arp =
  ( concat (replicate 14 [1,2]) <> [3,4,3,4] <> concat (replicate 8 [1,2])
  , [ bars (bar c0  ef0  g0)  (bar afN1 c0   ef0) -- 1: Cm | Ab
    , bars (bar ef0 g0   bb0) (bar bbN1 d0   f0)  -- 2: Eb | Bb
    , bars (bar fN1 afN1 c0)  (bar c0   ef0  g0)  -- 3: Fm | Cm (bridge)
    , bars (bar afN1 c0  ef0) (bar gN1  bN1  d0)  -- 4: Ab | G  (bridge)
    ]
  , emptyPlSynthT
      { osc0_oct = 7, osc0_vol = 130, osc0_waveform = SAW
      , osc1_oct = 8, osc1_detune = 12, osc1_vol = 80, osc1_waveform = SQR
      , env_attack = 30, env_sustain = 500, env_release = 2200, env_master = 80
      , fx_filter = 2, fx_freq = 3500, fx_resonance = 60
      , fx_delay_time = 6, fx_delay_amt = 70
      , fx_pan_freq = 6, fx_pan_amt = 80
      , lfo_fx_freq = 1, lfo_freq = 4, lfo_amt = 90, lfo_waveform = SIN
      }
  )
  where
    bar a b c = [a,b,c,a+12, b+12,c+12,b+12,a+12, c,b,a,b, c,a+12,c,b]

-- | Wide detuned-saw lead with delay; melody A (1-3) and melody B (4-5).
lead :: ([Word8], [[Word8]], PlSynthT)
lead =
  ( concat
      [ replicate 8 0     -- intro + groove
      , [1,2,1,3,1,2,1,3] -- melody A
      , [0,0,0,0]         -- break
      , [4,5,4,5,4,5,4,5] -- melody B
      , [0,0,0,0]         -- bridge
      , [1,2,1,3,1,2,1,3] -- melody A reprise
      , [4,5,4,5]         -- melody B reprise
      , [0,0,0,0]         -- outro
      ]
  , [ bars mACm mAAb     -- 1: A over Cm | Ab
    , bars mAEb mABb     -- 2: A over Eb | Bb
    , bars mAEb mABbFill -- 3: A with a run up and back down
    , bars mBCm mBAb     -- 4: B over Cm | Ab
    , bars mBEb mBBb     -- 5: B over Eb | Bb
    ]
  , emptyPlSynthT
      { osc0_oct = 7, osc0_vol = 160, osc0_waveform = SAW
      , osc1_oct = 7, osc1_detune = 18, osc1_vol = 150, osc1_waveform = SAW
      , env_attack = 250, env_sustain = 2800, env_release = 12000, env_master = 110
      , fx_filter = 2, fx_freq = 4200, fx_resonance = 90
      , fx_delay_time = 6, fx_delay_amt = 55
      , fx_pan_freq = 4, fx_pan_amt = 40
      }
  )
  where
    mACm     = [g0,0,c1,0,   ef1,0,c1,0,  g1,0,0,0,    ef1,0,c1,0]
    mAAb     = [f1,0,ef1,0,  c1,0,ef1,0,  af1,0,0,0,   f1,0,ef1,0]
    mAEb     = [g1,0,ef1,0,  g1,0,bb1,0,  g1,0,0,0,    ef1,0,bb0,0]
    mABb     = [d1,0,bb0,0,  d1,0,f1,0,   d1,0,0,0,    bb0,0,g0,0]
    mABbFill = [d1,0,f1,0,   g1,0,bb1,0,  af1,0,g1,0,  f1,0,d1,0]
    mBCm     = [c2,0,0,0,    bb1,0,af1,0, g1,0,0,0,    ef1,0,g1,0]
    mBAb     = [af1,0,0,0,   f1,0,ef1,0,  c1,0,0,0,    ef1,0,f1,0]
    mBEb     = [g1,0,bb1,0,  c2,0,0,0,    bb1,0,g1,0,  ef1,0,f1,0]
    mBBb     = [d1,0,f1,0,   g1,0,f1,0,   d1,0,c1,0,   bb0,0,c1,0]

-- | Slow-attack chord pad, two saws an octave + detune apart,
-- LFO-swept lowpass, slow wide panning. One note per half-bar.
pad :: ([Word8], [[Word8]], PlSynthT)
pad =
  ( concat (replicate 14 [1,2]) <> [3,4,3,4] <> concat (replicate 8 [1,2])
  , [ bars (bar c1  g0)  (bar af0 ef1) -- 1: Cm | Ab
    , bars (bar ef1 bb0) (bar bb0 d1)  -- 2: Eb | Bb
    , bars (bar f1  c1)  (bar c1  g0)  -- 3: Fm | Cm (bridge)
    , bars (bar af0 ef1) (bar g0  b0)  -- 4: Ab | G  (bridge)
    ]
  , emptyPlSynthT
      { osc0_oct = 6, osc0_vol = 110, osc0_waveform = SAW
      , osc1_oct = 7, osc1_detune = 24, osc1_vol = 100, osc1_waveform = SAW
      , env_attack = 16000, env_sustain = 28000, env_release = 40000, env_master = 55
      , fx_filter = 2, fx_freq = 4500, fx_resonance = 70
      , fx_pan_freq = 3, fx_pan_amt = 90
      , lfo_fx_freq = 1, lfo_freq = 3, lfo_amt = 110, lfo_waveform = SIN
      }
  )
  where
    bar a b = [a,0,0,0,0,0,0,0, b,0,0,0,0,0,0,0]

-- | Pitch-dropping sine thump (xenv on both oscillators).
kick :: ([Word8], [[Word8]], PlSynthT)
kick =
  ( concat
      [ [0,0,0,0]         -- intro
      , [1,1,1,2]         -- groove enters
      , [1,1,1,2,1,1,1,2] -- melody A
      , [0,0,1,2]         -- break
      , [1,1,1,2,1,1,1,2] -- melody B
      , [0,0,3,3]         -- bridge
      , [1,1,1,2,1,1,1,2] -- melody A reprise
      , [1,1,1,2]         -- melody B reprise
      , [1,2,0,0]         -- outro
      ]
  , [ concat (replicate 8 [c0,0,0,0])                -- 1: four on the floor
    , concat (replicate 7 [c0,0,0,0]) <> [c0,0,c0,0] -- 2: doubled turnaround
    , concat (replicate 4 [c0,0,0,0, 0,0,0,0])       -- 3: half-time
    ]
  , emptyPlSynthT
      { osc0_oct = 7, osc0_xenv = 1, osc0_vol = 255
      , osc1_oct = 7, osc1_xenv = 1, osc1_vol = 255
      , env_attack = 100, env_sustain = 0, env_release = 3800, env_master = 140
      , fx_filter = 2, fx_freq = 500, fx_resonance = 254
      }
  )

-- | Noise + dropping tone, notch-filtered, on the backbeat.
snare :: ([Word8], [[Word8]], PlSynthT)
snare =
  ( concat
      [ [0,0,0,0]         -- intro
      , [0,0,0,2]         -- announce the lead
      , [1,1,1,2,1,1,1,2] -- melody A
      , [0,0,0,3]         -- break, roll into B
      , [1,1,1,2,1,1,1,2] -- melody B
      , [0,0,0,3]         -- bridge, roll out
      , [1,1,1,2,1,1,1,2] -- melody A reprise
      , [1,1,1,2]         -- melody B reprise
      , [1,2,0,0]         -- outro
      ]
  , [ backbeat                                            -- 1: hits on 2 and 4
    , take 24 backbeat <> [0,0,f0,0, f0,0,f0,f0]          -- 2: with a fill
    , replicate 16 0 <> [f0,0,f0,0, f0,0,f0,0] <> replicate 8 f0 -- 3: build-up roll
    ]
  , emptyPlSynthT
      { osc0_oct = 8, osc0_xenv = 1, osc0_vol = 180
      , osc1_oct = 7, osc1_vol = 150, osc1_waveform = TRI
      , noise_fader = 210
      , env_attack = 60, env_sustain = 220, env_release = 5200, env_master = 115
      , fx_filter = 4, fx_freq = 9000, fx_resonance = 240
      , fx_delay_time = 5, fx_delay_amt = 25
      , fx_pan_freq = 5, fx_pan_amt = 50
      }
  )
  where
    backbeat = concat (replicate 4 [0,0,0,0, f0,0,0,0])

-- | Pure highpassed noise ticks; pitch is irrelevant for the noise channel.
hats :: ([Word8], [[Word8]], PlSynthT)
hats =
  ( concat
      [ [0,0,1,1]         -- intro
      , [1,1,1,1]         -- groove
      , [1,1,1,1,1,1,1,2] -- melody A
      , [1,1,1,1]         -- break
      , [2,2,2,2,2,2,2,2] -- melody B
      , [0,0,1,1]         -- bridge
      , [1,1,1,1,1,1,1,2] -- melody A reprise
      , [2,2,2,2]         -- melody B reprise
      , [1,1,1,0]         -- outro
      ]
  , [ concat (replicate 8 [0,0,bb0,0]) -- 1: offbeat 8ths
    , replicate 32 bb0                 -- 2: full 16ths
    ]
  , emptyPlSynthT
      { noise_fader = 160
      , env_attack = 20, env_sustain = 40, env_release = 800, env_master = 60
      , fx_filter = 1, fx_freq = 9000, fx_resonance = 120
      , fx_pan_freq = 7, fx_pan_amt = 70
      }
  )

-- | High sine pings drenched in a long 4.5-row delay, fast panning.
-- Carry the intro, break and bridge.
blips :: ([Word8], [[Word8]], PlSynthT)
blips =
  ( concat
      [ [1,2,1,2]      -- intro
      , replicate 12 0 -- groove + melody A
      , [1,2,1,2]      -- break
      , replicate 8 0  -- melody B
      , [3,4,3,4]      -- bridge
      , replicate 12 0 -- reprises
      , [1,2,1,2]      -- outro
      ]
  , [ bars [c1,0,0,0,  0,0,ef1,0, 0,0,g1,0,  0,0,0,0] -- 1: Cm | Ab
         [ef1,0,0,0, 0,0,c1,0,  0,0,af1,0, 0,0,0,0]
    , bars [ef1,0,0,0, 0,0,g1,0,  0,0,bb1,0, 0,0,0,0] -- 2: Eb | Bb
         [f1,0,0,0,  0,0,d1,0,  0,0,bb0,0, 0,0,0,0]
    , bars [f1,0,0,0,  0,0,af1,0, 0,0,c2,0,  0,0,0,0] -- 3: Fm | Cm (bridge)
         [g1,0,0,0,  0,0,ef1,0, 0,0,c1,0,  0,0,0,0]
    , bars [af1,0,0,0, 0,0,ef1,0, 0,0,c1,0,  0,0,0,0] -- 4: Ab | G (bridge)
         [g1,0,0,0,  0,0,d1,0,  0,0,b0,0,  0,0,0,0]
    ]
  , emptyPlSynthT
      { osc0_oct = 9, osc0_vol = 120, osc0_waveform = SIN
      , osc1_oct = 8, osc1_detune = 10, osc1_vol = 70, osc1_waveform = SQR
      , env_attack = 40, env_sustain = 300, env_release = 5000, env_master = 85
      , fx_filter = 3, fx_freq = 6000, fx_resonance = 130
      , fx_delay_time = 9, fx_delay_amt = 110
      , fx_pan_freq = 7, fx_pan_amt = 130
      , lfo_fx_freq = 1, lfo_freq = 5, lfo_amt = 60, lfo_waveform = TRI
      }
  )
