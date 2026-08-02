{-# LANGUAGE OverloadedRecordDot #-}

{-| A few-minute looping cracktro tune in C minor.

Each track lays its patterns over the arrangement's sections ("Tune") and
'resolve' turns that into what pl_synth wants: a pattern table and a sequence
indexing it. A block shorter than its section repeats to fill it, as a tracker
would.

Harmony: the main loop is Cm-Ab-Eb-Bb (i-VI-III-VII), two chords per pattern;
the bridge is Fm-Cm-Ab-G (iv-i-VI-V).
-}
module Music.Tune
  ( withTune
  , sampleRate
  ) where

import Sound.PlSynth (PlSynthT (..), emptyPlSynthT, withPlSynth, withSong, withSongTracks, pattern SAW, pattern SIN, pattern SQR, pattern TRI)

import Control.Monad (forM_, when)
import Data.Int (Int16)
import Data.List (elemIndex, nub)
import Data.Word (Word8)
import Foreign (Ptr, castPtr, peekElemOff, pokeElemOff, sizeOf)

import Tune (Section (..), sectionSteps)
import qualified Tune

-- | pl_synth renders 16-bit stereo at this rate.
sampleRate :: Int
sampleRate = 44100

-- | Samples per row; rows are 16th notes at 'Tune.bpm'.
rowLength :: Int
rowLength = sampleRate * 60 `div` (Tune.bpm * 4)

-- | Two bars of 16ths.
rowsPerStep :: Int
rowsPerStep = Tune.beatsPerStep * 4

-- | The rendered loop, exactly one pass of the sequence grid.
withTune :: (Ptr Word8 -> Int -> IO r) -> IO r
withTune action =
  -- withPlSynth fills the generator's static wavetables; rendering without it
  -- dereferences NULL.
  withPlSynth $ withSongTracks (fromIntegral rowLength) (map resolve tracks) \song ->
    withSong song \ptr len -> do
      foldTailIntoStart (castPtr ptr) len loopBytes
      action ptr (min len loopBytes)
  where
    loopBytes = Tune.steps * rowsPerStep * rowLength * 2 * sizeOf (0 :: Int16)

{- | Wrap the render's overhang back onto its start.

The render runs past the sequence grid by the last notes' release and delay
tails (@pl_synth_song_len@ adds @pl_synth_instrument_len@), but the loop point
is the grid end — looping the raw buffer plays a few beats long per pass.
Folding the tail back is exactly what a looping tracker would play.
-}
foldTailIntoStart :: Ptr Int16 -> Int -> Int -> IO ()
foldTailIntoStart samples len loopBytes =
  when (len > loopBytes) $
    forM_ [0 .. min grid ((len - loopBytes) `div` 2) - 1] \i -> do
      a <- peekElemOff samples i
      b <- peekElemOff samples (grid + i)
      pokeElemOff samples i (clamped (fromIntegral a + fromIntegral b))
  where
    grid = loopBytes `div` 2
    clamped s = fromIntegral (max (-32768) (min 32767 (s :: Int)))

-- | One pattern: two 16-row bars of 16th notes.
type Pattern = [Word8]

data Track = Track
  { name :: String
  , plays :: [(Section, [Pattern])]
  -- ^ Every section, in order.
  , synth :: PlSynthT
  }

{- | The track as pl_synth takes it: a pattern table, and a sequence of 1-based
indexes into it (0 plays nothing).
-}
resolve :: Track -> ([Word8], [[Word8]], PlSynthT)
resolve track
  | map fst track.plays /= [minBound .. maxBound] =
      error $ track.name <> ": every section must appear once, in order"
  | otherwise = (map index sequenced, table, track.synth)
  where
    sequenced = concatMap (uncurry fill) track.plays
    fill section block
      | null block || sectionSteps section `mod` length block /= 0 =
          error $
            track.name
              <> ": "
              <> show section
              <> " runs "
              <> show (sectionSteps section)
              <> " steps, which "
              <> show (length block)
              <> " patterns don't fill"
      | any ((/= rowsPerStep) . length) block =
          error $ track.name <> ": " <> show section <> " has a pattern that isn't " <> show rowsPerStep <> " rows"
      | otherwise = take (sectionSteps section) (cycle block)
    table = nub (filter (/= rest) sequenced)
    index p = maybe 0 (fromIntegral . succ) (elemIndex p table)

rest :: Pattern
rest = replicate rowsPerStep 0

-- | Two 16-row bars make one pattern.
bars :: [Word8] -> [Word8] -> Pattern
bars a b
  | length a == 16 && length b == 16 = a <> b
  | otherwise = error "bar must be exactly 16 rows"

tracks :: [Track]
tracks = [bass, arp, lead, pad, kick, snare, hats, blips]

-- * Note names

{- | pl_synth's note numbers, relative to each instrument's octave setting: 135
is C (~C3 at oct 7), and a semitone is 1.
-}
c0, d0, ef0, f0, g0, af0, bb0, b0 :: Word8
c0 = 135
d0 = c0 + 2
ef0 = c0 + 3
f0 = c0 + 5
g0 = c0 + 7
af0 = c0 + 8
bb0 = c0 + 10
b0 = c0 + 11

fN1, gN1, afN1, bbN1, bN1 :: Word8
fN1 = f0 - 12
gN1 = g0 - 12
afN1 = af0 - 12
bbN1 = bb0 - 12
bN1 = b0 - 12

c1, d1, ef1, f1, g1, af1, bb1, c2 :: Word8
c1 = c0 + 12
d1 = d0 + 12
ef1 = ef0 + 12
f1 = f0 + 12
g1 = g0 + 12
af1 = af0 + 12
bb1 = bb0 + 12
c2 = c0 + 24

-- * Tracks

{- ORMOLU_DISABLE -}

-- | Driving octave bass on a square + slightly chorused saw, lowpassed.
bass :: Track
bass =
  Track
    { name = "bass"
    , plays =
        [ (Intro,           [rest])
        , (Groove,          [cmAb, ebBb])
        , (MelodyA,         [cmAb, ebBb, cmAb, ebBb, cmAb, ebBb, cmAb, ebBbFill])
        , (Break,           [cmAb, ebBb, cmAb, ebBbFill])
        , (MelodyB,         [cmAb, ebBb, cmAb, ebBb, cmAb, ebBb, cmAb, ebBbFill])
        , (Bridge,          [fmCm, abG])
        , (MelodyAReprise,  [cmAb, ebBb, cmAb, ebBb, cmAb, ebBb, cmAb, ebBbFill])
        , (MelodyBReprise,  [cmAb, ebBb])
        , (Outro,           [cmAb, ebBb])
        , (OutroTail,       [rest])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 6, osc0_vol = 192, osc0_waveform = SQR
          , osc1_oct = 6, osc1_detune = 9, osc1_vol = 110, osc1_waveform = SAW
          , env_attack = 60, env_sustain = 1200, env_release = 3500, env_master = 100
          , fx_filter = 2, fx_freq = 800, fx_resonance = 120
          }
    }
  where
    cmAb     = bars (bar c0)   (bar afN1)
    ebBb     = bars (bar ef0)  (bar bbN1)
    fmCm     = bars (bar fN1)  (bar c0)
    abG      = bars (bar afN1) (bar gN1)
    ebBbFill = bars (bar ef0)  leadingTone
    bar r       = [r,0,r+12,0, r,0,r+12,0, r,0,r+12,0, r,r,r+12,0]
    leadingTone = [bbN1,0,bb0,0, bbN1,0,bb0,0, bbN1,0,bbN1,0, bN1,0,bN1,bN1]

{- | The signature chip arpeggio: saw + octave square, dotted-8th delay, LFO
sweeping a lowpass, ping-pong panning.
-}
arp :: Track
arp =
  Track
    { name = "arp"
    , plays =
        [ (Intro,           [cmAb, ebBb])
        , (Groove,          [cmAb, ebBb])
        , (MelodyA,         [cmAb, ebBb])
        , (Break,           [cmAb, ebBb])
        , (MelodyB,         [cmAb, ebBb])
        , (Bridge,          [fmCm, abG])
        , (MelodyAReprise,  [cmAb, ebBb])
        , (MelodyBReprise,  [cmAb, ebBb])
        , (Outro,           [cmAb, ebBb])
        , (OutroTail,       [cmAb, ebBb])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 7, osc0_vol = 130, osc0_waveform = SAW
          , osc1_oct = 8, osc1_detune = 12, osc1_vol = 80, osc1_waveform = SQR
          , env_attack = 30, env_sustain = 500, env_release = 2200, env_master = 80
          , fx_filter = 2, fx_freq = 3500, fx_resonance = 60
          , fx_delay_time = 6, fx_delay_amt = 70
          , fx_pan_freq = 6, fx_pan_amt = 80
          , lfo_fx_freq = 1, lfo_freq = 4, lfo_amt = 90, lfo_waveform = SIN
          }
    }
  where
    cmAb = bars (bar c0  ef0  g0)  (bar afN1 c0   ef0)
    ebBb = bars (bar ef0 g0   bb0) (bar bbN1 d0   f0)
    fmCm = bars (bar fN1 afN1 c0)  (bar c0   ef0  g0)
    abG  = bars (bar afN1 c0  ef0) (bar gN1  bN1  d0)
    bar a b c = [a,b,c,a+12, b+12,c+12,b+12,a+12, c,b,a,b, c,a+12,c,b]

-- | Wide detuned-saw lead with delay, over melody A and melody B.
lead :: Track
lead =
  Track
    { name = "lead"
    , plays =
        [ (Intro,           [rest])
        , (Groove,          [rest])
        , (MelodyA,         [aCmAb, aEbBb, aCmAb, aEbBbRun])
        , (Break,           [rest])
        , (MelodyB,         [bCmAb, bEbBb])
        , (Bridge,          [rest])
        , (MelodyAReprise,  [aCmAb, aEbBb, aCmAb, aEbBbRun])
        , (MelodyBReprise,  [bCmAb, bEbBb])
        , (Outro,           [rest])
        , (OutroTail,       [rest])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 7, osc0_vol = 160, osc0_waveform = SAW
          , osc1_oct = 7, osc1_detune = 18, osc1_vol = 150, osc1_waveform = SAW
          , env_attack = 250, env_sustain = 2800, env_release = 12000, env_master = 110
          , fx_filter = 2, fx_freq = 4200, fx_resonance = 90
          , fx_delay_time = 6, fx_delay_amt = 55
          , fx_pan_freq = 4, fx_pan_amt = 40
          }
    }
  where
    aCmAb    = bars aCm aAb
    aEbBb    = bars aEb aBb
    aEbBbRun = bars aEb aBbRun
    bCmAb    = bars bCm bAb
    bEbBb    = bars bEb bBb
    aCm    = [g0,0,c1,0,   ef1,0,c1,0,  g1,0,0,0,    ef1,0,c1,0]
    aAb    = [f1,0,ef1,0,  c1,0,ef1,0,  af1,0,0,0,   f1,0,ef1,0]
    aEb    = [g1,0,ef1,0,  g1,0,bb1,0,  g1,0,0,0,    ef1,0,bb0,0]
    aBb    = [d1,0,bb0,0,  d1,0,f1,0,   d1,0,0,0,    bb0,0,g0,0]
    aBbRun = [d1,0,f1,0,   g1,0,bb1,0,  af1,0,g1,0,  f1,0,d1,0]
    bCm    = [c2,0,0,0,    bb1,0,af1,0, g1,0,0,0,    ef1,0,g1,0]
    bAb    = [af1,0,0,0,   f1,0,ef1,0,  c1,0,0,0,    ef1,0,f1,0]
    bEb    = [g1,0,bb1,0,  c2,0,0,0,    bb1,0,g1,0,  ef1,0,f1,0]
    bBb    = [d1,0,f1,0,   g1,0,f1,0,   d1,0,c1,0,   bb0,0,c1,0]

{- | Slow-attack chord pad, two saws an octave + detune apart, LFO-swept
lowpass, slow wide panning. One note per half-bar.
-}
pad :: Track
pad =
  Track
    { name = "pad"
    , plays =
        [ (Intro,           [cmAb, ebBb])
        , (Groove,          [cmAb, ebBb])
        , (MelodyA,         [cmAb, ebBb])
        , (Break,           [cmAb, ebBb])
        , (MelodyB,         [cmAb, ebBb])
        , (Bridge,          [fmCm, abG])
        , (MelodyAReprise,  [cmAb, ebBb])
        , (MelodyBReprise,  [cmAb, ebBb])
        , (Outro,           [cmAb, ebBb])
        , (OutroTail,       [cmAb, ebBb])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 6, osc0_vol = 110, osc0_waveform = SAW
          , osc1_oct = 7, osc1_detune = 24, osc1_vol = 100, osc1_waveform = SAW
          , env_attack = 16000, env_sustain = 28000, env_release = 40000, env_master = 55
          , fx_filter = 2, fx_freq = 4500, fx_resonance = 70
          , fx_pan_freq = 3, fx_pan_amt = 90
          , lfo_fx_freq = 1, lfo_freq = 3, lfo_amt = 110, lfo_waveform = SIN
          }
    }
  where
    cmAb = bars (bar c1  g0)  (bar af0 ef1)
    ebBb = bars (bar ef1 bb0) (bar bb0 d1)
    fmCm = bars (bar f1  c1)  (bar c1  g0)
    abG  = bars (bar af0 ef1) (bar g0  b0)
    bar a b = [a,0,0,0,0,0,0,0, b,0,0,0,0,0,0,0]

-- | Pitch-dropping sine thump (xenv on both oscillators).
kick :: Track
kick =
  Track
    { name = "kick"
    , plays =
        [ (Intro,           [rest])
        , (Groove,          [floor4, floor4, floor4, turnaround])
        , (MelodyA,         [floor4, floor4, floor4, turnaround])
        , (Break,           [rest, rest, floor4, turnaround])
        , (MelodyB,         [floor4, floor4, floor4, turnaround])
        , (Bridge,          [rest, rest, halfTime, halfTime])
        , (MelodyAReprise,  [floor4, floor4, floor4, turnaround])
        , (MelodyBReprise,  [floor4, floor4, floor4, turnaround])
        , (Outro,           [floor4, turnaround])
        , (OutroTail,       [rest])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 7, osc0_xenv = 1, osc0_vol = 255
          , osc1_oct = 7, osc1_xenv = 1, osc1_vol = 255
          , env_attack = 100, env_sustain = 0, env_release = 3800, env_master = 140
          , fx_filter = 2, fx_freq = 500, fx_resonance = 254
          }
    }
  where
    floor4     = concat (replicate 8 [c0,0,0,0])
    turnaround = concat (replicate 7 [c0,0,0,0]) <> [c0,0,c0,0]
    halfTime   = concat (replicate 4 [c0,0,0,0, 0,0,0,0])

-- | Noise + dropping tone, notch-filtered, on the backbeat.
snare :: Track
snare =
  Track
    { name = "snare"
    , plays =
        [ (Intro,           [rest])
        , (Groove,          [rest, rest, rest, backbeatFill])
        , (MelodyA,         [backbeat, backbeat, backbeat, backbeatFill])
        , (Break,           [rest, rest, rest, buildRoll])
        , (MelodyB,         [backbeat, backbeat, backbeat, backbeatFill])
        , (Bridge,          [rest, rest, rest, buildRoll])
        , (MelodyAReprise,  [backbeat, backbeat, backbeat, backbeatFill])
        , (MelodyBReprise,  [backbeat, backbeat, backbeat, backbeatFill])
        , (Outro,           [backbeat, backbeatFill])
        , (OutroTail,       [rest])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 8, osc0_xenv = 1, osc0_vol = 180
          , osc1_oct = 7, osc1_vol = 150, osc1_waveform = TRI
          , noise_fader = 210
          , env_attack = 60, env_sustain = 220, env_release = 5200, env_master = 115
          , fx_filter = 4, fx_freq = 9000, fx_resonance = 240
          , fx_delay_time = 5, fx_delay_amt = 25
          , fx_pan_freq = 5, fx_pan_amt = 50
          }
    }
  where
    backbeat     = concat (replicate 4 [0,0,0,0, f0,0,0,0])
    backbeatFill = take 24 backbeat <> [0,0,f0,0, f0,0,f0,f0]
    buildRoll    = replicate 16 0 <> [f0,0,f0,0, f0,0,f0,0] <> replicate 8 f0

-- | Pure highpassed noise ticks; pitch is irrelevant for the noise channel.
hats :: Track
hats =
  Track
    { name = "hats"
    , plays =
        [ (Intro,           [rest, rest, eighths, eighths])
        , (Groove,          [eighths])
        , (MelodyA,         [eighths, eighths, eighths, eighths, eighths, eighths, eighths, sixteenths])
        , (Break,           [eighths])
        , (MelodyB,         [sixteenths])
        , (Bridge,          [rest, rest, eighths, eighths])
        , (MelodyAReprise,  [eighths, eighths, eighths, eighths, eighths, eighths, eighths, sixteenths])
        , (MelodyBReprise,  [sixteenths])
        , (Outro,           [eighths])
        , (OutroTail,       [eighths, rest])
        ]
    , synth =
        emptyPlSynthT
          { noise_fader = 160
          , env_attack = 20, env_sustain = 40, env_release = 800, env_master = 60
          , fx_filter = 1, fx_freq = 9000, fx_resonance = 120
          , fx_pan_freq = 7, fx_pan_amt = 70
          }
    }
  where
    eighths    = concat (replicate 8 [0,0,bb0,0])
    sixteenths = replicate 32 bb0

{- | High sine pings drenched in a long 4.5-row delay, fast panning. They carry
the intro, the break and the bridge.
-}
blips :: Track
blips =
  Track
    { name = "blips"
    , plays =
        [ (Intro,           [cmAb, ebBb])
        , (Groove,          [rest])
        , (MelodyA,         [rest])
        , (Break,           [cmAb, ebBb])
        , (MelodyB,         [rest])
        , (Bridge,          [fmCm, abG])
        , (MelodyAReprise,  [rest])
        , (MelodyBReprise,  [rest])
        , (Outro,           [cmAb, ebBb])
        , (OutroTail,       [cmAb, ebBb])
        ]
    , synth =
        emptyPlSynthT
          { osc0_oct = 9, osc0_vol = 120, osc0_waveform = SIN
          , osc1_oct = 8, osc1_detune = 10, osc1_vol = 70, osc1_waveform = SQR
          , env_attack = 40, env_sustain = 300, env_release = 5000, env_master = 85
          , fx_filter = 3, fx_freq = 6000, fx_resonance = 130
          , fx_delay_time = 9, fx_delay_amt = 110
          , fx_pan_freq = 7, fx_pan_amt = 130
          , lfo_fx_freq = 1, lfo_freq = 5, lfo_amt = 60, lfo_waveform = TRI
          }
    }
  where
    cmAb = bars [c1,0,0,0,  0,0,ef1,0, 0,0,g1,0,  0,0,0,0]
                [ef1,0,0,0, 0,0,c1,0,  0,0,af1,0, 0,0,0,0]
    ebBb = bars [ef1,0,0,0, 0,0,g1,0,  0,0,bb1,0, 0,0,0,0]
                [f1,0,0,0,  0,0,d1,0,  0,0,bb0,0, 0,0,0,0]
    fmCm = bars [f1,0,0,0,  0,0,af1,0, 0,0,c2,0,  0,0,0,0]
                [g1,0,0,0,  0,0,ef1,0, 0,0,c1,0,  0,0,0,0]
    abG  = bars [af1,0,0,0, 0,0,ef1,0, 0,0,c1,0,  0,0,0,0]
                [g1,0,0,0,  0,0,d1,0,  0,0,b0,0,  0,0,0,0]
