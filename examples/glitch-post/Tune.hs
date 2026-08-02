{-# LANGUAGE OverloadedRecordDot #-}

{-| The tune's timeline: the section table of @tunez.txt@ (135 BPM, 4\/4, one
table step = two bars) turned into per-frame performance knobs, plus the
boundary events for drift tracing.

Everything here is pure and runs off a /song position/ in seconds. The caller
supplies the clock — in practice the playhead of the looping OpenAL source
carrying this very tune — and positions wrap at 'loopLength' just like the tune
does, so the knobs loop seamlessly with it.
-}
module Tune
  ( Knobs (..)
  , knobsAt
  , Section (..)
  , label
  , sectionSteps
  , steps
  , bpm
  , beatsPerStep
  , beforeStart
  , eventsBetween
  ) where

-- | What the scene performs each frame. Everything is 0–1.
data Knobs = Knobs
  { banner :: Float
  {- ^ The letters: 0 free swarm, 1 assembled. Scheduled over the calm
  sections, releasing onto the next downbeat — the burst lands on the drop.
  -}
  , beat :: Float
  -- ^ The kick envelope: 1 on each kick, decaying over the beat.
  , energy :: Float
  -- ^ Section energy.
  , build :: Float
  -- ^ Snare rolls and fills, ramping 0→1 into a drop.
  , impact :: Float
  -- ^ A short flash decaying from each drop.
  , spin :: Float
  {- ^ Rotation direction for the tunnel twist and the vortex: +1 normally,
  −1 through Melody B and its reprise. The tunnel must /integrate/ this into
  an angle (see 'Tunnel.twistRate'); the swarm just follows the flipped
  tangential force with its own inertia.
  -}
  }

-- | The arrangement, in order. The outro is split where its kick drops out.
data Section
  = Intro
  | Groove
  | MelodyA
  | Break
  | MelodyB
  | Bridge
  | MelodyAReprise
  | MelodyBReprise
  | Outro
  | OutroTail
  deriving (Eq, Ord, Show, Enum, Bounded)

label :: Section -> String
label = \case
  Intro -> "Intro"
  Groove -> "Groove"
  MelodyA -> "Melody A"
  Break -> "Break"
  MelodyB -> "Melody B"
  Bridge -> "Bridge"
  MelodyAReprise -> "Melody A reprise"
  MelodyBReprise -> "Melody B reprise"
  Outro -> "Outro"
  OutroTail -> "Outro (kick gone)"

{- | A section as the scene reads it: where it starts in the table, how hot it
runs, and whether there's a kick. The energy and kick columns are this scene's
reading of the tune, not part of the table.
-}
data Part = Part
  { section :: Section
  , start :: Int
  -- ^ Its first step, 1-based as in the table.
  , energy :: Float
  , kick :: Float
  }

parts :: [Part]
parts =
  [ Part Intro 1 0.25 0
  , Part Groove 5 0.55 1
  , Part MelodyA 9 0.8 1
  , Part Break 17 0.35 0
  , Part MelodyB 21 1.0 1
  , Part Bridge 29 0.3 0
  , Part MelodyAReprise 33 0.8 1
  , Part MelodyBReprise 41 1.0 1
  , Part Outro 45 0.3 1
  , Part OutroTail 47 0.15 0
  ]

steps :: Int
steps = 48

bpm :: Int
bpm = 135

-- | One table step is two bars.
beatsPerStep :: Int
beatsPerStep = 8

beatLength, barLength, stepLength :: Float
beatLength = 60 / fromIntegral bpm
barLength = 4 * beatLength
stepLength = fromIntegral beatsPerStep * beatLength

-- | Steps are 1-based, as in the table.
stepTime :: Int -> Float
stepTime n = fromIntegral (n - 1) * stepLength

-- | Positions wrap here, into the intro.
loopLength :: Float
loopLength = fromIntegral steps * stepLength

wrapSong :: Float -> Float
wrapSong x = x - loopLength * fromIntegral (floor (x / loopLength) :: Int)

partOf :: Section -> Part
partOf s = head [p | p <- parts, p.section == s]

sectionSteps :: Section -> Int
sectionSteps s = ends - (partOf s).start
  where
    ends = case drop 1 (dropWhile ((/= s) . (.section)) parts) of
      next : _ -> next.start
      [] -> steps + 1

-- | From the downbeat @from@ starts on to the one that ends @to@.
through :: Section -> Section -> (Float, Float)
through from to = (stepTime (partOf from).start, stepTime ((partOf to).start + sectionSteps to))

knobsAt :: Float -> Knobs
knobsAt (wrapSong -> t) =
  Knobs
    { banner = bannerAt t
    , beat = kickAt t * (1 - beatPhase) ** 5
    , energy = energyAt t
    , build = buildAt t
    , impact = impactAt t
    , spin = spinAt t
    }
  where
    beatPhase = let b = t / beatLength in b - fromIntegral (floor b :: Int)

-- | The part playing at @t@, as an index into 'parts'.
partAt :: Float -> Int
partAt t = last [i | (i, p) <- zip [0 ..] parts, stepTime p.start <= t]

{- | Eased over the first bar of each part; the intro blends from the outro's
tail, so the loop seam is smooth.
-}
energyAt :: Float -> Float
energyAt t = before.energy + (now.energy - before.energy) * smoothstep 0 barLength (t - stepTime now.start)
  where
    i = partAt t
    now = parts !! i
    before = parts !! (if i == 0 then length parts - 1 else i - 1)

-- | Hard-edged on purpose: the kick lands /on/ the downbeat.
kickAt :: Float -> Float
kickAt t = (parts !! partAt t).kick

{- | One banner per calm stretch. Assembly starts half a bar in and takes two
bars; release finishes a quarter-bar before the stretch ends, so the burst is in
flight exactly when the next downbeat drops.
-}
bannerAt :: Float -> Float
bannerAt t = maximum (0 : map window [Break `through` Break, Bridge `through` Bridge, Outro `through` OutroTail])
  where
    window (from, to) =
      smoothstep (from + 0.5 * barLength) (from + 2.5 * barLength) t
        * (1 - smoothstep (to - 1.5 * barLength) (to - 0.25 * barLength) t)

-- | Snare rolls and fills the tune plays into a drop.
snareRoll, drumsBuildBack :: Float
snareRoll = minsec 1 7
drumsBuildBack = minsec 1 46.7

-- | The fills, squared so they swell.
buildAt :: Float -> Float
buildAt t = maximum (0 : [u * u | (s, e) <- ramps, t >= s, t < e, let u = (t - s) / (e - s)])
  where
    ramps =
      [ (dropAt MelodyA - 2 * barLength, dropAt MelodyA)
      , (snareRoll, dropAt MelodyB)
      , (drumsBuildBack, dropAt MelodyAReprise)
      , (dropAt MelodyBReprise - 2 * barLength, dropAt MelodyBReprise)
      ]

{- | Reversed (−1) through Melody B and its reprise, easing over two bars on the
way in (right on the drop) and back out.
-}
spinAt :: Float -> Float
spinAt t = 1 - 2 * maximum (0 : map window [MelodyB `through` MelodyB, MelodyBReprise `through` MelodyBReprise])
  where
    window (from, to) =
      smoothstep from (from + 2 * barLength) t * (1 - smoothstep (to - 2 * barLength) to t)

-- | Weighted by how hard each drop hits; Melody B is the biggest.
impactAt :: Float -> Float
impactAt t = maximum (0 : [w * exp ((d - t) * 4) | (d, w) <- drops, t >= d, t - d < 1.5])
  where
    drops =
      [ (dropAt MelodyA, 0.7)
      , (dropAt MelodyB, 1.0)
      , (dropAt MelodyAReprise, 0.7)
      , (dropAt MelodyBReprise, 0.9)
      ]

-- | The downbeat a melody drops on.
dropAt :: Section -> Float
dropAt s = stepTime (partOf s).start

-- | A position in the tune, as the player's clock shows it.
minsec :: Int -> Float -> Float
minsec m s = fromIntegral m * 60 + s

----------------------------------------------------------------
-- Drift tracing
----------------------------------------------------------------

-- | Everything audible enough to check the sync against by ear.
events :: [(Float, String)]
events =
  [(stepTime p.start, label p.section) | p <- parts]
    <> [ (snareRoll, "snare roll")
       , (drumsBuildBack, "drums build back")
       ]

{- | Earlier than every event: 'eventsBetween' excludes its lower bound, so a
frame starting here still reports the boundary at zero.
-}
beforeStart :: Float
beforeStart = -1

{- | The events crossed between two (wrapped) song positions, handling the wrap
itself: crossing the loop point replays the intro boundary.
-}
eventsBetween :: Float -> Float -> [(Float, String)]
eventsBetween prev cur
  | cur >= prev = inRange prev cur
  | otherwise = inRange prev loopLength <> inRange beforeStart cur
  where
    inRange a b = [ev | ev@(s, _) <- events, a < s, s <= b]

smoothstep :: Float -> Float -> Float -> Float
smoothstep lo hi x =
  let u = min 1 (max 0 ((x - lo) / (hi - lo)))
  in u * u * (3 - 2 * u)
