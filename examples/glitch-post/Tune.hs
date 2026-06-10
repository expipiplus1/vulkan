{-| The tune's timeline: the section table of @tunez.txt@ (135 BPM, 4\/4,
one table step = two bars) turned into per-frame performance knobs, plus
the boundary events for drift tracing.

Everything here is pure and runs off a /song position/ in seconds. The
caller supplies the clock — in practice the playhead of the looping
OpenAL source carrying this very tune — and positions wrap at
'loopLength' just like the tune does, so the knobs loop seamlessly
with it.
-}
module Tune
  ( Knobs (..)
  , knobsAt
  , loopLength
  , wrapSong
  , eventsBetween
  ) where

-- | What the scene performs each frame. Everything is 0–1.
data Knobs = Knobs
  { kBanner :: Float
  {- ^ The letters: 0 free swarm, 1 assembled. Scheduled over the calm
  sections (Break, Bridge, Outro), releasing onto the next downbeat —
  the burst lands on the drop.
  -}
  , kBeat :: Float
  {- ^ The kick envelope: 1 on each kick, decaying over the beat; 0 in
  sections without a kick.
  -}
  , kEnergy :: Float
  -- ^ Section energy, eased over a bar at each boundary.
  , kBuild :: Float
  -- ^ Snare rolls and fills, ramping 0→1 into a drop.
  , kImpact :: Float
  -- ^ A short flash decaying from each drop.
  , kSpin :: Float
  {- ^ Rotation direction for the tunnel twist and the vortex: +1
  normally, easing to −1 over two bars for Melody B and its reprise.
  The tunnel must /integrate/ this into an angle (flipping a rate
  mid-flight would snap the rotation); the swarm just follows the
  flipped tangential force with its own inertia.
  -}
  }

beatLength, barLength, stepLength :: Float
beatLength = 60 / 135
barLength = 4 * beatLength
stepLength = 2 * barLength

-- | 2:50.667; positions wrap here, into the intro.
loopLength :: Float
loopLength = stepTime 49

-- | Steps are 1-based, as in the table.
stepTime :: Int -> Float
stepTime n = fromIntegral (n - 1) * stepLength

-- | Wrap a song position into @[0, 'loopLength')@.
wrapSong :: Float -> Float
wrapSong x = x - loopLength * fromIntegral (floor (x / loopLength) :: Int)

{- | @(first step, energy, kick, label)@ per section. The energy and kick
columns are this scene's reading of the tune, not part of the table; the
outro's kick is gone from step 47 (2:43.6), hence the extra row.
-}
sections :: [(Int, Float, Float, String)]
sections =
  [ (1, 0.25, 0, "Intro")
  , (5, 0.55, 1, "Groove")
  , (9, 0.8, 1, "Melody A")
  , (17, 0.35, 0, "Break")
  , (21, 1.0, 1, "Melody B")
  , (29, 0.3, 0, "Bridge")
  , (33, 0.8, 1, "Melody A reprise")
  , (41, 1.0, 1, "Melody B reprise")
  , (45, 0.3, 1, "Outro")
  , (47, 0.15, 0, "Outro (kick gone)")
  ]

knobsAt :: Float -> Knobs
knobsAt (wrapSong -> t) =
  Knobs
    { kBanner = bannerAt t
    , kBeat = kickAt t * (1 - beatPhase) ** 5
    , kEnergy = energyAt t
    , kBuild = buildAt t
    , kImpact = impactAt t
    , kSpin = spinAt t
    }
  where
    beatPhase = let b = t / beatLength in b - fromIntegral (floor b :: Int)

-- | The section containing @t@, as an index into 'sections'.
sectionIndex :: Float -> Int
sectionIndex t = last [i | (i, (s, _, _, _)) <- zip [0 ..] sections, stepTime s <= t]

{- | Eased over the first bar of each section; the intro blends from the
outro's tail, so the loop seam is smooth.
-}
energyAt :: Float -> Float
energyAt t = e0 + (e1 - e0) * smoothstep 0 barLength (t - stepTime s)
  where
    i = sectionIndex t
    (s, e1, _, _) = sections !! i
    (_, e0, _, _) = sections !! (if i == 0 then length sections - 1 else i - 1)

-- | Hard-edged on purpose: the kick lands /on/ the downbeat.
kickAt :: Float -> Float
kickAt t = g
  where
    (_, _, g, _) = sections !! sectionIndex t

{- | One banner per calm section. Assembly starts half a bar in and takes
two bars; release finishes a quarter-bar before the section ends, so the
burst is in flight exactly when the next downbeat drops.
-}
bannerAt :: Float -> Float
bannerAt t = maximum (0 : map window [(17, 21), (29, 33), (45, 49)])
  where
    window (s, e) =
      let
        s' = stepTime s
        e' = stepTime e
      in
        smoothstep (s' + 0.5 * barLength) (s' + 2.5 * barLength) t
          * (1 - smoothstep (e' - 1.5 * barLength) (e' - 0.25 * barLength) t)

-- | Snare rolls and fills building into a drop, squared so they swell.
buildAt :: Float -> Float
buildAt t = maximum (0 : [u * u | (s, e) <- ramps, t >= s, t < e, let u = (t - s) / (e - s)])
  where
    ramps =
      [ (stepTime 9 - 2 * barLength, stepTime 9) -- the fill before the first drop
      , (67.0, stepTime 21) -- the break's snare roll (1:07 → 1:11.1)
      , (106.7, stepTime 33) -- the bridge's drums building back (1:46.7 →)
      , (stepTime 41 - 2 * barLength, stepTime 41) -- into the second climax
      ]

{- | Reversed (−1) through Melody B and its reprise, easing over two bars
on the way in (right on the drop) and back out.
-}
spinAt :: Float -> Float
spinAt t = 1 - 2 * maximum (0 : map window [(21, 29), (41, 45)])
  where
    window (s, e) =
      let
        s' = stepTime s
        e' = stepTime e
      in
        smoothstep s' (s' + 2 * barLength) t * (1 - smoothstep (e' - 2 * barLength) e' t)

-- | Weighted by how hard each drop hits; Melody B is the biggest.
impactAt :: Float -> Float
impactAt t = maximum (0 : [w * exp ((d - t) * 4) | (d, w) <- drops, t >= d, t - d < 1.5])
  where
    drops = [(stepTime 9, 0.7), (stepTime 21, 1.0), (stepTime 33, 0.7), (stepTime 41, 0.9)]

----------------------------------------------------------------
-- Drift tracing
----------------------------------------------------------------

-- | Everything audible enough to check the sync against by ear.
events :: [(Float, String)]
events =
  [(stepTime s, l) | (s, _, _, l) <- sections]
    <> [ (67.0, "snare roll")
       , (106.7, "drums build back")
       ]

{- | The events crossed between two (wrapped) song positions, handling the
wrap itself: crossing the loop point replays the intro boundary.
-}
eventsBetween :: Float -> Float -> [(Float, String)]
eventsBetween prev cur
  | cur >= prev = inRange prev cur
  | otherwise = inRange prev loopLength <> inRange (-1) cur
  where
    inRange a b = [ev | ev@(s, _) <- events, a < s, s <= b]

smoothstep :: Float -> Float -> Float -> Float
smoothstep lo hi x =
  let u = min 1 (max 0 ((x - lo) / (hi - lo)))
  in u * u * (3 - 2 * u)
