{-| The cave's layout, in metres.

Seven chambers hollowed from a ball of rock: a central stage holding the knot and the
orb that lights it, and six dim backrooms on the axes at 'sideDistance', each with its
own glowstone ("Lights"), reached by axis-aligned halls.

Shared by "Scene" (which pushes these to "Pipeline.Voxels.Gen") and "Lights" (whose
glowstones must land at 'sideCentres'), so the rooms and their lamps cannot drift.
-}
module Scene.Cave
  ( worldScale
  , gridN
  , rockThreshold
  , carveBand
  , hallBand
  , chamberRadius
  , sideRadius
  , sideDistance
  , hallRadius
  , caveRadius
  , sideCentres
  ) where

import Data.Word (Word32)
import Geomancy (Vec3, vec3)

{- | Half-size of the voxel grid's world box (grid @[0,1]³@ → @[-worldScale, worldScale]³@).

Paired with 'gridN' this fixes the cube edge at @2 * worldScale \/ gridN@ = 1 m.
-}
worldScale :: Float
worldScale = 64

-- | Voxel grid resolution per axis.
gridN :: Word32
gridN = 256

{- | Rock where the billow field exceeds the (room-biased) threshold.

Low enough (89% rock) that the sponge is light-tight: seal the halls and under 1% of
the central chamber can see a side chamber's lamp, so the halls are the only way in.
-}
rockThreshold :: Float
rockThreshold = 1/64

{- | Over how many metres a chamber's threshold bias decays to 'rockThreshold'.

Chambers have no wall surface of their own: this is the band across which the noise
takes over and closes the rock, and it is what leaves them ragged rather than round. It
also sets how far a chamber reaches past its nominal radius — about @0.7 * carveBand@.
-}
carveBand :: Float
carveBand = 2

{- | As 'carveBand', for the halls.

Much tighter, or the halls' ragged fringe widens them into light pipes: at
@hallBand = carveBand@ some 87% of the central chamber can see a lamp, against 36% here.
-}
hallBand :: Float
hallBand = 1.5

-- | The central stage, holding the knot and the orb that lights it.
chamberRadius :: Float
chamberRadius = 12

-- | Each of the six backrooms, lit by one dim glowstone at its centre.
sideRadius :: Float
sideRadius = 15

-- | Centre-to-centre from the stage to a backroom.
sideDistance :: Float
sideDistance = sideRadius + chamberRadius + 2

-- | The halls joining the chambers along each axis.
hallRadius :: Float
hallRadius = 1.5

-- | The rock ball everything is hollowed from; beyond it is the outer void.
caveRadius :: Float
caveRadius = sideDistance * 2

-- | The six backroom centres, on the axes — where the glowstones hang ("Lights").
sideCentres :: [Vec3]
sideCentres =
  [ vec3 sideDistance 0 0
  , vec3 (-sideDistance) 0 0
  , vec3 0 sideDistance 0
  , vec3 0 (-sideDistance) 0
  , vec3 0 0 sideDistance
  , vec3 0 0 (-sideDistance)
  ]
