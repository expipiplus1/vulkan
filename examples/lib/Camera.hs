{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Camera where

import           Control.Lens
import           Foreign.Storable.Generic
import           GHC.Generics                   ( Generic )
import           Linear

data Camera = Camera
  { camPosition    :: V3 Float
  , camOrientation :: Quaternion Float
  , camAspect      :: Float
  , camFOV         :: Float
    -- ^ Vertical field of view in Radians
  }

data CameraMatrices = CameraMatrices
  { cmViewInverse :: M44 Float
  , cmProjInverse :: M44 Float
  }
  deriving (Generic, GStorable)

initialCamera :: Camera
initialCamera =
  Camera (V3 0 0 (-10)) (axisAngle (V3 0 0 1) 0) (16 / 9) (pi / 4)

-- >>> viewMatrix initialCamera
-- V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 10.0) (V4 0.0 0.0 0.0 1.0)
viewMatrix :: Camera -> M44 Float
viewMatrix Camera {..} = inv44 $ mkTransformation camOrientation camPosition

-- >>> projectionMatrix initialCamera
-- V4 (V4 0.3611771 0.0 0.0 0.0) (V4 0.0 0.6420926 0.0 0.0) (V4 0.0 0.0 0.0 0.1) (V4 0.0 0.0 1.0 0.0)
--
-- >>> tan (1.5 / 2)
-- 0.9315964599440725
projectionMatrix :: Camera -> M44 Float
projectionMatrix Camera {..} =
  let cotFoV = 1 / tan (camFOV / 2)
      dx     = cotFoV / camAspect
      dy     = cotFoV
      zNear  = 0.1
  in  V4 (V4 dx 0 0 0) (V4 0 dy 0 0) (V4 0 0 0 zNear) (V4 0 0 1 0)

-- >>> projectRay initialCamera (V2 0 0)
-- (V3 0.0 0.0 (-10.0),V3 0.0 0.0 1.0)
--
-- >>> projectRay initialCamera (V2 0 1)
-- (V3 0.0 0.0 (-10.0),V3 0.0 0.38268346 0.9238795)
--
-- >>> projectRay initialCamera (V2 1 0)
-- (V3 0.0 0.0 (-10.0),V3 0.5929577 0.0 0.8052336)
projectRay
  :: Camera
  -> V2 Float
  -- ^ Ray position on screen in [-1..1]^2
  -> (V3 Float, V3 Float)
  -- ^ Origin, Direction
projectRay c scr2 =
  let viewInverse       = inv44 $ viewMatrix c
      projInverse       = inv44 $ projectionMatrix c
      origin            = (viewInverse !* point (V3 0 0 0)) ^. _xyz
      targetScreenSpace = V4 (scr2 ^. _x) (scr2 ^. _y) 1 1
      target            = projInverse !* targetScreenSpace
      dir = normalize ((viewInverse !* vector (target ^. _xyz)) ^. _xyz)
  in  (origin, dir)

-- >>> projectToScreen initialCamera (V3 0 0 (-9.8))
-- V3 0.0 0.0 0.5000005
--
-- >>> projectToScreen initialCamera (V3 0 0 (-10))
-- V3 NaN NaN Infinity
--
-- >>> projectToScreen initialCamera (V3 0 0 (-9.9))
-- V3 0.0 0.0 0.9999962
--
-- >>> projectToScreen initialCamera (V3 0 0 1000)
-- V3 0.0 0.0 9.900991e-5
projectToScreen :: Camera -> V3 Float -> V3 Float
projectToScreen c =
  normalizePoint . (projectionMatrix c !*) . (viewMatrix c !*) . point
