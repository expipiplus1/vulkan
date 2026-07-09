{-| Auto-exposure.

Middle grey over the metered mean luminance ('target'), bounded by the sensor's gain
range and approached in EV space ('adapt'). The bounds are the point: a sensor cannot
amplify indefinitely, so an unlit scene reads as dark rather than as a badly-exposed
lit one.
-}
module Exposure
  ( key
  , minExposure
  , maxExposure
  , target
  , adapt
  ) where

-- | Middle grey; the luminance 'target' maps the scene mean onto.
key :: Float
key = 0.18

{- | The sensor's gain range.

'maxExposure' bounds how far a dark scene may be lifted — the cave's outer shell wants
~100x, and granting it turns unlit rock into mid-grey mud.
-}
minExposure, maxExposure :: Float
minExposure = 0.05
maxExposure = 8

-- | Exposure mapping @lum@ onto 'key', clamped to the gain range.
target :: Float -> Float
target lum
  | lum > 1e-5 = min maxExposure (max minExposure (key / lum))
  | otherwise = maxExposure

{- | Move @current@ exposure toward @goal@ over @dt@ seconds.

Smoothed in EV (log2) space, so a stop takes the same time wherever it starts, and
per-second rather than per-frame. Rising exposure is dark adaptation — the slow
direction, as in the eye.
-}
adapt :: Float -> Float -> Float -> Float
adapt dt current goal = 2 ** (evNow + (evGoal - evNow) * alpha)
  where
    evNow = logBase 2 (max minExposure current)
    evGoal = logBase 2 goal
    tau = if evGoal > evNow then 2.0 else 0.4
    alpha = 1 - exp (negate dt / tau)
