{-# LANGUAGE OverloadedRecordDot #-}

{-| The EVSM encoding parameters.

Specialization constants of both the occluder that writes the moment cubes
("Pipeline.Shadow.Occluder") and the resolve that decodes them
("Pipeline.Shade.Shader"). The moments bake them in, and the static lights' cube
faces are rendered once at setup, so these cannot vary per frame.
-}
module Pipeline.Shadow.Params
  ( Params (..)
  ) where

import Vulkan.Utils.Pipeline.Specialization (Specialization (..), SpecializationConst (..))

data Params = Params
  { far :: Float
  {- ^ Divisor taking light-space distance into the warp's unit range.

  @exp(2 * warpC * d)@ overflows fp32 past @d ≈ 1.48@, so nothing may sit further
  than @1.48 * far@ from a light; below that it trades moment precision, and wants
  to be about the scene radius.
  -}
  , warpC :: Float
  -- ^ Exponential warp constant, fitting the squared moments inside fp32.
  }
  deriving (Eq, Ord, Show)

-- | Ascending @constant_id@, as both shaders declare them.
instance Specialization Params where
  specializationData p = [packConstData p.far, packConstData p.warpC]
