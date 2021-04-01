module XrUtils
  ( identityPose
  ) where

import           OpenXR.Core10

identityPose :: Posef
identityPose = Posef { orientation = Quaternionf { x = 0, y = 0, z = 0, w = 1 }
                     , position    = Vector3f { x = 0, y = 0, z = 0 }
                     }

