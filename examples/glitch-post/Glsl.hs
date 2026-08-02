{-# LANGUAGE QuasiQuotes #-}

{-| The GLSL the demo's shaders share.

Chunks splice into 'glsl' quasiquotes as @$name@ — 'Vulkan.Utils.ShaderQQ.Interpolate'
inserts them with 'show', which 'GlslChunk' turns into raw GLSL — through the
interpolating 'comp' \/ 'vert' compilers, so anything a shader shares with
another shader (or with its own dispatch) is written once.

Eye space is Vulkan-flavoured: x right, y down, z straight into the screen, the
camera at the origin looking down +z.
-}
module Glsl
  ( GlslChunk (..)
  , comp
  , vert
  , zNear
  , bannerZ
  , octahedronZ
  , zFar
  , eyeSpace
  , tau
  , hsv
  , colourFrag
  ) where

import Data.ByteString (ByteString)
import Language.Haskell.TH (Exp, Q)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)
import qualified Vulkan.Utils.ShaderQQ.GLSL.Glslang as Glslang

-- | A GLSL fragment for @$@-splicing into 'glsl' quasiquotes; 'show' is raw text.
newtype GlslChunk = GlslChunk String

instance Show GlslChunk where
  show (GlslChunk s) = s

-- | The stage compilers, pre-applied: @$(comp [glsl|…|])@ splices chunks.
comp, vert :: String -> Q Exp
comp = compileShaderQ Nothing "comp" Nothing
vert = compileShaderQ Nothing "vert" Nothing

-- | The scene's landmarks along eye-space z, from the camera outward.
zNear, bannerZ, octahedronZ, zFar :: Float
zNear = 0.7
bannerZ = 2
octahedronZ = 2.8
zFar = 14

eyeSpace :: GlslChunk
eyeSpace =
  GlslChunk
    [glsl|
    const float zNear = $zNear;
    const float bannerZ = $bannerZ;
    const float octahedronZ = $octahedronZ;
    const float zFar = $zFar;
    |]

tau :: GlslChunk
tau = GlslChunk [glsl|const float TAU = 6.283185307179586;|]

-- | Hue-saturation-value to RGB, as in the other examples' shaders.
hsv :: GlslChunk
hsv =
  GlslChunk
    [glsl|
    vec3 hsv(float h, float s, float v) {
      vec3 k = mod(vec3(5.0, 3.0, 1.0) + fract(h) * 6.0, 6.0);
      return v - v * s * clamp(min(k, 4.0 - k), 0.0, 1.0);
    }
    |]

-- | The fragment stage the tunnel and the sparks share.
colourFrag :: ByteString
colourFrag =
  [Glslang.frag|
    #version 450

    layout(location = 0) in vec3 fragColor;
    layout(location = 0) out vec4 outColor;

    void main() {
      outColor = vec4(fragColor, 1.0);
    }
  |]
