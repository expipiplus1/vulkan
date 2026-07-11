{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Command-line options.

Every runtime tweakable of the demo, fully resolved at parse time (the
@--outside@ preset folded into the camera and output defaults). Defaults come
from the modules that own the values ('Camera.initial', 'Exposure.defaults',
'Scene.defaultTweaks'), so a bare @visibility-buffer@ renders the same demo as
before.
-}
module Options
  ( Options (..)
  , getOptions
  ) where

import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Options.Applicative
import qualified Vulkan.Core10 as Vk

import qualified Exposure
import qualified Pipeline.Shade as Shade
import qualified Scene
import qualified Scene.Camera as Camera

data Options = Options
  { headless :: Bool
  , orbit :: Camera.Orbit
  -- ^ The launch viewpoint: the view preset with any explicit overrides.
  , output :: FilePath
  -- ^ Where the headless PNG lands.
  , extent :: Vk.Extent2D
  -- ^ The headless render extent (square, @--size@).
  , width :: Int
  , height :: Int
  , debugMode :: Word32
  , meter :: Exposure.Meter
  , tweaks :: Scene.Tweaks
  }
  deriving (Show)

getOptions :: IO Options
getOptions =
  execParser $
    info
      (helper <*> optionsP)
      (fullDesc <> progDesc "Visibility-buffer deferred-shading showcase over a frame graph")

optionsP :: Parser Options
optionsP =
  resolve
    <$> switch (long "headless" <> help "Render once to a PNG plus debug dumps and run the checks, instead of opening a window")
    <*> switch (long "outside" <> help "Start outside the cave (the auto-exposure bench)")
    <*> optional (option auto (long "azimuth" <> metavar "RAD" <> help "Initial camera azimuth (defaults to the view preset's)"))
    <*> optional (option auto (long "elevation" <> metavar "RAD" <> help "Initial camera elevation (defaults to the view preset's)"))
    <*> optional (option auto (long "distance" <> metavar "M" <> help "Initial camera distance from the target (defaults to the view preset's)"))
    <*> optional (strOption (long "output" <> metavar "PNG" <> help "Headless PNG path (defaults to a per-preset name)"))
    <*> option auto (long "size" <> metavar "PX" <> value 256 <> showDefault <> help "Headless render size (square)")
    <*> option auto (long "width" <> metavar "PX" <> value 1024 <> showDefault <> help "Initial window width")
    <*> option auto (long "height" <> metavar "PX" <> value 1024 <> showDefault <> help "Initial window height")
    <*> option auto (long "debug-mode" <> metavar "N" <> value 0 <> showDefault <> help "Debug view (windowed and the headless PNG): 0 beauty, 1 albedo, 2 metalness, 3 roughness, 4 normal, 5 object id, 6 ao")
    <*> meterP
    <*> tweaksP
  where
    resolve headless outside azimuth elevation distance output size width height debugMode meter tweaks =
      Options
        { headless
        , orbit =
            Camera.Orbit
              { azimuth = fromMaybe base.azimuth azimuth
              , elevation = fromMaybe base.elevation elevation
              , distance = fromMaybe base.distance distance
              }
        , output = fromMaybe (if outside then "visibility-buffer-outside.png" else "visibility-buffer.png") output
        , extent = Vk.Extent2D size size
        , width
        , height
        , debugMode
        , meter
        , tweaks
        }
      where
        base = if outside then Camera.outside else Camera.initial

meterP :: Parser Exposure.Meter
meterP =
  Exposure.Meter
    <$> knob "exposure-key" d.key "Middle grey the metered mean luminance maps onto"
    <*> knob "exposure-min" d.minExposure "Sensor gain floor"
    <*> knob "exposure-max" d.maxExposure "Sensor gain ceiling (bounds how far a dark scene is lifted)"
  where
    d = Exposure.defaults

tweaksP :: Parser Scene.Tweaks
tweaksP =
  Scene.Tweaks
    <$> tuningP
    <*> knob "bloom-strength" d.bloomStrength "Bloom mix bias toward the pyramid"
    <*> knob "bloom-radius" d.bloomRadius "Bloom upsample tent radius"
    <*> knob "ao-radius" d.aoRadius "SSAO gather radius, in world units"
    <*> knob "ao-intensity" d.aoIntensity "SSAO obscurance strength (0 disables)"
    <*> knob "ao-bias" d.aoBias "SSAO horizon bias against self-occlusion"
    <*> knob "ao-sharpness" d.aoSharpness "AO blur edge-stop (higher keeps edges crisper)"
  where
    d = Scene.defaultTweaks

tuningP :: Parser Shade.Tuning
tuningP =
  Shade.Tuning
    <$> knob "ambient" d.ambient "Uniform environment radiance"
    <*> knob "indirect" d.indirect "Fraction of irradiance seen as environment"
    <*> knob "bleed" d.bleed "Light-bleed reduction: Chebyshev bounds below this read as fully shadowed"
    <*> knob "shadow-bias" d.shadowBias "Receiver bias in normalized shadow distance"
    <*> knob "normal-bias" d.normalBias "Receiver offset along the normal, metres"
  where
    d = Shade.defaultTuning

-- | A float option with a shown default.
knob :: String -> Float -> String -> Parser Float
knob name def h = option auto (long name <> metavar "X" <> value def <> showDefault <> help h)
