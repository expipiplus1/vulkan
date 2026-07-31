{-# LANGUAGE OverloadedRecordDot #-}

{-| Visibility-buffer deferred-shading showcase.

Two front-ends over one 'Rendering.Passes.addScenePasses' frame-graph description:

  * @visibility-buffer@ — windowed (GLFW) viewer, presenting every frame;
  * @visibility-buffer --headless@ — one render to a PNG plus deterministic
    checks, for development.

All the tweakables (view presets, camera, exposure, shading, bloom) are
command-line options; see @--help@ ("Options").
-}
module Main
  ( main
  ) where

import qualified Headless
import qualified Options
import qualified Windowed

main :: IO ()
main = do
  opts <- Options.getOptions
  if opts.headless
    then Headless.main opts
    else Windowed.main opts
