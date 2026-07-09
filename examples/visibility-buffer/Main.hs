{-| Visibility-buffer deferred-shading showcase.

Two front-ends over one 'Scene.addScenePasses' frame-graph description:

  * @visibility-buffer@ — windowed (GLFW) viewer, presenting every frame;
  * @visibility-buffer --headless@ — one fixed-extent render to a PNG plus
    deterministic checks, for development;
  * @visibility-buffer --headless --outside@ — the same, from outside the cave.
-}
module Main
  ( main
  ) where

import System.Environment (getArgs)

import qualified Headless
import qualified Windowed

main :: IO ()
main =
  getArgs >>= \case
    ("--headless" : "--outside" : _) -> Headless.main Headless.outsideView
    ("--headless" : _) -> Headless.main Headless.interiorView
    _ -> Windowed.main
