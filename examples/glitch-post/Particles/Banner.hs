{-| The banner the swarm assembles into: a message in a 5×7 bitfont, laid out as
one goal cell per particle.
-}
module Particles.Banner
  ( goals
  ) where

import Geomancy.Vec2 (Vec2, vec2)

{- | One goal per particle, round-robin over the lit cells with a little jitter,
so a handful of particles thicken each pixel.
-}
goals :: Int -> [Vec2]
goals n =
  [ vec2 (cellSize * x + jitter (3 * i)) (cellSize * y + jitter (3 * i + 1))
  | i <- [0 .. n - 1]
  , let (x, y) = cells !! (i `mod` length cells)
  ]

-- | Cell pitch in eye-space units, small enough that the text clears the tube's tiles.
cellSize :: Float
cellSize = 0.056

jitter :: Int -> Float
jitter k = 0.02 * (hash k - 0.5)

hash :: Int -> Float
hash k =
  let x = sin (fromIntegral k * 12.9898) * 43758.5453
  in x - fromIntegral (floor x :: Int)

{- | The lit cells in cell coordinates: x right, y /down/ (matching eye space),
origin at the banner's centre.
-}
cells :: [(Float, Float)]
cells =
  [ (fromIntegral (glyphPitch * column + x) - lineWidth line / 2, fromIntegral y - glyphMiddle + baseline row)
  | (row, line) <- zip [0 :: Int ..] message
  , (column, char) <- zip [0 :: Int ..] line
  , (y, pixels) <- zip [0 :: Int ..] (glyph char)
  , (x, pixel) <- zip [0 :: Int ..] pixels
  , pixel == '#'
  ]
  where
    lineWidth line = fromIntegral (glyphPitch * length line - 1) :: Float
    baseline row = lineSpacing * (fromIntegral row - fromIntegral (length message - 1) / 2)

message :: [String]
message = ["HASKELL", "\9829VULKAN\9829"]

-- | Five columns and a space.
glyphPitch :: Int
glyphPitch = 6

-- | The middle row of a seven-row glyph.
glyphMiddle :: Float
glyphMiddle = 3

-- | Seven-row glyphs plus two rows of leading.
lineSpacing :: Float
lineSpacing = 9

-- | A tiny 5×7 bitfont covering the message's characters (and a heart).
glyph :: Char -> [String]
glyph = \case
  'A' ->
    [ ".###."
    , "#...#"
    , "#...#"
    , "#####"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'E' ->
    [ "#####"
    , "#...."
    , "#...."
    , "####."
    , "#...."
    , "#...."
    , "#####"
    ]
  'H' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#####"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'K' ->
    [ "#...#"
    , "#..#."
    , "#.#.."
    , "##..."
    , "#.#.."
    , "#..#."
    , "#...#"
    ]
  'L' ->
    [ "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#####"
    ]
  'N' ->
    [ "#...#"
    , "##..#"
    , "#.#.#"
    , "#..##"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'S' ->
    [ ".####"
    , "#...."
    , "#...."
    , ".###."
    , "....#"
    , "....#"
    , "####."
    ]
  'U' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , ".###."
    ]
  'V' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , ".#.#."
    , "..#.."
    ]
  '\9829' ->
    [ ".#.#."
    , "#####"
    , "#####"
    , ".###."
    , "..#.."
    , "....."
    , "....."
    ]
  _ -> replicate 7 "....."
