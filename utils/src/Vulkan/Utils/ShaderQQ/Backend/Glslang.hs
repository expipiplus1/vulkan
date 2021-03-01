module Vulkan.Utils.ShaderQQ.Backend.Glslang
  ( GlslangError
  , GlslangWarning
  , processGlslangMessages
  ) where

import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.List.Extra
import           System.FilePath

type GlslangError = String
type GlslangWarning = String

processGlslangMessages :: BSL.ByteString -> ([GlslangWarning], [GlslangError])
processGlslangMessages =
  foldr grep ([], []) . filter (not . null) . lines . BSL.unpack
 where
  grep line (ws, es) | "WARNING: " `isPrefixOf` line = (cut line : ws, es)
                     | "ERROR: " `isPrefixOf` line   = (ws, cut line : es)
                     | otherwise                     = (ws, es)

  cut line = takeFileName path <> msg
    where (path, msg) = break (== ':') . drop 1 $ dropWhile (/= ' ') line
