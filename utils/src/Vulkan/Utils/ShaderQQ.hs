module Vulkan.Utils.ShaderQQ
  ( comp
  , frag
  , geom
  , tesc
  , tese
  , vert
  , compileShaderQ
  , compileShader
  , processValidatorMessages
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Char
import           Data.FileEmbed
import           Data.List.Extra
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process.Typed

comp :: QuasiQuoter
comp = shaderQQ "comp"

frag :: QuasiQuoter
frag = shaderQQ "frag"

geom :: QuasiQuoter
geom = shaderQQ "geom"

tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

tese :: QuasiQuoter
tese = shaderQQ "tese"

vert :: QuasiQuoter
vert = shaderQQ "vert"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = QuasiQuoter
  { quoteExp  = compileShaderQ stage
  , quotePat  = bad "pattern"
  , quoteType = bad "type"
  , quoteDec  = bad "declaration"
  }
 where
  bad :: String -> a
  bad s = error $ "Can't use " <> stage <> " quote in a " <> s <> " context"

-- | Compile a glsl shader to spir-v using glslangValidator.
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: String
  -- ^ stage
  -> String
  -- ^ glsl code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ stage code = do
  loc <- location
  result <- compileShader (Just loc) stage code
  bs <- case result of
    Left [] ->
      fail "Unknown validator error"
    Left messages -> do
      reportError $ prepare messages
      pure mempty

    Right ([], bs) ->
      pure bs
    Right (messages, bs) -> do
      reportWarning $ prepare messages
      pure bs
  bsToExp bs
  where
    prepare [singleLine] = singleLine
    prepare multiline    = intercalate "\n" $ "glslangValidator:" : map (mappend "        ") multiline

-- | Compile a glsl shader to spir-v using glslangValidator
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> String
  -- ^ stage
  -> String
  -- ^ glsl code
  -> m (Either [String] ([String], ByteString))
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc stage code = do
  liftIO $ withSystemTempDirectory "th-shader" $ \dir -> do
    let codeWithLineDirective = maybe code (insertLineDirective code) loc
    let shader = dir <> "/shader." <> stage
        spirv  = dir <> "/shader.spv"
    writeFile shader codeWithLineDirective

    (rc, out, err) <- readProcess $ proc "glslangValidator" ["-S", stage, "-V", shader, "-o", spirv]
    let messages = processValidatorMessages (out <> err)
    case rc of
      ExitSuccess -> do
        bs <- BS.readFile spirv
        pure $ Right (messages, bs)
      ExitFailure _rc ->
        pure $ Left messages

processValidatorMessages :: BSL.ByteString -> [String]
processValidatorMessages = foldr grep [] . filter (not . null) . lines . BSL.unpack
  where
    grep line acc
      | "WARNING: " `isPrefixOf` line = cut line : acc
      | "ERROR: "   `isPrefixOf` line = cut line : acc
      | otherwise                     = acc

    cut line = takeFileName path <> msg
      where
        (path, msg) = break (== ':') . drop 1 $ dropWhile (/= ' ') line

-- If possible, insert a #line directive after the #version directive (as well
-- as the extension which allows filenames in line directives.
insertLineDirective :: String -> Loc -> String
insertLineDirective code Loc {..} =
  let isVersionDirective = ("#version" `isPrefixOf`) . dropWhile isSpace
      codeLines = lines code
      (beforeVersion, afterVersion) = break isVersionDirective codeLines
      lineDirective =
        [ "#extension GL_GOOGLE_cpp_style_line_directive : enable"
        , "#line "
          <> show (fst loc_start + length beforeVersion + 1)
          <> " \""
          <> loc_filename
          <> "\""
        ]
  in  case afterVersion of
        []     -> code
        v : xs -> unlines $ beforeVersion <> [v] <> lineDirective <> xs
