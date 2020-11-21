module Vulkan.Utils.ShaderQQ
  ( glsl
  , comp
  , frag
  , geom
  , tesc
  , tese
  , vert
  , GLSLError
  , GLSLWarning
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
import           Vulkan.Utils.ShaderQQ.Interpolate

-- $setup
-- >>> :set -XQuasiQuotes

-- | 'glsl' is a QuasiQuoter which produces GLSL source code with @#line@
-- directives inserted so that error locations point to the correct location in
-- the Haskell source file. It also permits basic string interpolation.
--
-- - Interpolated variables are prefixed with @$@
-- - They can optionally be surrounded with braces like @${foo}@
-- - Interpolated variables are converted to strings with 'show'
-- - To escape a @$@ use @\\$@
--
-- It is intended to be used in concert with 'compileShaderQ' like so
--
-- @
-- myConstant = 3.141 -- Note that this will have to be in a different module
-- myFragmentShader = $(compileShaderQ "frag" [glsl|
--   #version 450
--   const float myConstant = ${myConstant};
--   main (){
--   }
-- |])
-- @
--
-- An explicit example (@<interactive>@ is from doctest):
--
-- >>> let version = 450 :: Int in [glsl|#version $version|]
-- "#version 450\n#extension GL_GOOGLE_cpp_style_line_directive : enable\n#line 46 \"<interactive>\"\n"
--
-- Note that line number will be thrown off if any of the interpolated
-- variables contain newlines.
glsl :: QuasiQuoter
glsl = (badQQ "glsl")
  { quoteExp = \s -> do
                 loc <- location
                 -- Insert the directive here, `compileShaderQ` will insert
                 -- another one, but it's before this one, so who cares.
                 let codeWithLineDirective = insertLineDirective s loc
                 interpExp codeWithLineDirective
  }

-- | QuasiQuoter for creating a compute shader.
--
-- Equivalent to calling @$(compileShaderQ "comp" [glsl|...|])@ without
-- interpolation support.
comp :: QuasiQuoter
comp = shaderQQ "comp"

-- | QuasiQuoter for creating a fragment shader.
--
-- Equivalent to calling @$(compileShaderQ "frag" [glsl|...|])@ without
-- interpolation support.
frag :: QuasiQuoter
frag = shaderQQ "frag"

-- | QuasiQuoter for creating a geometry shader.
--
-- Equivalent to calling @$(compileShaderQ "geom" [glsl|...|])@ without
-- interpolation support.
geom :: QuasiQuoter
geom = shaderQQ "geom"

-- | QuasiQuoter for creating a tessellation control shader.
--
-- Equivalent to calling @$(compileShaderQ "tesc" [glsl|...|])@ without
-- interpolation support.
tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

-- | QuasiQuoter for creating a tessellation evaluation shader.
--
-- Equivalent to calling @$(compileShaderQ "tese" [glsl|...|])@ without
-- interpolation support.
tese :: QuasiQuoter
tese = shaderQQ "tese"

-- | QuasiQuoter for creating a vertex shader.
--
-- Equivalent to calling @$(compileShaderQ "vert" [glsl|...|])@ without
-- interpolation support.
vert :: QuasiQuoter
vert = shaderQQ "vert"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ stage }

-- * Utilities

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
  loc                <- location
  (warnings, result) <- compileShader (Just loc) stage code
  case warnings of
    []    -> pure ()
    _some -> reportWarning $ prepare warnings

  bs <- case result of
    Left []     -> fail "glslangValidator failed with no errors"
    Left errors -> do
      reportError $ prepare errors
      pure mempty
    Right bs -> pure bs

  bsToExp bs

 where
  prepare [singleLine] = singleLine
  prepare multiline =
    intercalate "\n" $ "glslangValidator:" : map (mappend "        ") multiline

type GLSLError = String
type GLSLWarning = String

-- | Compile a glsl shader to spir-v using glslangValidator
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> String
  -- ^ stage
  -> String
  -- ^ glsl code
  -> m ([GLSLWarning], Either [GLSLError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc stage code =
  liftIO $ withSystemTempDirectory "th-shader" $ \dir -> do
    let codeWithLineDirective = maybe code (insertLineDirective code) loc
    let shader = dir <> "/shader." <> stage
        spirv  = dir <> "/shader.spv"
    writeFile shader codeWithLineDirective

    (rc, out, err) <- readProcess
      $ proc "glslangValidator" ["-S", stage, "-V", shader, "-o", spirv]
    let (warnings, errors) = processValidatorMessages (out <> err)
    case rc of
      ExitSuccess -> do
        bs <- BS.readFile spirv
        pure (warnings, Right bs)
      ExitFailure _rc -> pure (warnings, Left errors)

processValidatorMessages :: BSL.ByteString -> ([GLSLWarning], [GLSLError])
processValidatorMessages = foldr grep ([], []) . filter (not . null) . lines . BSL.unpack
  where
    grep line (ws, es)
      | "WARNING: " `isPrefixOf` line = (cut line : ws, es)
      | "ERROR: "   `isPrefixOf` line = (ws, cut line : es)
      | otherwise                     = (ws, es)

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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

badQQ :: String -> QuasiQuoter
badQQ name = QuasiQuoter (bad "expression")
                         (bad "pattern")
                         (bad "type")
                         (bad "declaration")
 where
  bad :: String -> a
  bad context =
    error $ "Can't use " <> name <> " quote in a " <> context <> " context"
