module Vulkan.Utils.ShaderQQ
  ( comp
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

-- | QuasiQuoter for creating a compute shader
comp :: QuasiQuoter
comp = shaderQQ "comp"

-- | QuasiQuoter for creating a fragment shader
frag :: QuasiQuoter
frag = shaderQQ "frag"

-- | QuasiQuoter for creating a geometry shader
geom :: QuasiQuoter
geom = shaderQQ "geom"

-- | QuasiQuoter for creating a tessellation control shader
tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

-- | QuasiQuoter for creating a tessellation evaluation shader
tese :: QuasiQuoter
tese = shaderQQ "tese"

-- | QuasiQuoter for creating a vertex shader
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
