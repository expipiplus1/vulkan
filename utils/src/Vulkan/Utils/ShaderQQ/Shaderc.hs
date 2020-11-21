module Vulkan.Utils.ShaderQQ.Shaderc
  ( hlsl
  , comp
  , frag
  , geom
  , tesc
  , tese
  , vert
  , ShadercError
  , ShadercWarning
  , compileShaderQ
  , compileShader
  , processShadercMessages
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.FileEmbed
import           Data.Foldable                  ( asum )
import           Data.List.Extra
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit
import           System.IO.Temp
import           System.Process.Typed
import           Text.ParserCombinators.ReadP
import           Vulkan.Utils.ShaderQQ.Interpolate

-- $setup
-- >>> :set -XQuasiQuotes

-- | 'hlsl' is a QuasiQuoter which produces HLSL source code with a @#line@
-- directive inserted so that error locations point to the correct location in
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
-- myFragmentShader = $(compileShaderQ "frag" [hlsl|
--   static const float myConstant = ${myConstant};
--   float main (){
--     return myConstant;
--   }
-- |])
-- @
--
-- An explicit example (@<interactive>@ is from doctest):
--
-- >>> let foo = 450 :: Int in [hlsl|const float foo = $foo|]
-- "#line 31 \"<interactive>\"\nconst float foo = 450"
--
-- Note that line number will be thrown off if any of the interpolated
-- variables contain newlines.
hlsl :: QuasiQuoter
hlsl = (badQQ "hlsl")
  { quoteExp = \s -> do
                 loc <- location
                 -- Insert the directive here, `compileShaderQ` will insert
                 -- another one, but it's before this one, so who cares.
                 let codeWithLineDirective = insertLineDirective s loc
                 interpExp codeWithLineDirective
  }

-- | QuasiQuoter for creating a compute shader.
--
-- Equivalent to calling @$(compileShaderQ "comp" [hlsl|...|])@ without
-- interpolation support.
comp :: QuasiQuoter
comp = shaderQQ "comp"

-- | QuasiQuoter for creating a fragment shader.
--
-- Equivalent to calling @$(compileShaderQ "frag" [hlsl|...|])@ without
-- interpolation support.
frag :: QuasiQuoter
frag = shaderQQ "frag"

-- | QuasiQuoter for creating a geometry shader.
--
-- Equivalent to calling @$(compileShaderQ "geom" [hlsl|...|])@ without
-- interpolation support.
geom :: QuasiQuoter
geom = shaderQQ "geom"

-- | QuasiQuoter for creating a tessellation control shader.
--
-- Equivalent to calling @$(compileShaderQ "tesc" [hlsl|...|])@ without
-- interpolation support.
tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

-- | QuasiQuoter for creating a tessellation evaluation shader.
--
-- Equivalent to calling @$(compileShaderQ "tese" [hlsl|...|])@ without
-- interpolation support.
tese :: QuasiQuoter
tese = shaderQQ "tese"

-- | QuasiQuoter for creating a vertex shader.
--
-- Equivalent to calling @$(compileShaderQ "vert" [hlsl|...|])@ without
-- interpolation support.
vert :: QuasiQuoter
vert = shaderQQ "vert"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ stage }

-- * Utilities

-- | Compile a HLSL shader to SPIR-V using glslc (from the shaderc project)
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: String
  -- ^ stage
  -> String
  -- ^ glsl or code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ stage code = do
  loc                <- location
  (warnings, result) <- compileShader (Just loc) stage code
  case warnings of
    []    -> pure ()
    _some -> reportWarning $ prepare warnings

  bs <- case result of
    Left []     -> fail "glslc failed with no errors"
    Left errors -> do
      reportError $ prepare errors
      pure mempty
    Right bs -> pure bs

  bsToExp bs

 where
  prepare [singleLine] = singleLine
  prepare multiline =
    intercalate "\n" $ "glslc:" : map (mappend "        ") multiline

type ShadercError = String
type ShadercWarning = String

-- | Compile a HLSL shader to spir-v using glslc
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> String
  -- ^ stage
  -> String
  -- ^ HLSL code
  -> m ([ShadercWarning], Either [ShadercError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc stage code =
  liftIO $ withSystemTempDirectory "th-shader" $ \dir -> do
    let codeWithLineDirective = maybe code (insertLineDirective code) loc
    let shader = dir <> "/shader.hlsl"
        spirv  = dir <> "/shader.spv"
    writeFile shader codeWithLineDirective

    (rc, out, err) <- readProcess $ proc
      "glslc"
      ["-fshader-stage=" <> stage, "-x", "hlsl", shader, "-o", spirv]
    let (warnings, errors) = processShadercMessages (out <> err)
    case rc of
      ExitSuccess -> do
        bs <- BS.readFile spirv
        pure (warnings, Right bs)
      ExitFailure _rc -> pure (warnings, Left errors)

processShadercMessages :: BSL.ByteString -> ([ShadercWarning], [ShadercError])
processShadercMessages = foldMap parseMsg . lines . BSL.unpack

-- >>> parseMsg "blah"
-- ([],[])
--
-- >>> parseMsg "blah"
-- ([],["blah"])
--
-- >>> parseMsg "foo:2: error: unknown var"
-- ([],["foo:2: unknown var"])
--
-- >>> parseMsg "foo:2: warning: unknown var"
-- (["foo:2: unknown var"],[])
--
-- >>> parseMsg "bar:2: error: 'a' : unknown variable"
-- ([],["bar:2: 'a' : unknown variable"])
--
-- >>> parseMsg "f:o: error: f:o:2: 'a' : unknown variable"
-- ([],["f:o:2: 'a' : unknown variable"])
--
-- >>> parseMsg "f:o: error: f:o:2: 'return' : type does not match, or is not convertible to, the function's return type"
-- ([],["f:o:2: 'return' : type does not match, or is not convertible to, the function's return type"])
--
-- >>> parseMsg "foo: foo(1): error at column 3, HLSL parsing failed."
-- ([],["foo:1: error at column 3, HLSL parsing failed."])
parseMsg :: String -> ([ShadercWarning], [ShadercError])
parseMsg = runParser $ foldl1
  (<++)
  [ do
    f    <- filename
    line <- between colon colon number
    skipSpaces
    t   <- msgType
    msg <- manyTill get eof
    pure $ formatMsg t f line msg
  , do
    f <- filename
    colon *> skipSpaces
    t    <- msgType
    _    <- string f
    line <- between (char ':') (char ':') number
    skipSpaces
    msg <- manyTill get eof
    pure $ formatMsg t f line msg
  , do
    f <- filename
    colon *> skipSpaces
    _    <- string f
    line <- between (char '(') (char ')') number
    colon *> skipSpaces
    let t x = ([], [x])
    msg <- manyTill get eof
    pure $ formatMsg t f line msg
  , do
    _ <- number
    skipSpaces
    _ <- string "errors generated"
    eof
    pure ([], [])
  , do
    -- Unknown format
    msg <- manyTill get eof
    eof
    pure ([], [msg])
  ]
 where
  formatMsg t f line msg = t (f <> ":" <> show line <> ": " <> msg)
  filename = many1 get
  number   = readS_to_P (reads @Integer)
  colon    = void $ char ':'
  msgType =
    asum
        [ (\x -> ([], [x])) <$ string "error"
        , (\x -> ([x], [])) <$ string "warning"
        ]
      <* colon
      <* skipSpaces

runParser :: Monoid p => ReadP p -> String -> p
runParser p s = case readP_to_S p s of
  [(r, "")] -> r
  _         -> mempty

-- Insert a #line directive with the specified location at the top of the file
insertLineDirective :: String -> Loc -> String
insertLineDirective code Loc {..} =
  let lineDirective =
        "#line " <> show (fst loc_start) <> " \"" <> loc_filename <> "\""
  in  lineDirective <> "\n" <> code

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
