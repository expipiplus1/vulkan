module Vulkan.Utils.ShaderQQ.GLSL
  ( glsl
  , insertLineDirective
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Vulkan.Utils.Internal                  ( badQQ )
import           Vulkan.Utils.ShaderQQ.Interpolate
import           Data.Char
import           Data.List.Extra

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
-- An explicit example (@<interactive>@ is from doctest):
--
-- >>> let version = 450 :: Int in [glsl|#version $version|]
-- "#version 450\n#extension GL_GOOGLE_cpp_style_line_directive : enable\n#line ... \"<interactive>\"\n"
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
