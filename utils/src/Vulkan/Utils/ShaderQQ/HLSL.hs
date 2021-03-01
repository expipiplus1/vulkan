module Vulkan.Utils.ShaderQQ.HLSL where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Vulkan.Utils.Internal                  ( badQQ )
import           Vulkan.Utils.ShaderQQ.Interpolate

-- | 'hlsl' is a QuasiQuoter which produces HLSL source code with a @#line@
-- directive inserted so that error locations point to the correct location in
-- the Haskell source file. It also permits basic string interpolation.
--
-- - Interpolated variables are prefixed with @$@
-- - They can optionally be surrounded with braces like @${foo}@
-- - Interpolated variables are converted to strings with 'show'
-- - To escape a @$@ use @\\$@
--
-- An explicit example (@<interactive>@ is from doctest):
--
-- >>> let foo = 450 :: Int in [hlsl|const float foo = $foo|]
-- "#line 77 \"<interactive>\"\nconst float foo = 450"
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

-- Insert a #line directive with the specified location at the top of the file
insertLineDirective :: String -> Loc -> String
insertLineDirective code Loc {..} =
  let lineDirective =
        "#line " <> show (fst loc_start) <> " \"" <> loc_filename <> "\""
  in  lineDirective <> "\n" <> code
