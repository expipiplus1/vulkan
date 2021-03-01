module Vulkan.Utils.ShaderQQ.GLSL.Shaderc
  ( glsl
  , comp
  , frag
  , geom
  , tesc
  , tese
  , vert
  , rgen
  , rint
  , rahit
  , rchit
  , rmiss
  , rcall
  , task
  , mesh
  , compileShaderQ
  , compileShader
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString                                 ( ByteString )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Vulkan.Utils.Internal                           ( badQQ )
import           Vulkan.Utils.ShaderQQ.ShaderType
import           Vulkan.Utils.ShaderQQ.Backend.Shaderc           ( ShadercError, ShadercWarning )
import qualified Vulkan.Utils.ShaderQQ.Backend.Shaderc.Internal as Shaderc
import qualified Vulkan.Utils.ShaderQQ.GLSL                     as GLSL

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
-- myFragmentShader = $(compileShaderQ Nothing "frag" [glsl|
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
-- "#version 450\n#extension GL_GOOGLE_cpp_style_line_directive : enable\n#line 52 \"<interactive>\"\n"
--
-- Note that line number will be thrown off if any of the interpolated
-- variables contain newlines.
glsl :: QuasiQuoter
glsl = GLSL.glsl

-- | QuasiQuoter for creating a compute shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "comp" [glsl|...|])@ without
-- interpolation support.
comp :: QuasiQuoter
comp = shaderQQ "comp"

-- | QuasiQuoter for creating a fragment shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "frag" [glsl|...|])@ without
-- interpolation support.
frag :: QuasiQuoter
frag = shaderQQ "frag"

-- | QuasiQuoter for creating a geometry shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "geom" [glsl|...|])@ without
-- interpolation support.
geom :: QuasiQuoter
geom = shaderQQ "geom"

-- | QuasiQuoter for creating a tessellation control shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "tesc" [glsl|...|])@ without
-- interpolation support.
tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

-- | QuasiQuoter for creating a tessellation evaluation shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "tese" [glsl|...|])@ without
-- interpolation support.
tese :: QuasiQuoter
tese = shaderQQ "tese"

-- | QuasiQuoter for creating a vertex shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "vert" [glsl|...|])@ without
-- interpolation support.
vert :: QuasiQuoter
vert = shaderQQ "vert"

-- | QuasiQuoter for creating a ray generation shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rgen" [glsl|...|])@ without
-- interpolation support.
rgen :: QuasiQuoter
rgen = rayShaderQQ "rgen"

-- | QuasiQuoter for creating an intersection shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rint" [glsl|...|])@ without
-- interpolation support.
rint :: QuasiQuoter
rint = rayShaderQQ "rint"

-- | QuasiQuoter for creating an any-hit shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rahit" [glsl|...|])@ without
-- interpolation support.
rahit :: QuasiQuoter
rahit = rayShaderQQ "rahit"

-- | QuasiQuoter for creating a closest hit shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rchit" [glsl|...|])@ without
-- interpolation support.
rchit :: QuasiQuoter
rchit = rayShaderQQ "rchit"

-- | QuasiQuoter for creating a miss shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rmiss" [glsl|...|])@ without
-- interpolation support.
rmiss :: QuasiQuoter
rmiss = rayShaderQQ "rmiss"

-- | QuasiQuoter for creating a callable shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rcall" [glsl|...|])@ without
-- interpolation support.
rcall :: QuasiQuoter
rcall = rayShaderQQ "rcall"

-- | QuasiQuoter for creating a task shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "task" [glsl|...|])@ without
-- interpolation support.
task :: QuasiQuoter
task = shaderQQ "task"

-- | QuasiQuoter for creating a mesh shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "mesh" [glsl|...|])@ without
-- interpolation support.
mesh :: QuasiQuoter
mesh = shaderQQ "mesh"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ Nothing stage }

rayShaderQQ :: String -> QuasiQuoter
rayShaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ (Just "spv1.4") stage }

-- * Utilities

-- | Compile a GLSL shader to spir-v using glslc.
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: Maybe String
  -- ^ Argument to pass to `--target-env`
  -> String
  -- ^ stage
  -> String
  -- ^ glsl shader code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ targetEnv stage = Shaderc.compileShaderQ targetEnv GLSL stage Nothing

-- | Compile a GLSL shader to spir-v using glslc.
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> Maybe String
  -- ^ Argument to pass to `--target-env`
  -> String
  -- ^ stage
  -> String
  -- ^ glsl shader code
  -> m ([ShadercWarning], Either [ShadercError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc targetEnv stage = Shaderc.compileShader loc targetEnv GLSL stage Nothing
