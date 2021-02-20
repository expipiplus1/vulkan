module Vulkan.Utils.ShaderQQ.HLSL.Shaderc
  ( hlsl
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
import qualified Vulkan.Utils.ShaderQQ.HLSL                     as HLSL

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
-- myFragmentShader = $(compileShaderQ Nothing "frag" Nothing [hlsl|
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
hlsl = HLSL.hlsl

-- | QuasiQuoter for creating a compute shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "comp" Nothing [hlsl|...|])@ without
-- interpolation support.
comp :: QuasiQuoter
comp = shaderQQ "comp"

-- | QuasiQuoter for creating a fragment shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "frag" Nothing [hlsl|...|])@ without
-- interpolation support.
frag :: QuasiQuoter
frag = shaderQQ "frag"

-- | QuasiQuoter for creating a geometry shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "geom" Nothing [hlsl|...|])@ without
-- interpolation support.
geom :: QuasiQuoter
geom = shaderQQ "geom"

-- | QuasiQuoter for creating a tessellation control shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "tesc" Nothing [hlsl|...|])@ without
-- interpolation support.
tesc :: QuasiQuoter
tesc = shaderQQ "tesc"

-- | QuasiQuoter for creating a tessellation evaluation shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "tese" Nothing [hlsl|...|])@ without
-- interpolation support.
tese :: QuasiQuoter
tese = shaderQQ "tese"

-- | QuasiQuoter for creating a vertex shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "vert" Nothing [hlsl|...|])@ without
-- interpolation support.
vert :: QuasiQuoter
vert = shaderQQ "vert"

-- | QuasiQuoter for creating a ray generation shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rgen" Nothing [hlsl|...|])@ without
-- interpolation support.
rgen :: QuasiQuoter
rgen = rayShaderQQ "rgen"

-- | QuasiQuoter for creating an intersection shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rint" Nothing [hlsl|...|])@ without
-- interpolation support.
rint :: QuasiQuoter
rint = rayShaderQQ "rint"

-- | QuasiQuoter for creating an any-hit shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rahit" Nothing [hlsl|...|])@ without
-- interpolation support.
rahit :: QuasiQuoter
rahit = rayShaderQQ "rahit"

-- | QuasiQuoter for creating a closest hit shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rchit" Nothing [hlsl|...|])@ without
-- interpolation support.
rchit :: QuasiQuoter
rchit = rayShaderQQ "rchit"

-- | QuasiQuoter for creating a miss shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rmiss" Nothing [hlsl|...|])@ without
-- interpolation support.
rmiss :: QuasiQuoter
rmiss = rayShaderQQ "rmiss"

-- | QuasiQuoter for creating a callable shader.
--
-- Equivalent to calling @$(compileShaderQ (Just "spv1.4") "rcall" Nothing [hlsl|...|])@ without
-- interpolation support.
rcall :: QuasiQuoter
rcall = rayShaderQQ "rcall"

-- | QuasiQuoter for creating a task shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "task" Nothing [hlsl|...|])@ without
-- interpolation support.
task :: QuasiQuoter
task = shaderQQ "task"

-- | QuasiQuoter for creating a mesh shader.
--
-- Equivalent to calling @$(compileShaderQ Nothing "mesh" Nothing [hlsl|...|])@ without
-- interpolation support.
mesh :: QuasiQuoter
mesh = shaderQQ "mesh"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ Nothing stage Nothing }

rayShaderQQ :: String -> QuasiQuoter
rayShaderQQ stage = (badQQ stage) { quoteExp = compileShaderQ (Just "spv1.4") stage Nothing }

-- * Utilities

-- | Compile a HLSL shader to spir-v using glslc.
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: Maybe String
  -- ^ Argument to pass to `--target-env`
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to pass to `-fentry-point=` to specify entry-point function name
  -> String
  -- ^ hlsl shader code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ targetEnv = Shaderc.compileShaderQ targetEnv HLSL

-- | Compile a HLSL shader to spir-v using glslc.
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> Maybe String
  -- ^ Argument to pass to `--target-env`
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to pass to `-fentry-point=` to specify entry-point function name
  -> String
  -- ^ hlsl shader code
  -> m ([ShadercWarning], Either [ShadercError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc targetEnv = Shaderc.compileShader loc targetEnv HLSL
