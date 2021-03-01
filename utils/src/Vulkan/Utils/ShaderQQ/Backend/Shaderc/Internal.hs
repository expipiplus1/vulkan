module Vulkan.Utils.ShaderQQ.Backend.Shaderc.Internal
  ( compileShaderQ
  , compileShader
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.FileEmbed
import           Language.Haskell.TH
import           System.Exit
import           System.IO.Temp
import           System.Process.Typed
import           Vulkan.Utils.ShaderQQ.ShaderType
import qualified Vulkan.Utils.ShaderQQ.GLSL    as GLSL
import qualified Vulkan.Utils.ShaderQQ.HLSL    as HLSL
import           Vulkan.Utils.ShaderQQ.Backend.Shaderc
import           Vulkan.Utils.ShaderQQ.Backend.Internal

-- * Utilities

-- | Compile a GLSL/HLSL shader to SPIR-V using glslc (from the shaderc project)
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: Maybe String
  -- ^ Argument to pass to `--target-spv`
  -> ShaderType
  -- ^ Argument to specify between glsl/hlsl shader
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to specify entry-point function name for hlsl
  -> String
  -- ^ glsl or hlsl shader code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ targetSpv shaderType stage entryPoint code = do
  loc                <- location
  (warnings, result) <- compileShader (Just loc) targetSpv shaderType stage entryPoint code
  bs <- messageProcess "glslc" reportWarning fail (warnings, result)
  bsToExp bs

-- | Compile a GLSL/HLSL shader to spir-v using glslc
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> Maybe String
  -- ^ Argument to pass to `--target-spv`
  -> ShaderType
  -- ^ Argument to specify between glsl/hlsl shader
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to specify entry-point function name for hlsl
  -> String
  -- ^ glsl or hlsl shader code
  -> m ([ShadercWarning], Either [ShadercError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc targetSpv shaderType stage entryPoint code =
  liftIO $ withSystemTempDirectory "th-shader" $ \dir -> do
    let codeWithLineDirective = maybe code (case shaderType of
                                              GLSL -> GLSL.insertLineDirective code
                                              HLSL -> HLSL.insertLineDirective code
                                           ) loc
    let shader = dir <> "/shader." <> stage
        spirv  = dir <> "/shader.spv"
    writeFile shader codeWithLineDirective

    let targetArgs = case targetSpv of
          Nothing -> []
          Just t  -> ["--target-spv=" <> t]
        -- https://github.com/google/shaderc/blob/01dd72d6079ebdc0f96859365ba7abb1b62758bf/glslc/src/main.cc#L64
        entryPointArgs = case entryPoint of
          Nothing -> []
          Just name -> case shaderType of
            GLSL -> []
            HLSL -> ["-fentry-point=" <> name] 
        args = targetArgs ++ entryPointArgs ++ ["-fshader-stage=" <> stage, "-x", show shaderType, shader, "-o", spirv]
    (rc, out, err) <- readProcess $ proc "glslc" args
    let (warnings, errors) = processShadercMessages (out <> err)
    case rc of
      ExitSuccess -> do
        bs <- BS.readFile spirv
        pure (warnings, Right bs)
      ExitFailure _rc -> pure (warnings, Left errors)
