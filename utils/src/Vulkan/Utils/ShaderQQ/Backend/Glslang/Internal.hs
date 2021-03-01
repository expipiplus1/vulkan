module Vulkan.Utils.ShaderQQ.Backend.Glslang.Internal
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
import           Vulkan.Utils.ShaderQQ.Backend.Glslang
import           Vulkan.Utils.ShaderQQ.Backend.Internal

-- * Utilities

-- | Compile a GLSL/HLSL shader to spir-v using glslangValidator.
--
-- Messages are converted to GHC warnings or errors depending on compilation success.
compileShaderQ
  :: Maybe String
  -- ^ Argument to pass to `--target-env`
  -> ShaderType
  -- ^ Argument to specify between glsl/hlsl shader
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to specify entry-point function name
  -> String
  -- ^ glsl or hlsl shader code
  -> Q Exp
  -- ^ Spir-V bytecode
compileShaderQ targetEnv shaderType stage entryPoint code = do
  loc                <- location
  (warnings, result) <- compileShader (Just loc) targetEnv shaderType stage entryPoint code
  bs <- messageProcess "glslangValidator" reportWarning fail (warnings, result)
  bsToExp bs

-- | Compile a GLSL/HLSL shader to spir-v using glslangValidator
compileShader
  :: MonadIO m
  => Maybe Loc
  -- ^ Source location
  -> Maybe String
  -- ^ Argument to pass to `--target-env`
  -> ShaderType
  -- ^ Argument to specify between glsl/hlsl shader
  -> String
  -- ^ stage
  -> Maybe String
  -- ^ Argument to specify entry-point function name
  -> String
  -- ^ glsl or hlsl shader code
  -> m ([GlslangWarning], Either [GlslangError] ByteString)
  -- ^ Spir-V bytecode with warnings or errors
compileShader loc targetEnv shaderType stage entryPoint code =
  liftIO $ withSystemTempDirectory "th-shader" $ \dir -> do
    let codeWithLineDirective = maybe code (case shaderType of
                                              GLSL -> GLSL.insertLineDirective code
                                              HLSL -> HLSL.insertLineDirective code
                                           ) loc
    let shader = dir <> "/shader." <> stage
        spirv  = dir <> "/shader.spv"
    writeFile shader codeWithLineDirective

    let targetArgs = case targetEnv of
          Nothing -> []
          Just t  -> ["--target-env", t]
        shaderTypeArgs = case shaderType of
          GLSL -> []
          HLSL -> ["-D"]
        -- https://github.com/KhronosGroup/glslang/issues/1045#issuecomment-328707953
        entryPointArgs = case entryPoint of
          Nothing -> []
          Just name -> case shaderType of
            GLSL -> ["-e", name, "--source-entry-point", "main"]
            HLSL -> ["-e", name]
        args = targetArgs ++ shaderTypeArgs ++ entryPointArgs ++ ["-S", stage, "-V", shader, "-o", spirv]
    (rc, out, err) <- readProcess $ proc "glslangValidator" args
    let (warnings, errors) = processGlslangMessages (out <> err)
    case rc of
      ExitSuccess -> do
        bs <- BS.readFile spirv
        pure (warnings, Right bs)
      ExitFailure _rc -> pure (warnings, Left errors)
