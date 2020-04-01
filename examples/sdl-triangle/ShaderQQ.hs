module ShaderQQ
  ( comp
  , frag
  , geom
  , tesc
  , tese
  , vert
  , compileShader
  ) where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.FileEmbed
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import           System.IO
import           System.IO.Temp
import           System.Process.Typed

comp = shaderQQ "comp"
frag = shaderQQ "frag"
geom = shaderQQ "geom"
tesc = shaderQQ "tesc"
tese = shaderQQ "tese"
vert = shaderQQ "vert"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = QuasiQuoter
  { quoteExp  = \code -> do
                  bs <- runIO $ compileShader stage code
                  bsToExp bs
  , quotePat  = bad "pattern"
  , quoteType = bad "type"
  , quoteDec  = bad "declaration"
  }
 where
  bad :: String -> a
  bad s = error $ "Can't use " <> stage <> " quote in a " <> s <> " context"

-- | Compile a glsl shader to spir-v using glslangValidator
compileShader
  :: String
  -- ^ stage
  -> String
  -- ^ glsl code
  -> IO ByteString
  -- ^ Spir-V bytecode
compileShader stage code = withSystemTempDirectory "th-shader" $ \dir -> do
  let shader = dir <> "/shader." <> stage
      spirv  = dir <> "/shader.spv"
  writeFile shader code
  let p = setStderr inherit
        $ proc "glslangValidator" ["-S", stage, "-V", shader, "-o", spirv]
  runProcess_ p
  -- 'runIO' suggests flushing as GHC may not
  hFlush stderr
  BS.readFile spirv

