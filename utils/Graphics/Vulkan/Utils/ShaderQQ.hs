module Graphics.Vulkan.Utils.ShaderQQ
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
import           Data.List.Extra
import           Data.Char

comp = shaderQQ "comp"
frag = shaderQQ "frag"
geom = shaderQQ "geom"
tesc = shaderQQ "tesc"
tese = shaderQQ "tese"
vert = shaderQQ "vert"

shaderQQ :: String -> QuasiQuoter
shaderQQ stage = QuasiQuoter
  { quoteExp  = \code -> do
                  loc <- location
                  bs  <- runIO $ compileShader (Just loc) stage code
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
  :: Maybe Loc
  -- ^ Source location
  -> String
  -- ^ stage
  -> String
  -- ^ glsl code
  -> IO ByteString
  -- ^ Spir-V bytecode
compileShader loc stage code = withSystemTempDirectory "th-shader" $ \dir -> do
  let codeWithLineDirective = maybe code (`insertLineDirective` code) loc
  let shader = dir <> "/shader." <> stage
      spirv  = dir <> "/shader.spv"
  writeFile shader codeWithLineDirective
  let -- TODO: writing to stdout here breaks HIE
      p =
        setStderr inherit
          . setStdout (useHandleOpen stderr)
          . setStdin closed
          $ proc "glslangValidator" ["-S", stage, "-V", shader, "-o", spirv]
  runProcess_ p
  -- 'runIO' suggests flushing as GHC may not
  hFlush stderr
  BS.readFile spirv

-- If possible, insert a #line directive after the #version directive (as well
-- as the extension which allows filenames in line directives.
insertLineDirective :: Loc -> String -> String
insertLineDirective Loc {..} code =
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
