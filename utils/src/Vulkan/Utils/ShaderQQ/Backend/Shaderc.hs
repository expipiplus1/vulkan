module Vulkan.Utils.ShaderQQ.Backend.Shaderc
  ( ShadercError
  , ShadercWarning
  , processShadercMessages
  ) where

import           Control.Monad                  ( void )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Foldable                  ( asum )
import           Text.ParserCombinators.ReadP

type ShadercError = String
type ShadercWarning = String

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
