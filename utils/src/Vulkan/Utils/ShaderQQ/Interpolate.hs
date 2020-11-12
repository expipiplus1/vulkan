{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Vulkan.Utils.ShaderQQ.Interpolate
  ( interpExp
  ) where

import           Control.Applicative            ( liftA2 )
import           Data.Char
import           Language.Haskell.TH
import           Text.ParserCombinators.ReadP

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Data.Proxy

-- | 'interpExp' performs very simple interpolation of Haskell
-- values into 'String's.
--
-- - Interpolated variables are prefixed with @$@
-- - They can optionally be surrounded with braces like @${foo}@
-- - Interpolated variables are converted to strings with 'show'
-- - To escape a @$@ use @\\$@
--
-- >>> let foo = 123 in $(interpExp "hello, $foo")
-- "hello, 123"
--
-- >>> let foo = "world" in $(interpExp "hello, \\$foo")
-- "hello, $foo"
--
-- >>> let foo = "world" in $(interpExp "hello\r\n\rworld")
-- "hello\r\n\rworld"
interpExp :: String -> Q Exp
interpExp =
  foldEither (litE (stringL ""))
             (appE (varE 'show) . varOrConE)
             (litE . stringL)
             (\e1 e2 -> [|$e1 <> $e2|])
    . parse

----------------------------------------------------------------
-- The parser
----------------------------------------------------------------

type Var = String

-- >>> parse ""
-- []
--
-- >>> parse "hello $world"
-- [Right "hello ",Left "world"]
--
-- >>> parse "$hello$world"
-- [Left "hello",Left "world"]
--
-- >>> parse "$"
-- [Right "$"]
--
-- >>> parse "hi"
-- [Right "hi"]
--
-- >>> parse "h$hi"
-- [Right "h",Left "hi"]
--
-- >>> parse "$$hi"
-- [Right "$",Left "hi"]
--
-- >>> parse "$1"
-- [Right "$1"]
--
-- >>> parse "$$$"
-- [Right "$$$"]
--
-- >>> parse "\\"
-- [Right "\\"]
--
-- >>> parse "\\$"
-- [Right "$"]
--
-- >>> parse "\\$hi"
-- [Right "$hi"]
--
-- >>> parse "\\\\$hi"
-- [Right "\\$hi"]
--
-- >>> parse "\\hi"
-- [Right "\\hi"]
--
-- >>> parse "$hi\\$foo"
-- [Left "hi",Right "$foo"]
--
-- >>> parse "hello, \\$foo"
-- [Right "hello, \\$foo"]
--
-- >>> parse "${fo'o}bar"
-- [Left "fo'o",Right "bar"]
--
-- >>> parse "\\"
-- [Right "\\"]
--
-- >>> parse "\\\\$"
-- [Right "\\$"]
parse :: String -> [Either Var String]
parse s =
  let -- A haskell var or con
      ident = (:) <$> satisfy (isLower <||> isUpper <||> (== '_')) <*> munch
        (isAlphaNum <||> (== '\'') <||> (== '_'))
      braces = between (char '{') (char '}')
      -- parse a var, a '$' followed by an ident
      var    = char '$' *> (Left <$> (ident +++ braces ident))
      -- Everything up to a '$' or '\'
      normal = Right <$> munch1 ((/= '$') <&&> (/= '\\'))
      -- escape a $
      escape = char '\\' *> (Right <$> (string "$" +++ pure "\\"))
      -- One normal or var
      -- - Check escaped '$' first
      -- - variables, starting with $
      -- - normal string
      one    = normal +++ var +++ escape
      parser = many one <* eof
  in  case readP_to_S parser s of
        [(r, "")] -> foldr mergeRights [] r
        _         -> error "Failed to parse string"

mergeRights :: Either Var String -> [Either Var String] -> [Either Var String]
mergeRights = \case
  Left  v -> (Left v :)
  Right n -> \case
    (Right m : xs) -> Right (n <> m) : xs
    xs             -> Right n : xs

(<&&>), (<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
(<&&>) = liftA2 (&&)

----------------------------------------------------------------
-- Misc utilities
----------------------------------------------------------------

varOrConE :: String -> ExpQ
varOrConE n = (if isLower (head n) then varE else conE) . mkName $ n

foldEither
  :: (Foldable t, Functor t)
  => c
  -> (a -> c)
  -> (b -> c)
  -> (c -> c -> c)
  -> t (Either a b)
  -> c
foldEither i l r f = foldr f i . fmap (either l r)
