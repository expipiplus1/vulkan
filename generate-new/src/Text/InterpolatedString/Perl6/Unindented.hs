-- | Wrappers for the QuasiQuoters in 'Text.InterpolatedString.Perl6' which
-- remove indentation from the string passed to the quasiquoter.
--
--
-- @
-- trailingNewline :: Text
-- foo = [qci|
--   foo
--
--   |]
-- @
--
-- @
-- noTrailingNewline :: Text
-- noTrailingNewline = [qci|
--   foo
--   |]
-- @
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.InterpolatedString.Perl6.Unindented (qqi, qci, qi, showQ)where

import           Data.Char
import           Data.List
import           Prettyprinter
import           Prettyprinter.Render.String
import           Language.Haskell.TH.Quote
import           Prelude
import           Text.InterpolatedString.Perl6

wrapQuasi :: (String -> String) -> QuasiQuoter -> QuasiQuoter
wrapQuasi f QuasiQuoter {..} =
  QuasiQuoter (quoteExp . f) (quotePat . f) (quoteType . f) (quoteDec . f)

-- | Strips empty lines from the beginning and end. Removes the common space
-- prefix from the nonempty lines
unindent :: String -> String
unindent s =
  let stripEmptyLines  = dropWhile (== "") . dropWhileEnd (== "")
      ls               = stripEmptyLines . lines $ s
      strippedLastLine = if onlySpace (last ls) then init ls else ls
      nonEmpties       = filter (/= "") strippedLastLine
      minIndent        = case nonEmpties of
        [] -> 0
        _  -> minimum (length . takeWhile (== ' ') <$> nonEmpties)
      unindented = drop minIndent <$> strippedLastLine
  in  intercalate "\n" unindented

onlySpace :: String -> Bool
onlySpace = all isSpace

qqi :: QuasiQuoter
qqi = wrapQuasi unindent qq

qci :: QuasiQuoter
qci = wrapQuasi unindent qc

qi :: QuasiQuoter
qi = wrapQuasi unindent q

instance {-# OVERLAPPING  #-} ShowQ (Doc ()) where
  showQ = renderString . layoutPretty (LayoutOptions Unbounded)
