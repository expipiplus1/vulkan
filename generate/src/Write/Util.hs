{-# LANGUAGE LambdaCase #-}

module Write.Util
  ( intercalatePrepend
  , emptyLineSep
  , vcatPara
  ) where

import           Data.Text.Prettyprint.Doc

-- | 'intercalatePrepend d (x:xs)' will prepend with a space d to xs
intercalatePrepend :: Doc () -> [Doc ()] -> [Doc ()]
intercalatePrepend _ []     = []
intercalatePrepend i (m:ms) = m : ((i <+>) <$> ms)

emptyLineSep :: Foldable f => f (Doc a) -> Doc a
emptyLineSep = concatWith (\a b -> a <> line <> line <> b)

-- | 'vcat' but insers a leading and trailing newline if the list is non-empty
vcatPara :: [Doc a] -> Doc a
vcatPara = \case
  [] -> mempty
  xs -> line <> vcat xs <> line

