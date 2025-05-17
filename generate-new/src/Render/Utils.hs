{-# language CPP #-}
module Render.Utils
  where

import qualified Data.Text                     as T
import           Prettyprinter
import           Foreign.Marshal.Alloc          ( allocaBytes
                                                , allocaBytesAligned
                                                )
import           Relude
import           Text.Wrap
import qualified Language.Haskell.TH as TH

parenList :: Foldable f => f (Doc ()) -> Doc ()
parenList = genericList (<>) "(" ")"

braceList :: Foldable f => f (Doc ()) -> Doc ()
braceList = genericList (<>) "{" "}"

braceList' :: Foldable f => f (Doc ()) -> Doc ()
braceList' = genericList (<+>) "{" "}"

genericList
  :: Foldable f
  => (Doc () -> Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
  -> f (Doc ())
  -> Doc ()
genericList s l r ds = case toList ds of
  []  -> l <> r
  [d] -> l `s` d `s` r
  _   -> align $ vsep $ zipWith (<+>) (l : repeat ",") (toList ds) <> [r]

-- | Wrap text sensibly
comment :: Text -> Doc ()
comment c =
  let ls = wrapTextToLines defaultWrapSettings 72 c
      prependSpace t = if T.null t then t else " " <> t
  in  vsep $ zipWith (<>) ("-- |" : repeat "--") (pretty . prependSpace <$> ls)

commentNoWrap :: Text -> Doc ()
commentNoWrap c =
  let ls = T.lines c
      prependSpace t = if T.null t then t else " " <> t
  in  vsep $ zipWith (<>) ("-- |" : repeat "--") (pretty . prependSpace <$> ls)

doBlock :: [Doc ()] -> Doc ()
doBlock = \case
  []    -> "pure ()"
  [s]   -> s
  stmts -> "do" <> line <> indent 2 (vsep stmts)

unReservedWord :: Text -> Text
unReservedWord t = if t `elem` (keywords <> preludeWords) then t <> "'" else t
 where
  keywords =
    [ "as"
    , "case"
    , "class"
    , "data family"
    , "data instance"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "family"
    , "forall"
    , "foreign"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "mdo"
    , "module"
    , "newtype"
    , "of"
    , "proc"
    , "qualified"
    , "rec"
    , "then"
    , "type"
    , "where"
    ]
  preludeWords = ["filter", "id"]

chooseAlign :: Int -> (Doc ann, TH.Name, Doc ann -> Doc ann)
chooseAlign align = if align <= 8
  then ("allocaBytes", 'allocaBytes, id)
  else ("allocaBytesAligned", 'allocaBytesAligned, (<+> viaShow align))

#if MIN_VERSION_template_haskell(2,17,0)
plainTVcompat :: TH.Name -> TH.TyVarBndr TH.Specificity
plainTVcompat = (`TH.PlainTV` TH.SpecifiedSpec)
#else
plainTVcompat :: TH.Name -> TH.TyVarBndr
plainTVcompat = TH.PlainTV
#endif
