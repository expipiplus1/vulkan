{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Marshal.Util
  ( isPassAsPointerType
  , writePokes
  , isPassByValue
  , isPassByConstPointer
  , intercalateArrows
  , funName
  , funGetLengthName
  , funGetAllName
  , ptrName
  , dropVk
  , dropVkType
  , dropPointer
  , unReservedWord
  , simpleTypeName
  , isSimpleType
  , appendWithVendor
  ) where

import           Control.Category                         ( (>>>) )
import           Data.Foldable
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Type

import           Write.Monad

-- | Is this a type we don't want to marshal
isPassAsPointerType :: Type -> Bool
isPassAsPointerType = \case
  TypeName n
    | n
      `elem` [ "MirConnection"
             , "wl_display"
             , "wl_surface"
             , "Display"
             , "xcb_connection_t"
             , "AHardwareBuffer"
             , "ANativeWindow"
             , "CAMetalLayer"
             , "SECURITY_ATTRIBUTES"
             ]
    -> True
  _ -> False

writePokes :: Doc () -> [Doc ()] -> WE (Doc ())
writePokes ptr ds = do
  let writePoke :: Int -> (Doc () -> Doc ())
      writePoke n d = [qci|pokeElemOff {ptr} {n} {d}|]
  tellImport "Foreign.Storable" "pokeElemOff"
  pure . hsep $ punctuate "*>" (zipWith writePoke [0..] ds)

isPassByValue :: Parameter -> Bool
isPassByValue = pType >>> \case
  Float      -> True
  Void       -> True
  Char       -> True
  Int        -> True
  Ptr _ _    -> False
  Array{}    -> False
  TypeName _ -> True
  Proto _ _  -> False

isPassByConstPointer :: Parameter -> Bool
isPassByConstPointer = \case
  Parameter _ (Ptr Const _) Nothing Nothing -> True
  _ -> False

intercalateArrows :: [Doc ()] -> Doc ()
intercalateArrows = hsep . punctuate (space <> "->" <> space)

funName :: Text -> Text
funName = T.lowerCaseFirst . dropVk

funGetLengthName :: Text -> Maybe Text
funGetLengthName n =
  let withoutVk = dropVk n
  in  asum
      $   ($ withoutVk)
      .   uncurry replacePrefix
      <$> [("get", "getNum"), ("enumerate", "getNum")]

funGetAllName :: Text -> Maybe Text
funGetAllName n =
  let withoutVk = dropVk n
  in  asum
      $   ($ withoutVk)
      .   uncurry replacePrefix
      <$> [("get", "getAll"), ("enumerate", "enumerateAll")]

replacePrefix :: Text -> Text -> Text -> Maybe Text
replacePrefix prefix replacement t = do
  dropped <- T.dropPrefix prefix t
  pure $ replacement <> dropped

ptrName :: Text -> Text
ptrName = ("p" <>) . T.upperCaseFirst

dropVk :: Text -> Text
dropVk = T.lowerCaseFirst . T.dropPrefix' "vk"

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"

dropPointer :: Text -> Text
dropPointer =
  T.lowerCaseFirst
    . uncurry (<>)
    . first (\p -> if T.all (== 'p') p then "" else p)
    . T.span isLower

appendWithVendor :: Text -> Text -> Text
appendWithVendor a b =
  let prefix = T.dropWhileEnd isUpper a
      vendor = T.takeWhileEnd isUpper a
  in prefix <> b <> vendor

unReservedWord :: Text -> Text
unReservedWord t = if t `elem` (keywords ++ preludeWords) then t <> "'" else t
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
    preludeWords =
      [ "filter"
      ]

simpleTypeName :: Type -> Maybe Text
simpleTypeName = \case
  Float      -> pure "Float"
  Void       -> Nothing
  Char       -> pure "CChar"
  Int        -> pure "CInt"
  Ptr _ _    -> Nothing
  Array{}    -> Nothing
  TypeName n -> pure n
  Proto _ _  -> Nothing

isSimpleType :: Type -> Bool
isSimpleType = \case
  Float      -> True
  Void       -> False
  Char       -> True
  Int        -> True
  Ptr _ _    -> False
  Array{}    -> False
  TypeName _ -> True
  Proto _ _  -> False

