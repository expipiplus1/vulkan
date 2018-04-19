{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Struct
  ( writeStruct
  , fixMemberName
  ) where

import           Control.Bool
import           Data.Char
import           Data.List.Extra
import           Data.Text                                (Text)
import qualified Data.Text.Extra                                as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element
import           Write.Util

writeStruct :: Struct -> Either [SpecError] WriteElement
writeStruct s@Struct {..} = case sStructOrUnion of
  AStruct -> do
    (weDoc, imports, extensions) <- structDoc s
    let weName       = "Struct: " <> sName
        weExtensions = extensions ++ ["DuplicateRecordFields"]
        weImports =
          imports
            ++ [ Import "Foreign.Ptr"      ["plusPtr"]
               , Import "Foreign.Storable" ["Storable(..)"]
               ]
        weProvides = [TypeConstructor sName, Term sName]
        weDepends  = nubOrd (concatMap (typeDepends . smType) sMembers)
    pure WriteElement {..}
  AUnion -> do
    (weDoc, imports, extensions) <- unionDoc s
    let smNames      = toConstructorName <$> (smName <$> sMembers)
        weName       = "Union: " <> sName
        weExtensions = extensions
        weImports =
          imports
            ++ [ Import "Foreign.Ptr"      ["castPtr"]
               , Import "Foreign.Storable" ["Storable(..)"]
               ]
        weProvides = TypeConstructor sName : (Term <$> smNames)
        weDepends  = nubOrd $ concatMap (typeDepends . smType) sMembers
    pure WriteElement {..}

----------------------------------------------------------------
-- Struct
----------------------------------------------------------------

structDoc :: Struct -> Either [SpecError] (Doc (), [Import], [Text])
structDoc s@Struct{..} = do
  let membersFixedNames = fixMemberName <$> sMembers
  (memberDocs, imports, extensions) <- unzip3 <$> traverse memberDoc membersFixedNames
  pure ([qci|
  -- | TODO: Struct comments
  data {sName} = {sName}
    \{ {indent (-2) . vsep $
       intercalatePrepend "," memberDocs
      }
    }
    deriving (Eq, Show)

  instance Storable {sName} where
    sizeOf ~_ = {sSize}
    alignment ~_ = {sAlignment}
    peek ptr = {sName} <$> {indent (-4) . vsep $
                            (intercalatePrepend "<*>" $
                              memberPeekDoc <$> membersFixedNames)}
    poke ptr poked = {indent (-3) . vsep $
                      (intercalatePrepend "*>" $
                       memberPokeDoc s <$> membersFixedNames)}
|], concat imports ++ [Import "Foreign.Storable" ["Storable"]], concat extensions)

memberDoc :: StructMember -> Either [SpecError] (Doc (), [Import], [Text])
memberDoc StructMember{..} = do
  (t, (is, es)) <- toHsType smType
  pure ([qci|
  {smName} :: {t}
|], is, es)

memberPeekDoc :: StructMember -> Doc ()
memberPeekDoc StructMember{..} = [qci|
  peek (ptr `plusPtr` {smOffset})
|]

memberPokeDoc :: Struct -> StructMember -> Doc ()
memberPokeDoc Struct{..} StructMember{..} = [qci|
  poke (ptr `plusPtr` {smOffset}) ({smName} (poked :: {sName}))
|]

----------------------------------------------------------------
-- Unions
----------------------------------------------------------------

unionDoc :: Struct -> Either [SpecError] (Doc (), [Import], [Text])
unionDoc Struct{..} = do
  let membersFixedNames = fixUnionMemberName <$> sMembers
  (memberDocs, imports, extensions ) <- unzip3 <$> traverse unionMemberDoc membersFixedNames
  pure ([qci|
  -- | TODO: Union comments
  data {sName}
    = {indent (-2) . vsep $
       intercalatePrepend "|" memberDocs}
    deriving (Eq, Show)

  -- | _Note_: peek is undefined as we wouldn't know which constructor to use
  instance Storable {sName} where
    sizeOf ~_ = {sSize}
    alignment ~_ = {sAlignment}
    peek _   = error "peek @{sName}"
    poke ptr = \case
      {indent 0 . vcat $ unionMemberPokeDoc <$> membersFixedNames}
|], concat imports ++ [Import "Foreign.Storable" ["Storable"]], concat extensions ++ ["LambdaCase"])

unionMemberDoc :: StructMember -> Either [SpecError] (Doc (), [Import], [Text])
unionMemberDoc StructMember{..} = do
  (t, (is, es)) <- toHsTypePrec 10 smType
  pure ([qci|
  {smName} {t}
|], is, es)

unionMemberPokeDoc :: StructMember -> Doc ()
unionMemberPokeDoc StructMember{..} = [qci|
  {smName} e -> poke (castPtr ptr) e
|]


----------------------------------------------------------------
-- Name utils
----------------------------------------------------------------

fixMemberName :: StructMember -> StructMember
fixMemberName StructMember {..} =
  StructMember {smName = toRecordMemberName smName, ..}

fixUnionMemberName :: StructMember -> StructMember
fixUnionMemberName StructMember {..} =
  StructMember {smName = toConstructorName smName, ..}

-- | Prefix with "vk", make hungarian notation uppercase
toRecordMemberName :: Text -> Text
toRecordMemberName = ("vk" <>) . T.upperCaseFirst . uppercaseHungarian

-- | Prefix with "Vk", make hungarian notation uppercase
toConstructorName :: Text -> Text
toConstructorName = ("Vk" <>) . T.upperCaseFirst . uppercaseHungarian

-- | drop the first word if it is just @p@s and @s@s
uppercaseHungarian :: Text -> Text
uppercaseHungarian t = case T.break isUpper t of
  (firstWord, remainder) | T.all ((== 'p') <||> (== 's')) firstWord ->
    T.map toUpper firstWord <> remainder
  _ -> t
