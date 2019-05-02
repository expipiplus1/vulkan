{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Struct
  ( writeStruct
  , fixMemberName
  , toRecordMemberName
  , toMarshalledRecordMemberName
  ) where

import           Control.Bool
import           Data.Char
import           Data.List.Extra
import           Data.Functor
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Foldable
import           Control.Monad

import           Spec.Savvy.Struct
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Util
import           Write.Monad

writeStruct :: Struct -> Write WriteElement
writeStruct s@Struct {..} = case sStructOrUnion of
  AStruct -> runWE ("Struct: " <> sName) $ do
    tellExtension "DuplicateRecordFields"
    tellImport "Foreign.Ptr"      "plusPtr"
    tellImport "Foreign.Storable" "Storable(..)"
    tellExport (TypeConstructor sName)
    tellExport (Term sName)
    tellBootElem <=< liftWrite . runWE ("Struct boot: " <> sName) $ do
      tellExport (TypeConstructor sName)
      pure $ \_ -> pretty $ "data" T.<+> sName
    tellDepend (WE.TypeName "Zero")
    let termDepends = \case
          Just vs -> PatternName <$> vs
          Nothing -> []
    tellDepends $ nubOrd
      (  concatMap (typeDepends . smType)   sMembers
      ++ concatMap (termDepends . smValues) sMembers
      )
    structDoc s
  AUnion -> runWE ("Union: " <> sName) $ do
    tellImport "Foreign.Ptr"      "castPtr"
    tellImport "Foreign.Storable" "Storable(..)"
    let smNames = toConstructorName <$> (smName <$> sMembers)
    traverse_ tellExport $ TypeConstructor sName : (Term <$> smNames)
    tellDepends $ nubOrd (concatMap (typeDepends . smType) sMembers)
    tellBootElem <=< liftWrite . runWE ("Union boot: " <> sName) $ do
      tellExport (TypeConstructor sName)
      pure $ \_ -> pretty $ "data" T.<+> sName
    unionDoc s

----------------------------------------------------------------
-- Struct
----------------------------------------------------------------

structDoc :: Struct -> WE (DocMap -> Doc ())
structDoc s@Struct {..} = do
  let membersFixedNames = fixMemberName <$> sMembers
  memberDocs <- traverse (memberDoc sName) sMembers
  tellImport "Foreign.Storable" "Storable"
  let zeroDocs = membersFixedNames <&> \case
        StructMember{..}
          | smName == "vkSType"
          , smType == TypeName "VkStructureType"
          , Just [v] <- smValues
          -> pretty v
          | otherwise
          -> "zero"
  pure $ \getDoc -> [qci|
  {document getDoc (TopLevel sName)}
  data {sName} = {sName}
    \{ {indent (-2) . vsep $
       intercalatePrepend "," (($ getDoc) <$> memberDocs)
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

  instance Zero {sName} where
    zero = {sName} {indent 0 . vsep $ (zeroDocs :: [Doc()])}
|]

memberDoc
  :: Text
  -> StructMember
  -> WE (DocMap -> Doc ())
memberDoc parentName StructMember{..} = do
  t <- toHsType smType
  pure $ \getDoc -> [qci|
  {document getDoc (Nested parentName smName)}
  {toRecordMemberName smName} :: {t}
|]

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

unionDoc
  :: Struct -> WE (DocMap -> Doc ())
unionDoc Struct{..} = do
  let membersFixedNames = fixUnionMemberName <$> sMembers
  memberDocs <- traverse (unionMemberDoc sName) membersFixedNames
  tellExtension "LambdaCase"
  tellImport "Foreign.Storable" "Storable"
  pure $ \getDoc -> [qci|
  {document getDoc (TopLevel sName)}
  data {sName}
    = {indent (-2) . vsep $
       intercalatePrepend "|" (($ getDoc) <$> memberDocs)}
    deriving (Eq, Show)

  -- | _Note_: peek is undefined as we wouldn't know which constructor to use
  instance Storable {sName} where
    sizeOf ~_ = {sSize}
    alignment ~_ = {sAlignment}
    peek _   = error "peek @{sName}"
    poke ptr = \case
      {indent 0 . vcat $ unionMemberPokeDoc <$> membersFixedNames}

  instance Zero {sName} where
    zero = {smName . head $ membersFixedNames} zero
|]

unionMemberDoc
  :: Text
  -- ^ Parent name
  -> StructMember
  -> WE (DocMap -> Doc ())
unionMemberDoc parentName StructMember{..} = do
  t <- toHsTypePrec 10 smType
  pure $ \getDoc -> [qci|
    {document getDoc (Nested parentName smName)}
    {smName} {t}
  |]

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

-- | Keep name, except for really short names
toMarshalledRecordMemberName :: Text -> Text -> Text
toMarshalledRecordMemberName structName n =
  if T.length n == 1
    then T.lowerCaseFirst $ structName <> T.upperCaseFirst n
    else n

-- | Prefix with "Vk", make hungarian notation uppercase
toConstructorName :: Text -> Text
toConstructorName = ("Vk" <>) . T.upperCaseFirst . uppercaseHungarian

-- | drop the first word if it is just @p@s and @s@s
uppercaseHungarian :: Text -> Text
uppercaseHungarian t = case T.break isUpper t of
  (firstWord, remainder) | T.all ((== 'p') <||> (== 's')) firstWord ->
    T.map toUpper firstWord <> remainder
  _ -> t
