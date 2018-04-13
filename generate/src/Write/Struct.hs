{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Struct
  ( writeStruct
  ) where

import           Data.Maybe
import           Data.Text                                (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element
import           Write.Util

writeStruct :: Struct -> Either [SpecError] WriteElement
writeStruct s@Struct {..} = do
  (weDoc, imports, extensions) <- structDoc s
  let weExtensions = extensions ++ ["DuplicateRecordFields"]
      weImports    = imports
      weProvides   = []
      weDepends    = []
  pure $ WriteElement {..}

structDoc :: Struct -> Either [SpecError] (Doc (), [Import], [Text])
structDoc s@Struct{..} = do
  (memberDocs, imports, extensions) <- unzip3 <$> traverse memberDoc sMembers
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
                              memberPeekDoc <$> sMembers)}
    poke ptr poked = {indent (-3) . vsep $
                      (intercalatePrepend "*>" $
                       memberPokeDoc s <$> sMembers)}
|], concat imports, concat extensions)

memberDoc :: StructMember -> Either [SpecError] (Doc (), [Import], [Text])
memberDoc StructMember{..} = do
  (t, (is, es)) <- toHsType smType
  pure $ ([qci|
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

{-
----------------------------------------------------------------
--
----------------------------------------------------------------

  pure [qc|{predocComment structComment}
data {stName st} =
  {stName st}\{ {indent (-2) .  vsep $
                 (intercalateRecordCommas structMemberDocs ++
                  [fromString "\}"])}
  deriving (Eq, Ord, Show)

{writeStructStorableInstance env st}
|]

writeUnionType :: UnionType -> Write Doc
writeUnionType ut = do
  let unionComment = unlines $ maybeToList (utComment ut)
                             -- Enable this when we can relink the comments.
                             -- ++ utUsage ut
  unionMemberDocs <- traverse writeUnionMember (utMembers ut)
  env <- askTypeEnv
  tellRequiredName
    (ExternalName (ModuleName "Foreign.Storable") "Storable(..)")
  tellRequiredName
    (ExternalName (ModuleName "Foreign.Ptr") "castPtr")
  tellExtension "Strict"
  pure [qc|{predocComment unionComment}
data {utName ut} = {indent (-2) . vsep $
                    intercalatePrepend (fromString "|") unionMemberDocs}
  deriving (Eq, Ord, Show)

{writeUnionStorableInstance env ut}
|]

writeUnionMember :: StructMember -> Write Doc
writeUnionMember um = do
  let constructorName = unionConstructorName um
      constructorComment = fromMaybe "" (smComment um)
  -- Monkey face :)
  constructorTypes <- (:[]) <$> cTypeToHsType (smCType um)
  pure [qc|{prettyPrint $ ConDecl (Ident constructorName) constructorTypes} {postdocComment constructorComment}|]

writeUnionStorableInstance :: TypeEnv -> UnionType -> Doc
writeUnionStorableInstance env ut
  | null (utMembers ut) = error "zero member union...?"
  | otherwise = let memberPacking = calculateMemberPacking env (utMembers ut)
                    TypeInfo unionSize unionAlignment = getTypeInfo env (utName ut)
                in [qc|{predocComment
"_Note_: peek is undefined as we wouldn't know which constructor to use"}
instance Storable {utName ut} where
  sizeOf ~_ = {unionSize}
  alignment ~_ = {unionAlignment}
  peek ~_ = error "peek@{utName ut}"
  poke ptr poked = case poked of
                     {indent 0 .vsep $ writeMatchAndPokeMember <$> memberPacking}
|]

writeMatchAndPokeMember :: MemberInfo -> Doc
writeMatchAndPokeMember mi =
  let constructorName = unionConstructorName (miMember mi)
  in [qc|{constructorName} e -> poke (castPtr ptr) e|]

unionConstructorName :: StructMember -> String
unionConstructorName = upperFirst . sanitizedName

intercalateRecordCommas :: [Doc] -> [Doc]
intercalateRecordCommas = intercalatePrepend (fromString ",")

intercalateInfixAp :: [Doc] -> [Doc]
intercalateInfixAp = intercalatePrepend (fromString "<*>")

writeStructMember :: StructMember -> Write Doc
writeStructMember sm = do
  let memberComment = postdocComment (fromMaybe "" (smComment sm))
  hsType <- cTypeToHsTypeString (smCType sm)
  pure [qc|{sanitizedName sm} :: {hsType} {memberComment}|]

-- | The namespace gets super polluted without these "vk" prefixes
sanitizedName :: StructMember -> String
sanitizedName sm = "vk" ++ upperFirst (smName sm)

writeStructStorableInstance :: TypeEnv -> StructType -> Doc
writeStructStorableInstance env st
  | null (stMembers st) = error "zero member struct...?"
  | otherwise = let memberPacking = calculateMemberPacking env (stMembers st)
                    TypeInfo structSize structAlignment = getTypeInfo env (stName st)
                in [qc|instance Storable {stName st} where
  sizeOf ~_ = {structSize}
  alignment ~_ = {structAlignment}
  peek ptr = {stName st} <$> {indent (-4) . vsep $
                              ((intercalateInfixAp $
                                writePeekMember <$> memberPacking))}
  poke ptr poked = {indent (-3) . vsep $
                    ((intercalatePrepend (fromString "*>") $
                     writePokeMember st <$> memberPacking))}
|]

writePeekMember :: MemberInfo -> Doc
writePeekMember mi = [qc|peek (ptr `plusPtr` {miOffset mi})|]

writePokeMember :: StructType -> MemberInfo -> Doc
writePokeMember st mi =
  [qc|poke (ptr `plusPtr` {miOffset mi}) ({sanitizedName (miMember mi)} (poked :: {stName st}))|]

-}
