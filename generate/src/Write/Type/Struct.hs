{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Struct
  ( writeStructTypes
  , writeUnionTypes
  ) where

import Spec.Type
import Text.InterpolatedString.Perl6
import Language.Haskell.Exts.Syntax (ConDecl(..), Name(..))
import Language.Haskell.Exts.Pretty
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils
import Write.TypeConverter
import Data.Maybe(fromMaybe, maybeToList)
import Data.String

writeStructTypes :: TypeEnv -> [StructType] -> String
writeStructTypes env sts = [qc|-- * Struct Types

{vcat $ writeStructType env <$> sts}|] 

writeStructType :: TypeEnv -> StructType -> Doc
writeStructType env st = let structComment = unlines $ 
                                         maybeToList (stComment st) 
                                         -- Enable this when we can relink the
                                         -- comments.
                                         -- ++ stUsage st 
                             structMemberDocs = writeStructMember env <$> 
                                                  stMembers st
                         in [qc|{predocComment structComment}
data {stName st} =
  {stName st}\{ {indent (-2) .  vsep $ 
                 (intercalateRecordCommas structMemberDocs ++ 
                  [fromString "\}"])}
  deriving (Eq)

{writeStructStorableInstance env st}
|]

writeUnionTypes :: TypeEnv -> [UnionType] -> String
writeUnionTypes env sts = [qc|-- * Union Types

{vcat $ writeUnionType env <$> sts}|] 

writeUnionType :: TypeEnv -> UnionType -> Doc
writeUnionType env ut = let unionComment = unlines $ maybeToList (utComment ut) 
                                               -- Enable this when we can relink
                                               -- the comments.
                                               -- ++ utUsage ut 
                            unionMemberDocs = writeUnionMember <$> 
                                                utMembers ut
                        in [qc|{predocComment unionComment}
data {utName ut} = {indent (-2) . vsep $
                    intercalatePrepend (fromString "|") unionMemberDocs}
  deriving (Eq)

{writeUnionStorableInstance env ut}
|]

writeUnionMember :: StructMember -> Doc
writeUnionMember um = 
  let constructorName = unionConstructorName um
      constructorTypes = [cTypeToHsType' (smCType um)]
      constructorComment = fromMaybe "" (smComment um)
  in [qc|{prettyPrint $ ConDecl (Ident constructorName) constructorTypes} {postdocComment constructorComment}|]

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

writeStructMember :: TypeEnv -> StructMember -> Doc
writeStructMember te sm = 
  let memberComment = postdocComment (fromMaybe "" (smComment sm))
  in [qc|{sanitizedName sm} :: {cTypeToHsTypeString (smCType sm)} {memberComment}|]

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

