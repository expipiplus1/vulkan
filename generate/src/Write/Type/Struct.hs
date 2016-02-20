{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Struct
  ( writeStructTypes
  , writeUnionTypes
  ) where

import Spec.Type
import Data.Char(toUpper)
import Text.InterpolatedString.Perl6
import Language.Haskell.Exts.Syntax (ConDecl(..), Name(..))
import Language.Haskell.Exts.Pretty
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.TypeConverter
import Data.List(foldl1')
import Write.Utils
import Data.Maybe(fromMaybe, maybeToList)
import Data.String

writeStructTypes :: TypeConverter -> [StructType] -> String
writeStructTypes tc sts = [qc|-- * Struct Types

{vcat $ writeStructType tc <$> sts}|] 

writeStructType :: TypeConverter -> StructType -> Doc
writeStructType tc st = let structComment = unlines $ 
                                        maybeToList (stComment st) 
                                        -- Enable this when we can relink the
                                        -- comments.
                                        -- ++ stUsage st 
                            structMemberDocs = writeStructMember tc <$> 
                                                 stMembers st
                        in [qc|{predocComment structComment}
data {stName st} =
  {stName st}\{ {indent (-2) .  vsep $ 
                 (intercalateRecordCommas structMemberDocs ++ 
                  [fromString "\}"])}
  deriving (Eq)

{writeStructStorableInstance st}
|]

writeUnionTypes :: TypeConverter -> [UnionType] -> String
writeUnionTypes tc sts = [qc|-- * Union Types

{vcat $ writeUnionType tc <$> sts}|] 

writeUnionType :: TypeConverter -> UnionType -> Doc
writeUnionType _ ut = let unionComment = unlines $ maybeToList (utComment ut) 
                                             -- Enable this when we can relink
                                             -- the comments.
                                             -- ++ utUsage ut 
                          unionMemberDocs = writeUnionMember <$> 
                                              utMembers ut
                      in [qc|{predocComment unionComment}
data {utName ut} = {indent (-2) . vsep $
                    intercalatePrepend (fromString "|") unionMemberDocs}
  deriving (Eq)

{writeUnionStorableInstance ut}
|]

writeUnionMember :: StructMember -> Doc
writeUnionMember um = 
  let constructorName = unionConstructorName um
      constructorTypes = [cTypeToHsType' (smCType um)]
      constructorComment = fromMaybe "" (smComment um)
  in [qc|{prettyPrint $ ConDecl (Ident constructorName) constructorTypes} {postdocComment constructorComment}|]

writeUnionStorableInstance :: UnionType -> Doc
writeUnionStorableInstance ut 
  | null (utMembers ut) = error "zero member union...?"
  | otherwise = let memberPacking = calculateMemberPacking () (utMembers ut)
                    unionAlignment = foldl1' lcm -- You know, just in case!
                                      (miAlignment <$> memberPacking)
                    unalignedSize = maximum (miSize <$> memberPacking)
                    unionSize = alignTo unionAlignment unalignedSize
                in [qc|{predocComment 
"_Note_: peek is undefined as we wouldn't know which constructor to use"}
instance Storable {utName ut} where
  sizeOf _ = {unionSize}
  alignment _ = {unionAlignment}
  peek _ = error "peek@{utName ut}"
  poke ptr poked = case poked of
                     {indent 0 .vsep $ writeMatchAndPokeMember <$> memberPacking}
|]

writeMatchAndPokeMember :: MemberInfo -> Doc
writeMatchAndPokeMember mi = 
  let constructorName = unionConstructorName (miMember mi)
  in [qc|{constructorName} e -> poke ptr e|]

unionConstructorName :: StructMember -> String
unionConstructorName = upperFirst . sanitizedName 

intercalateRecordCommas :: [Doc] -> [Doc]
intercalateRecordCommas = intercalatePrepend (fromString ",")

intercalateInfixAp :: [Doc] -> [Doc]
intercalateInfixAp = intercalatePrepend (fromString "<*>")

intercalatePrepend :: Doc -> [Doc] -> [Doc]
intercalatePrepend _ [] = []
intercalatePrepend i (m:ms) = m : ((i <+>) <$> ms)

writeStructMember :: TypeConverter -> StructMember -> Doc
writeStructMember tc sm = 
  let memberComment = postdocComment (fromMaybe "" (smComment sm))
  in [qc|{sanitizedName sm} :: {tc (smCType sm)} {memberComment}|]

-- | The namespace gets super polluted without these "vk" prefixes
sanitizedName :: StructMember -> String
sanitizedName sm = "vk" ++ upperFirst (smName sm)

upperFirst :: String -> String
upperFirst "" = ""
upperFirst (x:xs) = toUpper x : xs

writeStructStorableInstance :: StructType -> Doc
writeStructStorableInstance st 
  | null (stMembers st) = error "zero member struct...?"
  | otherwise = let memberPacking = calculateMemberPacking () (stMembers st)
                    structAlignment = foldl1' lcm -- You know, just in case!
                                      (miAlignment <$> memberPacking)
                    lastMemberPacking = last memberPacking
                    unalignedSize = miOffset lastMemberPacking +
                                    miSize lastMemberPacking
                    structSize = alignTo structAlignment unalignedSize
                in [qc|instance Storable {stName st} where
  sizeOf _ = {structSize}
  alignment _ = {structAlignment}
  peek ptr = {stName st} <$> {indent (-4) . vsep $ 
                              ((intercalateInfixAp $ 
                                writePeekMember <$> memberPacking))}
  poke ptr poked = {indent (-3) . vsep $ 
                    ((intercalatePrepend (fromString "*>") $ 
                     writePokeMember <$> memberPacking))}
|]

data MemberInfo = MemberInfo { miMember :: !StructMember
                             , miSize :: !Int
                             , miAlignment :: !Int
                             , miOffset :: !Int
                             }

writePeekMember :: MemberInfo -> Doc
writePeekMember mi = [qc|peek (ptr `plusPtr` {miOffset mi})|]

writePokeMember :: MemberInfo -> Doc
writePokeMember mi = [qc|poke (ptr `plusPtr` {miOffset mi}) ({sanitizedName (miMember mi)} poked)|]


type TypeEnv = ()

-- | Takes a list of members and calculates their packing 
calculateMemberPacking :: TypeEnv -> [StructMember] -> [MemberInfo]
calculateMemberPacking env = comb go 0
  where go offset m = let cType = smCType m
                          size = sizeofCType env cType
                          alignment = alignofCType env cType
                          alignedOffset = alignTo alignment offset
                      in (alignedOffset + size, 
                          MemberInfo m size alignment alignedOffset)

comb :: (s -> a -> (s, b)) -> s -> [a] -> [b]
comb f initialState = go initialState []
  where go _ bs []     = reverse bs
        go s bs (a:as) = let (s', b) = f s a
                         in go s' (b:bs) as

alignTo :: Int -> Int -> Int
alignTo alignment offset = ((offset + alignment - 1) `div` alignment) *
                           alignment

sizeofCType :: TypeEnv -> CType -> Int
sizeofCType _ _ = 4

alignofCType :: TypeEnv -> CType -> Int
alignofCType _ _ = 4
