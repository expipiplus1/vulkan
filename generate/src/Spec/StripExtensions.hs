{-# LANGUAGE PatternGuards #-}

module Spec.StripExtensions 
  ( stripWSIExtensions
  ) where

import Spec
import Spec.Type
import Write.TypeConverter(cTypeNames)
import Data.Maybe(catMaybes)

-- | 'stripWSIExtensions' removes everything that depends upon any windowing
-- system headers
stripWSIExtensions :: Spec -> Spec
stripWSIExtensions spec = 
  let allTypes = sTypes spec
      platformTypes = catMaybes . fmap typeDeclToPlatformType $ sTypes spec
      disallowedPlatformTypes = filter (isDisallowedTypeName . ptName) $ 
                                   platformTypes
      disallowedTypes = transitiveClosure (reverseDependencies allTypes) 
                                          (==) 
                                          (APlatformType <$> 
                                             disallowedPlatformTypes)
      isInDisallowed  = flip elem disallowedTypes
      allowedTypeDecls = filter (not . isInDisallowed) allTypes
  in spec{sTypes = allowedTypeDecls}

--
-- Not exactly optimal in terms of complxity, but wins out in code simplicity 
--

-- | Given all the type decls filter them by whether they contain this name
reverseDependencies :: [TypeDecl] -> TypeDecl -> [TypeDecl]
reverseDependencies ts t = filter (flip dependsOn t) ts 

-- | 'dependsOn x y' returns True if x depends on y
dependsOn :: TypeDecl -> TypeDecl -> Bool
dependsOn x y 
  | Just dependeeName <- typeDeclTypeName y 
  , dependees <- typeDeclDependees x
  = elem dependeeName dependees
dependsOn _ _ = False

-- | Is the type one which we can't supply
isDisallowedTypeName :: String -> Bool
isDisallowedTypeName = not . flip elem allowedTypes

-- | A list of all types which are not part of any WSI extension
allowedTypes :: [String]
allowedTypes = [ "void"
               , "char"
               , "float"
               , "uint8_t"
               , "uint32_t"
               , "uint64_t"
               , "int32_t"
               , "size_t"
               ]

typeDeclDependees :: TypeDecl -> [String]
typeDeclDependees (AnInclude _)          = []
typeDeclDependees (ADefine _)            = []
typeDeclDependees (ABaseType bt)         = []
typeDeclDependees (APlatformType pt)     = []
typeDeclDependees (ABitmaskType bmt)     = cTypeNames $ bmtCType bmt
typeDeclDependees (AHandleType ht)       = cTypeNames $ htCType ht
typeDeclDependees (AnEnumType et)        = []
typeDeclDependees (AFuncPointerType fpt) = cTypeNames $ fptCType fpt
typeDeclDependees (AStructType st)       = concatMap (cTypeNames . smCType) 
                                           $ stMembers st
typeDeclDependees (AUnionType ut)        = concatMap (cTypeNames . smCType) 
                                           $ utMembers ut


-- | From ghc Util
transitiveClosure :: (a -> [a])         -- Successor function
                  -> (a -> a -> Bool)   -- Equality predicate
                  -> [a]
                  -> [a]                -- The transitive closure

transitiveClosure succ eq xs
 = go [] xs
 where
   go done []                      = done
   go done (x:xs) | x `is_in` done = go done xs
                  | otherwise      = go (x:done) (succ x ++ xs)

   _ `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
                    | otherwise = x `is_in` ys
