{-# LANGUAGE PatternGuards #-}

module Spec.StripExtensions
  ( stripExtensions
  ) where

import           Data.List           (partition)
import           Data.Maybe          (catMaybes)
import           Spec.Command
import           Spec.Extension
import           Spec.Spec
import           Spec.Type
import           Write.TypeConverter (cTypeDependencyNames)
import           Write.Utils

-- | 'stripExtensions' removes everything that depends upon any unsupported types
-- and extensions that aren't in the "vulkan" profile
stripExtensions :: Spec -> Spec
stripExtensions spec =
  let allTypes = sTypes spec
      allCommands = sCommands spec
      allExtensions = sExtensions spec
      platformTypes = catMaybes . fmap typeDeclToPlatformType $ sTypes spec
      disallowedPlatformTypes = filter (isDisallowedTypeName . ptName) $
                                   platformTypes
      disallowedTypes = transitiveClosure (reverseDependencies allTypes)
                                          (==)
                                          (APlatformType <$>
                                             disallowedPlatformTypes)
      disallowedTypeNames = catMaybes . fmap typeDeclTypeName $ disallowedTypes
      isInDisallowed  = flip elem disallowedTypes
      allowedTypeDecls = filter (not . isInDisallowed) allTypes
      isInDisallowedNames = flip elem disallowedTypeNames
      (disallowedCommands, allowedCommands) = partition
                                                (any isInDisallowedNames . commandDependencies)
                                                allCommands
      disallowedCommandNames = fmap cName disallowedCommands
      isInDisallowedCommands = flip elem disallowedCommandNames
      profileExtensions = filter ((== "vulkan") . eSupported) allExtensions
      allowedExtensions = filter
                            ((&&)
                              <$> not . any isInDisallowedNames . eTypeNames
                              <*> not . any isInDisallowedCommands . eCommandNames)
                            profileExtensions
  in spec{ sTypes = allowedTypeDecls
         , sCommands = allowedCommands
         , sExtensions = allowedExtensions
         }

commandDependencies :: Command -> [String]
commandDependencies c = concatMap cTypeDependencyNames types
  where types = cReturnType c : fmap pType (cParameters c)

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
typeDeclDependees (ABaseType _)         = []
typeDeclDependees (APlatformType _)     = []
typeDeclDependees (ABitmaskType bmt)     = cTypeDependencyNames $ bmtCType bmt
typeDeclDependees (AHandleType ht)       = cTypeDependencyNames $ htCType ht
typeDeclDependees (AnEnumType _)        = []
typeDeclDependees (AFuncPointerType fpt) = cTypeDependencyNames $ fptCType fpt
typeDeclDependees (AStructType st)       = concatMap (cTypeDependencyNames . smCType)
                                           $ stMembers st
typeDeclDependees (AUnionType ut)        = concatMap (cTypeDependencyNames . smCType)
                                           $ utMembers ut

