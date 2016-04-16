{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Spec.Graph where

import           Control.Arrow     ((&&&))
import           Data.HashMap.Lazy as M
import           Data.HashSet      as S
import           Data.Maybe        (catMaybes, fromMaybe, maybeToList)
import           Language.C.Types
import           Prelude           hiding (Enum)
import           Spec.Bitmask
import           Spec.Command      as Command
import           Spec.Constant     as Constant
import           Spec.Enum
import           Spec.Spec
import           Spec.Type         hiding (ABaseType, ABitmaskType, ADefine,
                                    AFuncPointerType, AHandleType,
                                    APlatformType, AStructType, AUnionType,
                                    AnEnumType, AnInclude)
import qualified Spec.Type         as T

-- | Info is a more useful representation of the specification
data SpecGraph = SpecGraph{ gVertices      :: [Vertex]
                          , gNameVertexMap :: M.HashMap String Vertex
                          }

data Vertex = Vertex{ vName         :: String
                    , vDependencies :: [Vertex]
                    , vSourceEntity :: SourceEntity
                    }

data SourceEntity = AnInclude Include
                  | ADefine Define
                  | ABaseType BaseType
                  | APlatformType PlatformType
                  | ABitmaskType BitmaskType (Maybe Bitmask)
                  | AHandleType HandleType
                  | AnEnumType EnumType
                  | AFuncPointerType FuncPointerType
                  | AStructType StructType
                  | AUnionType UnionType
                  | ACommand Command
                  | AnEnum Enum
                  | ABitmask Bitmask
                  | AConstant Constant
  deriving (Show)

-- | Look up the name in the graph, error if it's not there
requiredLookup :: SpecGraph -> String -> Vertex
requiredLookup graph name = M.lookupDefault err name (gNameVertexMap graph)
  where err = error ("Failed to find required name in graph: " ++ name)

allReachableFromNames :: SpecGraph -> [String] -> S.HashSet String
allReachableFromNames graph names = allReachable vertices
  where vertices = getVertex <$> names
        getVertex name = let err = error ("allReachableFromNames given name not in graph: " ++ name)
                         in M.lookupDefault err name (gNameVertexMap graph)

allReachable :: [Vertex] -> S.HashSet String
allReachable vs = go (S.fromList (vName <$> vs)) (concatMap vDependencies vs)
  where go s [] = s
        go s (x:xs) = if S.member (vName x) s
                        then go s xs
                        else go (S.insert (vName x) s) (xs ++ vDependencies x)

--------------------------------------------------------------------------------
-- Converting a spec
--------------------------------------------------------------------------------

getSpecGraph :: Spec -> SpecGraph
getSpecGraph spec = graph
  where gVertices = (typeDeclToVertex graph <$> sTypes spec) ++
                    (constantToVertex graph <$> sConstants spec) ++
                    (enumToVertex <$> sEnums spec) ++
                    (bitmaskToVertex graph <$> sBitmasks spec) ++
                    (commandToVertex graph <$> sCommands spec)
        gNameVertexMap = M.fromList ((vName &&& id) <$> gVertices)
        graph = SpecGraph{..}

--------------------------------------------------------------------------------
-- Boring boilerplate conversions
--------------------------------------------------------------------------------

typeDeclToVertex :: SpecGraph -> TypeDecl -> Vertex
typeDeclToVertex graph td =
  let lookupNameMay name = M.lookup name (gNameVertexMap graph)
      lookupName name = fromMaybe (error ("Depended upon name not in spec: " ++ name)) (lookupNameMay name)
  in case td of
       T.AnInclude i ->
         Vertex{ vName = iName i
               , vDependencies = []
               , vSourceEntity = AnInclude i
               }
       T.ADefine d ->
         Vertex{ vName = dName d
               , vDependencies = []
               , vSourceEntity = ADefine d
               }
       T.ABaseType bt ->
         Vertex{ vName = btName bt
               , vDependencies = lookupName <$>
                                 cTypeDependencyNames (btCType bt)
               , vSourceEntity = ABaseType bt
               }
       T.APlatformType pt ->
         Vertex{ vName = ptName pt
               , vDependencies = [lookupName (ptRequires pt)]

               , vSourceEntity = APlatformType pt
               }
       T.ABitmaskType bmt ->
         let bm = (lookupNameMay =<< bmtRequires bmt)
         in Vertex{ vName = bmtName bmt
                  , vDependencies = (lookupName <$>
                                     cTypeDependencyNames (bmtCType bmt))
                                   ++ maybeToList bm
                  , vSourceEntity = ABitmaskType bmt (bm >>= vertexToBitmask)
                  }
       T.AHandleType ht ->
         Vertex{ vName = htName ht
               , vDependencies = catMaybes . fmap lookupNameMay $
                                   cTypeDependencyNames (htCType ht)
               , vSourceEntity = AHandleType ht
               }
       T.AnEnumType et ->
         Vertex{ vName = etName et
               , vDependencies = []
               , vSourceEntity = AnEnumType et
               }
       T.AFuncPointerType fpt ->
         Vertex{ vName = fptName fpt
               , vDependencies = lookupName <$> cTypeDependencyNames (fptCType fpt)
               , vSourceEntity = AFuncPointerType fpt
               }
       T.AStructType st ->
         Vertex{ vName = stName st
               , vDependencies = lookupName <$>
                                 concatMap memberDependencyNames (stMembers st)
               , vSourceEntity = AStructType st
               }
       T.AUnionType ut ->
         Vertex{ vName = utName ut
               , vDependencies = lookupName <$>
                                 concatMap memberDependencyNames (utMembers ut)
               , vSourceEntity = AUnionType ut
               }

commandToVertex :: SpecGraph -> Command -> Vertex
commandToVertex graph command =
  let lookupName name = fromMaybe
                          (error ("Depended upon name not in spec: " ++ name))
                          (M.lookup name (gNameVertexMap graph))
  in Vertex{ vName = Command.cName command
           , vDependencies = let parameterTypes = pType <$> cParameters command
                                 allTypes = cReturnType command : parameterTypes
                             in lookupName <$>
                                concatMap cTypeDependencyNames allTypes
           , vSourceEntity = ACommand command
           }

enumToVertex :: Enum -> Vertex
enumToVertex enum =
  Vertex{ vName = eName enum
        , vDependencies = []
        , vSourceEntity = AnEnum enum
        }

bitmaskToVertex :: SpecGraph -> Bitmask -> Vertex
bitmaskToVertex _ bitmask =
  Vertex{ vName = bmName bitmask
        , vDependencies = []
        , vSourceEntity = ABitmask bitmask
        }

constantToVertex :: SpecGraph -> Constant -> Vertex
constantToVertex _ constant =
  Vertex{ vName = Constant.cName constant
        , vDependencies = []
        , vSourceEntity = AConstant constant
        }

--
-- Converting from a graph
--

vertexToBitmask :: Vertex -> Maybe Bitmask
vertexToBitmask v = case vSourceEntity v of
                      ABitmask bm -> Just bm
                      _ -> Nothing

vertexToConstant :: Vertex -> Maybe Constant
vertexToConstant v = case vSourceEntity v of
                       AConstant c -> Just c
                       _ -> Nothing

getGraphConstants :: SpecGraph -> [Constant]
getGraphConstants graph = catMaybes (vertexToConstant <$> gVertices graph)

vertexCType :: Vertex -> Maybe CType
vertexCType v = case vSourceEntity v of
                  ABaseType bt -> Just $ btCType bt
                  ABitmaskType bmt _ -> Just $ bmtCType bmt
                  AHandleType ht -> Just $ htCType ht
                  AFuncPointerType fpt -> Just $ fptCType fpt
                  _ -> Nothing

getGraphCTypes :: SpecGraph -> [(String, CType)]
getGraphCTypes graph =
  catMaybes $ (\v -> (vName v,) <$> vertexCType v) <$> gVertices graph

vertexToUnionType :: Vertex -> Maybe UnionType
vertexToUnionType v = case vSourceEntity v of
                    AUnionType u -> Just u
                    _ -> Nothing

getGraphUnionTypes :: SpecGraph -> [UnionType]
getGraphUnionTypes graph = catMaybes (vertexToUnionType <$> gVertices graph)

vertexToStructType :: Vertex -> Maybe StructType
vertexToStructType v = case vSourceEntity v of
                    AStructType s -> Just s
                    _ -> Nothing

getGraphStructTypes :: SpecGraph -> [StructType]
getGraphStructTypes graph = catMaybes (vertexToStructType <$> gVertices graph)

vertexToEnumType :: Vertex -> Maybe EnumType
vertexToEnumType v = case vSourceEntity v of
                    AnEnumType et -> Just et
                    _ -> Nothing

getGraphEnumTypes :: SpecGraph -> [EnumType]
getGraphEnumTypes graph = catMaybes (vertexToEnumType <$> gVertices graph)


------------------------------------------------------------------------------
-- predicates
------------------------------------------------------------------------------

isIncludeVertex :: Vertex -> Bool
isIncludeVertex vertex
  | AnInclude _ <- vSourceEntity vertex = True
  | otherwise = False

isTypeConstructor :: Vertex -> Bool
isTypeConstructor v =
  case vSourceEntity v of
    AnInclude _ -> False
    ADefine _ -> False
    ABaseType _ -> True
    APlatformType _ -> False
    ABitmaskType _ _ -> True
    AHandleType _ -> True
    AnEnumType _ -> True
    AFuncPointerType _ -> False
    AStructType _ -> True
    AUnionType _ -> True
    ACommand _ -> False
    AnEnum _ -> True
    ABitmask _ -> True
    AConstant _ -> False

------------------------------------------------------------------------------
-- Dependency utils
------------------------------------------------------------------------------

cTypeDependencyNames :: CType -> [String]
cTypeDependencyNames cType =
  case cType of
    TypeSpecifier _ Void
      -> ["void"]
    TypeSpecifier _ (Char Nothing)
      -> ["char"]
    TypeSpecifier _ Float
      -> ["float"]
    TypeSpecifier _ (TypeName t)
      -> [unCIdentifier t]
    TypeSpecifier _ (Struct t)
      -> [unCIdentifier t]
    Ptr _ t
      -> cTypeDependencyNames t
    Array s t
      -> arraySizeDependencyNames s ++ cTypeDependencyNames t
    Proto ret ps
      -> cTypeDependencyNames ret ++ concatMap parameterTypeNames ps
    _ -> error ("Failed to get depended on names for C type:\n" ++ show cType)

arraySizeDependencyNames :: ArrayType CIdentifier -> [String]
arraySizeDependencyNames arraySize =
  case arraySize of
    VariablySized -> []
    Unsized -> []
    SizedByInteger _ -> []
    SizedByIdentifier i -> [unCIdentifier i]

parameterTypeNames :: ParameterDeclaration CIdentifier -> [String]
parameterTypeNames (ParameterDeclaration _ t) = cTypeDependencyNames t

memberDependencyNames :: StructMember -> [String]
memberDependencyNames = cTypeDependencyNames . smCType

