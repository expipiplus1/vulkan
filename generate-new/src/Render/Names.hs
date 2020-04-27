module Render.Names
  ( HasRenderedNames
  , withRenderedNames
  , specRenderedNames
  , isStructOrUnion
  , getRenderedStruct
  , isNewtype
  , getResolveAlias
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Data.List                      ( partition )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( ask )

import           Haskell.Name
import           Render.Element
import           Spec.Types

type HasRenderedNames r = MemberWithError (Input RenderedNames) r

data RenderedNames = RenderedNames
  { rnStructs                :: HashMap HName Struct
  , rnUnions                 :: HashSet HName
  , rnEnums                  :: HashSet HName
  , rnNonDispatchableHandles :: HashSet HName
  , rnDispatchableHandles    :: HashSet HName
  , rnResolveAlias           :: HName -> HName
  }

instance Semigroup RenderedNames where
  rn1 <> rn2 = RenderedNames
    { rnStructs                = concatBoth rnStructs
    , rnUnions                 = concatBoth rnUnions
    , rnEnums                  = concatBoth rnEnums
    , rnNonDispatchableHandles = concatBoth rnNonDispatchableHandles
    , rnDispatchableHandles    = concatBoth rnDispatchableHandles
    , rnResolveAlias           = appEndo (concatBoth (Endo . rnResolveAlias))
    }
   where
    concatBoth :: Monoid a => (RenderedNames -> a) -> a
    concatBoth f = f rn1 <> f rn2

instance Monoid RenderedNames where
  mempty = RenderedNames mempty mempty mempty mempty mempty id

specRenderedNames :: HasRenderParams r => Spec -> Sem r RenderedNames
specRenderedNames Spec {..} = do
  RenderParams {..} <- input
  let
    rnStructs =
      Map.fromList [ (mkTyName . sName $ s, s) | s <- toList specStructs ]
    rnUnions = Set.fromList (mkTyName . sName <$> toList specUnions)
    rnEnums  = Set.fromList
      [ mkTyName n
      | Enum {..} <- toList specEnums
      , n <- eName : [ flags | ABitmask flags <- pure eType ]
      ]
    (dispHandles, nonDispHandles) =
      partition ((== Dispatchable) . hDispatchable) $ toList specHandles
    rnDispatchableHandles = Set.fromList (mkTyName . hName <$> dispHandles)
    rnNonDispatchableHandles =
      Set.fromList (mkTyName . hName <$> nonDispHandles)
    aliasMap = Map.fromList
      (  [ (mkTyName aName, mkTyName aTarget)
         | Alias {..} <- toList specAliases
         , TypeAlias == aType
         ]
      <> [ (mkTyName flags, mkTyName eName)
         | Enum {..}      <- toList specEnums
         , ABitmask flags <- pure eType
         , flags /= eName
         ]
      )
    -- TODO: Handle alias cycles!
    rnResolveAlias n = maybe n rnResolveAlias (Map.lookup n aliasMap)
  pure RenderedNames { .. }

withRenderedNames
  :: HasRenderParams r => Spec -> Sem (Input RenderedNames ': r) a -> Sem r a
withRenderedNames spec a = do
  rns <- specRenderedNames spec
  runInputConst rns a

isStructOrUnion :: HasRenderedNames r => HName -> Sem r Bool
isStructOrUnion n = do
  RenderedNames {..} <- input
  let n' = rnResolveAlias n
  pure (Map.member n' rnStructs || Set.member n' rnUnions)

getRenderedStruct :: HasRenderedNames r => HName -> Sem r (Maybe Struct)
getRenderedStruct n = do
  RenderedNames {..} <- input
  let n' = rnResolveAlias n
  pure (Map.lookup n' rnStructs)

isNewtype :: HasRenderedNames r => HName -> Sem r Bool
isNewtype n = do
  RenderedNames {..} <- input
  let n' = rnResolveAlias n
  pure (Set.member n' rnNonDispatchableHandles || Set.member n' rnEnums)

getResolveAlias :: HasRenderedNames r => Sem r (HName -> HName)
getResolveAlias = rnResolveAlias <$> input

