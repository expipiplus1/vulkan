{-# language NoPolyKinds #-}
{-# language RankNTypes #-}

module Build
  ( Ref
  , create
  , Actions
  , runActions
  , runActionsWithRecreator
  , DoRecreate (..)
  , actionsGraph
  , Action
  , use
  ) where

import           Algebra.Graph.AdjacencyIntMap  (AdjacencyIntMap,  fromAdjacencyIntSets
                                                , induce
                                                , postIntSet
                                                , transitiveClosure
                                                , transpose
                                                )
import           Algebra.Graph.AdjacencyIntMap.Algorithm
                                                ( topSort )
import           Barbies
import           Barbies.Bare
import           Barbies.Constraints            ( Dict(Dict) )
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont       ( ContT
                                                , callCC
                                                )
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce                    ( coerce )
import           Data.Dependent.Map             ( DMap )
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.GADT.Compare              ( GCompare(..)
                                                , GEq(..)
                                                , GOrdering(GEQ, GGT, GLT)
                                                )
import qualified Data.IntMap                   as Map
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntSet                   as Set
import           Data.IntSet                    ( IntSet )
import           Data.Kind                      ( Type )
import           Data.List                      ( find
                                                , intercalate
                                                )
import           Data.Maybe
import           Data.Some                      ( Some(Some) )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           Data.Typeable                  ( Typeable )
import           Debug.Trace
import           Type.Reflection                ( typeRep )
import           Unsafe.Coerce                  ( unsafeCoerce )

newtype Ref s a = Ref
  { unRef :: Int
  }
  deriving (Show)

instance GEq (Ref s) where
  geq h1 h2 = case gcompare h1 h2 of
    GEQ -> Just Refl
    _   -> Nothing

instance GCompare (Ref s) where
  gcompare (Ref r1 :: Ref s a) (Ref r2 :: Ref s b) = case compare r1 r2 of
    LT -> GLT
    EQ -> case unsafeCoerce @_ @(a :~: b) Refl of
      Refl -> GEQ
    GT -> GGT


data Action s (m :: Type -> Type) a where
  Pure ::a -> Action s m a
  Ap ::Action s m (a -> b) -> Action s m a -> Action s m b
  FMap ::(a -> b) -> Action s m a -> Action s m b
  -- This has to be lazy so that the MonadFix instance for Actions is useful
  UseRef :: ~(Ref s a) -> Action s m a

instance Functor (Action s m) where
  fmap = FMap
instance Applicative (Action s m) where
  pure  = Pure
  (<*>) = Ap

runAction :: Monad m => Action s m a -> ([Some (Ref s)], LookupRef s m a)
runAction = \case
  Pure a -> ([], pure a)
  Ap f x ->
    let (fds, f') = runAction f
        (xds, x') = runAction x
    in  (fds <> xds, f' <*> x')
  FMap f x -> let (xds, x') = runAction x in (xds, f <$> x')
  UseRef r -> ([Some r], lookupRef r)

----------------------------------------------------------------
-- Action
----------------------------------------------------------------

use :: Ref s a -> Action s m a
use = UseRef

----------------------------------------------------------------
-- Actions
----------------------------------------------------------------

newtype Actions s m a = Actions { unActions :: State (ActionsState s m) a }
  deriving (Functor, Applicative, Monad, MonadFix)

data ActionsState s m = ActionsState
  { asNextRef :: Int
  , asCreate  :: DMap (Ref s) (LookupRef s m)
  , asDepends :: IntMap IntSet
  , asNames   :: IntMap String
  }

initialActionState = ActionsState { asNextRef = 0
                                  , asCreate  = mempty
                                  , asDepends = mempty
                                  , asNames   = mempty
                                  }

sortRefs
  :: AdjacencyIntMap -> [Some (Ref s)] -> ActionsState s m -> [DSum (Ref s) (LookupRef s m)]
sortRefs graph initRefs ActionsState {..} =
  let initInts = [ i | Some (Ref i) <- initRefs ]
      reachable =
        let t = transitiveClosure graph
        in  Set.unions
              (Set.fromList initInts : [ postIntSet i t | i <- initInts ])
      filtered = induce (`Set.member` reachable) graph
      sorted   = reverse $ case topSort filtered of
        Left  c -> error "cycle in graph"
        Right r -> r
      acts = DMap.toAscList asCreate
  in   (acts !!) <$> sorted

closeDirty
  :: AdjacencyIntMap -> [Some (Ref s)] -> IntSet
closeDirty graph dirty =
  let dirtyInts = [ i | Some (Ref i) <- dirty ]
      reachable =
        let t = transitiveClosure graph
        in  Set.unions
              (Set.fromList dirtyInts : [ postIntSet i t | i <- dirtyInts ])
  in reachable

create
  :: forall m a s
   . Monad m
  => String
  -> Action s m (m a)
  -> Actions s m (Ref s a)
create name act = do
  r <- newRef @s @m @a
  let (actDepends, actCreate) = runAction act
  Actions $ modify'
    (\as -> as
      { asCreate  = DMap.insert r (lift =<< actCreate) (asCreate as)
      , asDepends = Map.insert
                      (unRef r)
                      (Set.fromList [ d | Some (Ref d) <- actDepends ])
                      (asDepends as)
      , asNames   = Map.insert (unRef r) name (asNames as)
      }
    )
  pure r

runActions
  :: (Monad m, TraversableB f)
  => (forall s . Actions s m (f (Ref s)))
  -> m (f Identity)
runActions a = undefined -- snd (runActionsWithRecreator a)

runActionsWithRecreator
  :: (Monad m, TraversableB f, ApplicativeB f)
  => (forall s . Actions s m (f (Ref s)))
  -> (Recreator m f, m (f Identity))
runActionsWithRecreator (Actions c) =
  let
    (rs, s@ActionsState {..}) = runState c initialActionState
    needed                    = bfoldMap ((: []) . Some) rs
    graph    = fromAdjacencyIntSets . Map.toList $ asDepends
    transposed = transpose graph
    sorted                    = sortRefs graph needed s
    fromResolved resolved =
      runIdentity
        . runLookupRef resolved
        . btraverse (fmap Identity . lookupRef)
        $ rs

    makeNew = do
      resolved <- foldM
        (\done (r :=> m) -> do
          m' <- runLookupRef done m
          pure $ DMap.insert r (Identity m') done
        )
        mempty
        sorted
      pure $ fromResolved resolved

    recreate dirty old = do
      let dirtyRefs = bfoldMap
            (\case
              Pair DoRecreate r -> [Some r]
              _                 -> []
            )
            (bzip dirty rs)
          dirtyClosure = closeDirty transposed dirtyRefs
          oldRefs = bfoldMap (\(Pair r v) -> DMap.singleton r v) (bzip rs old)
      resolved <- foldM
        (\done (r :=> m) -> do
          let new = Identity <$> runLookupRef done m
          m' <- if unRef r `Set.member` dirtyClosure
            then new
            else maybe new pure (DMap.lookup r oldRefs)
          pure $ DMap.insert r m' done
        )
        mempty
        sorted
      pure $ fromResolved resolved
  in
    (recreate, makeNew)

type Recreator m f
  = f DoRecreate -> f Identity -> m (f Identity)

data DoRecreate a = DoRecreate | DoNotRecreate

-- | Generate a graphviz description for this set of actions
--
-- >>> actionsGraph $ do pure ()
-- "digraph G {}"
--
-- >>> actionsGraph $ do create "foo" (pure (Identity ()))
actionsGraph :: Actions s m a -> String
actionsGraph (Actions c) =
  let ActionsState {..} = execState c initialActionState
      edges = [ (p, c) | (p, cs) <- Map.toList asDepends, c <- Set.toList cs ]
  in  "digraph G {"
        <> intercalate
             ";"
             (  [ show a <> "->" <> show b | (a, b) <- edges ]
             <> [ show n <> " [label=" <> show name <> "]"
                | (n, name) <- Map.toList asNames
                ]
             )
        <> "}"

----------------------------------------------------------------
--
----------------------------------------------------------------

newtype LookupRef s m a = LookupRef { unLookupRef :: ReaderT (DMap (Ref s) Identity) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

runLookupRef :: DMap (Ref s) Identity -> LookupRef s m a -> m a
runLookupRef r (LookupRef x) = runReaderT x r

lookupRef :: Monad m => Ref s a -> LookupRef s m a
lookupRef r = do
  a <- LookupRef . asks $ DMap.lookup r
  pure $ maybe (error ("lookupRef: missing ref: " <> show r)) runIdentity a

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

newRef :: Actions s m (Ref s a)
newRef = Actions $ do
  as <- get
  let r = asNextRef as
  put (as { asNextRef = succ r })
  pure $ Ref r

  {-
module Build
  ( Ref
  , create
  , use
  , useEq
  , Actions
  , actionsGraph
  , runActions
  -- , runActionsWithRecreator
  , runActionsWithRecreator'
  , DoRecreate(..)
  ) where

import           Barbies
import           Barbies.Bare
import           Barbies.Constraints            ( Dict(Dict) )
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce                    ( coerce )
import           Data.Dependent.Map             ( DMap )
import qualified Data.Dependent.Map            as DMap
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.GADT.Compare              ( GCompare(..)
                                                , GEq(..)
                                                , GOrdering(GEQ, GGT, GLT)
                                                )
import qualified Data.IntMap                   as Map
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntSet                   as Set
import           Data.IntSet                    ( IntSet )
import           Data.List                      ( find
                                                , intercalate
                                                )
import           Data.Maybe
import           Data.Some                      ( Some(Some) )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           Data.Typeable                  ( Typeable )
import           Debug.Trace
import           Type.Reflection                ( typeRep )

----------------------------------------------------------------
-- Resource Graph
--
-- The problem is that there are dependencies in the construction of resources,
-- so when one resource goes out of date such as the swapchain, the resources
-- created from it are also out of date and must be regenerated.
----------------------------------------------------------------

----------------------------------------------------------------
-- Actions
----------------------------------------------------------------

create
  :: (Monad m, Typeable a)
  => String
  -> Action s m (m a)
  -> Actions s m (Ref s m a)
create name a = Actions $ do
  let deps = actionDepends a
  ActionsState {..} <- get
  let n = asNextRef
      r = Ref { refRef    = n
              , refName   = name
              , refCreate = cached t (lift =<< runAction a)
              }
      t = TRef r
  put
    (ActionsState
      { asNextRef  = succ n
      , asVertices = Some t : asVertices
      , asEdges    = [ (refRef d, n) | Some (TRef d) <- deps ] <> asEdges
      }
    )
  pure r

--
-- Consuming actions
--

-- | Generate a graphviz description for this set of actions
--
-- >>> actionsGraph $ do pure ()
-- "digraph G {}"
--
-- >>> actionsGraph $ do create "foo" (pure (Identity ()))
actionsGraph :: Actions s m a -> String
actionsGraph (Actions c) =
  let ActionsState {..} = execState c initialActionState
  in  "digraph G {"
        <> intercalate
             ";"
             (  [ show a <> "->" <> show b | (a, b) <- asEdges ]
             <> [ show n <> " [label=" <> show name <> "]"
                | Some (TRef (Ref n name _)) <- asVertices
                ]
             )
        <> "}"

runActions
  :: (BareB f, Monad m, TraversableB (f Covered))
  => (forall s . Actions s m (f Covered (Ref s m)))
  -> m (f Bare Identity)
runActions (Actions c) =
  let hs = evalState c initialActionState
  in  runCache
        $ fmap bstrip
        . btraverse (\Ref {..} -> Identity <$> refCreate)
        $ hs

data DoRecreate a = DoRecreate | DoNotRecreate

closure :: (Int -> IntSet) -> IntSet -> IntSet
closure f xs =
  let xs' = Set.unions (xs : (f <$> Set.toList xs))
  in  if Set.size xs == Set.size xs' then xs else closure f xs'

dependencyClosure :: ActionsState s m -> [Vertex] -> [Vertex]
dependencyClosure ActionsState {..} =
  let childMap :: IntMap IntSet
      childMap = Map.fromListWith
        (<>)
        [ (unVertex p, Set.singleton (unVertex c)) | (p, c) <- asEdges ]
      children :: Int -> IntSet
      children = (\k -> Map.findWithDefault Set.empty k childMap)
  in  coerce . Set.toList . closure children . Set.fromList . coerce

dirtyClosure
  :: forall f m s
   . (BareB f, Monad m, TraversableB (f Covered), ApplicativeB (f Covered))
  => f Covered (Ref s m)
  -> ActionsState s m
  -> f Covered DoRecreate
  -> f Covered DoRecreate
dirtyClosure refs finalState dirty =
  let initialDirtyVertices :: [Vertex]
      initialDirtyVertices = bfoldMap
        (\case
          Pair DoRecreate    Ref { refRef } -> [refRef]
          Pair DoNotRecreate _              -> []
        )
        (bzip dirty refs)
      allDirtyVertices = dependencyClosure finalState initialDirtyVertices
  in  bmap
        (\case
          Ref { refRef } ->
            if refRef `elem` allDirtyVertices then DoRecreate else DoNotRecreate
        )
        refs

type Recreator m f
  = f Covered DoRecreate -> f Bare Identity -> m (f Bare Identity)

-- runActionsWithRecreator
--   :: (BareB f, Monad m, TraversableB (f Covered), ApplicativeB (f Covered))
--   => (forall s . Actions s m (f Covered (Ref s m)))
--   -> (Recreator m f, m (f Bare Identity))
-- runActionsWithRecreator (Actions c) =
--   let (refs, finalState) = runState c initialActionState
--       generate =
--         fmap bstrip . btraverse (\Ref {..} -> Identity <$> refCreate) $ refs
--       new = runCache generate
--       recreator initialDirty old =
--         let closedDirty = dirtyClosure refs finalState initialDirty
--             clean       = bfoldMap
--               (\case
--                 Pair DoRecreate    _          -> mempty
--                 Pair DoNotRecreate (Pair v r) -> DMap.singleton r v
--               )
--               (bzip closedDirty (bzip (bcover old) refs))
--         in  runCacheWithInitial clean generate
--   in  (recreator, new)

{-# ANN runActionsWithRecreator' ("Hlint: ignore Use Just" :: String) #-}
runActionsWithRecreator'
  :: forall f m
   . (BareB f, Monad m, TraversableB (f Covered), ApplicativeB (f Covered))
  => (forall s . Actions s m (f Covered (Ref s m)))
  -> (Recreator m f, m (f Bare Identity))
runActionsWithRecreator' (Actions c) =
  let (refs, finalState) = runState c initialActionState
      generate =
        fmap bstrip . btraverse (\Ref {..} -> Identity <$> refCreate) $ refs
      makeNew = runCache generate
      recreator initialDirty old =
        let
            -- Find the Refs of these dirty elements
            initialDirtyVertices :: [Some (Ref () m)]
            initialDirtyVertices = bfoldMap
              (\case
                Pair DoRecreate    r -> [Some r]
                Pair DoNotRecreate _ -> mempty
              )
              (bzip initialDirty refs)

            -- Get all the references
            allVertices :: [Some (Ref () m)]
            allVertices = bfoldMap (\r -> [Some r]) refs
            -- And a map from Vertices to Refs
            findRef :: Vertex -> Some (Ref () m)
            findRef v = fromMaybe
              (error "missing ref")
              (find (\(Some Ref { refRef }) -> refRef == v) allVertices)

            oldCache = undefined
            -- oldCache = bfoldMap (\(Pair r v) -> DMap.singleton r v)
            --                     (bzip refs (bcover old))

            -- Accumulate a map of replacements
            newVerts :: CacheT (TRef () m) m (DMap (Ref () m) Identity)
            newVerts = undefined
            -- flip evalStateT (initialDirtyVertices, DMap.empty)
            --   $ let
            --       go = get >>= \case
            --         -- If there are no dirty vertices we are done
            --         ([], new) -> pure new
            --         -- If there is a dirty vertex:
            --         (Some r@(TRef Ref { refRef, refCreate }) : rs, _) -> do
            --           -- Recreate it, find its children and add them to the
            --           -- pending list, also insert the new result into the
            --           -- replacements
            --           x' <- lift $ removeFromCache r *> refCreate
            --           let
            --             irefEq = Nothing
            --             eq     = fromMaybe False $ do
            --               Dict          <- irefEq
            --               Identity oldX <- DMap.lookup r oldCache
            --               pure (x' == oldX)
            --             dirtyRefs =
            --               [ findRef child
            --               | (p, child) <- asEdges finalState
            --               , p == refRef
            --               ]
            --           unless eq
            --             $ modify'
            --                 (bimap (const (dirtyRefs <> rs))
            --                        (DMap.insert r (Identity x'))
            --                 )
            --           go
            --     in  go
        in  undefined
        -- do
        --   m <- runCacheWithInitial oldCache newVerts
        --   pure . bstrip $ bzipWith (\o ref -> fromMaybe o (DMap.lookup ref m))
        --                            (bcover old)
        --                            refs
  in  (recreator, makeNew)
----------------------------------------------------------------
-- Action
----------------------------------------------------------------

data Ref s m a = Ref
  { refRef    :: Vertex
  , refName   :: String
  , refCreate :: ~(CacheT (TRef s m) m a)
  }
  deriving Functor

data TRef s m a where
  TRef ::Typeable a => { unTRef :: Ref s m a } -> TRef s m a

instance GEq (TRef s m) where
  geq h1 h2 = case gcompare h1 h2 of
    GEQ -> Just Refl
    _   -> Nothing

instance GCompare (TRef s m) where
  gcompare (TRef r1 :: TRef s m a) (TRef r2 :: TRef s m b) =
    case compare (refRef r1) (refRef r2) of
      LT -> GLT
      EQ -> gcompare (typeRep @a) (typeRep @b)
      GT -> GGT

data Action s m a where
  Pure ::a -> Action s m a
  Ap ::Action s m (a -> b) -> Action s m a -> Action s m b
  FMap ::(a -> b) -> Action s m a -> Action s m b
  UseRef ::Maybe (Dict Eq a) -> TRef s m a -> Action s m a

instance Functor m => Functor (Action s m) where
  fmap = FMap

instance Applicative m => Applicative (Action s m) where
  pure  = Pure
  (<*>) = Ap

use :: Typeable a => Ref s m a -> Action s m a
use = UseRef Nothing . TRef

useEq :: (Typeable a, Eq a) => Ref s m a -> Action s m a
useEq = UseRef (Just Dict) . TRef

runAction :: Monad m => Action s m a -> CacheT (TRef s m) m a
runAction = \case
  Pure a -> pure a
  Ap f x ->
    let f' = runAction f
        x' = runAction x
    in  f' <*> x'
  FMap   f x               -> f <$> runAction x
  UseRef _ (TRef Ref {..}) -> refCreate

actionDepends :: Applicative m => Action s m a -> [Some (TRef s m)]
actionDepends = \case
  Pure _     -> mempty
  Ap     f x -> actionDepends f <> actionDepends x
  FMap   _ x -> actionDepends x
  UseRef _ h -> [Some h]

newtype Vertex = Vertex { unVertex :: Int }
  deriving (Eq, Ord, Enum)
  deriving newtype Show

data ActionsState s m = ActionsState
  { asNextRef  :: Vertex
  , asVertices :: [Some (TRef s m)]
  , asEdges    :: [(Vertex, Vertex)]
  }

initialActionState :: ActionsState s m
initialActionState = ActionsState (Vertex 0) [] []

newtype Actions s m a = Actions { _unActions :: State (ActionsState s m) a }
  deriving newtype (Functor, Applicative, Monad)

----------------------------------------------------------------
-- Simple cache
----------------------------------------------------------------

newtype CacheT k m a = CacheT { _unCacheT :: StateT (DMap k Identity) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

cached :: (GCompare k, Monad m) => k a -> CacheT k m a -> CacheT k m a
cached k (CacheT v) = CacheT $ gets (DMap.lookup k) >>= \case
  Nothing -> do
    r <- v
    modify' (DMap.insert k (Identity r))
    pure r
  Just (Identity r) -> pure r

removeFromCache :: (GCompare k, Monad m) => k a -> CacheT k m ()
removeFromCache k = CacheT $ modify' (DMap.delete k)

runCache :: Monad m => CacheT k m a -> m a
runCache = runCacheWithInitial DMap.empty

runCacheWithInitial :: Monad m => DMap k Identity -> CacheT k m a -> m a
runCacheWithInitial initial (CacheT a) = evalStateT a initial
-}

