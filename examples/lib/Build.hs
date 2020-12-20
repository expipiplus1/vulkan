{-# language NoPolyKinds #-}
{-# language RankNTypes #-}

module Build
  ( Ref
  , create
  , Actions
  , runActions
  , runActionsWithRecreator
  , DoRecreate(..)
  , actionsGraph
  , Action
  , use
  , useEq
  , useOld
  ) where

import           Algebra.Graph.AdjacencyIntMap  ( AdjacencyIntMap
                                                , fromAdjacencyIntSets
                                                , induce
                                                , postIntSet
                                                , transitiveClosure

                                                )
import           Algebra.Graph.AdjacencyIntMap.Algorithm
                                                ( topSort )
import           Barbies
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
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
import           Data.List                      ( intercalate
                                                )
import           Data.Some                      ( Some(Some) )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           Unsafe.Coerce                  ( unsafeCoerce )
import Data.Functor.Compose
import Data.Bool
import Data.Foldable
import Data.Maybe

newtype Ref s a = Ref { unRef :: Int }
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


data Action s a where
  Pure ::a -> Action s a
  Ap ::Action s (a -> b) -> Action s a -> Action s b
  FMap ::(a -> b) -> Action s a -> Action s b
  -- This has to be lazy so that the MonadFix instance for Actions is useful
  UseRef :: Cond a -> ~(Ref s a) -> Action s a
  UseOldRef :: ~(Ref s a) -> Action s (Maybe a)

instance Functor (Action s) where
  fmap = FMap
instance Applicative (Action s) where
  pure  = Pure
  (<*>) = Ap

data Dep s a where
  Dep :: { _depRef :: Ref s a
         , _depCond :: Cond a
         } -> Dep s a

newtype Cond a = Cond (a -> a -> Bool)

runAction
  :: Monad m
  => Action s a
  -> (([Some (Dep s)], [Some (Ref s)]), LookupRef s m a)
runAction = \case
  Pure a -> (mempty, pure a)
  Ap f x ->
    let f' = Compose $ runAction f
        x' = Compose $ runAction x
    in  getCompose $ f' <*> x'
  FMap   f x  -> getCompose . fmap f . Compose . runAction $ x
  UseRef c r  -> (([Some (Dep r c)], []), lookupRef r)
  UseOldRef r -> (([], [Some r]), lookupOldRef r)

----------------------------------------------------------------
-- Action
----------------------------------------------------------------

use :: Ref s a -> Action s a
use = useCond (\_ _ -> True)

-- | A shorthand for @'useCond' (/=)@
useEq :: Eq a => Ref s a -> Action s a
useEq = useCond (/=)

-- | Compare the old and the new values for this dependency, becomes an edge in
-- the dependency graph if this predicate returns True
useCond :: (a -> a -> Bool) -> Ref s a -> Action s a
useCond p = UseRef (Cond p)

useOld :: Ref s a -> Action s (Maybe a)
useOld = UseOldRef

----------------------------------------------------------------
-- Actions
----------------------------------------------------------------

newtype Actions s m a = Actions { _unActions :: State (ActionsState s m) a }
  deriving (Functor, Applicative, Monad, MonadFix)

data ActionsState s m = ActionsState
  { asNextRef    :: Int
  , asCreate     :: DMap (Ref s) (LookupRef s m)
  , asDepends    :: IntMap (DMap (Ref s) Cond)
  , asOldDepends :: IntMap IntSet
  , asNames      :: IntMap String
  }

initialActionState :: ActionsState s m
initialActionState = ActionsState { asNextRef    = 0
                                  , asCreate     = mempty
                                  , asDepends    = mempty
                                  , asOldDepends = mempty
                                  , asNames      = mempty
                                  }

sortRefs
  :: AdjacencyIntMap
  -> IntSet
  -> ActionsState s m
  -> [DSum (Ref s) (LookupRef s m)]
sortRefs graph initInts ActionsState {..} =
  let reachable =
        let t = transitiveClosure graph
        in  Set.unions
              (initInts : [ postIntSet i t | i <- Set.toList initInts ])
      filtered = induce (`Set.member` reachable) graph
      sorted   = reverse $ case topSort filtered of
        Left  _ -> error "cycle in graph"
        Right r -> r
      acts = DMap.toAscList asCreate
  in  (acts !!) <$> sorted

create
  :: forall m a s
   . Monad m
  => String
  -> Action s (m a)
  -> Actions s m (Ref s a)
create name act = do
  r <- newRef @s @m @a
  let ((actDepends, actOldDepends), actCreate) = runAction act
      addCondDepends new old =
        Map.insert (unRef r) (DMap.fromList [ d :=> c | Some (Dep d c) <- new ]) old
      addDepends new old =
        Map.insert (unRef r) (Set.fromList [ d | Some (Ref d) <- new ]) old
  Actions $ modify'
    (\as -> as { asCreate     = DMap.insert r (lift =<< actCreate) (asCreate as)
               , asDepends    = addCondDepends actDepends (asDepends as)
               , asOldDepends = addDepends actOldDepends (asOldDepends as)
               , asNames      = Map.insert (unRef r) name (asNames as)
               }
    )
  pure r

unCondDeps :: DMap (Ref s) f -> IntSet
unCondDeps = Set.fromList . fmap (\(Some (Ref r)) -> r) . DMap.keys

makeNew
  :: (Foldable t, Monad m)
  => t (DSum (Ref s) (LookupRef s m))
  -> (DMap (Ref s) Identity -> b)
  -> m b
makeNew sorted fromResolved = do
  resolved <- foldM
    (\done (r :=> m) -> do
      m' <- runLookupRef mempty done m
      pure $ DMap.insert r (Identity m') done
    )
    mempty
    sorted
  pure $ fromResolved resolved

runActions
  :: (Monad m, TraversableB f)
  => (forall s . Actions s m (f (Ref s)))
  -> m (f Identity)
runActions (Actions c) =
  let (rs, s@ActionsState {..}) = runState c initialActionState
      needed                    = bfoldMap (Set.singleton . unRef) rs
      graph =
        fromAdjacencyIntSets
          . fmap (fmap unCondDeps)
          . Map.toList
          $ asDepends
      sorted = sortRefs graph needed s
      fromResolved resolved =
        runIdentity
          . runLookupRef mempty resolved
          . btraverse (fmap Identity . lookupRef)
          $ rs
  in  makeNew sorted fromResolved

runActionsWithRecreator
  :: forall m f
   . (Monad m, TraversableB f, ApplicativeB f)
  => (forall s . Actions s m (f (Ref s)))
  -> (Recreator m f, m (f Identity))
runActionsWithRecreator (Actions c) =
  let
    (rs, s@ActionsState {..}) = runState c initialActionState
    needed                    = bfoldMap (Set.singleton . unRef) rs
    graph =
      fromAdjacencyIntSets . fmap (fmap unCondDeps) . Map.toList $ asDepends
    sorted = sortRefs graph needed s
    fromResolved oldMap resolved =
      runIdentity
        . runLookupRef oldMap resolved
        . btraverse (fmap Identity . lookupRef)
        $ rs

    reverseDeps :: DMap (Ref ()) CondDependees
    reverseDeps = DMap.fromListWithKey
      (const (<>))
      [ childRef :=> CondDependees [(p, cond)]
      | (p, children)       <- Map.toList asDepends
      , (childRef :=> cond) <- DMap.toList children
      ]

    realiseOldRef
      :: forall a
       . RefMap ()
      -> Ref () a
      -> StateT (RefMap (), IntSet) m (Identity a)
    realiseOldRef oldRefs ref = case DMap.lookup ref oldRefs of
      Just v  -> pure v
      Nothing -> Identity <$> realiseRef (Just oldRefs) ref

    realiseRef
      :: forall a
       . Maybe ( RefMap ())
      -> Ref () a
      -> StateT (RefMap (), IntSet) m a
    realiseRef oldRefsMb ref = do
      (refsWeHave, foundDirty) <- get
      case DMap.lookup ref refsWeHave of
        Just v -> pure (runIdentity v)
        Nothing
          | unRef ref `Set.notMember` foundDirty, Just oldRefs <- oldRefsMb, Just r <- DMap.lookup
            ref
            oldRefs -> do
            modify' (first $ DMap.insert ref r)
            pure $ runIdentity r
        Nothing -> do
          -- First make sure that all the dependencies are accounted for by
          -- building them if they're missing
          let deps = Map.findWithDefault mempty (unRef ref) asDepends
          for_ (DMap.toList deps)
            $ \(depRef :=> _) -> realiseRef oldRefsMb (coerce depRef)
          let oldDeps = Map.findWithDefault mempty (unRef ref) asOldDepends
          case oldRefsMb of
            Nothing -> pure ()
            Just oldRefs ->
               for_ (Set.toList oldDeps)
              $ realiseOldRef oldRefs
              . Ref

          -- Then create this value
          let ourCreate =
                DMap.findWithDefault (error "missing ref creator") ref asCreate
          newRefs <- gets fst
          r       <- lift $ runLookupRef (fromMaybe mempty oldRefsMb) newRefs ourCreate
          modify' (first $ DMap.insert ref (Identity r))
          pure r

    regenRef
      :: Maybe (RefMap ()) -> Ref () a -> StateT (RefMap (), IntSet) m (Identity a)
    regenRef oldRefs ref = do
      r <- realiseRef oldRefs ref
      -- Mark its dependees as dirty if they fail the condition
      let CondDependees dependees =
            DMap.findWithDefault mempty ref reverseDeps
          dirtyDependees = Set.fromList
            [ d
            | (d, Cond cond) <- dependees
            , case DMap.lookup ref =<< oldRefs of
              Nothing              -> True
              Just (Identity oldR) -> cond oldR r
            ]
      modify' $ second (Set.union dirtyDependees)
      pure $ Identity r

    recreate dirty old = do
      let initialDirtyRefs = bfoldMap
            (\case
              Pair DoRecreate (Ref r) -> Set.singleton r
              _                       -> mempty
            )
            (bzip dirty rs)
          oldRefs = bfoldMap (\(Pair r v) -> DMap.singleton r v) (bzip rs old)

      let

      flip evalStateT (mempty, initialDirtyRefs) $ btraverse
        (\case
          -- We have been explicitly asked to recreate this one
          Pair DoRecreate    (Pair _        ref) -> regenRef (Just oldRefs) ref
          Pair DoNotRecreate (Pair oldValue ref) -> do
            foundDirty <- gets snd
            if unRef ref `Set.member` foundDirty
              then regenRef (Just oldRefs) ref
              else pure oldValue
        )
        (bzip dirty (bzip old rs))

    createNew = do
      flip evalStateT mempty $ btraverse (regenRef mempty) rs
  in
    (recreate, createNew)

newtype CondDependees a = CondDependees [(Int, Cond a)]
  deriving newtype (Semigroup, Monoid)

type Recreator m f = f DoRecreate -> f Identity -> m (f Identity)

data DoRecreate a = DoRecreate | DoNotRecreate

-- | Generate a graphviz description for this set of actions
--
-- Dependencies on old values are shown with dotted lines
actionsGraph :: Actions s m a -> String
actionsGraph (Actions c) =
  let ActionsState {..} = execState c initialActionState
      toEdges ds = [ (p, child) | (p, cs) <- Map.toList ds, child <- Set.toList cs ]
      edges    = toEdges (unCondDeps <$> asDepends)
      oldEdges = toEdges asOldDepends
  in  "digraph G {"
        <> intercalate
             ";"
             (  [ show a <> "->" <> show b | (a, b) <- edges ]
             <> [ show a
                  <> "->"
                  <> show b
                  <> " [style=dashed"
                  <> bool "" ";dir=back" (a == b)
                  <> "]"
                | (a, b) <- oldEdges
                ]
             <> [ show n <> " [label=" <> show name <> "]"
                | (n, name) <- Map.toList asNames
                ]
             )
        <> "}"

----------------------------------------------------------------
--
----------------------------------------------------------------

type RefMap s = DMap (Ref s) Identity
data LookupData s = LookupData
  { ldOldRefMap :: RefMap s
  , ldRefMap    :: RefMap s
  }
newtype LookupRef s m a = LookupRef { _unLookupRef :: ReaderT (LookupData s) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

runLookupRef :: RefMap s -> RefMap s -> LookupRef s m a -> m a
runLookupRef oldRefMap refMap (LookupRef x) =
  runReaderT x (LookupData oldRefMap refMap)

lookupRef :: Monad m => Ref s a -> LookupRef s m a
lookupRef r = do
  a <- LookupRef . asks $ DMap.lookup r . ldRefMap
  pure $ maybe (error ("lookupRef: missing ref: " <> show r)) runIdentity a

lookupOldRef :: Monad m => Ref s a -> LookupRef s m (Maybe a)
lookupOldRef r =
  fmap (fmap runIdentity) . LookupRef . asks $ DMap.lookup r . ldOldRefMap

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

newRef :: Actions s m (Ref s a)
newRef = Actions $ do
  as <- get
  let r = asNextRef as
  put (as { asNextRef = succ r })
  pure $ Ref r
