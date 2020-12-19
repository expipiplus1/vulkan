module Build
  ( Handle
  , create
  , use
  , Actions
  , actionsGraph
  , runActions
  ) where

import           Barbies
import           Barbies.Bare
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Dependent.Map             ( DMap )
import qualified Data.Dependent.Map            as DMap
import           Data.Functor.Identity
import           Data.GADT.Compare              ( GCompare(..)
                                                , GEq(..)
                                                , GOrdering(GEQ, GGT, GLT)
                                                )
import           Data.List                      ( intercalate )
import           Data.Type.Equality             ( (:~:)(Refl) )
import           Unsafe.Coerce                  ( unsafeCoerce )

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

create :: Monad m => String -> Action m (m a) -> Actions (Handle m a)
create name a = Actions $ do
  let deps = actionDepends a
  (n, rs, es) <- get
  let r = Ref n name
  put (succ n, r : rs, [ (n, refRef d) | d <- deps ] <> es)
  let -- TODO: Remove this loop
      h = Handle { handleRef    = r
                 , handleCreate = cached h (lift =<< runAction a)
                 }
  pure h

--
-- Consuming actions
--

-- | Generate a graphviz description for this set of actions
actionsGraph :: Actions a -> String
actionsGraph (Actions c) =
  let (_, rs, es) = execState c (0, [], [])
  in  "digraph G {"
        <> intercalate
             ";"
             (  [ show a <> "->" <> show b | (a, b) <- es ]
             <> [ show n <> " [label=" <> show name <> "]" | Ref n name <- rs ]
             )
        <> "}"

runActions
  :: (BareB f, Monad m, TraversableB (f Covered))
  => Actions (f Covered (Handle m))
  -> m (f Bare Identity)
runActions (Actions c) =
  let hs = evalState c (0, [], [])
  in  runCache
        $ fmap bstrip
        . btraverse (\Handle {..} -> Identity <$> handleCreate)
        $ hs

----------------------------------------------------------------
-- Action
----------------------------------------------------------------

data Ref = Ref
  { refRef   :: Int
  , _refName :: String
  }
  deriving (Eq, Ord)

-- TODO: Make the GCompare instance safer, either by remembering the type or
-- having a phantom parameter tied to the dmap
data Handle m a = Handle
  { handleRef    :: Ref
  , handleCreate :: CacheT (Handle m) m a
  }

data Ref' a = Ref' {
                   }

instance GEq (Handle m) where
  geq h1 h2 = case gcompare h1 h2 of
    GEQ -> Just Refl
    _   -> Nothing

instance GCompare (Handle m) where
  gcompare (h1 :: Handle m a) (h2 :: Handle m b) =
    let r1 = handleRef h1
        r2 = handleRef h2
    in  case compare r1 r2 of
          LT -> GLT
          EQ -> case unsafeCoerce @_ @(a :~: b) Refl of
            Refl -> GEQ
          GT -> GGT

data Action m a where
  Pure ::a -> Action m a
  Ap ::Action m (a -> b) -> Action m a -> Action m b
  FMap ::(a -> b) -> Action m a -> Action m b
  UseHandle ::Handle m a -> Action m a

instance Functor m => Functor (Action m) where
  fmap = FMap

instance Applicative m => Applicative (Action m) where
  pure  = Pure
  (<*>) = Ap

use :: Handle m a -> Action m a
use = UseHandle

runAction :: Monad m => Action m a -> CacheT (Handle m) m a
runAction = \case
  Pure a -> pure a
  Ap f x ->
    let f' = runAction f
        x' = runAction x
    in  f' <*> x'
  FMap f x              -> f <$> runAction x
  UseHandle Handle {..} -> handleCreate

actionDepends :: Applicative m => Action m a -> [Ref]
actionDepends = \case
  Pure _                -> []
  Ap   f x              -> actionDepends f <> actionDepends x
  FMap _ x              -> actionDepends x
  UseHandle Handle {..} -> [handleRef]

newtype Actions a = Actions { _unActions :: State (Int, [Ref], [(Int, Int)]) a }
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

runCache :: Monad m => CacheT k m a -> m a
runCache (CacheT a) = evalStateT a DMap.empty
