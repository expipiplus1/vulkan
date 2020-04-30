{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Render.Stmts
  ( HasStmts
  , Stmt
  , StmtE
  , RenderedStmts(..)
  , renderStmts
  , renderStmtsIO
  , renderSubStmts
  , renderSubStmtsIO
  , Value(..)
  , Inline(..)
  , Ref
  , stmt
  , nameRef
  , useViaName
  , useViaNameMaybe
  , pureStmt
  , unitStmt
  , use
  , after
  , refType
  , freshName
  , freeNames
  , ValueDoc(..)
  , AddrDoc(..)
  , UnitDoc(..)
  , FunDoc(..)
  , CmdsDoc(..)
  ) where

import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.Map             ( DMap
                                                , Some(..)
                                                )
import qualified Data.GADT.Compare             as DMap
import qualified Data.GADT.Show                as DMap
import qualified Data.List.Extra               as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text.Extra                ( lowerCaseFirst
                                                , snoc
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Type.Equality
import           Data.Typeable
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.State
import           Prelude                        ( lookup
                                                , showsPrec
                                                , until
                                                )
import           Relude                  hiding ( State
                                                , Type
                                                , evalState
                                                , execState
                                                , get
                                                , gets
                                                , modify'
                                                , runState
                                                )

import           Error
import           Haskell
import           Render.Element
import           Render.Utils

----------------------------------------------------------------
-- Some handy newtypes for tagging results
----------------------------------------------------------------
-- TODO: Reduce duplication
newtype AddrDoc = AddrDoc { unAddrDoc  :: Doc () }
  deriving Show
newtype ValueDoc = ValueDoc { unValueDoc :: Doc () }
  deriving Show
newtype UnitDoc = UnitDoc { unUnitDoc :: Doc () }
  deriving Show
newtype FunDoc = FunDoc { unFunDoc :: Doc () }
  deriving Show
newtype CmdsDoc = CmdsDoc { unCmdsDoc :: Doc () }
  deriving Show


----------------------------------------------------------------
-- Statements are written in a monad where one can lookup 'Ref's
-- for their value
----------------------------------------------------------------

type HasStmts r = (Member Fixpoint r, HasErr r)
type Stmt s (r :: [Effect]) = Sem (StmtE s r ': r)
type StmtE s (r :: [Effect]) = State (ActionsState s r)

data ActionsState s (r :: [Effect]) = ActionsState
  { asRefMap           :: DMap (Ref s) (Value' s r)
    -- ^ We keep a map of references to values
  , asNextRefVal       :: Int
  , asStmts            :: [(Some (Ref s), Value (Doc ()))]
    -- ^ The values we need to render, reversed
  , asUsedRefs         :: [(Some (Ref s), Text)]
    -- ^ The refs of 'use'd values, used to construct asTimesUsed
  , asTimesUsed        :: Some (Ref s) -> ZOM
    -- ^ The number of times each value is used
  , asUsedNames        :: Set Text
    -- ^ Names we shouldn't use as binders
  , asRefNames         :: Map Text (Some (Ref s))
    -- ^ Sometimes it's convenient to refer to 'Ref's by name later on in the
    -- computation.
  , asParentUseViaName :: forall a . Typeable a => Text -> Sem r (Maybe a)
  }

data ZOM
  = Zero
  | One
  | Many

----------------------------------------------------------------
-- Rendering statements
----------------------------------------------------------------

data RenderedStmts a
  = ContTStmts a
    -- ^ This value is of type "ContT something'
  | IOStmts a
    -- ^ This value is of type "IO something'

renderStmts
  :: forall r a
   . (HasStmts r, HasRenderParams r, HasRenderElem r)
  => Set.Set Text
  -- ^ Names we shouldn't use for bound variables
  -> (forall s . Sem (StmtE s r ': r) (Ref s a))
  -> Sem r (RenderedStmts (Doc ()))
renderStmts = renderStmts' id (const (pure Nothing))

renderStmtsIO
  :: forall r a
   . (HasStmts r, HasRenderElem r, HasRenderParams r)
  => Set.Set Text
  -- ^ Names we shouldn't use for bound variables
  -> (forall s . Sem (StmtE s r ': r) (Ref s a))
  -> Sem r (Doc ())
  -- ^ Throws if this would involve rendering a ContT action
renderStmtsIO badNames a =
  renderStmts' id (const (pure Nothing)) badNames a >>= \case
    ContTStmts _ -> throw "Rendering a ContTAction for IO"
    IOStmts    a -> pure a

renderSubStmts
  :: forall r a s
   . (HasStmts r, HasRenderParams r, HasRenderElem r)
  => (  forall s'
      . Sem (StmtE s' (StmtE s r ': r) ': StmtE s r ': r) (Ref s' a)
     )
  -> Stmt s r (RenderedStmts (Doc ()))
renderSubStmts a = do
  badNames <- gets @(ActionsState s r) asUsedNames
  renderStmts' id useViaNameMaybe badNames a

renderSubStmtsIO
  :: forall r a s
   . (HasStmts r, HasRenderParams r, HasRenderElem r)
  => (  forall s'
      . Sem (StmtE s' (StmtE s r ': r) ': StmtE s r ': r) (Ref s' a)
     )
  -> Stmt s r (Doc ())
renderSubStmtsIO a = renderSubStmts a >>= \case
  ContTStmts _ -> throw "Rendering a ContTAction for IO"
  IOStmts    a -> pure a

renderStmts'
  :: forall r q a
   . (HasStmts q, HasErr r, HasRenderParams r, HasRenderElem r)
  => (forall x . Sem q x -> Sem r x)
  -- ^ Raise the generating context into the rending context, usually 'id'
  -> (forall a . Typeable a => Text -> Sem q (Maybe a))
  -- ^ Lookup a ref by name in the parent scope
  -> Set.Set Text
  -- ^ Names we should use for bound variables
  -> (forall s . Sem (StmtE s q ': q) (Ref s a))
  -> Sem r (RenderedStmts (Doc ()))
renderStmts' lift parentRef badNames a = do
  ActionsState {..} <- lift (runStmts badNames parentRef a)

  let (con, r) = if any (isContTAction . snd) asStmts
        then (ContTStmts, renderStmtContT)
        else (IOStmts, renderStmtIO)

  -- Special case, if the last statement is just returning () and the
  -- penultimate statement has type () then don't render the redundant pure.
  -- TODO: Take off any number of `pure ()`s
  let neatenedStmts = case asStmts of
        (_, Pure _ x1Doc) : x2@(This r2, _) : xs
          | "()" == (show x1Doc :: String)
          , Just Value' {..} <- DMap.lookup r2 asRefMap
          , Just t <- rType
          , ConT ''() == t
          -> x2 : xs
        xs -> xs

  -- Always render "_" binders
  let binder (This ref) end = case DMap.lookup ref asRefMap of
        Just Value' { rNameHint = Just "_" } | not end -> Just "_"
        _ -> bool (List.lookup (This ref) asUsedRefs) Nothing end
  con . doBlock <$> sequence
    [ r (binder ref end) v
    | ((ref, v), end) <- reverse (zip neatenedStmts (True : repeat False))
    ]

runStmts
  :: forall r a s
   . HasStmts r
  => Set.Set Text
  -- ^ Names we should use for bound variables
  -> (forall a . Typeable a => Text -> Sem r (Maybe a))
  -- ^ Lookup a ref by name in the parent scope
  -> Sem (StmtE s r ': r) (Ref s a)
  -> Sem r (ActionsState s r)
runStmts badNames parentRef a = mdo
  let initialState =
        ActionsState DMap.empty 0 [] [] timesUsed badNames mempty parentRef
      timesUsed r = case filter ((== r) . fst) asUsedRefs of
        []  -> Zero
        [_] -> One
        _   -> Many

  x@ActionsState {..} <- execState initialState $ returning =<< a
  pure x

-- | Make sure that the last rendered statement has the value of the given ref
returning :: forall a r s . HasErr r => Ref s a -> Sem (StmtE s r ': r) ()
returning r = do
    -- Make sure it's in the output somewhere, force rendering otherwise
    -- InlineOnce and AlwaysInline pures won't end up in the output.
  _ <- use' True r

  -- Look at the rendered statements
  gets @(ActionsState s r) asStmts >>= \case
    -- This is already the most recent rendered stmt, leave things as they
    -- are
    (l, _) : _ | l == This r -> pure ()

    -- This is not the most recent statement, render a "Pure" action with
    -- just this result in
    _                        -> after =<< redundantPure r

-- | Wrap a ref in a 'Pure' action
redundantPure
  :: forall a r s . HasErr r => Ref s a -> Sem (StmtE s r ': r) (Ref s a)
redundantPure ref@Ref{} = do
  Value'{} <- lookupRef @r ref
  stmt Nothing Nothing (Pure AlwaysInline <$> use ref)

renderStmtContT
  :: (HasRenderElem r, HasRenderParams r)
  => Maybe Text
  -> Value (Doc ())
  -> Sem r (Doc ())
renderStmtContT = renderStmt
  pure
  (\a -> do
    tellImport 'lift
    pure ("lift $" <+> a)
  )

renderStmtIO :: HasErr r => Maybe Text -> Value (Doc ()) -> Sem r (Doc ())
renderStmtIO = renderStmt
  (const (throw "Trying to render a ContT statment in an IO context"))
  pure

renderStmt
  :: (Doc () -> Sem r (Doc ()))
  -- ^ Render ContT
  -> (Doc () -> Sem r (Doc ()))
  -- ^ Render IO
  -> Maybe Text
  -> Value (Doc ())
  -> Sem r (Doc ())
renderStmt renderContT renderIO bind = \case
  Pure _ d -> case bind of
    Nothing -> pure $ "pure $" <+> d
    Just b  -> pure $ "let" <+> pretty b <+> "=" <+> d
  IOAction d -> case bind of
    Nothing -> renderIO d
    Just b  -> ((pretty b <+> "<-") <+>) <$> renderIO d
  ContTAction d -> case bind of
    Nothing -> renderContT d
    Just b  -> ((pretty b <+> "<-") <+>) <$> renderContT d

----------------------------------------------------------------
-- Generating references
----------------------------------------------------------------

stmt
  :: forall a r s
   . (Coercible a (Doc ()), Typeable a)
  => Maybe Type
  -> Maybe Text
  -> Stmt s r (Value a)
  -> Stmt s r (Ref s a)
stmt ty name val = do
  let s = Value' ty name (Right val)
  ref <- newRef
  modify' @(ActionsState s r)
    (\st -> st { asRefMap = DMap.insert ref s (asRefMap st) })
  pure ref

-- | Give a 'Ref' a name to which can be used to refer to it later with
-- 'useViaName'
--
-- TODO: Use a user specified type here, not text
nameRef :: forall s r a . HasErr r => Text -> Ref s a -> Stmt s r ()
nameRef name ref = do
  names <- gets @(ActionsState s r) asRefNames
  case Map.lookup name names of
    Nothing -> modify' @(ActionsState s r)
      (\st -> st { asRefNames = Map.insert name (This ref) (asRefNames st) })
    Just (This x) | Just Refl <- x `DMap.geq` ref -> pure ()
                  | otherwise -> throw $ "Duplicate Refs named " <> name

-- | Get a 'Ref' which was named earlier with 'nameRef'
useViaName
  :: forall r a s
   . (HasErr r, Typeable a)
  => Text
  -> Sem (StmtE s r ': r) a
useViaName name =
  note ("Unable to find ref named " <> name) =<< useViaNameMaybe name

-- | Get a 'Ref' which was named earlier with 'nameRef'
useViaNameMaybe
  :: forall r a s
   . (HasErr r, Typeable a)
  => Text
  -> Sem (StmtE s r ': r) (Maybe a)
useViaNameMaybe name = do
  names <- gets @(ActionsState s r) asRefNames
  case Map.lookup name names of
    Nothing                          -> do
      -- If we don't find anything here, check the parent
      parentRef <- gets @(ActionsState s r) asParentUseViaName
      raise $ parentRef name
    Just (This (x@Ref{} :: Ref s x)) -> case eqT @x @a of
      Just Refl -> Just <$> use x
      Nothing -> throw $ "Trying to use ref " <> name <> " with incorrect type"

-- A helper for making an AlwaysInline Pure statement
pureStmt :: (Typeable a, Coercible a (Doc ())) => a -> Stmt s r (Ref s a)
pureStmt = stmt Nothing Nothing . pure . Pure AlwaysInline

-- A helper for making a statement with no name and unit type
unitStmt
  :: (Typeable a, Coercible a (Doc ()))
  => Stmt s r (Value a)
  -> Stmt s r (Ref s a)
unitStmt = stmt (Just (ConT ''())) Nothing

newRef :: forall a r s . Typeable a => Sem (StmtE s r ': r) (Ref s a)
newRef = do
  r <- gets @(ActionsState s r) asNextRefVal
  modify' @(ActionsState s r) (\s -> s { asNextRefVal = succ r })
  pure (Ref r)

----------------------------------------------------------------
-- Things one can do while writing a statement
----------------------------------------------------------------

-- | Insert a dependency on this ref in this statement, but don't use the value
-- itself. Useful for enforcing ordering.
after :: forall r a s . HasErr r => Ref s a -> Sem (StmtE s r ': r) ()
after = void . renderRef False

-- | Add a dependency on this value and use it
use :: forall r a s . HasErr r => Ref s a -> Sem (StmtE s r ': r) a
use = use' False

refType :: forall r a s. HasErr r => Ref s a -> Sem (StmtE s r ': r) Type
refType =
  note "Trying to get the type of an untyped Ref" . rType <=< lookupRef @r

use' :: forall r a s . HasErr r => Bool -> Ref s a -> Sem (StmtE s r ': r) a
use' forceRender ref = do
  (s@Value'{}, v) <- renderRef forceRender ref
  var <- varName ref (rNameHint s)
  let varNameD = coerce @(Doc ()) (pretty var)
  used <- gets @(ActionsState s r) asTimesUsed
  modify' @(ActionsState s r)
    (\st -> st { asUsedRefs = (This ref, var) : asUsedRefs st })
  pure $ case v of
    Pure NeverInline _ -> varNameD
    Pure InlineOnce  d -> case used (This ref) of
      Zero -> error "Impossible, using a ref with zero usages"
      One  -> coerce (parens @()) d
      Many -> varNameD
    Pure AlwaysInline d -> coerce (parens @()) d
    IOAction    _       -> varNameD
    ContTAction _       -> varNameD

varName :: forall s r a . Ref s a -> Maybe Text -> Stmt s r Text
varName ref hint = do
  -- Check if this reference already has a name
  usedRefs <- gets @(ActionsState s r) asUsedRefs
  case Prelude.lookup (This ref) usedRefs of
    Just v -> pure v

    Nothing | Just "_" <- hint -> pure "_"

    -- If it doesn't make one up which isn't in 'usedNames'
    Nothing -> freshName (hint <|> Just ("x" <> show (unRef ref)))

freshName :: forall s r . Maybe Text -> Stmt s r Text
freshName hint = do
  usedNames <- gets @(ActionsState s r) asUsedNames
  let proposed = unReservedWord . lowerCaseFirst . fromMaybe "x" $ hint
      actual   = until (`Set.notMember` usedNames) (`snoc` '\'') proposed
  modify' @(ActionsState s r)
    (\st -> st { asUsedNames = Set.insert actual (asUsedNames st) })
  pure actual

freeNames :: forall s r . [Text] -> Stmt s r ()
freeNames names = modify' @(ActionsState s r)
  (\st ->
    st { asUsedNames = asUsedNames st Set.\\ Set.fromList names }
  )

renderRef
  :: forall r a s
   . HasErr r
  => Bool
  -- ^ Force writing a statement for this, even if it should be inlined
  -> Ref s a
  -> Sem (StmtE s r ': r) (Value' s r a, Value a)
renderRef forceRender ref = do
  s@Value'{} <- lookupRef @r ref
  used       <- gets @(ActionsState s r) asTimesUsed
  case rAction s of
    -- This has already been evaluated
    Left  v -> pure (s, v)
    Right a -> do
      v <- a
      let s'       = s { rAction = Left v }
          doRender = forceRender || case v of
            Pure AlwaysInline _ -> False
            Pure InlineOnce _ | One <- used (This ref) -> False
            _                   -> True
      modify' @(ActionsState s r)
        (\st -> st
          { asStmts  = if doRender
                         then (This ref, coerce v) : asStmts st
                         else asStmts st
          , asRefMap = DMap.insert ref s' (asRefMap st)
          }
        )
      pure (s, v)

lookupRef
  :: forall q r a s
   . (HasErr r, Member (StmtE s q) r)
  => Ref s a
  -> Sem r (Value' s q a)
lookupRef ref = do
  refMap <- gets @(ActionsState s q) asRefMap
  note "Trying to lookup a missing ref" $ DMap.lookup ref refMap

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

data Value' s r (a :: *) where
  Value'
    :: Coercible (Doc ()) a
    => { rType     :: Maybe Type
       , rNameHint :: Maybe Text
       , rAction   :: Either (Value a) (Sem (StmtE s r ': r) (Value a))
       }
    -> Value' s r a

-- | A value on the RHS of an assignment
data Value a
  = Pure Inline a
  | IOAction a
  | ContTAction a
  deriving (Show)

isContTAction :: Value a -> Bool
isContTAction = \case
  ContTAction _ -> True
  _             -> False

data Inline
  = AlwaysInline
  | InlineOnce
    -- ^ Only inline if this value is used once.
  | NeverInline
  deriving (Show)

-- | A handle with which one can refer to the results of other statements
data Ref s (a :: *) where
  Ref :: Typeable a => { unRef :: Int } -> Ref s a

deriving instance Eq (Ref s a)
deriving instance Ord (Ref s a)
deriving instance Show (Ref s a)

instance DMap.GEq (Ref s) where
  geq :: forall s a b . Ref s a -> Ref s b -> Maybe (a :~: b)
  geq (Ref a) (Ref b) = case eqT @a @b of
    Just Refl | a == b -> Just Refl
    _                  -> Nothing

instance DMap.GCompare (Ref s) where
  gcompare :: forall s a b . Ref s a -> Ref s b -> DMap.GOrdering a b
  gcompare a@(Ref ai) b@(Ref bi) = case DMap.geq a b of
    Just Refl -> DMap.GEQ
    Nothing   -> case compare (ai, typeRep a) (bi, typeRep b) of
      EQ -> error "impossible"
      LT -> DMap.GLT
      GT -> DMap.GGT

instance DMap.GShow (Ref s) where
  gshowsPrec = showsPrec

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- test1 :: Sem (StmtE s r : r) (Ref (Doc ()))
-- test1 = stmt Nothing Nothing (pure (IOAction "print 1"))

-- inlineOnce :: Sem (StmtE s r : r) (Ref (Doc ()))
-- inlineOnce =
--   stmt Nothing (Just "inlinedOnce") (pure (Pure InlineOnce "do not repeat"))

-- inlineNever :: Sem (StmtE s r : r) (Ref (Doc ()))
-- inlineNever =
--   stmt Nothing (Just "neverInlined") (pure (Pure NeverInline "do not inline"))

-- inlineAlways :: Sem (StmtE s r : r) (Ref (Doc ()))
-- inlineAlways =
--   stmt Nothing (Just "alwaysInlined") (pure (Pure AlwaysInline "always inline"))

-- test2 :: IO (Either (Vector Text) (Doc ()))
-- test2 =
--   runFinal . embedToFinal @IO . runErr . renderStmtsContT @IO $ do
--     o  <- inlineAlways
--     r2 <- stmt Nothing (Just "r2") $ do
--       o' <- use o
--       r1 <- test1
--       liftIO $ putStrLn "rendering r2"
--       r1' <- use r1
--       pure $ ContTAction $ "ContT hello" <+> parens r1' <+> parens o'
--     stmt Nothing (Just "r3") $ do
--       o' <- use o
--       r2' <- use r2
--       liftIO $ putStrLn "rendering r3"
--       pure $ ContTAction $ "ContT world" <+> parens r2' <+> parens o'

-- test3 :: IO (Either (Vector Text) (Doc ()))
-- test3 =
--   runFinal . embedToFinal @IO . runErr . renderStmtsContT @IO $ do
--     x <- stmt Nothing (Just "r") $ do
--       pure (IOAction @(Doc ()) ("IO"))
--     y <- stmt Nothing (Just "q") $ do
--       x' <- use x
--       pure (IOAction @(Doc ()) ("IO" <+> x'))
--     o <- stmt
--       Nothing
--       (Just "inlinedOnce")
--       (do
--         pure (Pure @(Doc ()) AlwaysInline "do not repeat")
--       )

--     after x

--     pure o


-- -- foo :: (forall q. Members q r => Sem q a) -> Sem r a
-- -- foo = id

-- data RunnableThing es a where
--   RunnableThing :: (forall r .Members es r => Sem r a) -> RunnableThing es a

-- foo :: Members es r => RunnableThing es a -> Sem r a
-- foo (RunnableThing f) = f

-- bar :: Members es r => (forall q. Members es q => Sem q a) -> Sem r a
-- bar a = a
