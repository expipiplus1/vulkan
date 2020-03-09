{-# LANGUAGE DeriveFunctor #-}
{-# language RecursiveDo #-}

module Render.Stmts
  -- ( after , use, stmt )
  -- ( Stmts
  -- , Stmt
  -- , HasStmt
  -- , Action(..)
  -- , Inline(..)
  -- , Ref
  -- , StmtResult(..)
  -- , renderStmtsContT
  -- , renderStmtsIO
  -- , pureStmt
  -- , ioStmt
  -- , stmt
  -- , after
  -- , use
  -- , useWithType
  -- , refType

  -- , LookupRef
  -- , StmtState
  -- )
where

import           Relude                  hiding ( Type
                                                , State
                                                , modify'
                                                , evalState
                                                , get
                                                , gets
                                                , Reader
                                                , ask
                                                , asks
                                                , runReader
                                                , execState
                                                , runState
                                                )
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Reader
import           Polysemy.Fixpoint
import           Control.Monad.Fix
import qualified Data.List.Extra               as List
import           Data.Text.Extra                ( lowerCaseFirst )
import           Data.Text.Prettyprint.Doc
import           Data.Type.Equality
import           Data.Typeable
import qualified Data.GADT.Compare             as DMap
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.Map             ( DMap
                                                , Some(..)
                                                , DSum(..)
                                                )
import qualified Data.Set                      as Set
import           Data.Vector                    ( Vector )

import           Render.Utils
import           Haskell
import           Error

----------------------------------------------------------------
-- Statements are written in a monad where one can lookup 'Ref's
-- for their value
----------------------------------------------------------------

data ActionsState r = ActionsState
  { asRefMap     :: DMap Ref (Stmt r)
    -- ^ We keep a map of references to values
  , asNextRefVal :: Int
  , asStmts      :: [(Some Ref, Value (Doc ()))]
    -- ^ The values we need to render, reversed
  , asUsedRefs   :: [(Some Ref, Text)]
    -- ^ The refs of 'use'd values, used to construct asTimesUsed
  , asTimesUsed  :: Some Ref -> ZOM
    -- ^ The number of times each value is used
  }

data ZOM
  = Zero
  | One
  | Many

incZOM :: ZOM -> ZOM
incZOM = \case
  Zero -> One
  One -> Many
  Many -> Many

----------------------------------------------------------------
-- Rendering statements
----------------------------------------------------------------

renderStmtsContT
  :: forall m r a
   . (Member (Final m) r, MonadFix m, HasErr r)
  => Proxy m
  -> Sem (State (ActionsState r) ': r) (Ref a)
  -> Sem r (Doc ())
renderStmtsContT _ a = fixpointToFinal @m $ mdo
  let initialState = ActionsState DMap.empty 0 [] [] timesUsed
      timesUsed r = case filter ((== r) . fst) asUsedRefs of
        []  -> Zero
        [_] -> One
        _   -> Many
  ActionsState {..} <- raise $ execState initialState $ use' True =<< a
  doBlock <$> sequence
    [ renderStmtContT (bool (List.lookup ref asUsedRefs) Nothing end) v
    | ((ref, v), end) <- reverse (zip asStmts (True : repeat False))
    ]

renderStmtContT :: HasErr r => Maybe Text -> Value (Doc ()) -> Sem r (Doc ())
renderStmtContT = renderStmt pure (\a -> pure ("liftIO $" <+> a))

renderStmtIO :: HasErr r => Maybe Text -> Value (Doc ()) -> Sem r (Doc ())
renderStmtIO =
  renderStmt (const (throw "Rendering a ContTAction for IO")) pure

renderStmt
  :: HasErr r
  => (Doc () -> Sem r (Doc ()))
  -- ^ Render ContT
  -> (Doc () -> Sem r (Doc ()))
  -- ^ Render IO
  -> Maybe Text
  -> Value (Doc ())
  -> Sem r (Doc ())
renderStmt renderContT renderIO bind = \case
  Pure _            d -> case bind of
    Nothing -> pure $ "pure $" <+> d
    Just b  -> pure $ "let" <+> pretty b <+> "=" <+> d
  IOAction d -> do
    case bind of
      Nothing -> renderIO d
      Just b  -> ((pretty b <+> "<-") <+>) <$> renderIO d
  ContTAction d -> case bind of
    Nothing -> renderContT d
    Just b  -> ((pretty b <+> "<-") <+>) <$> renderContT d

----------------------------------------------------------------
-- Generating references
----------------------------------------------------------------

stmt
  :: forall a r
   . (Coercible a (Doc ()), Typeable a)
  => Maybe Type
  -> Maybe Text
  -> Sem (State (ActionsState r) ': r) (Value a)
  -> Sem (State (ActionsState r) ': r) (Ref a)
stmt ty name val = do
  let s = Stmt ty name (Right val)
  ref <- newRef
  modify' @(ActionsState r)
    (\st -> st { asRefMap = DMap.insert ref s (asRefMap st) })
  pure ref

newRef :: forall a r . Typeable a => Sem (State (ActionsState r) ': r) (Ref a)
newRef = do
  r <- gets @(ActionsState r) asNextRefVal
  modify' @(ActionsState r) (\s -> s { asNextRefVal = succ r })
  pure (Ref r)

----------------------------------------------------------------
-- Things one can do while writing a statement
----------------------------------------------------------------

-- | Insert a dependency on this ref in this statement, but don't use the value
-- itself. Useful for enforcing ordering.
after :: forall r a . HasErr r => Ref a -> Sem (State (ActionsState r) ': r) ()
after = void . use' False

use :: forall r a . HasErr r => Ref a -> Sem (State (ActionsState r) ': r) a
use = use' False

use'
  :: forall r a
   . HasErr r
  => Bool
  -> Ref a
  -> Sem (State (ActionsState r) ': r) a
use' forceRender ref = do
  (s@Stmt{}, v) <- use'' forceRender ref
  let varName :: Text
      varName  = fromMaybe ("x" <> show (unRef ref)) (rNameHint s)
      varNameD = coerce @(Doc ()) (pretty varName)
  used <- gets @(ActionsState r) asTimesUsed
  modify' @(ActionsState r)
    (\st -> st { asUsedRefs = (This ref, varName) : asUsedRefs st })
  pure $ case v of
    Pure NeverInline _ -> varNameD
    Pure InlineOnce  d -> case used (This ref) of
      Zero -> error "Impossible, using a ref with zero usages"
      One  -> d
      Many -> varNameD
    Pure AlwaysInline d -> d
    IOAction    _       -> varNameD
    ContTAction _       -> varNameD

use''
  :: forall r a
   . HasErr r
  => Bool
  -> Ref a
  -> Sem (State (ActionsState r) ': r) (Stmt r a, Value a)
use'' forceRender ref = do
  refMap   <- gets @(ActionsState r) asRefMap
  s@Stmt{} <- note "Trying to lookup a missing ref" $ DMap.lookup ref refMap
  used     <- gets @(ActionsState r) asTimesUsed
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
      modify' @(ActionsState r)
        (\st -> st
          { asStmts  = if doRender
                         then (This ref, coerce v) : asStmts st
                         else asStmts st
          , asRefMap = DMap.insert ref s' (asRefMap st)
          }
        )
      pure (s, v)

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

data Stmt r (a :: *) where
  Stmt
    :: Coercible (Doc ()) a
    => { rType     :: Maybe Type
       , rNameHint :: Maybe Text
       , rAction   :: Either (Value a) (Sem (State (ActionsState r) ': r) (Value a))
       }
    -> Stmt r a

-- | A value on the RHS of an assignment
data Value a
  = Pure Inline a
  | IOAction a
  | ContTAction a
  deriving (Show, Functor)

isContTAction :: Value a -> Bool
isContTAction = \case
  ContTAction _ -> True
  _             -> False

isIOAction :: Value a -> Bool
isIOAction = \case
  IOAction _ -> True
  _          -> False

data Inline
  = AlwaysInline
  | InlineOnce
    -- ^ Only inline if this value is used once.
  | NeverInline
  deriving (Show)

-- | A handle with which one can refer to the results of other statements
data Ref (a :: *) where
  Ref :: Typeable a => { unRef :: Int } -> Ref a

deriving instance Eq (Ref a)
deriving instance Ord (Ref a)

instance DMap.GEq Ref where
  geq :: forall a b . Ref a -> Ref b -> Maybe (a :~: b)
  geq (Ref a) (Ref b) = case eqT @a @b of
    Just Refl | a == b -> Just Refl
    _                  -> Nothing

instance DMap.GCompare Ref where
  gcompare :: forall a b . Ref a -> Ref b -> DMap.GOrdering a b
  gcompare a@(Ref ai) b@(Ref bi) = case DMap.geq a b of
    Just Refl -> DMap.GEQ
    Nothing   -> case compare (ai, typeRep a) (bi, typeRep b) of
      EQ -> error "impossible"
      LT -> DMap.GLT
      GT -> DMap.GGT

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

test1 :: Sem (State (ActionsState r) : r) (Ref (Doc ()))
test1 = stmt Nothing Nothing (pure (IOAction "print 1"))

inlineOnce :: Sem (State (ActionsState r) : r) (Ref (Doc ()))
inlineOnce =
  stmt Nothing (Just "inlinedOnce") (pure (Pure InlineOnce "do not repeat"))

inlineNever :: Sem (State (ActionsState r) : r) (Ref (Doc ()))
inlineNever =
  stmt Nothing (Just "neverInlined") (pure (Pure NeverInline "do not inline"))

inlineAlways :: Sem (State (ActionsState r) : r) (Ref (Doc ()))
inlineAlways =
  stmt Nothing (Just "alwaysInlined") (pure (Pure AlwaysInline "always inline"))

test2 :: IO (Either (Vector Text) (Doc ()))
test2 =
  runFinal . embedToFinal @IO . runErr . renderStmtsContT (Proxy @IO) $ do
    o  <- inlineAlways
    r2 <- stmt Nothing (Just "r2") $ do
      o' <- use o
      r1 <- test1
      liftIO $ putStrLn "rendering r2"
      r1' <- use r1
      pure $ ContTAction $ "ContT hello" <+> parens r1' <+> parens o'
    stmt Nothing (Just "r3") $ do
      o' <- use o
      r2' <- use r2
      liftIO $ putStrLn "rendering r3"
      pure $ ContTAction $ "ContT world" <+> parens r2' <+> parens o'

test3 :: IO (Either (Vector Text) (Doc ()))
test3 =
  runFinal . embedToFinal @IO . runErr . renderStmtsContT (Proxy @IO) $ do
    o <- inlineNever
    stmt Nothing (Just "r") $ do
      use o
      pure (Pure @(Doc ()) AlwaysInline "r + 1")


{-
----------------------------------------------------------------
-- Things to do while constructing a single action
----------------------------------------------------------------

-- useWithType :: HasErr r => Ref a -> Action r (Type, a)
-- useWithType ref = do
--   after ref
--   LookupRef l <- ask
--   let (d, StmtResultWithDepends {..}) = l ref
--   t <- note "No type registered for statement" $ rType result
--   pure (t, d)

----------------------------------------------------------------
-- Types
----------------------------------------------------------------


-- | The monad in which one writes individual actions
type Action r a
  =  (Member (State ActionDepends) r, Member (Reader (LookupRef r)) r)
  => Actions r a

newtype ActionDepends = ActionDepends { aDepends :: Set (Some Ref) }

-- | The reified reference map
newtype LookupRef r = LookupRef (forall a. Ref a -> Sem (State ActionDepends ': Reader (LookupRef r) ': State LookupLazy ': r) (a, Stmt r a))
type LookupLazy = DMap Ref SomeDoc
data SomeDoc a where
  SomeDoc :: Coercible a (Doc ()) => a -> SomeDoc a

----------------------------------------------------------------
-- Things to do with multiple actions
----------------------------------------------------------------

type Actions r a = forall q . Member (State (ActionsState q)) r => Sem r a

-- data StmtWithDepends r a = StmtWithDepends
--   { wdStmt :: StmtWithDepends r a
--   , wdDepends :: Set.Set (Some Ref)
--   }

-- | Get a reference for an action to be used later
stmt
  :: forall a r
   . (Typeable a, Coercible a (Doc ()))
  => Maybe Type
  -> Maybe Text
  -> Sem (State ActionDepends ': Reader (LookupRef r) ': State LookupLazy ': r) (Value a)
  -> Sem (State (ActionsState r) ': r) (Ref a)
stmt ty name action = do
  r <- newRef
  let w = Stmt ty name action
  modify' @(ActionsState r) (\st -> st { asRefMap = (r :=> w) : (asRefMap st) })
  pure r

newRef :: forall a r . Typeable a => Sem (State (ActionsState r) ': r) (Ref a)
newRef = do
  r <- gets @(ActionsState r) asNextRefVal
  modify' @(ActionsState r) (\s -> s { asNextRefVal = succ r })
  pure (Ref r)

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

resolve
  :: forall a r . LookupRef r
  -> Sem (State ActionDepends ': Reader (LookupRef r) ': r) (Value a)
  -> Sem r (ActionDepends, Value a)
resolve l = runReader l . runState (ActionDepends mempty)

-- | Render a do block of these statements
renderStmtsContT
  :: forall a r
   . (HasErr r, Coercible a (Doc ()))
  => Sem (State (ActionsState r) ': r) (Ref a)
  -> Sem r (Doc ())
renderStmtsContT a = do
  (ActionsState stmts _, ref) <- runState (ActionsState [] 0) a

  let renderValue :: forall a . Coercible a (Doc ()) => Value a -> a
      renderValue = \case
        Pure _ d      -> d
        IOAction    d -> d
        ContTAction d -> d

  let dMap = DMap.fromList stmts
      lookupRef :: forall a . Ref a -> Stmt r a
      lookupRef r = case DMap.lookup r dMap of
        Nothing -> error "unknown ref"
        Just s  -> s

      lookupM :: LookupRef r
      lookupM = LookupRef $ \r -> do
        lazyMap :: LookupLazy <- get
        case lookupRef r of
          s@Stmt {..} -> case DMap.lookup r lazyMap of
            Nothing          -> (, s) . renderValue <$> rAction
            Just (SomeDoc v) -> pure (v, s)

  (reqs, _) :: _ <- case lookupRef ref of
    Stmt {..} ->
      runState DMap.empty
        . runReader lookupM
        . runState (ActionDepends mempty)
        $ rAction

  let isStmtUsed = const True
      usedStmts  = filter isStmtUsed stmts

  pure $ vsep [ coerce x | _ :=> SomeDoc x <- DMap.toList reqs ]
  -- stmtDocs <-
  --   (forV (reverse stmts) $ \(_ :=> Stmt {..}) ->
  --     fmap viaShow
  --       . runReader lookupRef
  --       . evalState (ActionDepends mempty)
  --       $ rAction
  --   )
  -- pure $ vsep stmtDocs

  -- refMap <- resolveStmts a
  -- let
  --   testAction
  --     :: (forall a . Action a -> Bool) -> DSum Ref StmtResultWithDepends -> Bool
  --   testAction p (_ :=> StmtResultWithDepends (StmtResult _ _ a) _) = p a
  --   (contTSection, ioSection) = List.breakEnd (testAction isContTAction) refMap
  --   useIOSection              = any (testAction isIOAction) ioSection
  -- let
  --   rendered refMap = catMaybes
  --     [ case rAction of
  --         Pure _ d | isLast  -> Just $ "pure $" <+> d
  --         -- TODO: This still processes side effects even though the result is
  --         -- elided... Probably not the end of the world we shouldn't be
  --         -- generating unused things anyway.
  --         Pure _ _ | None <- referencedTimes refMap (This r) -> Nothing
  --         Pure DoInline    _ -> Nothing
  --         Pure DoNotInline d -> Just $ "let" <+> var <+> "=" <+> d
  --         IOAction d | None <- referencedTimes refMap (This r) ->
  --           Just $ "liftIO $" <+> d
  --         IOAction d    -> Just $ var <+> "<- liftIO $" <+> d
  --         ContTAction d | None <- referencedTimes refMap (This r) -> Just d
  --         ContTAction d -> Just $ var <+> "<-" <+> d
  --     | (r :=> StmtResultWithDepends {..}, isLast) <- zip
  --       refMap
  --       (replicate (length refMap - 1) False ++ [True])
  --     , let StmtResult {..} = stmtResultAsDoc result
  --     , let var = getVarNameAsDoc r result
  --     ]
  -- if useIOSection
  --   then do
  --     -- Render the IO section
  --     ioSectionDoc <- renderStmtsIO' ioSection
  --     -- Use the last IO action (the returned one) as a template for the action
  --     -- of this new doc.
  --     let lastIOAction = List.last ioSection
  --     pure $ doBlock
  --       (show <$> rendered
  --         (  contTSection
  --         <> [ (\(Ref r :=> s) -> Ref r :=> s
  --                { result = (stmtResultAsDoc (result s))
  --                             { rAction = IOAction ioSectionDoc
  --                             }
  --                }
  --              )
  --                lastIOAction
  --            ]
  --         )
  --       )
  --   else pure $ doBlock (show <$> rendered refMap)


----------------------------------------------------------------
-- Tests
----------------------------------------------------------------


-- test1 :: Either (Vector Text) (Doc ())
-- test1 = run . runErr . renderStmtsContT $ do
--   stmt @(Doc ()) @('[Error (Vector Text)]) Nothing Nothing
--     $ pure (IOAction ("hello" :: Doc ()))
-}

{-

-- stmt
--   :: Typeable a
--   => Sem (State StmtState ': Reader LookupRef ': r) (StmtResult a)
--   -> Stmts r (Ref a)
-- stmt s = do
--   lookupRef <- gets refMap
--   let lookupRef' = LookupRef $ \r -> case DMap.lookup r lookupRef of
--         Nothing -> error "Impossible, getting a nonexistent ref"
--         Just x  -> case result x of
--           StmtResult {..} -> case rAction of
--             Pure DoInline d -> (coerce (parens @()) d, x)
--             _               -> (getVarName r (result x), x)
--   (stmtState, res) <- runReader lookupRef' . runState (StmtState mempty) $ s
--   r                <- newRef
--   modify'
--     (\st -> st
--       { refMap = DMap.insert
--                    r
--                    (StmtResultWithDepends res (stmtDepends stmtState))
--                    (refMap st)
--       }
--     )
--   pure r

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

type Stmts m r a = Member (State (StmtsState m)) r => Sem r a

type HasStmt m r = (Member (State StmtState) r, Member (Reader (LookupRef m)) r)


stmtResultAsDoc :: StmtResult a -> StmtResult (Doc ())
stmtResultAsDoc = \case
  r@StmtResult{} -> coerce r

data StmtResultWithDepends m a = StmtResultWithDepends
  { result  :: StmtResult m a
  , depends :: Set (Some Ref)
  }

data StmtsState m = StmtsState
  { nextRef :: Int
  , refMap  :: DMap Ref (StmtResultWithDepends m)
  }

data ReferencedTimes = None | Once | Many


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | A simple inlined pure statement, useful for getting refs for already
-- declared variables.
pureStmt :: (Coercible (Doc ()) a, Typeable a) => Type -> a -> Stmts r (Ref a)
pureStmt ty doc =
  stmt $ pure $ StmtResult (Just ty) Nothing (Pure DoInline doc)

-- | A named IO statement
ioStmt
  :: (Coercible (Doc ()) a, Typeable a) => Type -> a -> Stmt r (StmtResult a)
ioStmt ty doc = pure $ StmtResult (Just ty) Nothing (IOAction doc)

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

-- | Render a do block of these statements
renderStmtsIO
  :: (HasErr r, Coercible a (Doc ()))
  => Sem (State StmtsState ': r) (Ref a)
  -> Sem r (Doc ())
renderStmtsIO a = renderStmtsIO' =<< resolveStmts a

renderStmtsIO'
  :: HasErr r
  => [DSum Ref StmtResultWithDepends]
  -> Sem r (Doc ())
renderStmtsIO' refMap = do
  results <- sequenceV $ catMaybes
    [ case rAction of
        Pure _ d | isLast -> Just $ pure $ "pure $" <+> d
        Pure _ _ | None <- referencedTimes refMap (This r) -> Nothing
        Pure DoInline _   -> Nothing
        Pure DoNotInline d ->
          Just $ pure $ "let" <+> var <+> "=" <+> d
        IOAction d | None <- referencedTimes refMap (This r) -> Just $ pure d
        IOAction d -> Just $ pure $ var <+> "<-" <+> d
        ContTAction _ ->
          Just $ throw "ContT action while rendering IO statements"
    | (r :=> StmtResultWithDepends {..}, isLast) <- zip
      refMap
      (replicate (length refMap - 1) False ++ [True])
    , let StmtResult {..} = stmtResultAsDoc result
    , let var = getVarNameAsDoc r result
    ]
  pure $ doBlock (show <$> results)

-- TODO: This incorrectly counts: uses of removed pure actions and uses
-- for ordering only
referencedTimes
  :: [DSum Ref StmtResultWithDepends] -> Some Ref -> ReferencedTimes
referencedTimes refMap r =
  case [ () | _ :=> x <- refMap, Set.member r (depends x) ] of
    []  -> None
    [_] -> Once
    _   -> Many

-- | Tie the loop and get all the statements along with their refs. Make sure
-- the last statement returns the value given by the ref.
resolveStmts
  :: forall r a
   . HasErr r
  => Sem (State StmtsState ': r) (Ref a)
  -> Sem r [DSum Ref StmtResultWithDepends]
resolveStmts a = mdo
  StmtsState {..} <- execState (StmtsState 0 mempty) $ do
    ref@(Ref _) <- a
    unlessM (isMostRecentRef ref) $ do
      _ <- stmt @a $ do
        LookupRef l <- ask
        let (d, StmtResultWithDepends {..}) = l ref
        StmtResult{} <- pure result
        after ref
        pure $ StmtResult (rType result) Nothing (Pure DoInline d)
      pure ()
  forV_ (DMap.toList refMap) $ \(_ :=> StmtResultWithDepends _ deps) ->
    forV_ deps
      $ \(This ref) -> unless (DMap.member ref refMap) (throw "missing ref")
  pure $ DMap.toAscList refMap

getVarName :: forall a . Ref a -> StmtResult a -> a
getVarName r StmtResult {..} = maybe
  (coerce ("x" <> viaShow (refVal r) :: Doc ()))
  (coerce (pretty @Text @()) . unReservedWord . lowerCaseFirst)
  rNameHint

getVarNameAsDoc :: forall a . Ref a -> StmtResult a -> Doc ()
getVarNameAsDoc ref res@StmtResult{} = coerce $ getVarName ref res


isMostRecentRef :: Ref a -> Stmts r Bool
isMostRecentRef (Ref i) = (== succ i) <$> gets nextRef

-- | Get the type of the value represented by a @Ref@ in a @Stmts@
-- computation.
refType :: HasErr r => Ref a -> Stmts r Type
refType ref = do
  StmtResultWithDepends {..} <- note "Trying to get a nonexistent ref"
    =<< gets (DMap.lookup ref . refMap)
  t <- note "No type registered for statement" $ rType result
  pure t

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- test1 :: Either (Vector Text) (Doc ())
-- test1 =
--   runIdentity . runFinal . fixpointToFinal @Identity . runErr . renderStmtsContT $ pure ()

-- test2 :: Either (Vector Text) (Doc ())
-- test2 =
--   runIdentity . runFinal . fixpointToFinal @Identity . runErr . renderStmtsContT $ do
--     a <- stmt $ do
--       pure $ StmtResult (ConT ''()) (Just "a") (IOAction "pure 1")
--     b <- stmt $ do
--       a' <- use a
--       pure $ StmtResult (ConT ''()) (Just "b") (IOAction ("pure 2 + " <> a'))
--     pure ()

-- test3 :: IO (Either (Vector Text) (Doc ()))
-- test3 = runFinal . fixpointToFinal @IO . embedToFinal @IO . runErr . renderStmtsContT $ do
--   a <- stmt $ do
--     liftIO (putStrLn "Generating a")
--     pure $ StmtResult (ConT ''()) (Just "a") (Pure DoInline "pure 1")
--   b <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating b")
--     pure $ StmtResult (ConT ''()) (Just "b") (IOAction ("pure 2 + " <> a'))
--   pure ()

-- test4 :: IO (Either (Vector Text) (Doc ()))
-- test4 = runFinal . fixpointToFinal @IO . embedToFinal @IO . runErr . renderStmtsContT $ do
--   a <- stmt $ do
--     liftIO (putStrLn "Generating a")
--     pure $ StmtResult (ConT ''()) (Just "a") (Pure DoInline "pure 1")
--   b <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating b")
--     pure $ StmtResult (ConT ''()) (Just "b") (IOAction ("pure 2 + " <> a'))
--   _ <- stmt $ do
--     b' <- use b
--     liftIO (putStrLn "Generating unnamed")
--     pure $ StmtResult (ConT ''()) Nothing (IOAction ("pure 9 + " <> b'))
--   stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating c")
--     pure $ StmtResult (ConT ''()) (Just "c") (IOAction ("pure 3 + " <> a'))
--   pure ()

-- test5 :: IO (Either (Vector Text) (Doc ()))
-- test5 = runFinal . fixpointToFinal @IO . embedToFinal @IO . runErr . renderStmtsContT $ do
--   b <- stmt $ do
--     a' <- use (Ref 999)
--     liftIO (putStrLn "Generating b")
--     pure $ StmtResult (ConT ''()) (Just "b") (IOAction ("pure 2 + " <> a'))
--   pure ()

-- test6 :: IO (Either (_ Text) (Doc ()))
-- test6 = runFinal . embedToFinal @IO . runErr . renderStmtsContT $ do
--   a <- stmt $ do
--     liftIO (putStrLn "Generating a")
--     pure $ StmtResult @(Doc ()) Nothing (Just "a") (ContTAction "ContT undefined 1")
--   b <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating b")
--     pure $ StmtResult Nothing (Just "b") (IOAction ("IO 2 + " <> a'))
--   _ <- stmt $ do
--     b' <- use b
--     liftIO (putStrLn "Generating unnamed")
--     pure $ StmtResult Nothing Nothing (IOAction ("IO 9 + " <> b'))
--   _ <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating c")
--     pure $ StmtResult Nothing (Just "c") (Pure DoNotInline ("3 + " <> a'))
--   pure b
-}
