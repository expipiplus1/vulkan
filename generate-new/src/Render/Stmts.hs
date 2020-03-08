{-# LANGUAGE DeriveFunctor #-}
{-# language RecursiveDo #-}

module Render.Stmts
  ( Stmts
  , Stmt
  , HasStmt
  , Action(..)
  , Inline(..)
  , Ref
  , StmtResult(..)
  , renderStmtsContT
  , renderStmtsIO
  , stmt
  , after
  , use
  , Fixpoint

  , LookupRef
  , StmtState
  )
where

import           Relude                  hiding ( Type
                                                , State
                                                , modify'
                                                , evalState
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
import qualified Data.List.Extra               as List
import           Polysemy.Fixpoint
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

import           Render.Utils
import           Haskell
import           Error

type Stmts r a = Member (State StmtsState) r => Sem r a

type Stmt r a
  = (Member (State StmtState) r, Member (Reader LookupRef) r) => Sem r a

type HasStmt r = (Member (State StmtState) r, Member (Reader LookupRef) r)

newtype LookupRef = LookupRef (forall a. Ref a -> (a, StmtResultWithDepends a))

data Action a
  = Pure Inline a
  | IOAction a
  | ContTAction a
  deriving (Show, Functor)

isContTAction :: Action a -> Bool
isContTAction = \case
  ContTAction _ -> True
  _             -> False

isIOAction :: Action a -> Bool
isIOAction = \case
  IOAction _ -> True
  _          -> False

data Inline = DoInline | DoNotInline
  deriving (Show)

data Ref (a :: *) where
  Ref :: Typeable a => Int -> Ref a

refVal :: Ref a -> Int
refVal (Ref n) = n

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

data StmtResult (a :: *) where
  StmtResult
    :: Coercible (Doc ()) a
    => { rType     :: Maybe Type
       , rNameHint :: Maybe Text
       , rAction   :: Action a
       }
    -> StmtResult a

stmtResultAsDoc :: StmtResult a -> StmtResult (Doc ())
stmtResultAsDoc = \case
  r@StmtResult{} -> coerce r

data StmtResultWithDepends a = StmtResultWithDepends
  { result  :: StmtResult a
  , depends :: Set (Some Ref)
  }

data StmtsState = StmtsState
  { nextRef :: Int
  , refMap  :: DMap Ref StmtResultWithDepends
  , lookupRef :: LookupRef
  }

newtype StmtState = StmtState { stmtDepends :: Set (Some Ref) }

data ReferencedTimes = None | Once | Many

-- | Render a do block of these statements
renderStmtsIO
  :: (Member Fixpoint r, HasErr r, Coercible a (Doc ()))
  => Sem (State StmtsState ': r) (Ref a)
  -> Sem r (Doc ())
renderStmtsIO a = renderStmtsIO' =<< resolveStmts a

-- | Render a do block of these statements
renderStmtsContT
  :: (Member Fixpoint r, HasErr r, Coercible a (Doc ()))
  => Sem (State StmtsState ': r) (Ref a)
  -> Sem r (Doc ())
renderStmtsContT a = do
  refMap <- resolveStmts a
  let
    testAction
      :: (forall a . Action a -> Bool) -> DSum Ref StmtResultWithDepends -> Bool
    testAction p (_ :=> StmtResultWithDepends (StmtResult _ _ a) _) = p a
    (contTSection, ioSection) = List.breakEnd (testAction isContTAction) refMap
    useIOSection              = any (testAction isIOAction) ioSection
  let
    rendered refMap = catMaybes
      [ case rAction of
          Pure _ d | isLast  -> Just $ "pure $" <+> d
          -- TODO: This still processes side effects even though the result is
          -- elided... Probably not the end of the world we shouldn't be
          -- generating unused things anyway.
          Pure _ _ | None <- referencedTimes refMap (This r) -> Nothing
          Pure DoInline    _ -> Nothing
          Pure DoNotInline d -> Just $ "let" <+> var <+> "=" <+> d
          IOAction d | None <- referencedTimes refMap (This r) ->
            Just $ "liftIO $" <+> d
          IOAction d    -> Just $ var <+> "<- liftIO $" <+> d
          ContTAction d | None <- referencedTimes refMap (This r) -> Just d
          ContTAction d -> Just $ var <+> "<-" <+> d
      | (r :=> StmtResultWithDepends {..}, isLast) <- zip
        refMap
        (replicate (length refMap - 1) False ++ [True])
      , let StmtResult {..} = stmtResultAsDoc result
      , let var = getVarNameAsDoc r result
      ]
  if useIOSection
    then do
      -- Render the IO section
      ioSectionDoc <- renderStmtsIO' ioSection
      -- Use the last IO action (the returned one) as a template for the action
      -- of this new doc.
      let lastIOAction = List.last ioSection
      pure $ doBlock
        (show <$> rendered
          (  contTSection
          <> [ (\(Ref r :=> s) -> Ref r :=> s
                 { result = (stmtResultAsDoc (result s))
                              { rAction = IOAction ioSectionDoc
                              }
                 }
               )
                 lastIOAction
             ]
          )
        )
    else pure $ doBlock (show <$> rendered refMap)

renderStmtsIO'
  :: (Member Fixpoint r, HasErr r)
  => [DSum Ref StmtResultWithDepends]
  -> Sem r (Doc ())
renderStmtsIO' refMap = do
  results <- sequenceV $ catMaybes
    [ case rAction of
        Pure _ d | isLast -> Just $ pure $ "pure $" <+> d
        Pure _ _ | None <- referencedTimes refMap (This r) -> Nothing
        Pure DoInline _   -> Nothing
        Pure DoNotInline d ->
          Just $ pure $ "let" <+> pretty rNameHint <+> "=" <+> d
        IOAction d | None <- referencedTimes refMap (This r) -> Just $ pure d
        IOAction d -> Just $ pure $ pretty rNameHint <+> "<-" <+> d
        ContTAction _ ->
          Just $ throw "ContT action while rendering IO statements"
    | (r :=> StmtResultWithDepends {..}, isLast) <- zip
      refMap
      (replicate (length refMap - 1) False ++ [True])
    , let StmtResult {..} = stmtResultAsDoc result
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
   . (Member Fixpoint r, HasErr r, Coercible a (Doc ()))
  => Sem (State StmtsState ': r) (Ref a)
  -> Sem r [DSum Ref StmtResultWithDepends]
resolveStmts a = mdo
  StmtsState {..} <- execState (StmtsState 0 mempty lookupRef') $ do
    r@(Ref _) <- a
    unlessM (isMostRecentRef r) $ do
      _ <- stmt $ do
        (t, w) <- useWithType r
        pure $ StmtResult (Just t) Nothing (Pure DoInline w)
      pure ()
  forV_ (DMap.toList refMap) $ \(_ :=> StmtResultWithDepends _ deps) ->
    forV_ deps
      $ \(This ref) -> unless (DMap.member ref refMap) (throw "missing ref")
  let lookupRef' = LookupRef $ \r -> case DMap.lookup r refMap of
        Nothing -> error "Impossible, getting a nonexistent ref"
        Just x  -> case result x of
          StmtResult {..} -> case rAction of
            Pure DoInline d -> (coerce (parens @()) d, x)
            _               -> (getVarName r (result x), x)
  pure $ DMap.toAscList refMap

getVarName :: forall a . Ref a -> StmtResult a -> a
getVarName r StmtResult {..} = maybe
  (coerce ("x" <> viaShow (refVal r) :: Doc ()))
  (coerce (pretty @Text @()))
  rNameHint

getVarNameAsDoc :: forall a . Ref a -> StmtResult a -> Doc ()
getVarNameAsDoc ref res@StmtResult{} = coerce $ getVarName ref res

stmt
  :: Typeable a
  => Sem (State StmtState ': Reader LookupRef ': r) (StmtResult a)
  -> Stmts r (Ref a)
stmt s = do
  lookupRef'       <- gets lookupRef
  (stmtState, res) <- runReader lookupRef' . runState (StmtState mempty) $ s
  r                <- newRef
  modify'
    (\st -> st
      { refMap = DMap.insert
                   r
                   (StmtResultWithDepends res (stmtDepends stmtState))
                   (refMap st)
      }
    )
  pure r

newRef :: Typeable a => Stmts r (Ref a)
newRef = do
  r <- gets nextRef
  modify' (\s -> s { nextRef = succ r })
  pure (Ref r)

isMostRecentRef :: Ref a -> Stmts r Bool
isMostRecentRef (Ref i) = (== succ i) <$> gets nextRef

-- | Insert a dependency on this ref in this statement, but don't use the value
-- itself. Useful for enforcing ordering.
after :: Ref a -> Stmt t ()
after ref =
  modify' (\s -> s { stmtDepends = Set.insert (This ref) (stmtDepends s) })

-- | Because we use MonadFix to generate either a bound variable or an inline
-- representation here it's important that the @Stmt@ result is not strict in
-- the @Doc ()@ returned here!
use :: HasErr r => Ref a -> Stmt r a
use = fmap snd . useWithType

-- | Because we use MonadFix to generate either a bound variable or an inline
-- representation here it's important that the @Stmt@ result is not strict in
-- the @Doc ()@ returned here!
useWithType :: HasErr r => Ref a -> Stmt r (Type, a)
useWithType ref = do
  after ref
  LookupRef l <- ask
  let (d, StmtResultWithDepends {..}) = l ref
  t <- note "No type registered for statement" $ rType result
  pure (t, coerce d)

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

test6 :: IO (Either (_ Text) (Doc ()))
test6 = runFinal . fixpointToFinal @IO . embedToFinal @IO . runErr . renderStmtsContT $ do
  a <- stmt $ do
    liftIO (putStrLn "Generating a")
    pure $ StmtResult @(Doc ()) Nothing (Just "a") (ContTAction "ContT undefined 1")
  b <- stmt $ do
    a' <- use a
    liftIO (putStrLn "Generating b")
    pure $ StmtResult Nothing (Just "b") (IOAction ("IO 2 + " <> a'))
  _ <- stmt $ do
    b' <- use b
    liftIO (putStrLn "Generating unnamed")
    pure $ StmtResult Nothing Nothing (IOAction ("IO 9 + " <> b'))
  _ <- stmt $ do
    a' <- use a
    liftIO (putStrLn "Generating c")
    pure $ StmtResult Nothing (Just "c") (Pure DoNotInline ("3 + " <> a'))
  pure b
