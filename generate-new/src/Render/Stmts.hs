{-# LANGUAGE DeriveFunctor #-}
{-# language RecursiveDo #-}

module Render.Stmts
  ( Stmts
  , Stmt
  , Action(..)
  , Inline
  , Ref
  , StmtResult(..)
  , renderStmtsContT
  , renderStmtsIO
  , stmt
  , after
  , use

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
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Vector                    ( Vector )

import           Render.Utils
import           Haskell
import           Error

type Stmts r a = Sem (State StmtsState ': Reader LookupRef ': r) a

type Stmt r a
  = (Member (State StmtState) r, Member (Reader LookupRef) r) => Sem r a

type LookupRef = Ref -> (Doc (), StmtResult)

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

newtype Ref = Ref Int
  deriving newtype (Eq, Ord, Enum)

data StmtResult = StmtResult
  { rType     :: Type
  , rNameHint :: Maybe Text
  , rAction   :: Action (Doc ())
  }

data StmtsState = StmtsState
  { nextRef :: Ref
  , refMap  :: Map Ref (Set Ref, StmtResult)
  }

newtype StmtState = StmtState { stmtDepends :: Set Ref }

data ReferencedTimes = None | Once | Many

-- | Render a do block of these statements
renderStmtsContT :: (Member Fixpoint r, HasErr r) => Stmts r a -> Sem r (Doc ())
renderStmtsContT a = do
  refMap <- resolveStmts a
  let (contTSection, ioSection) =
        List.breakEnd (isContTAction . rAction . snd . snd) refMap
      useIOSection = any (isIOAction . rAction . snd . snd) ioSection
  let
    rendered refMap = catMaybes
      [ case rAction of
          Pure _ d | isLast                           -> Just $ "pure $" <+> d
          -- TODO: This still processes side effects even though the result is
          -- elided... Probably not the end of the world we shouldn't be
          -- generating unused things anyway.
          Pure _ _ | None <- referencedTimes refMap r -> Nothing
          Pure DoInline    _                          -> Nothing
          Pure DoNotInline d -> Just $ "let" <+> pretty rNameHint <+> "=" <+> d
          IOAction d | None <- referencedTimes refMap r ->
            Just $ "liftIO $" <+> d
          IOAction d    -> Just $ pretty rNameHint <+> "<- liftIO $" <+> d
          ContTAction d | None <- referencedTimes refMap r -> Just d
          ContTAction d -> Just $ pretty rNameHint <+> "<-" <+> d
      | ((r, (_, StmtResult {..})), isLast) <- zip
        refMap
        (replicate (length refMap - 1) False ++ [True])
      ]
  if useIOSection
    then do
      -- Render the IO section
      ioSectionDoc <- renderStmtsIO ioSection
      -- Use the last IO action (the returned one) as a template for the action
      -- of this new doc.
      let lastIOAction = List.last ioSection
      pure $ doBlock
        (show <$> rendered
          (  contTSection
          <> [ fmap (\r -> r { rAction = IOAction ioSectionDoc })
                 <$> lastIOAction
             ]
          )
        )
    else pure $ doBlock (show <$> rendered refMap)

renderStmtsIO
  :: (Member Fixpoint r, HasErr r)
  => [(Ref, (Set Ref, StmtResult))]
  -> Sem r (Doc ())
renderStmtsIO refMap = do
  results <- sequenceV $ catMaybes
    [ case rAction of
        Pure _ d | isLast -> Just $ pure $ "pure $" <+> d
        Pure _ _ | None <- referencedTimes refMap r -> Nothing
        Pure DoInline _ -> Nothing
        Pure DoNotInline d ->
          Just $ pure $ "let" <+> pretty rNameHint <+> "=" <+> d
        IOAction d | None <- referencedTimes refMap r -> Just $ pure d
        IOAction d -> Just $ pure $ pretty rNameHint <+> "<-" <+> d
        ContTAction _ ->
          Just $ throw "ContT action while rendering IO statements"
    | ((r, (_, StmtResult {..})), isLast) <- zip
      refMap
      (replicate (length refMap - 1) False ++ [True])
    ]
  pure $ doBlock (show <$> results)

-- TODO: This incorrectly counts: uses of removed pure actions and uses
-- for ordering only
referencedTimes :: [(Ref, (Set Ref, StmtResult))] -> Ref -> ReferencedTimes
referencedTimes refMap r =
  case [ () | (_, (deps, _)) <- refMap, Set.member r deps ] of
    []  -> None
    [_] -> Once
    _   -> Many

-- | Render a do block of these statements
resolveStmts
  :: (Member Fixpoint r, HasErr r)
  => Stmts r a
  -> Sem r [(Ref, (Set Ref, StmtResult))]
resolveStmts a = mdo
  StmtsState {..} <-
    runReader lookupRef . execState (StmtsState (Ref 0) mempty) $ a
  forV_ refMap $ \(deps, _) ->
    forV_ deps $ \ref -> unless (Map.member ref refMap) (throw "missing ref")
  let lookupRef r = case Map.lookup r refMap of
        Nothing     -> error "Impossible, getting a nonexistent ref"
        Just (_, x) -> case rAction x of
          Pure DoInline d -> (parens d, x)
          _               -> (pretty (rNameHint x), x)
  pure (Map.toAscList refMap)

stmt :: Sem (State StmtState ': Reader LookupRef ': r) StmtResult -> Stmts r Ref
stmt s = do
  (stmtState, res) <- raise . runState (StmtState mempty) $ s
  r                <- newRef
  modify'
    (\st ->
      st { refMap = Map.insert r (stmtDepends stmtState, res) (refMap st) }
    )
  pure r

newRef :: Stmts r Ref
newRef = do
  r <- gets nextRef
  modify' (\s -> s { nextRef = succ r })
  pure r

-- | Insert a dependency on this ref in this statement, but don't use the value
-- itself. Useful for enforcing ordering.
after :: Ref -> Stmt t ()
after ref = modify' (\s -> s { stmtDepends = Set.insert ref (stmtDepends s) })

-- | Because we use MonadFix to generate either a bound variable or an inline
-- representation here it's important that the @Stmt@ result is not strict in
-- the @Doc ()@ returned here!
use :: HasErr r => Ref -> Stmt r (Doc ())
use ref = do
  after ref
  r <- asks ($ ref)
  pure (fst @(Doc ()) @StmtResult r)

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

-- test6 :: IO (Either (Vector Text) (Doc ()))
-- test6 = runFinal . fixpointToFinal @IO . embedToFinal @IO . runErr . renderStmtsContT $ do
--   a <- stmt $ do
--     liftIO (putStrLn "Generating a")
--     pure $ StmtResult (ConT ''()) (Just "a") (ContTAction "ContT undefined 1")
--   b <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating b")
--     pure $ StmtResult (ConT ''()) (Just "b") (IOAction ("IO 2 + " <> a'))
--   _ <- stmt $ do
--     b' <- use b
--     liftIO (putStrLn "Generating unnamed")
--     pure $ StmtResult (ConT ''()) Nothing (IOAction ("IO 9 + " <> b'))
--   _ <- stmt $ do
--     a' <- use a
--     liftIO (putStrLn "Generating c")
--     pure $ StmtResult (ConT ''()) (Just "c") (Pure DoNotInline ("3 + " <> a'))
--   pure ()
