{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, DerivingStrategies, FlexibleContexts, KindSignatures, LambdaCase, PatternSynonyms, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskellQuotes, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module AutoApply
  ( autoapply
  , autoapplyDecs
  ) where

import           Control.Applicative
import           Control.Arrow                  ( (>>>) )
import           Control.Monad
import           Control.Monad.Logic            ( LogicT
                                                , observeManyT
                                                )
import           Control.Monad.Trans           as T
import           Control.Monad.Trans.Except
import           Control.Unification
import           Control.Unification.IntVar
import           Control.Unification.Types
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Fixedpoint
import           Data.Maybe
import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Desugar
import           Prelude                 hiding ( pred )

-- | @autoapply argsSubsuming argsUnifying fun@ creates an expression which is
-- equal to @fun@ applied to as many of the values in @argsSubsuming@ and
-- @argsUnifying@ as possible.
--
-- The types of first list of args must subsume the type of the argument
-- they're passed to. The types of the second list must merely unify.
autoapply
  :: [Name]
  -- ^ Values which will be used if their type subsumes the argument type
  -> [Name]
  -- ^ Values which will be used if their type unifies with the argument type
  -> Name
  -- ^ A function to apply to some values
  -> Q Exp
autoapply subsuming unifying fun = do
  unifyingInfos <- for unifying $ fmap (uncurry (Given Unifying)) . reifyVal
    "Argument"
  subsumingInfos <- for subsuming $ fmap (uncurry (Given Subsuming)) . reifyVal
    "Argument"
  funInfo <- uncurry Function <$> reifyVal "Function" fun
  autoapply1 (unifyingInfos <> subsumingInfos) funInfo

-- | @autoapplyDecs mkName argsSubsuming argsUnifying funs@ will wrap every
-- function in @funs@ by applying it to as many of the values in
-- @argsSubsuming@ and @argsUnifying@ as possible. The new function name will
-- be @mkName@ applied to the wrapped function name.
--
-- The types of first list of args must subsume the type of the argument
-- they're passed to. The types of the second list must merely unify.
--
-- Type signatures are not generated, so you may want to add these yourself or
-- turn on @NoMonomorphismRestriction@ if you have polymorphic constraints.
autoapplyDecs
  :: (String -> String)
  -- ^ A function to generate a new name for the wrapping function
  -> [Name]
  -- ^ A list of values which will be passed to any arguments their type subsumes
  -> [Name]
  -- ^ A list of values which will be passed to any arguments their type unify with
  -> [Name]
  -- ^ A list of function to wrap with the above parameters
  -> Q [Dec]
autoapplyDecs getNewName subsuming unifying funs = do
  unifyingInfos <- for unifying $ fmap (uncurry (Given Unifying)) . reifyVal
    "Argument"
  subsumingInfos <- for subsuming $ fmap (uncurry (Given Subsuming)) . reifyVal
    "Argument"
  funInfos <- for funs $ fmap (uncurry Function) . reifyVal "Function"
  let mkFun fun = do
        exp' <- autoapply1 (unifyingInfos <> subsumingInfos) fun
        pure $ FunD (mkName . getNewName . nameBase . fName $ fun)
                    [Clause [] (NormalB exp') []]
  traverse mkFun funInfos

-- | A given is something we can try to pass as an argument
data Given = Given
  { gUnificationType :: UnificationType
  , gName            :: Name
  , gType            :: DType
  }
  deriving Show

data UnificationType = Unifying | Subsuming
  deriving Show

-- | A function we are wrapping
data Function = Function
  { fName :: Name
  , fType :: DType
  }
  deriving (Show)

autoapply1 :: [Given] -> Function -> Q Exp
autoapply1 givens fun = do
  -- In this function we:
  --
  -- - Instantiate the command type with new unification variables
  -- - Split it into arguments and return type
  -- - Try to unify or subsume it with every 'Given' at every argument
  --   - If we can unify the monad of the 'Given' with that of the functions and
  --     unify the argument type, use that.
  --   - If nothing matches we just use an 'Argument'
  -- - Take the first result of all these tries

  let
    (fmap varBndrName -> cmdVarNames, preds, args, ret) = unravel (fType fun)
    defaultMaybe m = (Just <$> m) <|> pure Nothing
    liftQ :: Q a -> IntBindingT TypeF (LogicT Q) a
    liftQ = T.lift . T.lift
    errorToLogic go = runExceptT go >>= \case
      Left  (_ :: UFailure TypeF IntVar) -> empty
      Right x                            -> pure x
    -- Quant will invent new variable names for any unification variables
    -- still free
    quant t = do
      vs <- getFreeVars t
      for_ vs $ \v -> bindVar v . (UTerm . VarF) =<< liftQ (newName "a")


    -- Use LogicT so we can backtrack on failure
    genProvs :: LogicT Q [ArgProvenance]
    genProvs = evalIntBindingT $ do
      cmdVars  <- sequence [ (n, ) <$> freeVar | n <- cmdVarNames ]
      instArgs <- traverse
        (fmap (instWithVars cmdVars . snd) . liftQ . typeDtoF)
        args

      cmdM       <- UVar <$> freeVar
      retInst    <- fmap (instWithVars cmdVars . snd) . liftQ . typeDtoF $ ret

      -- A list of
      -- ( type to unify
      -- , predicate to use this match
      -- , the given providing the value
      -- )
      --
      -- The predicate is there to make sure we only match unifiable monads
      instGivens <- fmap concat . for givens $ \g@Given {..} -> do
        -- The Given applied as is
        nonApp <- do
          instTy <- uncurry inst <=< liftQ . typeDtoF $ gType
          v      <- liftQ $ newName "g"
          pure (instTy, pure (), BoundPure v g)
        -- The given, but in an applicative context, only possible if we can
        -- unify the monad and there is a Monad instance
        app <- case stripForall gType of
          (vars, DAppT m a) ->
            liftQ (isInstance ''Applicative [sweeten m]) >>= \case
              False -> pure Nothing
              True  -> do
                m' <- inst vars . snd <=< liftQ . typeDtoF $ m
                a' <- inst vars . snd <=< liftQ . typeDtoF $ a
                v  <- liftQ $ newName "g"
                let predicate = do
                      _ <- unify m' cmdM
                      pure ()
                pure $ Just (a', predicate, Bound v g)
          _ -> pure Nothing
        pure ([nonApp] <> toList app)

      as <- for instArgs $ \argTy ->
        defaultMaybe . asum $ instGivens <&> \(givenTy, predicate, g) -> do
          errorToLogic $ do
            predicate
            freshGivenTy <- freshen givenTy
            let u = case g of
                  Bound     _ Given {..} -> gUnificationType
                  BoundPure _ Given {..} -> gUnificationType
                  Argument  _ _          -> Unifying
            case u of
              Unifying  -> void $ unify freshGivenTy argTy
              Subsuming -> do
                s <- subsumes freshGivenTy argTy
                lift $ guard s
          pure g

      -- If we used any monadic bindings, we must have a Monad instance for
      -- the return variable. If it's polymorphic then assume an instance.
      when (any isMonadicBind (catMaybes as)) $ do
        a    <- UVar <$> freeVar
        ret' <- errorToLogic $ unify retInst (UTerm (AppF cmdM a))
        quant ret'
        retFrozen <- freeze <$> errorToLogic (applyBindings ret')
        case retFrozen of
          Just (Fix (AppF m _)) -> do
            let typeD = typeFtoD m
            liftQ (isInstance ''Applicative [sweeten typeD]) >>= \case
              False -> empty
              True  -> pure ()
          Nothing ->
            liftQ
              $ fail
                  "\"impossible\", return type didn't freeze while checking monadic bindings"
          _ -> empty

      -- Guard on all the instances being satisfiable
      --
      -- This must come after the Monadic binding checker so that the (possibly
      -- new) return type has been constrained a little.
      for_ preds $ \pred -> do

        -- Get the constraint with the correct unification variables
        instPred <- fmap (instWithVars cmdVars . snd) . liftQ . typeDtoF $ pred

        -- Quantify over any still free
        quant instPred

        -- Freeze it
        instFrozen <- freeze <$> errorToLogic (applyBindings instPred)

        case instFrozen of
          Just f -> do
            let (class', predArgs) = unfoldDType (typeFtoD f)
                typeArgs           = [ a | DTANormal a <- predArgs ]
            className <- case class' of
              DConT n -> pure n
              _ -> liftQ $ fail "unfolded predicate didn't begin with a ConT"

            -- Ignore when the name is a type family because of
            -- https://gitlab.haskell.org/ghc/ghc/issues/18153
            liftQ (reifyWithWarning className) >>= \case
              ClassI _ _ ->
                liftQ (isInstance className (sweeten <$> typeArgs)) >>= \case
                  False -> empty
                  True  -> pure ()
              FamilyI _ _ -> pure ()
              _ -> liftQ $ fail "Predicate name isn't a class or a type family"
          Nothing ->
            liftQ
              $ fail
                  "\"impossible\": predicate didn't freeze while checking predicates"


      for (zip args as) $ \case
        (_, Just p ) -> pure p
        (t, Nothing) -> (`Argument` t) <$> liftQ (newName "a")

  argProvenances <-
    note
      "\"Impossible\" Finding argument provenances failed (unless the function context containts a class with no instances)"
    .   listToMaybe
    =<< observeManyT 1 genProvs
  unless (length argProvenances == length args) $ fail
    "\"Impossible\", incorrect number of argument provenances were found"

  let bindGiven = \case
        BoundPure _ _ -> Nothing
        Bound     n g -> Just $ BindS (VarP n) (VarE (gName g))
        Argument  _ _ -> Nothing
      bs   = catMaybes (bindGiven <$> argProvenances)
      ret' = applyDExp
        (DVarE (fName fun))
        (argProvenances <&> \case
          Bound     n _             -> DVarE n
          BoundPure _ (Given _ n _) -> DVarE n
          Argument  n _             -> DVarE n
        )
  exp' <- dsDoStmts Nothing (bs <> [NoBindS (sweeten ret')])

  -- Typing the arguments here is important, if we don't then some skolems
  -- might escape!
  --
  -- Consider wrapping @f :: (forall a. a) -> ()@ (and supplying no arguments).
  -- We end up with the splice @myF x = f x@, and the @a@ in the argument to
  -- @f@ escapes. We can fix this by typing the pattern explicitly, thusly @myF
  -- (x :: forall a. a) = f x@
  pure $ LamE [ SigP (VarP n) (sweeten t) | Argument n t <- argProvenances ]
              (sweeten exp')

data ArgProvenance
  = Bound Name Given
    -- ^ Comes from a monadic binding
  | BoundPure Name Given
    -- ^ Comes from a pure binding, i.e. let ... in
  | Argument Name DType
    -- ^ Comes from an argument to the wrapped function
  deriving (Show)

isMonadicBind :: ArgProvenance -> Bool
isMonadicBind = \case
  Bound _ _ -> True
  _         -> False

----------------------------------------------------------------
-- Haskell types as a fixed point of TypeF
----------------------------------------------------------------

data TypeF a
  = AppF a a
  | VarF Name
  | ConF Name
  | ArrowF
  | LitF TyLit
  deriving (Show, Functor, Foldable, Traversable)

-- TODO: Derive this with generics
instance Unifiable TypeF where
  zipMatch (AppF l1 r1) (AppF l2 r2) =
    Just (AppF (Right (l1, l2)) (Right (r1, r2)))
  zipMatch (VarF n1) (VarF n2) | n1 == n2 = Just (VarF n1)
  zipMatch (ConF n1) (ConF n2) | n1 == n2 = Just (ConF n1)
  zipMatch ArrowF ArrowF                  = Just ArrowF
  zipMatch (LitF l1) (LitF l2) | l1 == l2 = Just (LitF l1)
  zipMatch _ _                            = Nothing

-- | Returns the type as a @Fix TypeF@ along with any quantified names. Drops
-- any context.
typeDtoF :: MonadFail m => DType -> m ([Name], Fix TypeF)
typeDtoF = traverse go . stripForall
 where
  go = \case
    DForallT{}      -> fail "TODO: Higher ranked types"
    DConstrainedT{} -> fail "TODO: Higher ranked types"
    DAppT l r       -> do
      l' <- go l
      r' <- go r
      pure $ Fix (AppF l' r')
    DAppKindT t _ -> go t
    DSigT     t _ -> go t
    DVarT n       -> pure . Fix $ VarF n
    DConT n       -> pure . Fix $ ConF n
    DArrowT       -> pure . Fix $ ArrowF
    DLitT l       -> pure . Fix $ LitF l
    DWildCardT    -> fail "TODO: Wildcards"

typeFtoD :: Fix TypeF -> DType
typeFtoD = unFix >>> \case
  AppF l r -> DAppT (typeFtoD l) (typeFtoD r)
  VarF n   -> DVarT n
  ConF n   -> DConT n
  ArrowF   -> DArrowT
  LitF l   -> DLitT l

varBndrName :: DTyVarBndrUnit -> Name
varBndrName = \case
  DPlainTV n ()   -> n
  DKindedTV n () _ -> n

-- | Raise foralls on the spine of the function type to the top
--
-- For example @forall a. a -> forall b. b@ becomes @forall a b. a -> b@
raiseForalls :: DType -> DType
raiseForalls = go >>> \case
  (vs, ctx, t) -> DForallT (DForallVis vs) . DConstrainedT ctx $ t
 where
  go = \case
    DForallT vs t -> let (vs', ctx', t') = go t in (telescopeBndrs vs <> vs', ctx', t')
    DConstrainedT ctx t ->
      let (vs', ctx', t') = go t in (vs', ctx <> ctx', t')
    l :~> r -> let (vs, ctx, r') = go r in (vs, ctx, l :~> r')
    t       -> ([], [], t)

pattern (:~>) :: DType -> DType -> DType
pattern l :~> r = DArrowT `DAppT` l `DAppT` r

-- | Instantiate a type with unification variables
inst
  :: BindingMonad TypeF IntVar m
  => [Name]
  -> Fix TypeF
  -> m (UTerm TypeF IntVar)
inst ns t = do
  vs <- sequence [ (n, ) <$> freeVar | n <- ns ]
  pure $ instWithVars vs t

-- | Instantiate a type with unification variables
instWithVars :: [(Name, IntVar)] -> Fix TypeF -> UTerm TypeF IntVar
instWithVars vs t =
  let go (Fix f) = case f of
        AppF l r                       -> UTerm (AppF (go l) (go r))
        VarF n | Just v <- lookup n vs -> UVar v
        VarF n                         -> UTerm (VarF n)
        ConF n                         -> UTerm (ConF n)
        ArrowF                         -> UTerm ArrowF
        LitF l                         -> UTerm (LitF l)
  in  go t

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

reifyVal :: String -> Name -> Q (Name, DType)
reifyVal d n = dsReify n >>= \case
  Just (DVarI name ty _) -> pure (name, ty)
  _                      -> fail $ d <> " " <> show n <> " isn't a value"

stripForall :: DType -> ([Name], DType)
stripForall = raiseForalls >>> \case
  DForallT vs (DConstrainedT _ ty) -> (varBndrName <$> telescopeBndrs vs, ty)
  DForallT vs ty   -> (varBndrName <$> telescopeBndrs vs, ty)
  DConstrainedT _ ty -> ([], ty)
  ty                 -> ([], ty)

telescopeBndrs :: DForallTelescope -> [DTyVarBndrUnit]
telescopeBndrs = \case
  DForallVis vs -> vs
  DForallInvis vs -> (() <$) <$> vs

unravel :: DType -> ([DTyVarBndrUnit], [DPred], [DType], DType)
unravel t =
  let (argList, ret) = unravelDType t
      go             = \case
        DFANil             -> ([], [], [])
        DFAForalls vs as -> (telescopeBndrs vs, [], []) <> go as
        DFACxt  preds as   -> ([], preds, []) <> go as
        DFAAnon a     as   -> ([], [], [a]) <> go as
  in  let (vs, preds, args) = go argList in (vs, preds, args, ret)

note :: MonadFail m => String -> Maybe a -> m a
note s = maybe (fail s) pure
