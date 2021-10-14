{-# language TemplateHaskellQuotes #-}
module Render.Command
  ( renderCommand
  , constrainStructVariables
  , addConstraints
  ) where

import           Data.List.Extra                ( nubOrd )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Text.Extra                ( upperCaseFirst )
import           Prettyprinter
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Vector.Extra              ( pattern (:<|)
                                                , pattern Empty
                                                , pattern Singleton
                                                )
import           Language.Haskell.TH.Datatype   ( quantifyType )
import           Language.Haskell.TH.Syntax     ( Pred
                                                , mkName
                                                , mkNameG_tc
                                                , nameBase
                                                , nameModule
                                                )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle
                                                , Type
                                                )

import           Control.Exception              ( throwIO )
import           Control.Monad.Trans.Cont
import           Foreign.C.Types
import           Foreign.Ptr
import qualified GHC.Ptr

import           CType                         as C
import qualified Data.Text.Extra               as T
import           Data.Traversable
import           Error
import           Haskell                       as H
import           Marshal
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.Names
import           Render.Peek
import           Render.Scheme
import           Render.SpecInfo
import           Render.State                   ( HasRenderState )
import           Render.Stmts
import           Render.Stmts.Alloc
import           Render.Stmts.Poke
import           Render.Stmts.Utils
import           Render.Type
import           Render.Utils
import           Spec.Parse

renderCommand
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     , HasRenderState r
     )
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand m@MarshaledCommand {..} = contextShow (unCName mcName) $ do
  RenderParams {..} <- input
  genRe ("command " <> unCName mcName) $ case commandOverrides mcName of
    Just o  -> o
    Nothing -> do
      renderForeignDecls mcCommand

      let siblingMap = Map.fromList
            [ (n, SiblingInfo (pretty . unCName $ n) (mpScheme p))
            | p <- V.toList mcParams
            , let n = pName . mpParam $ p
            ]
      let lookupSibling :: CName -> Maybe (SiblingInfo Parameter)
          lookupSibling = (`Map.lookup` siblingMap)


      let commandName = mkFunName mcName
      runInputConst lookupSibling
        $ if any ((isOutCount <||> isInOutCount) . mpScheme) mcParams
            then marshaledDualPurposeCommandCall commandName m
            else marshaledCommandCall commandName m

data StructVariables = StructVariables
  { negativeExtending :: [(Name, Name)]
  , positiveExtending :: [(Name, Name)]
  }
instance Monoid StructVariables where
  mempty = StructVariables mempty mempty
instance Semigroup StructVariables where
  sv1 <> sv2 = StructVariables { positiveExtending = both positiveExtending
                               , negativeExtending = both negativeExtending
                               }
    where both f = f sv1 <> f sv2
invertStructVariables :: StructVariables -> StructVariables
invertStructVariables (StructVariables nE pE) = StructVariables pE nE

structVariables
  :: (HasErr r, HasRenderedNames r) => Type -> Sem r StructVariables
  -- ^ negative (struct, chain var), positive (struct, chain var)
structVariables = \case
  ArrowT :@ l :@ r -> do
    sl <- structVariables l
    sr <- structVariables r
    pure (invertStructVariables sl <> sr)
  InfixT _ i t | i == typeName (TyConName ":::") -> structVariables t
  ConT n :@ VarT v ->
    getRenderedStruct (TyConName (T.pack (nameBase n))) <&> \case
      Just s | not (V.null (sExtendedBy s)) ->
        mempty { positiveExtending = [(n, v)] }
      _ -> mempty
  ConT n :@ _ | n == ''Ptr -> pure mempty
  f :@ x                  -> liftA2 (<>) (structVariables f) (structVariables x)
  ConT   _                -> pure mempty
  VarT   _                -> pure mempty
  TupleT _                -> pure mempty
  t -> throw $ "Unhandled Type constructor in structVariables: " <> show t

----------------------------------------------------------------
-- Calling this command
----------------------------------------------------------------

marshaledCommandCall
  :: ( HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     , HasRenderState r
     )
  => HName
  -> MarshaledCommand
  -> Sem r ()
marshaledCommandCall commandName m@MarshaledCommand {..} = do
  RenderParams {..} <- input

  rhs               <- commandRHS m

  ----------------------------------------------------------------
  -- The type of this command
  ----------------------------------------------------------------
  let paramName = pName . mpParam
      isArg p = case mpScheme p of
        ElidedLength{}    -> Nothing
        ElidedUnivalued _ -> Nothing
        ElidedVoid        -> Nothing
        Returned _        -> Nothing
        _                 -> Just p
      paramNaughtyNames = toList (paramName <$> V.mapMaybe isArg mcParams)
      paramNiceNames    = pretty . mkParamName <$> paramNaughtyNames
  cht    <- commandHaskellType True m
  chtDoc <- renderCommandHaskellType cht

  if not (cCanBlock mcCommand)
    then do
      tellExport (ETerm commandName)
      tellDocWithHaddock $ \getDoc -> vsep
        [ getDoc (TopLevel (cName mcCommand))
        , pretty commandName
          <+> indent 0 ("::" <> renderWithComments getDoc chtDoc)
        , pretty commandName <+> sep paramNiceNames <+> "=" <+> rhs
        ]
    else do
      ffiTy <- cToHsTypeWrapped
        DoLower
        (Proto
          (cReturnType mcCommand)
          [ (Nothing, pType)
          | Parameter {..} <- toList (cParameters mcCommand)
          ]
        )

      let safeOrUnsafeName = pretty commandName <> "SafeOrUnsafe"
          commandNameSafe  = TermName (unName commandName <> "Safe")
          dynName          = pretty $ getDynName mcCommand
          dynNameSafe      = getDynName mcCommand <> "Safe"
          dynNameUnsafe    = getDynName mcCommand <> "Unsafe"
          dynamicBindType  = ConT ''FunPtr :@ ffiTy ~> ffiTy
          chtSafeOrUnsafe  = CommandHaskellType
            { chtVars        = chtVars cht
            , chtConstraints = chtConstraints cht
            , chtArgs = V.singleton (Nothing, dynamicBindType) <> chtArgs cht
            , chtResult      = chtResult cht
            }
      chtSafeOrUnsafeDoc <- renderCommandHaskellType chtSafeOrUnsafe
      tellExport (ETerm commandName)
      tellExport (ETerm commandNameSafe)

      tellDocWithHaddock $ \getDoc -> vsep
        [ comment $ unName commandName <> " with selectable safeness"
        , safeOrUnsafeName
          <+> indent 0 ("::" <> renderWithComments getDoc chtSafeOrUnsafeDoc)
        , safeOrUnsafeName <+> dynName <+> sep paramNiceNames <+> "=" <+> rhs
        ]

      tellDocWithHaddock $ \getDoc -> vsep
        [ getDoc (TopLevel (cName mcCommand))
        , pretty commandName
          <+> indent 0 ("::" <> renderWithComments getDoc chtDoc)
        , pretty commandName
        <+> "="
        <+> safeOrUnsafeName
        <+> pretty dynNameUnsafe
        ]

      tellDocWithHaddock $ \getDoc -> vsep
        [ comment
        $  "A variant of '"
        <> unName commandName
        <> "' which makes a *safe* FFI call"
        , pretty commandNameSafe
          <+> indent 0 ("::" <> renderWithComments getDoc chtDoc)
        , pretty commandNameSafe
        <+> "="
        <+> safeOrUnsafeName
        <+> pretty dynNameSafe
        ]

--
-- The RHS text of the command
--
commandRHS
  :: ( HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     , HasRenderState r
     )
  => MarshaledCommand
  -> Sem r (Doc ())
commandRHS MarshaledCommand {..} | mcName == "xrEnumerateSwapchainImages" =
  pure "error \"todo\""
commandRHS m@MarshaledCommand {..} = context "commandRHS" $ do
  RenderParams {..} <- input

  let badNames = mempty
  stmtsDoc <- renderStmts badNames $ do
    -- Load the function from the pointer
    funRef    <- getCCall mcCommand

    -- Generate references to the parameters to use later
    paramRefs <- forV mcParams $ \MarshaledParam {..} -> if isElided mpScheme
      then pure Nothing
      else Just <$> do
        -- TODO: Don't calculate these twice, use the types from earlier
        -- (rendering the type sig)
        (_, _, ty) <- runRenderTypeContext
          $ schemeTypeNegativeWithContext mpScheme
        valueRef <-
          stmt ty (Just . unName . mkParamName . pName $ mpParam)
          . pure
          . Pure AlwaysInline
          . ValueDoc
          . pretty
          . mkParamName
          $ pName mpParam
        nameRef (unCName $ pName mpParam) valueRef
        pure valueRef

    -- poke all the parameters
    (pokeRefs, peekRefs) <-
      context "get pokes and peeks"
      $   V.unzip
      <$> V.zipWithM getPoke paramRefs mcParams

    -- Bind the result to _ if it can only return success
    includeReturnType <- marshaledCommandShouldIncludeReturnValue m
    let useEmptyBinder =
          not includeReturnType
            && null (cErrorCodes mcCommand)
            && cReturnType mcCommand
            /= Void

    -- Run the command and capture the result
    let name = bool "r" "_" useEmptyBinder
    rTy        <- cToHsType DoLower (cReturnType mcCommand)
    wrappedRef <- stmt (Just rTy) (Just name) $ do
      FunDoc fun <- use funRef
      pokes      <- traverseV use pokeRefs
      tellImport
        (mkName (T.unpack modulePrefix <> ".Internal.Utils.traceAroundEvent"))
      let traceName :: Text
          traceName = unCName mcName
      -- call the command
      pure
        .   IOAction
        .   ValueDoc
        $   "traceAroundEvent"
        <+> viaShow traceName
        <+> parens (sep (fun : (unValueDoc <$> toList pokes)))
    retRef        <- unwrapIdiomaticType (Just name) wrappedRef

    -- check the result
    checkedResult <- checkResultMaybe mcCommand retRef

    unitStmt $ do
      after checkedResult
      let peeks = catMaybes (toList peekRefs)
      rets <- traverse use $ if includeReturnType then retRef : peeks else peeks
      pure . Pure NeverInline $ tupled @() (unValueDoc <$> rets)

  case stmtsDoc of
    IOStmts d -> do
      tellImport 'liftIO
      pure $ "liftIO $" <+> d
    ContTStmts d -> do
      tellImport 'evalContT
      tellImport 'liftIO
      pure $ "liftIO . evalContT $" <+> d

----------------------------------------------------------------
-- Checking the result and throwing an exception if something went wrong
----------------------------------------------------------------

-- | Check the result if it has one
checkResultMaybe
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSiblingInfo Parameter r)
  => Command
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
checkResultMaybe Command {..} retRef = do
  RenderParams {..} <- input
  if null cErrorCodes || cReturnType /= successCodeType
    then pure retRef
    else checkResult retRef

checkResult
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSiblingInfo Parameter r)
  => Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
checkResult retRef = do
  check <- unitStmt $ do
    RenderParams {..} <- input
    ValueDoc ret      <- use retRef
    tellImport 'when
    tellImport 'throwIO
    let pat = mkPatternName firstSuccessCode
    tellImport pat
    tellImportWithAll exceptionTypeName
    pure
      .   IOAction
      .   UnitDoc
      $   "when"
      <+> parens (ret <+> "<" <+> pretty pat)
      <+> parens ("throwIO" <+> parens (pretty exceptionTypeName <+> ret))
  stmt Nothing Nothing $ do
    after check
    r <- use retRef
    pure . Pure AlwaysInline $ r

----------------------------------------------------------------
-- Dual purpose calls
--
-- Sometimes a command returning a vector will allow one to pass in nullPtr for
-- the vector and allocate some memory for the number of elements. This command
-- will intead of filling the vector return the number of potential elements in
-- the allocated count memory. It can then be called again with enough space
-- allocated for the return vector.
--
-- We assume the user will always want to read every element, so we:
--
-- - Poke all unrelated parameters
-- - allocate space for the count
-- - call the function with these parameters and nullPtr for the array
-- - Peek the number of elements
-- - allocate space for these
-- - call the function again with the same parameters and now with space for
--   the return array
-- - Return the array
--
-- The OpenXR style for this is a little different, the input and output length
-- parameters are separate, so we:
--
-- - Poke all unrelated parameters
-- - allocate space for the output count
-- - call the function with these parameters and nullPtr for the array and zero
--   for the input length
-- - Peek the number of elements from the output count
-- - allocate space for these
-- - call the function again with the same parameters and now with space for
--   the return array
-- - Return the array, with the length from the output count
--
----------------------------------------------------------------

marshaledDualPurposeCommandCall
  :: ( HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     , HasRenderState r
     )
  => HName
  -> MarshaledCommand
  -> Sem r ()
marshaledDualPurposeCommandCall commandName m@MarshaledCommand {..} = do
  RenderParams {..} <- input
  tellExport (ETerm commandName)

  --
  -- Find the parameters we want to fiddle with
  --
  countParamSets    <- findCountParameters m

  --
  -- Get the type of this function
  --
  includeReturnType <- marshaledCommandShouldIncludeReturnValue m
  let includeInOutCountTypes = False
  cht    <- commandHaskellType includeInOutCountTypes m
  chtDoc <- renderCommandHaskellType cht

  --
  -- Get var names for the LHS
  --
  let paramName = pName . mpParam
      isArg p = case mpScheme p of
        ElidedLength{}    -> Nothing
        ElidedUnivalued _ -> Nothing
        ElidedVoid        -> Nothing
        Returned   _      -> Nothing
        InOutCount _      -> Nothing
        OutCount   _      -> Nothing
        _                 -> Just p
      paramNaughtyNames = toList (paramName <$> V.mapMaybe isArg mcParams)
      paramNiceNames    = pretty . mkParamName <$> paramNaughtyNames


  let badNames = mempty
  stmtsDoc <- renderStmts badNames $ do

    --
    -- Load the function from the pointer
    --
    funRef <- getCCall mcCommand

    --
    -- Run once to get the length
    --
    (getLengthPokes, getLengthPeeks, countAddrsAndPeeks) <- pokesForGettingCount
      mcParams
      countParamSets
    let addrs = (\(_, b, _) -> b) <$> countAddrsAndPeeks
    ret1 <- runWithPokes False m funRef getLengthPokes

    for_ countAddrsAndPeeks $ \(n, _, peek) -> do
      r <- stmt Nothing Nothing $ do
        after ret1
        ValueDoc v <- use peek
        pure . Pure AlwaysInline . ValueDoc $ v
      nameRef (unCName n) r

    (getVectorsPokes, getVectorsPeeks) <- pokesForGettingResults
      getLengthPokes
      getLengthPeeks
      (V.zip addrs countParamSets)

    ret2 <- runWithPokes includeReturnType m funRef getVectorsPokes

    for_ (V.zip addrs countParamSets) $ \(countAddr, p) -> do
      let outCountParam = case p of
            SameIndex (IndexedParam _ p) _          -> p
            DifferentIndices _ (IndexedParam _ p) _ -> p
      finalCountRef <- stmt Nothing Nothing $ do
        after ret2
        countScheme <- case mpScheme outCountParam of
          InOutCount s -> pure s
          OutCount   s -> pure s
          _            -> throw "Trying to peek count without inout scheme"
        peekCount <-
          note "Unable to get peek for count"
            =<< peekStmtDirect (mpParam outCountParam) countAddr countScheme
        Pure InlineOnce <$> use peekCount
       -- TODO: disgusting, this stringly typed stuff
      nameRef (unCName (pName (mpParam outCountParam)) <> "::2") finalCountRef

    unitStmt $ do
      after ret2
      let peeks = catMaybes (toList getVectorsPeeks)
      rets <- traverse use $ if includeReturnType then ret2 : peeks else peeks
      pure . Pure NeverInline $ tupled @() (unValueDoc <$> rets)

  rhs <- case stmtsDoc of
    IOStmts d -> do
      tellImport 'liftIO
      pure $ "liftIO $" <+> d
    ContTStmts d -> do
      tellImport 'evalContT
      tellImport 'liftIO
      pure $ "liftIO . evalContT $" <+> d

  tellDocWithHaddock $ \getDoc -> vsep
    [ getDoc (TopLevel (cName mcCommand))
    , pretty commandName <+> indent 0 ("::" <> renderWithComments getDoc chtDoc)
    , pretty commandName <+> sep paramNiceNames <+> "=" <+> rhs
    ]

data CountParameter a
  = SameIndex a (Vector a)
    -- ^ The count parameter and the vectors
  | DifferentIndices a a (Vector a)
    -- ^ The InCapacity, OutCount and the vectors
  deriving (Show, Functor)

data IndexedParam = IndexedParam
  { indexedParamIndex  :: Int
  , _indexedParamParam :: MarshaledParam
  }
  deriving Show

-- | Returns the indices of the count parameters
findCountParameters
  :: forall r
   . HasErr r
  => MarshaledCommand
  -> Sem r (Vector (CountParameter IndexedParam))
findCountParameters MarshaledCommand {..} = liftA2 (<>)
                                                   inoutParams
                                                   separateParams
 where
  getParam :: Int -> IndexedParam
  getParam i = IndexedParam i (mcParams V.! i)

  inoutParams =
    let countIndices = V.findIndices (isInOutCount . mpScheme) mcParams
    in  for countIndices $ \i ->
          let s@(IndexedParam _ c) = getParam i
              isCounted p = case pLengths (mpParam p) of
                NamedLength nl :<| _ | nl == (pName . mpParam $ c) -> True
                _ -> False
          in  pure $ SameIndex s (getParam <$> V.findIndices isCounted mcParams)

  separateParams =
    let outCountIndices = V.findIndices (isOutCount . mpScheme) mcParams
    in
      for outCountIndices $ \i -> do
        let out@(IndexedParam _ outParam) = getParam i
        baseName <-
          note "output count parameter name didn't end in \"CountOutput\""
          . T.dropSuffix "CountOutput"
          . unCName
          . pName
          . mpParam
          $ outParam
        let inName    = baseName <> "CapacityInput"
            isInCount = (== inName) . unCName . pName . mpParam
        in'@(IndexedParam _ inCountParam) <-
          case V.findIndices isInCount mcParams of
            Empty ->
              throw
                $  "Couldn't find input capacity parameter sibling for "
                <> show baseName
            Singleton inCount -> pure (getParam inCount)
            _ ->
              throw
                $  "Found multiple input capacity parameters for "
                <> show baseName
        let isCounted p = case pLengths (mpParam p) of
              NamedLength nl :<| _ | nl == (pName . mpParam $ inCountParam) ->
                True
              _ -> False
        pure $ DifferentIndices
          in'
          out
          (getParam <$> V.findIndices isCounted mcParams)

pokesForGettingCount
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     , HasRenderState r
     )
  => Vector MarshaledParam
  -> Vector (CountParameter IndexedParam)
  -> Stmt
       s
       r
       ( Vector (Ref s ValueDoc)
       , Vector (Maybe (Ref s ValueDoc))
       , Vector (CName, Ref s AddrDoc, Ref s ValueDoc)
       )
  -- ^ (Get length pokes, get length peeks, count names, addresses and peeks (one per
  -- count parameter set))
pokesForGettingCount params countSets = do
  let sameIndexCounts :: Vector IndexedParam
      sameIndexCounts = flip V.mapMaybe countSets $ \case
        SameIndex i _      -> Just i
        DifferentIndices{} -> Nothing
      differentIndicesCounts :: Vector (IndexedParam, IndexedParam)
      differentIndicesCounts = flip V.mapMaybe countSets $ \case
        SameIndex _ _              -> Nothing
        DifferentIndices in' out _ -> Just (in', out)
      vecIndices :: Vector IndexedParam
      vecIndices = flip V.concatMap countSets $ \case
        SameIndex _ vs          -> vs
        DifferentIndices _ _ vs -> vs

  -- Replace the inout count with an 'allocateAndPeek' which will calloc the
  -- count and peek it back.
  sameIndexCountOverrides <- forV sameIndexCounts $ \(IndexedParam i p) -> if
    | InOutCount s <- mpScheme p -> do
      (addrRef, peek) <- allocateAndPeek (mpParam p) s
      pure (i, (pName (mpParam p), addrRef, peek))
    | otherwise -> throw "Count doesn't have inout type"

  -- Replace the out count with an 'allocateAndPeek' like above, and replace
  -- the in count with 0
  differentIndicesCountOverrides <-
    fmap (V.concat . V.toList)
    . forV differentIndicesCounts
    $ \(IndexedParam inIndex inParam, IndexedParam outIndex outParam) -> if
        | OutCount s <- mpScheme outParam -> do
          (addrRef, peek) <- allocateAndPeek (mpParam outParam) s
          pure $ V.fromList
            [ (outIndex, Left (pName (mpParam outParam), addrRef, peek))
            , (inIndex , Right inParam { mpScheme = ElidedUnivalued "0" })
            ]
        | otherwise -> throw
          "Different indexed count doesn't have OutCount type"

  -- Replace the vectors with nullPtr for this call
  vecOverrides <- forV vecIndices $ \(IndexedParam i p) ->
    if
      | Returned _ <- mpScheme p -> pure
        (i, Right p { mpScheme = ElidedUnivalued "nullPtr" })
      | otherwise -> throw $ "Vector doesn't have Returned type, has " <> show
        (mpScheme p)

  let lastTwo (_, b, c) = (b, c)
      gettingCountParams = (Right <$> params) V.// V.toList
        (  (second (Left . lastTwo) <$> sameIndexCountOverrides)
        <> (second (first lastTwo) <$> differentIndicesCountOverrides)
        <> vecOverrides
        )

  pokes <- forV (V.indexed gettingCountParams) $ \case
    (_, Left (addrRef, peek)) -> do
      poke <- castRef addrRef
      pure (poke, Just peek)
    (i, Right m@MarshaledParam {..}) -> if isElided mpScheme
      then getPoke Nothing m
      else do
        -- Don't name the input count ref, because we want the count read after
        -- the second call to have this name.
        ref <-
          if i
               `elem` (   indexedParamIndex
                      <$> sameIndexCounts
                      <>  (fst <$> differentIndicesCounts)
                      )
            then paramRefUnnamed m
            else paramRef m
        getPoke (Just ref) m

  pure
    ( fst <$> pokes
    , snd <$> pokes
    , (snd <$> sameIndexCountOverrides)
      <> V.mapMaybe (leftToMaybe . snd) differentIndicesCountOverrides
    )

pokesForGettingResults
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasStmts r
     , HasSiblingInfo Parameter r
     )
  => Vector (Ref s ValueDoc)
  -- ^ The pokes we used for the previous call
  -> Vector (Maybe (Ref s ValueDoc))
  -- ^ The peeks we used for the previous call
  -> Vector (Ref s AddrDoc, CountParameter IndexedParam)
  -- ^ The arrays to get returned (as well as the address of the out count)
  -> Stmt
       s
       r
       (Vector (Ref s ValueDoc), Vector (Maybe (Ref s ValueDoc)))
  -- ^ Pokes for this call, peeks for this call
pokesForGettingResults oldPokes oldPeeks countParamSets = do
  let outCountIndices = countParamSets <&> \(_, countParam) ->
        case countParam of
          SameIndex (IndexedParam i _) _          -> i
          DifferentIndices _ (IndexedParam i _) _ -> i

  inCountOverrides <- for countParamSets $ \(addr, countParam) ->
    case countParam of
      -- We can use the address directly
      SameIndex (IndexedParam i _) _ -> (i, ) <$> castRef addr
      -- We have to read the out param from the previous fetch,
      -- use it via its ref here so we don't generate duplicate peek stmts
      DifferentIndices (IndexedParam i _) (IndexedParam _ outParam) _ -> do
        v <- pureStmt =<< useViaName (unCName (pName (mpParam outParam)))
        pure (i, v)

  (vecPokeOverrides, vecPeekOverrides) <-
    fmap (V.unzip . V.concat . V.toList) . for countParamSets $ \(_, c) ->
      let (vs, nameModifier) = case c of
            SameIndex _ vs -> (vs, id)
            DifferentIndices _ (IndexedParam _ outParam) vs ->
              (vs, const . pName . mpParam $ outParam)
      in
        for vs $ \(IndexedParam i MarshaledParam {..}) -> do
          let lowered = lowerParamType mpParam
          let allocated = lowered
                { pLengths = case pLengths lowered of
                  NamedLength n :<| t ->
                    NamedLength (CName (unCName (nameModifier n))) :<| t
                  t -> t
                }
              usingSecondLen = lowered
                { pLengths = case pLengths lowered of
                               NamedLength n :<| t ->
                                 -- YUCK
                                 NamedLength
                                     (CName (unCName (nameModifier n) <> "::2"))
                                   :<| t
                               t -> t
                }
          scheme <- case mpScheme of
            Returned s -> pure s
            _          -> throw "Vector without a return scheme"
          addr                        <- allocate allocated scheme
          addrWithForgottenExtensions <- forgetStructExtensions (pType mpParam)
            =<< castRef addr
          peek <-
            note "Unable to get peek for returned value"
              =<< peekStmtDirect usingSecondLen addr scheme
          pure ((i, addrWithForgottenExtensions), (i, Just peek))

  let newPokes = oldPokes V.// toList (inCountOverrides <> vecPokeOverrides)
      newPeeks = oldPeeks
        V.// toList (((, Nothing) <$> outCountIndices) <> vecPeekOverrides)

  pure (newPokes, newPeeks)


runWithPokes
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSiblingInfo Parameter r)
  => Bool
  -> MarshaledCommand
  -> Ref s FunDoc
  -> Vector (Ref s ValueDoc)
  -> Stmt s r (Ref s ValueDoc)
runWithPokes includeReturnType MarshaledCommand {..} funRef pokes = do
  RenderParams {..} <- input
  -- Bind the result to _ if it can only return success
  let useEmptyBinder =
        not includeReturnType
          && null (cErrorCodes mcCommand)
          && cReturnType mcCommand
          /= Void
  retRef <- stmt Nothing (Just (bool "r" "_" useEmptyBinder)) $ do
    FunDoc fun <- use funRef
    pokes      <- traverseV use pokes
    tellImport
      (mkName (T.unpack modulePrefix <> ".Internal.Utils.traceAroundEvent"))
    let traceName :: Text
        traceName = unCName mcName
    -- call the command
    pure
      .   IOAction
      .   ValueDoc
      $   "traceAroundEvent"
      <+> viaShow traceName
      <+> parens (sep (fun : (unValueDoc <$> toList pokes)))

  checkResultMaybe mcCommand retRef

----------------------------------------------------------------
--
----------------------------------------------------------------

paramRef
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => MarshaledParam
  -> Stmt s r (Ref s ValueDoc)
paramRef mp@MarshaledParam {..} = do
  valueRef <- paramRefUnnamed mp
  nameRef (unCName $ pName mpParam) valueRef
  pure valueRef

paramRefUnnamed
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => MarshaledParam
  -> Stmt s r (Ref s ValueDoc)
paramRefUnnamed MarshaledParam {..} = do
  RenderParams {..} <- input
  -- TODO: don't get the type again here, use it from earlier (generating the
  -- sig)
  (_, _, ty) <- runRenderTypeContext $ schemeTypeNegativeWithContext mpScheme
  stmt ty (Just . unName . mkParamName . pName $ mpParam)
    . pure
    . Pure AlwaysInline
    . ValueDoc
    . pretty
    . mkParamName
    $ pName mpParam

castRef
  :: forall a b r s
   . HasErr r
  => (Typeable b, Coercible a b, Coercible b (Doc ()))
  => Ref s a
  -> Stmt s r (Ref s b)
castRef ref = stmt Nothing Nothing $ do
  r <- use ref
  pure . Pure AlwaysInline . coerce $ r

----------------------------------------------------------------
-- Getting C call
----------------------------------------------------------------

getCCall
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     )
  => Command
  -> Stmt s r (Ref s FunDoc)
getCCall c = if cIsDynamic c
  then getCCallDynamic c
  else pureStmt (FunDoc (pretty (getStaticName c)))

getCCallDynamic
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     )
  => Command
  -> Stmt s r (Ref s FunDoc)
getCCallDynamic c = do
  RenderParams {..} <- input
  let -- What to do in the case that this command isn't dispatched from a handle
      noHandlePtr = stmt Nothing (Just (unCName (cName c) <> "Ptr")) $ do
        -- TODO: Change this function pointer to a "global variable" with ioref and
        -- unsafePerformIO
        let getInstanceProcAddr' =
              mkFunName (CName $ lowerPrefix <> "GetInstanceProcAddr'")
        tellImport getInstanceProcAddr'
        tellImport 'nullPtr
        tellImport 'castFunPtr
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        fTyDoc <- renderTypeHighPrec =<< cToHsTypeWrapped
          DoLower
          (C.Proto
            (cReturnType c)
            [ (Just (unCName pName), pType)
            | Parameter {..} <- V.toList (cParameters c)
            ]
          )
        pure
          .   IOAction
          $   "castFunPtr @_ @"
          <>  fTyDoc
          <+> "<$>"
          <+> pretty getInstanceProcAddr'
          <+> "nullPtr"
          <+> parens ("Ptr" <+> dquotes (pretty (unCName (cName c))) <> "#")

      -- What do do if we need to extract the command pointer from a parameter
      cmdsFunPtr ptrRecTyName getCmdsFun paramName paramType = do
        cmdsRef <- stmt Nothing (Just "cmds") $ do
          paramTDoc <- renderType =<< cToHsType DoNotPreserve paramType
          getCmds   <- getCmdsFun
          pure . Pure InlineOnce . CmdsDoc $ getCmds <+> parens
            (pretty paramName <+> "::" <+> paramTDoc)
        nameRef "cmds" cmdsRef
        stmt Nothing (Just (unCName (cName c) <> "Ptr")) $ do
          let memberName = mkFuncPointerMemberName (cName c)
          tellImportWith ptrRecTyName memberName
          CmdsDoc cmds <- use cmdsRef
          pure . Pure InlineOnce $ pretty memberName <+> cmds

  ptr <- commandHandle c >>= \case
    Nothing                            -> noHandlePtr
    Just (Parameter {..}, Handle {..}) -> do
      let
        withImport member = do
          tellImportWithAll (mkTyName hName)
          pure $ pretty (member :: Text)
        instanceHandle = cmdsFunPtr (TyConName "InstanceCmds")
                                    (withImport "instanceCmds")
                                    paramName
                                    pType
        deviceHandle = cmdsFunPtr (TyConName "DeviceCmds")
                                  (withImport "deviceCmds")
                                  paramName
                                  pType
        paramName = mkParamName pName
      case hLevel of
        NoHandleLevel -> noHandlePtr
        Device        -> deviceHandle
        Instance      -> instanceHandle

  stmt Nothing (Just (unCName (cName c) <> "'")) $ do
    after <=< unitStmt $ do
      ptrDoc <- use ptr
      tellImport 'nullFunPtr
      let err  = "The function pointer for " <> unCName (cName c) <> " is null"
          cond = parens $ ptrDoc <+> "/= nullFunPtr"
      throwErrDoc err cond
    let dynName = getDynName c
    ptrDoc <- use ptr
    pure . Pure NeverInline . FunDoc $ pretty dynName <+> ptrDoc

-- | The handle of a command is the (dispatchable handle) first parameter.
commandHandle :: HasSpecInfo r => Command -> Sem r (Maybe (Parameter, Handle))
commandHandle Command {..} = case cParameters V.!? 0 of
  Just p@Parameter {..} | TypeName t <- pType -> fmap (p, ) <$> getHandle t
  _ -> pure Nothing

renderForeignDecls
  :: forall r
   . ( HasErr r
     , HasSpecInfo r
     , HasRenderElem r
     , HasRenderParams r
     , HasRenderedNames r
     )
  => Command
  -> Sem r ()
renderForeignDecls c@Command {..} = do
  let tellFFI :: Bool -> Maybe CName -> Text -> Doc () -> Sem r ()
      tellFFI unsafe ffiName name tyDoc =
        tellDoc
          .  vsep
          $  ["foreign import ccall"]
          <> bool
               []
               ["#if !defined(SAFE_FOREIGN_CALLS)", indent 2 "unsafe", "#endif"]
               unsafe
          <> [ indent 2 $ maybe (dquotes "dynamic" <+>)
                                ((<+>) . dquotes . pretty . unCName)
                                ffiName
                                (pretty name)
             , indent 2 $ "::" <+> tyDoc
             ]

  ffiTy <- cToHsTypeWrapped
    DoLower
    (Proto cReturnType
           [ (Nothing, pType) | Parameter {..} <- toList cParameters ]
    )
  if cIsDynamic
    then do
      let dynamicBindType = ConT ''FunPtr :@ ffiTy ~> ffiTy
          dynName         = getDynName c
      dynamicBindTypeDoc <- renderType dynamicBindType
      importConstructors dynamicBindType
      if cCanBlock
        then do
          tellFFI True  Nothing (dynName <> "Unsafe") dynamicBindTypeDoc
          tellFFI False Nothing (dynName <> "Safe")   dynamicBindTypeDoc
        else tellFFI True Nothing dynName dynamicBindTypeDoc
    else do
      let staticName = getStaticName c
      staticBindTypeDoc <- renderType ffiTy
      importConstructors ffiTy
      if cCanBlock
        then do
          tellFFI True  (Just cName) (staticName <> "Unsafe") staticBindTypeDoc
          tellFFI False (Just cName) (staticName <> "Safe")   staticBindTypeDoc
        else tellFFI True (Just cName) staticName staticBindTypeDoc

----------------------------------------------------------------
-- Poking values
----------------------------------------------------------------

-- | This also takes care of allocating for returned values
getPoke
  :: ( HasErr r
     , HasRenderElem r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     , HasRenderState r
     )
  => Maybe (Ref s ValueDoc)
  -- ^ Might be nothing if this is elided
  -> MarshaledParam
  -> Stmt s r (Ref s ValueDoc, Maybe (Ref s ValueDoc))
  -- ^ (poke, peek if it's a returned value)
getPoke valueRef MarshaledParam {..} = do
  (poke, peek) <- case mpScheme of
    Returned s -> do
      (addrRef, peek) <- allocateAndPeek (lowerParamType mpParam) s
      -- TODO: implement ref casting
      addrRef'        <- stmt Nothing Nothing $ do
        AddrDoc addr <- use addrRef
        pure . Pure AlwaysInline . ValueDoc $ addr
      pure (addrRef', Just peek)
    _ -> (, Nothing) <$> case valueRef of
      Nothing -> do
        -- Give the refs names now as they didn't get one earlier
        r <- getPokeDirectElided (lowerParamType mpParam) mpScheme
        nameRef (unCName $ pName mpParam) r
        pure r
      Just valueRef -> getPokeDirect (lowerParamType mpParam) mpScheme valueRef

  -- If any extensible structs are passed to the command they are passed as
  -- `Ptr (SomeStruct StructName)`, coerce them to that type using
  -- `forgetExtensions`
  forgottenPoke <- forgetStructExtensions (pType mpParam) poke

  pure (forgottenPoke, peek)

-- | If possible, wrap a value in `forgetExtensions`
forgetStructExtensions
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
  => CType
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
forgetStructExtensions ty poke = do
  forgottenPoke <- case ty of
    Ptr _ (TypeName s) -> getStruct s >>= \case
      Just s | not (V.null (sExtendedBy s)) ->
        fmap Just . stmt Nothing Nothing $ do
          tellImport (TermName "forgetExtensions")
          ValueDoc p <- use poke
          pure . Pure InlineOnce . ValueDoc $ "forgetExtensions" <+> p
      _ -> pure Nothing
    _ -> pure Nothing
  pure $ fromMaybe poke forgottenPoke

-- | Parameters of type foo[x] are passed as pointers if x is large
lowerParamType :: Parameter -> Parameter
lowerParamType p@Parameter {..} =
  let large    = (> 16)
      largeSym = const True -- TODO: implement
  in  case pType of
        Array _ (NumericArraySize n) _ | large n     -> p
        Array _ (SymbolicArraySize n) _ | largeSym n -> p
        Array q _ elem                               -> p { pType = Ptr q elem }
        _                                            -> p

----------------------------------------------------------------
-- Command type rendering
----------------------------------------------------------------

data CommandHaskellType a = CommandHaskellType
  { chtVars        :: Vector a
  , chtConstraints :: Vector a
  , chtArgs        :: Vector (Maybe Documentee, a)
  , chtResult      :: a
  }
  deriving (Functor, Foldable, Traversable)

renderCommandHaskellType
  :: (HasRenderElem r, HasRenderParams r)
  => CommandHaskellType Type
  -> Sem r (CommandHaskellType (Doc ()))
renderCommandHaskellType CommandHaskellType {..} = do
  chtVars        <- traverse renderTypeHighPrec chtVars
  chtConstraints <- traverse renderType chtConstraints
  chtArgs        <- traverse (traverse renderTypeHighPrec) chtArgs
  chtResult      <- renderType chtResult
  pure CommandHaskellType { .. }

commandHaskellType
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
  => Bool
  -> MarshaledCommand
  -> Sem r (CommandHaskellType Type)
commandHaskellType includeInOutCountTypes m@MarshaledCommand {..} = do
  (negativeArgs, positiveArgs, (argsWithName, results)) <- runRenderTypeContext
    do
      is <- commandInputTypes includeInOutCountTypes m
      r  <- commandReturnTypes includeInOutCountTypes m
      pure (is, r)
  let chtArgs = first (Just . Nested mcName) <$> argsWithName
      chtVars =
        fromList
          $  (VarT . cVarName <$> (negativeArgs <> positiveArgs))
          <> [VarT ioVar]
      chtConstraints =
        fromList
          $  (negativeArgConstraints =<< negativeArgs)
          <> (positiveArgConstraints =<< positiveArgs)
          <> [ConT ''MonadIO :@ VarT ioVar]
      chtResult = VarT ioVar :@ foldl' (:@) (TupleT (length results)) results
  pure CommandHaskellType { .. }

positiveArgConstraints :: ConstrainedVar -> [Type]
positiveArgConstraints = \case
  Unconstrained _ -> []
  Extends s v ->
    [ ConT (mkName "Extendss") :@ s :@ VarT v
    , ConT (mkName "PokeChain") :@ VarT v
    , ConT (mkName "PeekChain") :@ VarT v
    ]
  Inherits _ v -> [ConT (mkName "FromCStruct") :@ VarT v]

negativeArgConstraints :: ConstrainedVar -> [Type]
negativeArgConstraints = \case
  Unconstrained _ -> []
  Extends s v ->
    [ ConT (mkName "Extendss") :@ s :@ VarT v
    , ConT (mkName "PokeChain") :@ VarT v
    ]
  Inherits _ v -> [ConT (mkName "ToCStruct") :@ VarT v]

commandInputTypes
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasContextState r)
  => Bool
  -> MarshaledCommand
  -> Sem r (V.Vector (CName, H.Type))
commandInputTypes includeInOutCountTypes MarshaledCommand {..} =
  let neg = if includeInOutCountTypes
        then schemeTypeNegativeWithContext
        else \case
          InOutCount _ -> pure Nothing
          OutCount   _ -> pure Nothing
          s            -> schemeTypeNegativeWithContext s
  in  V.mapMaybe id <$> traverseV (marshaledParamTypeWithName neg) mcParams

-- | Get a list of the types of values which should be in the return value of
-- this command.
commandReturnTypes
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasContextState r)
  => Bool
  -- ^ Should we return the value of 'InOutCount' parameters
  -> MarshaledCommand
  -> Sem r (V.Vector H.Type)
commandReturnTypes includeInOutCountTypes m@MarshaledCommand {..} = do
  includeReturnType <- marshaledCommandShouldIncludeReturnValue m
  let pos = \case
        InOutCount _ | not includeInOutCountTypes -> pure Nothing
        OutCount _ | not includeInOutCountTypes -> pure Nothing
        s -> schemeTypePositiveWithContext s
  pts <-
    V.mapMaybe id
      <$> traverseV (fmap (fmap snd) . marshaledParamTypeWithName pos) mcParams
  r <- case mcReturn of
    Void -> pure V.empty
    _ | not includeReturnType -> pure V.empty
    r -> V.singleton <$> cToHsTypeWithContext DoNotPreserve r
  pure (r <> pts)

-- | Render a CommandHaskellType documentation attached to the
-- parameters.
renderWithComments
  :: (Documentee -> Doc ()) -> CommandHaskellType (Doc ()) -> Doc ()
renderWithComments getDoc CommandHaskellType {..} =
  let
    withComment = \case
      (Nothing, d) -> d
      (Just n , d) -> getDoc n <> line <> d
    as = zipWith
      ($)
      (id : repeat ("->" <>))
      (indent 1 <$> (withComment <$> toList chtArgs) <> [chtResult])
    vars = if V.null chtVars
      then Nothing
      else Just ("forall" <+> hsep (toList chtVars))
    preds = if V.null chtConstraints
      then Nothing
      else Just (tupled (toList chtConstraints))
  in
    maybe mempty (\v -> indent 1 v <> line <> " .") vars
    <> maybe mempty (\p -> indent 1 p <> line <> "=>") preds
    <> vsep as

----------------------------------------------------------------
--
----------------------------------------------------------------

ioVar :: Name
ioVar = mkName "io"

-- | Any extensible structs have 'PokeChain' or 'PeekChain' constraints added
-- depending on their position polarity.
--
-- Inherited structs have the relevant inheriting class constraint
constrainStructVariables
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasRenderedNames r)
  => Type
  -> Sem r Type
constrainStructVariables t = do
  StructVariables ns ps <- structVariables t
  let both = nubOrd (ns <> ps)
  unless (null ns) $ tellImport (TyConName "PokeChain")
  unless (null ps) $ tellImport (TyConName "PeekChain")
  unless (null both) $ tellImport (TyConName "Extendss")
  pure $ addConstraints
    (  ((\(s, v) -> ConT (typeName (TyConName "Extendss")) :@ ConT s :@ VarT v)
       <$> both
       )
    <> ((ConT (typeName (TyConName "PokeChain")) :@) . VarT . snd <$> both)
    <> ((ConT (typeName (TyConName "PeekChain")) :@) . VarT . snd <$> ps)
    )
    t

-- | Add constraints to a type, folding them into an existing forall if
-- possible
addConstraints :: [Pred] -> Type -> Type
addConstraints new = quantifyType . \case
  ForallT vs ctx ty -> ForallT vs (ctx <> new) ty
  ty                -> ForallT [] new ty

----------------------------------------------------------------
-- ImportConstructors
----------------------------------------------------------------

-- | Foreign imports require constructors in scope for newtypes
--
-- TODO: This currently assumes that the newtypes are in the same module as any
-- synonyms pointing to them.
importConstructors
  :: forall r
   . (HasSpecInfo r, HasRenderElem r, HasRenderParams r, HasRenderedNames r)
  => Type
  -> Sem r ()
importConstructors t = do
  RenderParams {..} <- input
  let names = nubOrd $ allTypeNames t
      isNewtype' :: Name -> Sem r Bool
      isNewtype' n =
        pure (n `elem` (builtinNewtypes <> toList extraNewtypes))
          <||> isNewtype (TyConName . T.pack . nameBase $ n)
      setNameBase :: Name -> HName -> Name
      setNameBase n = case nameModule n of
        Nothing -> mkName . T.unpack . unName
        Just m  -> mkNameG_tc "" m . T.unpack . unName
  resolveAlias <- do
    ra <- getResolveAlias
    pure $ \n -> setNameBase n (ra . TyConName . T.pack . nameBase $ n)
  for_ names $ \n -> whenM (isNewtype' n) $ if n `elem` alwaysQualifiedNames
    then tellQualImportWithAll (resolveAlias n)
    else tellImportWithAll (resolveAlias n)

builtinNewtypes :: [Name]
builtinNewtypes =
  [ ''CChar
  , ''CSChar
  , ''CUChar
  , ''CShort
  , ''CUShort
  , ''CInt
  , ''CUInt
  , ''CLong
  , ''CULong
  , ''CPtrdiff
  , ''CSize
  , ''CWchar
  , ''CSigAtomic
  , ''CLLong
  , ''CULLong
  , ''CBool
  , ''CIntPtr
  , ''CUIntPtr
  , ''CIntMax
  , ''CUIntMax
  , ''CClock
  , ''CTime
  , ''CUSeconds
  , ''CSUSeconds
  , ''CFloat
  , ''CDouble
  ]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

getDynName :: Command -> Text
getDynName = ("mk" <>) . upperCaseFirst . unCName . cName

getStaticName :: Command -> Text
getStaticName = ("ffi" <>) . upperCaseFirst . unCName . cName
