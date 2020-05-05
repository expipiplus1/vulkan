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
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Vector.Extra              ( pattern (:<|)
                                                , pattern Empty
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
import           Render.Stmts
import           Render.Stmts.Alloc
import           Render.Stmts.Poke
import           Render.Stmts.Utils
import           Render.Type
import           Spec.Parse

renderCommand
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand m@MarshaledCommand {..} = contextShow (unCName mcName) $ do
  RenderParams {..} <- input
  let Command {..} = mcCommand
  genRe ("command " <> unCName mcName) $ do
    renderForeignDecl mcCommand

    let siblingMap = Map.fromList
          [ (n, SiblingInfo (pretty . unCName $ n) (mpScheme p))
          | p <- V.toList mcParams
          , let n = pName . mpParam $ p
          ]
    let lookupSibling :: CName -> Maybe (SiblingInfo Parameter)
        lookupSibling = (`Map.lookup` siblingMap)


    let commandName = mkFunName mcName
    runInputConst lookupSibling $ if any (isInOutCount . mpScheme) mcParams
      then marshaledDualPurposeCommandCall commandName m
      else marshaledCommandCall commandName m

makeReturnType
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Bool
  -> MarshaledCommand
  -> Sem r H.Type
makeReturnType includeInOutCountTypes mc@MarshaledCommand {..}
  = do
    ts <- marshaledCommandReturnTypes includeInOutCountTypes mc
    pure $ VarT ioVar :@ foldl' (:@) (TupleT (length ts)) ts

structVariables
  :: (HasErr r, HasRenderedNames r) => Type -> Sem r ([Name], [Name])
  -- ^ negative, positive
structVariables = \case
  ArrowT :@ l :@ r -> do
    (nl, pl) <- structVariables l
    (nr, pr) <- structVariables r
    pure (pl <> nr, nl <> pr)
  InfixT _ i t | i == typeName (TyConName ":::") -> structVariables t
  ConT n :@ VarT v ->
    getRenderedStruct (TyConName (T.pack (nameBase n))) <&> \case
      Just s | not (V.null (sExtendedBy s)) -> ([], [v])
      _ -> ([], [])
  ConT n :@ _ | n == ''Ptr -> pure ([], [])
  f :@ x                  -> liftA2 (<>) (structVariables f) (structVariables x)
  ConT   _                -> pure ([], [])
  VarT   _                -> pure ([], [])
  TupleT _                -> pure ([], [])
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
     )
  => HName
  -> MarshaledCommand
  -> Sem r ()
marshaledCommandCall commandName m@MarshaledCommand {..} = do
  RenderParams {..} <- input

  includeReturnType <- marshaledCommandShouldIncludeReturnValue m

  tellExport (ETerm commandName)

  let badNames = mempty
  stmtsDoc <- renderStmts badNames $ do
    -- Load the function from the pointer
    funRef    <- getCCall mcCommand

    -- Generate references to the parameters to use later
    paramRefs <- forV mcParams $ \MarshaledParam {..} -> if isElided mpScheme
      then pure Nothing
      else Just <$> do
        ty       <- schemeTypeNegative mpScheme
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
    (pokeRefs, peekRefs) <- V.unzip <$> V.zipWithM getPoke paramRefs mcParams

    -- Bind the result to _ if it can only return success
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
      -- call the command
      pure . IOAction . ValueDoc $ sep (fun : (unValueDoc <$> toList pokes))
    retRef        <- unwrapIdiomaticType (Just name) wrappedRef

    -- check the result
    checkedResult <- checkResultMaybe mcCommand retRef

    unitStmt $ do
      after checkedResult
      let peeks = catMaybes (toList peekRefs)
      rets <- traverse use $ if includeReturnType then retRef : peeks else peeks
      pure . Pure NeverInline $ tupled @() (unValueDoc <$> rets)

  rhs <- case stmtsDoc of
    IOStmts d -> do
      tellImport 'liftIO
      pure $ "liftIO $" <+> d
    ContTStmts d -> do
      tellImport 'evalContT
      tellImport 'liftIO
      pure $ "liftIO . evalContT $" <+> d

  ----------------------------------------------------------------
  -- The type of this command
  ----------------------------------------------------------------
  nts <- marshaledCommandInputTypes m
  r   <- makeReturnType True m
  let t         = foldr arrowUniqueVars r nts
      paramName = pretty . mkParamName . pName . mpParam
      isArg p = case mpScheme p of
        ElidedLength{}    -> Nothing
        ElidedUnivalued _ -> Nothing
        ElidedVoid        -> Nothing
        Returned _        -> Nothing
        _                 -> Just p
      paramNames = toList (paramName <$> V.mapMaybe isArg mcParams)
  tDoc <- renderType . addMonadIO =<< constrainStructVariables t


  tellDocWithHaddock $ \getDoc -> vsep
    [ getDoc (TopLevel (cName mcCommand))
    , pretty commandName <+> "::" <+> indent 0 tDoc
    , pretty commandName <+> sep paramNames <+> "=" <+> rhs
    ]

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
-- - call the function again with the same parameters and not with space for
--   the return array
-- - Return the array
----------------------------------------------------------------

marshaledDualPurposeCommandCall
  :: ( HasErr r
     , HasSpecInfo r
     , HasRenderParams r
     , HasRenderElem r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
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
  countParamIndex <- case V.findIndices (isInOutCount . mpScheme) mcParams of
    Empty -> throw
      "Trying to render a dual purpose command with no InOutCount parameter"
    i :<| Empty -> pure i
    _ ->
      throw
        "Trying to render a dual purpose command with multiple InOutCount parameters"
  let countParam     = mcParams V.! countParamIndex
      countParamName = pName (mpParam countParam)
      isCounted p = pLengths p == V.singleton (NamedLength countParamName)
  vecParamIndices <- case V.findIndices (isCounted . mpParam) mcParams of
    Empty ->
      throw "Trying to render a dual purpose command with no output vector"
    is -> pure is

  --
  -- Get the type of this function
  --
  includeReturnType <- marshaledCommandShouldIncludeReturnValue m
  let negativeSchemeType = \case
        InOutCount _ -> pure Nothing
        s            -> schemeTypeNegative s
  nts <-
    V.mapMaybe id <$> traverseV (marshaledParamType negativeSchemeType) mcParams
  returnType <- makeReturnType False m
  let commandType = foldr (~>) returnType nts

  --
  -- Get var names for the LHS
  --
  let paramName = pretty . mkParamName . pName . mpParam
      isArg p = case mpScheme p of
        ElidedLength{}    -> Nothing
        ElidedUnivalued _ -> Nothing
        ElidedVoid        -> Nothing
        Returned   _      -> Nothing
        InOutCount _      -> Nothing
        _                 -> Just p
      paramNames = toList (paramName <$> V.mapMaybe isArg mcParams)


  let badNames = mempty
  stmtsDoc <- renderStmts badNames $ do

    --
    -- Load the function from the pointer
    --
    funRef <- getCCall mcCommand

    --
    -- Run once to get the length
    --
    (getLengthPokes, getLengthPeeks, countAddr, countPeek) <-
      pokesForGettingCount mcParams countParamIndex vecParamIndices
    ret1        <- runWithPokes False m funRef getLengthPokes

    filledCount <- stmt Nothing Nothing $ do
      after ret1
      ValueDoc v <- use countPeek
      pure . Pure AlwaysInline . ValueDoc $ v
    nameRef (unCName $ pName (mpParam countParam)) filledCount

    (getVectorsPokes, getVectorsPeeks) <- pokesForGettingResults
      mcParams
      getLengthPokes
      getLengthPeeks
      countAddr
      countParamIndex
      vecParamIndices

    ret2          <- runWithPokes includeReturnType m funRef getVectorsPokes

    finalCountRef <- stmt Nothing Nothing $ do
      after ret2
      countScheme <- case mpScheme countParam of
        InOutCount s -> pure s
        _            -> throw "Trying to peek count without inout scheme"
      peekCount <-
        note "Unable to get peek for count"
          =<< peekStmtDirect (mpParam countParam) countAddr countScheme
      Pure InlineOnce <$> use peekCount
    -- TODO: disgusting, this stringly typed stuff
    nameRef (unCName (pName (mpParam countParam)) <> "::2") finalCountRef

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

  tDoc <- renderType . addMonadIO =<< constrainStructVariables commandType
  tellDocWithHaddock $ \getDoc -> vsep
    [ getDoc (TopLevel (cName mcCommand))
    , pretty commandName <+> "::" <+> indent 0 tDoc
    , pretty commandName <+> sep paramNames <+> "=" <+> rhs
    ]

pokesForGettingCount
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasStmts r
     , HasSiblingInfo Parameter r
     , HasRenderedNames r
     )
  => Vector MarshaledParam
  -> Int
  -> Vector Int
  -> Stmt
       s
       r
       ( Vector (Ref s ValueDoc)
       , Vector (Maybe (Ref s ValueDoc))
       , Ref s AddrDoc
       , Ref s ValueDoc
       )
pokesForGettingCount params countIndex vecIndices = do
  countOverride <- case params V.!? countIndex of
    Nothing -> throw "Couldn't find count parameter for getting count"
    Just p
      | InOutCount s <- mpScheme p -> do
        (addrRef, peek) <- allocateAndPeek (mpParam p) s
        pure (countIndex, (addrRef, peek))
      | otherwise -> throw "Count doesn't have inout type"
  vecOverrides <- forV vecIndices $ \i -> case params V.!? i of
    Nothing -> throw "Couldn't find vector parameter for getting count"
    Just p
      | Returned _ <- mpScheme p -> pure
        (i, Right p { mpScheme = ElidedUnivalued "nullPtr" })
      | otherwise -> throw $ "Vector doesn't have Returned type, has " <> show
        (mpScheme p)
  let gettingCountParams =
        (Right <$> params)
          V.// (second Left countOverride : V.toList vecOverrides)

  pokes <- forV (V.indexed gettingCountParams) $ \case
    (_, Left (addrRef, peek)) -> do
      poke <- castRef addrRef
      pure (poke, Just peek)
    (i, Right m@MarshaledParam {..}) -> if isElided mpScheme
      then getPoke Nothing m
      else do
        -- Don't name the count ref, because we want to count read after the
        -- second call to have this name.
        ref <- if i == countIndex then paramRefUnnamed m else paramRef m
        getPoke (Just ref) m

  let (countAddr, countPeek) = case countOverride of
        (_, (addr, peek)) -> (addr, peek)

  pure (fst <$> pokes, snd <$> pokes, countAddr, countPeek)

pokesForGettingResults
  :: ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSpecInfo r
     , HasStmts r
     , HasSiblingInfo Parameter r
     )
  => Vector MarshaledParam
  -> Vector (Ref s ValueDoc)
  -> Vector (Maybe (Ref s ValueDoc))
  -> Ref s AddrDoc
  -> Int
  -> Vector Int
  -> Stmt
       s
       r
       ( Vector (Ref s ValueDoc)
       , Vector (Maybe (Ref s ValueDoc))
       )
  -- ^ Pokes for this call, peeks for this call
pokesForGettingResults params oldPokes oldPeeks countAddr countIndex vecIndices
  = do
    countOverride                        <- (countIndex, ) <$> castRef countAddr
    (vecPokeOverrides, vecPeekOverrides) <-
      fmap V.unzip . forV vecIndices $ \i -> do
        MarshaledParam {..} <- note "Unable to find vector for returning"
                                    (params V.!? i)
        let lowered        = lowerParamType mpParam
            usingSecondLen = lowered
              { pLengths = case pLengths lowered of
                             NamedLength n :<| t ->
                               -- YUCK
                               NamedLength (CName (unCName n <> "::2")) :<| t
                             t -> t
              }
        scheme <- case mpScheme of
          Returned s -> pure s
          _          -> throw "Vector without a return scheme"
        addr  <- allocate lowered scheme
        addr' <- castRef addr
        peek  <-
          note "Unable to get peek for returned value"
            =<< peekStmtDirect usingSecondLen addr scheme
        pure ((i, addr'), (i, Just peek))
    let newPokes = oldPokes V.// (countOverride : toList vecPokeOverrides)
        newPeeks =
          oldPeeks V.// ((countIndex, Nothing) : toList vecPeekOverrides)
    pure (newPokes, newPeeks)


runWithPokes
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSiblingInfo Parameter r)
  => Bool
  -> MarshaledCommand
  -> Ref s FunDoc
  -> Vector (Ref s ValueDoc)
  -> Stmt s r (Ref s ValueDoc)
runWithPokes includeReturnType MarshaledCommand {..} funRef pokes = do
  -- Bind the result to _ if it can only return success
  let useEmptyBinder =
        not includeReturnType
          && null (cErrorCodes mcCommand)
          && cReturnType mcCommand
          /= Void
  retRef <- stmt Nothing (Just (bool "r" "_" useEmptyBinder)) $ do
    FunDoc fun <- use funRef
    pokes      <- traverseV use pokes
    -- call the command
    pure . IOAction . ValueDoc $ sep (fun : (unValueDoc <$> toList pokes))

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
  ty                <- schemeTypeNegative mpScheme
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
        let getInstanceProcAddr' = mkFunName "vkGetInstanceProcAddr'"
        tellImport getInstanceProcAddr' -- TODO: Remove vulkan specific stuff here!
        tellImport 'nullPtr
        tellImport 'castFunPtr
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        fTyDoc <- renderTypeHighPrec =<< cToHsTypeWithHoles
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

renderForeignDecl
  :: ( HasErr r
     , HasSpecInfo r
     , HasRenderElem r
     , HasRenderParams r
     , HasRenderedNames r
     )
  => Command
  -> Sem r ()
renderForeignDecl c@Command {..} = do
  ffiTy <- cToHsType
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
      tellDoc $ vsep
        [ "foreign import ccall"
        , "#if !defined(SAFE_FOREIGN_CALLS)"
        , indent 2 "unsafe"
        , "#endif"
        , indent 2 "\"dynamic\"" <+> pretty dynName
        , indent 2 ("::" <+> dynamicBindTypeDoc)
        ]
    else do
      let staticName = getStaticName c
      staticBindTypeDoc <- renderType ffiTy
      importConstructors ffiTy
      tellDoc $ vsep
        [ "foreign import ccall"
        , "#if !defined(SAFE_FOREIGN_CALLS)"
        , indent 2 "unsafe"
        , "#endif"
        , indent 2 (dquotes (pretty (unCName cName)) <+> pretty staticName)
        , indent 2 ("::" <+> staticBindTypeDoc)
        ]


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
     )
  => Maybe (Ref s ValueDoc)
  -- ^ Might be nothing if this is elided
  -> MarshaledParam
  -> Stmt s r (Ref s ValueDoc, Maybe (Ref s ValueDoc))
  -- ^ (poke, peek if it's a returned value)
getPoke valueRef MarshaledParam {..} = do
  RenderParams {..} <- input
  case mpScheme of
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

-- | Parameters of type foo[x] are passed as pointers
lowerParamType :: Parameter -> Parameter
lowerParamType p@Parameter {..} = case pType of
  Array q _ elem -> p { pType = Ptr q elem }
  _              -> p

----------------------------------------------------------------
--
----------------------------------------------------------------

addMonadIO :: Type -> Type
addMonadIO = addConstraints
      [ConT ''MonadIO :@ VarT ioVar]

ioVar :: Name
ioVar = mkName "io"

-- | Any extensible structs have 'PokeChain' or 'PeekChain' constraints added
-- depending on their position polarity.
constrainStructVariables
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasRenderedNames r)
  => Type
  -> Sem r Type
constrainStructVariables t = do
  (ns, ps) <- structVariables t
  unless (null ns) $ tellImport (TyConName "PokeChain")
  unless (null ps) $ tellImport (TyConName "PeekChain")
  pure $ addConstraints
    (  ((ConT (typeName (TyConName "PokeChain")) :@) . VarT <$> (ns <> ps))
    <> ((ConT (typeName (TyConName "PeekChain")) :@) . VarT <$> ps)
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
      isNewtype' n = pure (n `elem` builtinNewtypes)
        <||> isNewtype (TyConName . T.pack . nameBase $ n)
      setNameBase :: Name -> HName -> Name
      setNameBase n = case nameModule n of
        Nothing -> mkName . T.unpack . unName
        Just m  -> mkNameG_tc "" m . T.unpack . unName
  resolveAlias <- do
    ra <- getResolveAlias
    pure $ \n -> setNameBase n (ra . TyConName . T.pack . nameBase $ n)
  for_ names $ \n -> whenM (isNewtype' n) $ tellImportWithAll (resolveAlias n)

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

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
