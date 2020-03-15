{-# language TemplateHaskellQuotes #-}
module Render.Command
  ( renderCommand
  ) where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , runReader
                                                , Type
                                                , Handle
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                ( upperCaseFirst )
import           Language.Haskell.TH.Syntax     ( nameBase )
import           Data.List.Extra                ( nubOrd )
import           Polysemy
import           Polysemy.Reader
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Map                      as Map

import           Control.Monad.Trans.Cont
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import qualified GHC.Ptr
import           Control.Exception              ( bracket
                                                , throwIO
                                                )

import           CType                         as C
import           Error
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Stmts.Alloc
import           Render.Scheme
import           Render.SpecInfo
import           Render.Type
import           Spec.Parse

renderCommand
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasStmts r)
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand m@MarshaledCommand {..} = contextShow mcName $ do
  RenderParams {..} <- ask
  let Command {..} = mcCommand
  genRe ("command " <> mcName) $ do
    ffiTy <- cToHsType
      DoLower
      (Proto cReturnType
             [ (Nothing, pType) | Parameter {..} <- toList cParameters ]
      )
    let dynamicBindType = ConT ''FunPtr :@ ffiTy ~> ffiTy
        dynName         = getDynName mcCommand
    dynamicBindTypeDoc <- renderType dynamicBindType
    importConstructors dynamicBindType
    tellDoc $ vsep
      [ mempty
      , "foreign import ccall"
      , "#if !defined(SAFE_FOREIGN_CALLS)"
      , indent 2 "unsafe"
      , "#endif"
      , indent 2 "\"dynamic\"" <+> pretty dynName
      , indent 2 ("::" <+> dynamicBindTypeDoc)
      , emptyDoc
      ]
    if any (isInOut . mpScheme) mcParams
      then marshaledDualPurposeCommandCall dynName m
      else marshaledCommandCall dynName m

paramType
  :: (HasErr r, Member (Reader RenderParams) r)
  => (MarshalScheme Parameter -> Sem r (Maybe H.Type))
  -> MarshaledParam
  -> Sem r (Maybe H.Type)
paramType st MarshaledParam {..} = contextShow (pName mpParam) $ do
  RenderParams {..} <- ask
  let Parameter {..} = mpParam
  n <- st mpScheme
  pure $ namedTy (mkParamName pName) <$> n

makeReturnType
  :: (HasErr r, HasRenderParams r) => Bool -> MarshaledCommand -> Sem r H.Type
makeReturnType includeReturnType MarshaledCommand {..} = do
  pts <- V.mapMaybe id <$> traverseV (paramType schemeTypePositive) mcParams
  r   <- case mcReturn of
    C.Void                    -> pure V.empty
    _ | not includeReturnType -> pure V.empty
    r                         -> V.singleton <$> cToHsType DoNotPreserve r
  let ts = r <> pts
  pure $ ConT ''IO :@ foldl' (:@) (TupleT (length ts)) ts

makeReturnPeeks
  :: HasErr r => MarshaledCommand -> Stmt s r (V.Vector (Ref s ValueDoc))
makeReturnPeeks MarshaledCommand {..} =
  sequence . V.mapMaybe id $ mcParams <&> \case
    MarshaledParam {..} | Returned r <- mpScheme ->
      Just $ stmt Nothing Nothing $ pure $ IOAction
        (ValueDoc "error \"return peek\"")

--       pure
--         $ Just
--         $ IOPoke
--         $ Assigned
--         $ note "Unable to get peek for Returned value"
--         $ Just (pretty (pName mpParam) <> " <- error \"TODO SHIP\"")
--         -- =<< renderPeekStmt
--         --       pName
--         --       mpParam
--         --         { pType = case pType mpParam of
--         --                     Ptr _ t -> t
--         --                     _       -> error "TODO, do this bit properly"
--         --         }
--         --       (AddrDoc (pretty (pName mpParam)))
--         --       r
    _ -> Nothing

----------------------------------------------------------------
-- Calling this command
----------------------------------------------------------------

marshaledCommandCall
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r, HasStmts r)
  => Text
  -> MarshaledCommand
  -> Sem r ()
marshaledCommandCall dynName m@MarshaledCommand {..} = do
  RenderParams {..} <- ask

  let siblingMap = Map.fromList
        [ (n, SiblingInfo (pretty n) (mpScheme p))
        | p <- V.toList mcParams
        , let n = pName . mpParam $ p
        ]
  let lookupSibling :: Text -> Maybe (SiblingInfo Parameter)
      lookupSibling = (`Map.lookup` siblingMap)

  runReader lookupSibling $ do

    let commandName = mkFunName mcName
        includeReturnType =
          mcReturn
            /= C.Void
            && (mcReturn == successCodeType --> any isSuccessCodeReturned
                                                    (cSuccessCodes mcCommand)
               )
    tellExport (ETerm commandName)
    nts <- V.mapMaybe id <$> traverseV (paramType schemeType) mcParams
    r   <- makeReturnType includeReturnType m
    let t         = foldr (~>) r nts
        paramName = pretty . mkParamName . pName . mpParam
        isArg p = case mpScheme p of
          ElidedLength _ _  -> Nothing
          ElidedUnivalued _ -> Nothing
          ElidedVoid        -> Nothing
          Returned _        -> Nothing
          _                 -> Just p
        paramNames = toList (paramName <$> V.mapMaybe isArg mcParams)

    let badNames = mempty
    stmtsDoc <- renderStmts badNames $ do
      -- Load the function from the pointer
      funRef    <- getCCall mcCommand

      -- Generate references to the parameters to use later
      paramRefs <- forV mcParams $ \MarshaledParam {..} -> if isElided mpScheme
        then pure Nothing
        else Just <$> do
          ty       <- schemeType mpScheme
          valueRef <-
            stmt ty (Just (mkParamName (pName mpParam)))
            . pure
            . Pure AlwaysInline
            . ValueDoc
            . pretty
            . mkParamName
            $ pName mpParam
          nameRef (pName mpParam) valueRef
          pure valueRef

      -- poke all the parameters
      (pokeRefs, peekRefs) <- V.unzip <$> V.zipWithM getPoke paramRefs mcParams

      -- Run the command and capture the result
      retRef               <- stmt Nothing (Just "r") $ do
        FunDoc fun <- use funRef
        pokes      <- traverseV use pokeRefs
        -- call the command
        pure . IOAction . ValueDoc $ sep (fun : (unValueDoc <$> toList pokes))

      -- check the result
      checkedResult <-
        if null (cErrorCodes mcCommand)
           || cReturnType mcCommand
           /= successCodeType
        then
          pure Nothing
        else
          Just <$> checkResult retRef

      unitStmt $ do
        after retRef
        -- TODO: Enforce the ordering here!
        traverse_ after checkedResult
        let peeks = catMaybes (toList peekRefs)
        rets <- traverse use
          $ if includeReturnType then retRef : peeks else peeks
        pure . Pure NeverInline $ tupled @() (unValueDoc <$> rets)

    rhs <- case stmtsDoc of
      IOStmts    d -> pure d
      ContTStmts d -> do
        tellImport 'evalContT
        pure $ "evalContT $" <+> d


    tDoc <- renderType t
    tellImport 'evalContT
    tellDoc
      . vsep
      $ [ pretty commandName <+> "::" <+> indent 0 tDoc
        , pretty commandName <+> sep paramNames <+> "=" <+> rhs
        ]

----------------------------------------------------------------
-- Checking the result and throwing an exception if something went wrong
----------------------------------------------------------------

checkResult
  :: (HasErr r, HasRenderParams r, HasRenderElem r)
  => Ref s ValueDoc
  -> Stmt s r (Ref s UnitDoc)
checkResult retRef = unitStmt $ do
  RenderParams {..} <- ask
  ValueDoc ret      <- use retRef
  tellImport 'when
  tellImport 'throwIO
  let pat = mkPatternName firstSuccessCode
  tellImport (ConName pat)
  tellImportWithAll (TyConName exceptionTypeName)
  pure
    .   IOAction
    .   UnitDoc
    $   "when"
    <+> parens (ret <+> "<" <+> pretty pat)
    <+> parens ("throwIO" <+> parens (pretty exceptionTypeName <+> ret))

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
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r, HasStmts r)
  => Text
  -> MarshaledCommand
  -> Sem r ()
marshaledDualPurposeCommandCall dynName m@MarshaledCommand {..} = do
  RenderParams {..} <- ask
  let commandName = mkFunName mcName
  tellExport (ETerm commandName)
  tellDoc $ pretty commandName <> " = undefined"

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
getCCall c = do
  RenderParams {..} <- ask
  let -- What to do in the case that this command isn't dispatched from a handle
      noHandle = stmt Nothing (Just (cName c)) $ do
        -- TODO: Change this function pointer to a "global variable" with ioref and
        -- unsafePerformIO
        tellImport (TermName "vkGetInstanceProcAddr'") -- TODO: Remove vulkan specific stuff here!
        tellImport 'nullPtr
        tellImport 'castFunPtr
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        let dynName = getDynName c
        fTyDoc <- renderTypeHighPrec =<< cToHsType
          DoLower
          (C.Proto
            (cReturnType c)
            [ (Just pName, pType) | Parameter {..} <- V.toList (cParameters c) ]
          )
        pure
          .   IOAction
          .   FunDoc
          $   pretty dynName
          <+> ". castFunPtr @_ @"
          <>  fTyDoc
          <+> "<$> vkGetInstanceProcAddr' nullPtr"
          <+> parens ("Ptr" <+> dquotes (pretty (cName c) <> "\\NUL") <> "#")

      -- What do do if we need to extract the command pointer from a parameter
      cmdsFun ptrRecTyName getCmdsFun paramName paramType = do
        cmdsRef <- stmt Nothing (Just "cmds") $ do
          paramTDoc <- renderType =<< cToHsType DoNotPreserve paramType
          getCmds   <- getCmdsFun
          pure . Pure InlineOnce . CmdsDoc $ getCmds <+> parens
            (pretty paramName <+> "::" <+> paramTDoc)
        nameRef "cmds" cmdsRef
        stmt Nothing (Just (cName c)) $ do
          let dynName    = getDynName c
              memberName = mkFuncPointerMemberName (cName c)
          tellImportWith ptrRecTyName (TermName memberName)
          CmdsDoc cmds <- use cmdsRef
          pure . Pure NeverInline . FunDoc $ pretty dynName <+> parens
            (pretty memberName <+> cmds)

  commandHandle c >>= \case
    Nothing                            -> noHandle
    Just (Parameter {..}, Handle {..}) -> do
      let
        withImport member = do
          tellImportWithAll (TyConName (mkTyName hName))
          pure $ pretty (member :: Text)
        instanceHandle = cmdsFun (TyConName "InstanceCmds")
                                 (withImport "instanceCmds")
                                 paramName
                                 pType
        deviceHandle = cmdsFun (TyConName "DeviceCmds")
                               (withImport "deviceCmds")
                               paramName
                               pType
        paramName = mkParamName pName
      case hLevel of
        NoHandleLevel -> noHandle
        Device        -> deviceHandle
        Instance      -> instanceHandle

-- | The handle of a command is the (dispatchable handle) first parameter.
commandHandle :: HasSpecInfo r => Command -> Sem r (Maybe (Parameter, Handle))
commandHandle Command {..} = case cParameters V.!? 0 of
  Just p@Parameter {..} | TypeName t <- pType -> fmap (p, ) <$> getHandle t
  _ -> pure Nothing

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
     )
  => Maybe (Ref s ValueDoc)
  -- ^ Might be nothing if this is elided
  -> MarshaledParam
  -> Stmt s r (Ref s ValueDoc, Maybe (Ref s ValueDoc))
getPoke valueRef MarshaledParam {..} = do
  RenderParams {..} <- ask
  case mpScheme of
    Returned s -> do
      (addrRef, peek) <- allocate (lowerParamType mpParam) s
      -- TODO: implement ref casting
      addrRef'        <- stmt Nothing Nothing $ do
        AddrDoc addr <- use addrRef
        pure . Pure AlwaysInline . ValueDoc $ addr
      pure (addrRef', Just peek)
    _ -> (, Nothing) <$> case valueRef of
      Nothing -> do
        -- Give the refs names now as they didn't get one earlier
        r <- getPokeDirectElided (lowerParamType mpParam) mpScheme
        nameRef (pName mpParam) r
        pure r
      Just valueRef -> getPokeDirect (lowerParamType mpParam) mpScheme valueRef

-- | Parameters of type foo[x] are passed as pointers
lowerParamType :: Parameter -> Parameter
lowerParamType p@Parameter {..} = case pType of
  Array q _ elem -> p { pType = Ptr q elem }
  _              -> p

----------------------------------------------------------------
-- ImportConstructors
----------------------------------------------------------------

-- | Foreign imports require constructors in scope for newtypes
importConstructors
  :: (HasSpecInfo r, HasRenderElem r, HasRenderParams r) => Type -> Sem r ()
importConstructors t = do
  let names = nubOrd $ allTypeNames t
  for_ names $ \n -> if n `elem` builtinConstructorParents
    then tellImportWithAll n
    else
      traverse_ (tellImportWithAll . TyConName)
        =<< (getConstructorParent . T.pack . nameBase $ n)

builtinConstructorParents :: [Name]
builtinConstructorParents =
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
getDynName = ("mk" <>) . upperCaseFirst . cName

infixr 2 -->
(-->) :: Bool -> Bool -> Bool
a --> b = not a || b
