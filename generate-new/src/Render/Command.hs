{-# language TemplateHaskellQuotes #-}
module Render.Command
  ( renderCommand
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , runReader
                                                , Type
                                                , Handle
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                ( upperCaseFirst )
import           Language.Haskell.TH.Syntax
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
import           Control.Exception              ( bracket, throwIO )

import           CType                         as C
import           Error
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.Peek
import           Render.Poke
import           Render.Scheme
import           Render.SpecInfo
import           Render.Type
import           Spec.Parse

renderCommand
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
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
        dynName = getDynName mcCommand
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
      ]
    marshaledCommandCall dynName m

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
  :: HasErr r => MarshaledCommand -> Sem r (V.Vector (AssignedPoke Parameter))
makeReturnPeeks MarshaledCommand {..} =
  fmap (V.mapMaybe id) . forV mcParams $ \case
    MarshaledParam {..} | Returned r <- mpScheme ->
      pure
        $   Just
        $   IOPoke
        $   Assigned
        $   note "Unable to get peek for Returned value"
        =<< renderPeekStmt
              pName
              mpParam
                { pType = case pType mpParam of
                            Ptr _ t -> t
                            _       -> error "TODO, do this bit properly"
                }
              (AddrDoc (pretty (pName mpParam)))
              r
    _ -> pure Nothing

----------------------------------------------------------------
-- Calling this command
----------------------------------------------------------------

marshaledCommandCall
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
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

    let n = mkFunName mcName
        includeReturnType =
          mcReturn
            /= C.Void
            && (mcReturn == successCodeType --> any isSuccessCodeReturned
                                                    (cSuccessCodes mcCommand)
               )
    tellExport (ETerm n)
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

    pokes :: V.Vector (Text, Poke (Assigned Parameter)) <-
      forV mcParams $ \m@MarshaledParam {..} ->
        (pName mpParam, ) <$> case mpScheme of
          Returned _s -> case pType mpParam of
            Ptr NonConst t -> do
              isStruct <- case t of
                TypeName n -> isJust <$> getStruct n
                _          -> pure False
              pure $ ContTPoke $ Assigned $ do
                tellImportWithAll ''ContT
                if isStruct
                  then do
                    tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve t
                    tellImportWithAll (TyConName "ToCStruct")
                    pure ("ContT" <+> parens ("withZeroCStruct @" <> tyDoc))
                  else do
                    tellImport 'free
                    tellImport 'bracket
                    tellImport 'calloc
                    tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve t
                    pure
                      (   "ContT $ bracket"
                      <+> parens ("calloc @" <> tyDoc)
                      <+> "free"
                      )
            _ -> throw "Returned scheme with non NonConst Ptr type"
          _ -> do
            -- TODO: Move this to elsewhere
            let lowerType p = p
                  { pType = case pType p of
                              Array q _ t -> Ptr q t
                              t           -> t
                  }
            poke <- getPokeDirect (lowerType mpParam) mpScheme
            pure
              (   (\(Direct f) -> Assigned $ do
                    tellImport 'plusPtr
                    ty <- cToHsType DoPreserve (pType mpParam)
                    f ty (ValueDoc (paramName m))
                  )
              <$> poke
              )

    let cCallName  = ValueDoc "cCall"
        returnName = "r"
    cCall       <- getCCall mcCommand
    returnPeeks <- toList <$> makeReturnPeeks m
    let
      returnStmt = IOPoke $ Assigned $ pure $ "pure" <+> tupled
        (  case mcReturn of
            C.Void                    -> []
            _ | not includeReturnType -> []
            _                         -> [returnName]
        <> [ pretty (pName mpParam)
           | MarshaledParam {..} <- toList mcParams
           , Returned _          <- pure mpScheme
           ]
        )
      throwStmt =
        if null (cErrorCodes mcCommand)
           || cReturnType mcCommand
           /= successCodeType
        then
          []
        else
          [ IOPoke $ \(ValueDoc r) -> Assigned $ do
              tellImport 'when
              tellImport 'throwIO
              let pat = mkPatternName firstSuccessCode
              tellImport (ConName pat)
              tellImportWithAll (TyConName exceptionTypeName)
              pure $ "when" <+> parens (r <+> "<" <+> pretty pat) <+> parens
                ("throwIO" <+> parens (pretty exceptionTypeName <+> r))
          ]

    let
      (cCallRet, rets) = if includeReturnType
        then
          ( ChainedPoke
            (ValueDoc returnName)
            (IOPoke $ \(ValueDoc c) vs ->
              Assigned $ pure $ c <+> sep (unValueDoc <$> vs)
            )
            (  (fmap (\f r _ _ -> f r) <$> throwStmt)
            <> (fmap (\f _ _ _ -> f) <$> returnPeeks <> [returnStmt])
            )
          , []
          )
        else
          ( ChainedPoke
            (ValueDoc returnName)
            (IOPoke $ \(ValueDoc c) vs ->
              Assigned $ pure $ c <+> sep (unValueDoc <$> vs)
            )
            (fmap (\f r _ _ -> f r) <$> throwStmt)
          , returnPeeks <> [returnStmt]
          )

    let ps =
          [ scanChainedPokes
              (V.toList pokes)
              (ChainedPoke cCallName (const <$> cCall) [cCallRet])
            ]
            <> rets

    pokesDoc <- renderPokesContTRet @Parameter Nothing (V.fromList ps)

    tDoc     <- renderType t
    tellImport 'runContT
    tellDoc
      . vsep
      $ [ pretty n <+> "::" <+> indent 0 tDoc
        , pretty n <+> sep paramNames <+> "= (`runContT` pure) $" <+> pokesDoc
        ]

-- Err, just follow the types...
scanChainedPokes :: [(Text, Poke a)] -> Poke ([ValueDoc] -> a) -> Poke a
scanChainedPokes pokes go = case pokes of
  []          -> ($ []) <$> go
  (n, p) : xs -> ChainedPoke
    (ValueDoc (pretty n))
    p
    [ scanChainedPokes (second (fmap const) <$> xs)
                       ((\f vs v -> f (v : vs)) <$> go)
    ]

----------------------------------------------------------------
-- Getting C call
----------------------------------------------------------------

getCCall
  :: (HasRenderParams r, HasSpecInfo r) => Command -> Sem r (AssignedPoke a)
getCCall c = do
  RenderParams {..} <- ask
  let
    -- TODO: Change this to a "global variable"
    noHandle = pure $ IOPoke
      (Assigned $ do
        let dynName = getDynName c
        tellImport (TermName "vkGetInstanceProcAddr'") -- TODO: Remove vulkan specific stuff here!
        tellImport 'nullPtr
        tellImport 'castFunPtr
        tellImportWith ''GHC.Ptr.Ptr 'GHC.Ptr.Ptr
        fTyDoc <- renderTypeHighPrec =<< cToHsType
          DoLower
          (C.Proto
            (cReturnType c)
            [ (Just pName, pType) | Parameter {..} <- V.toList (cParameters c) ]
          )
        pure
          $   pretty dynName
          <+> ". castFunPtr @_ @"
          <>  fTyDoc
          <+> "<$> vkGetInstanceProcAddr' nullPtr"
          <+> parens ("Ptr" <+> dquotes (pretty (cName c) <> "\\NUL") <> "#")
      )
    -- TODO: This is nasty, the "cmds" bound by the chained poke here is
    -- implicitly used elsewhere for dispatchable parameter creation.
    cmdsFun ptrRecTyName getCmdsFun paramName paramType = pure $ ChainedPoke
      (ValueDoc "cmds")
      (Pure
        DoNotInline
        (Assigned $ do
          paramTDoc <- renderType =<< cToHsType DoNotPreserve paramType
          pure $ pretty getCmdsFun <+> parens
            (pretty paramName <+> "::" <+> paramTDoc)
        )
      )
      [ Pure
          DoNotInline
          (\(ValueDoc cmds) -> Assigned $ do
            let dynName    = getDynName c
                memberName = mkFuncPointerMemberName (cName c)
            tellImportWith ptrRecTyName (TermName memberName)
            pure $ pretty dynName <+> parens (pretty memberName <+> cmds)
          )
      ]
  commandHandle c >>= \case
    Nothing                            -> noHandle
    Just (Parameter {..}, Handle {..}) -> do
      let instanceHandle =
            cmdsFun (TyConName "InstanceCmds") "instanceCmds" paramName pType
          deviceHandle = cmdsFun (TyConName "DeviceCmds")
                                 ("deviceCmds" :: Text)
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
