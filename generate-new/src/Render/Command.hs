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
import           Language.Haskell.TH            ( nameBase )

import           Foreign.C.Types
import           Foreign.Ptr
import           Control.Monad.Trans.Cont       ( ContT
                                                , runContT
                                                )

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Error
import           CType                         as C
import           CType.Size
import           Render.Element
import           Render.Type
import           Render.Scheme
import           Render.SpecInfo
import           Render.Poke

renderCommand
  :: (HasErr r, Member (Reader RenderParams) r, HasSpecInfo r)
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
    dynamicBindTypeDoc <- renderType dynamicBindType
    let dynName = "mk" <> upperCaseFirst cName
    marshaledCommandCall dynName m
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
  :: (HasErr r, HasRenderParams r) => MarshaledCommand -> Sem r H.Type
makeReturnType MarshaledCommand {..} = do
  pts <- V.mapMaybe id <$> traverseV (paramType schemeTypePositive) mcParams
  r   <- case mcReturn of
    C.Void -> pure V.empty
    r      -> V.singleton <$> cToHsType DoNotPreserve r
  let ts = pts <> r
  pure $ ConT ''IO :@ foldl' (:@) (TupleT (length ts)) ts

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
    tellExport (ETerm n)
    nts <- V.mapMaybe id <$> traverseV (paramType schemeType) mcParams
    r   <- makeReturnType m
    let t         = foldr (~>) r nts
        paramName = pretty . mkParamName . pName . mpParam
        isArg p = case mpScheme p of
          ElidedLength _ _  -> Nothing
          ElidedUnivalued _ -> Nothing
          ElidedVoid        -> Nothing
          Returned _        -> Nothing
          _                 -> Just p
        paramNames = toList (paramName <$> V.mapMaybe isArg mcParams)
        call = pretty dynName <+> parens "error \"Fun Ptr\"" <+> sep paramNames

    -- Mark all the parameters with a pure poke if they have one
    -- i.e. parameters for which we don't have to allocate
    let pureParams
          :: V.Vector (Either (Text, Poke (Assigned Parameter)) MarshaledParam)
        pureParams = mcParams <&> \mp@MarshaledParam {..} ->
          let Parameter {..} = mpParam
              to             = pType
          in  first (pName, ) $ case mpScheme of
                Normal from | from == to ->
                  Left $ Pure DoInline $ Assigned $ pure $ paramName mp
                Preserve from | from == to ->
                  Left $ Pure DoInline $ Assigned $ pure $ paramName mp
                VoidPtr | Ptr _ Void <- to ->
                  Left $ Pure DoInline $ Assigned $ pure $ paramName mp
                _ -> Right mp

    pokes :: V.Vector (Text, Poke (Assigned Parameter)) <-
      forV pureParams $ \case
        Left  purePoke              -> pure purePoke
        Right m@MarshaledParam {..} -> (pName mpParam, ) <$> case mpScheme of
          Returned s ->
            pure $ Pure DoInline $ Assigned $ pure "error \"returned\""
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

    let
      ps =
        [ scanChainedPokes
          (V.toList pokes)
          (IOPoke $ \vs ->
            Assigned $ pure "error \"call func\""
          )
        , IOPoke $ Assigned $ pure "error \"return\""
        ]

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
