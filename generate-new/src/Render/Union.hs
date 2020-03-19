module Render.Union
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , runReader
                                                , State
                                                , Type
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Reader

import           Foreign.Ptr
import           Control.Monad.Trans.Cont       ( runContT )
import           Foreign.Marshal.Alloc
import           Control.Exception              ( bracket )
import           Language.Haskell.TH            ( mkName )

import           Spec.Parse
import           Haskell                       as H
import           Error
import           CType
import           Render.Element
import           Render.Type
import           Render.Stmts.Poke
import           Render.Peek
import           Render.Stmts
import           Marshal.Struct
import           Marshal.Scheme
import           Marshal.Scheme.Zero
import           Render.Scheme
import           Render.SpecInfo
import           Render.Names

renderUnion
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledStruct AUnion
  -> Sem r RenderElement
renderUnion marshaled@MarshaledStruct {..} = context (unCName msName) $ do
  RenderParams {..} <- ask
  let Struct {..} = msStruct
  genRe ("union " <> unCName sName) $ do
    let n = mkTyName sName
    ms <- traverseV (renderUnionMember sName) msMembers
    tellDataExport n
    tellDoc $ "data" <+> pretty n <> line <> indent
      2
      (  vsep
      $  zipWith (<+>) ("=" : repeat "|") (toList ms)
      <> ["deriving (Show)"]
      )

    let -- No useful information in the siblings in a union..
        lookupMember :: CName -> Maybe (SiblingInfo StructMember)
        lookupMember = const Nothing
    runReader lookupMember $ do

      toCStructInstance marshaled
      zeroInstance marshaled

      case
          [ d
          | d@UnionDiscriminator {..} <- toList unionDiscriminators
          , udUnionType == msName
          ]
        of
          []  -> pure ()
          [d] -> peekUnionFunction d marshaled
          _ ->
            throw ("Found multiple union discriminators for " <> unCName sName)

renderUnionMember
  :: (HasErr r, HasRenderParams r, HasRenderElem r, HasSpecInfo r)
  => CName
  -- ^ union type name
  -> MarshaledStructMember
  -> Sem r (Doc ())
renderUnionMember tyName MarshaledStructMember {..} = do
  RenderParams {..} <- ask
  let StructMember {..} = msmStructMember
  let con               = mkConName tyName smName
  t    <- note "Union member is elided" =<< schemeType msmScheme
  tDoc <- renderTypeHighPrec t
  pure $ pretty con <+> tDoc

toCStructInstance
  :: forall r
   . ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledStruct AUnion
  -> Sem r ()
toCStructInstance MarshaledStruct {..} = do
  let addrVar = "p"
  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let
    Struct {..} = msStruct
    n           = mkTyName sName
    aVar        = mkVar "a"
    mVar        = "v"
    structT     = ConT (typeName n)
    mkCase :: MarshaledStructMember -> Sem r (Doc ())
    mkCase MarshaledStructMember {..} = do
      let con      = mkConName sName (smName msmStructMember)
          ty       = mkTyName sName
          badNames = fromList [mVar]

      pokeVal <- renderStmts badNames $ do
        addrRef <- stmt Nothing Nothing $ do
          unless (smOffset msmStructMember == 0)
            $ throw "Union has member with non-zero offset"
          tellImport 'castPtr
          pTyDoc <- renderTypeHighPrec
            =<< cToHsType DoPreserve (smType msmStructMember)
          pure
            . Pure AlwaysInline
            . AddrDoc
            $ ("castPtr @_ @" <> pTyDoc <+> addrVar)
        ty       <- schemeType msmScheme
        valueRef <-
          stmt ty Nothing . pure . Pure AlwaysInline . ValueDoc . pretty $ mVar
        getPokeIndirect msmStructMember msmScheme valueRef addrRef

      pokeDoc <- case pokeVal of
        IOStmts d -> do
          tellImport 'lift
          pure $ "lift $" <+> d
        ContTStmts d -> pure d

      tellImportWith ty con
      pure $ pretty con <+> "v" <+> "->" <+> pokeDoc

  cases           <- traverseV mkCase (toList msMembers)

  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)

  withZeroCStructTDoc <-
    let retVar = VarT (mkName "b")
    in  renderType
          ((ConT ''Ptr :@ structT ~> ConT ''IO :@ retVar) ~> ConT ''IO :@ retVar
          )

  tellImport 'runContT
  tellImport 'bracket
  tellImport 'callocBytes
  tellImport 'free

  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f = allocaBytesAligned"
      <+> viaShow sSize
      <+> viaShow sAlignment
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct ::" <+> pokeCStructTDoc
      , "pokeCStruct"
      <+> addrVar
      <+> "= (. const) . runContT .  \\case"
      <>  line
      <>  indent 2 (vsep cases)
      , "withZeroCStruct ::" <+> withZeroCStructTDoc
      , "withZeroCStruct = bracket (callocBytes" <+> viaShow sSize <> ") free"
      ]
    )

zeroInstance
  :: forall r
   . ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => MarshaledStruct AUnion
  -> Sem r ()
zeroInstance MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let n = mkTyName msName
  -- Use the first member with size equal to the struct
  zeroableMembers <-
    fmap catMaybes
    . forV (toList msMembers)
    $ runNonDetMaybe
    . \MarshaledStructMember {..} -> do
        (unionSize, _) <- getTypeSize (TypeName msName)
        zero           <- zeroScheme msmScheme >>= \case
          Nothing -> empty
          Just z  -> pure z
        let con = pretty (mkConName msName (smName msmStructMember))
        size <- case msmScheme of
          Normal t -> fst <$> getTypeSize t
          Preserve t -> fst <$> getTypeSize t
          Tupled n (Normal e) -> do
            (tSize, _) <- getTypeSize e
            pure $ tSize * fromIntegral n
          Tupled n (Preserve e) -> do
            (tSize, _) <- getTypeSize e
            pure $ tSize * fromIntegral n
          _ -> empty
        guard (size == unionSize)
        pure (con <+> zero)
  zeroMember <- case zeroableMembers of
    [] ->
      throw
        "Unable to find a simply-typed union constructor with size equal to the union"
    (x : _) -> pure x
  tellImportWithAll (TyConName "Zero")
  tellDoc $ "instance Zero" <+> pretty n <+> "where" <> line <> indent
    2
    ("zero =" <+> zeroMember)

peekUnionFunction
  :: forall r
   . ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     , HasStmts r
     )
  => UnionDiscriminator
  -> MarshaledStruct AUnion
  -> Sem r ()
peekUnionFunction UnionDiscriminator {..} MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let n        = mkTyName msName
      uTy      = ConT (typeName n)
      fName    = TermName ("peek" <> unName n)
      discTy   = ConT (typeName (mkTyName udSiblingType))
      discName = "tag"
      ptrName  = "p"

  cases <- forV msMembers $ \MarshaledStructMember {..} -> do
    (pat, con) <-
      note "Unable to find union constructor in discriminant map"
        $ find ((== smName msmStructMember) . snd) udValueConstructorMap
    let pat' = mkPatternName pat
        con' = mkConName msName con
    ty    <- cToHsType DoPreserve (smType msmStructMember)
    tyDoc <- renderTypeHighPrec ty
    tellImport 'castPtr
    let addr = AddrDoc ("castPtr @_ @" <> tyDoc <+> ptrName)
    tellImport pat'
    tellImportWith n con'
    subPeek <- renderStmtsIO mempty $ do
      addrRef <- stmt (Just (ConT ''Ptr :@ ty)) (Just "p") $ pure $ Pure
        InlineOnce
        addr
      note "Nothing to peek to fill union with"
        =<< peekStmt msmStructMember addrRef msmScheme
    pure $ pretty pat' <+> "->" <+> pretty con' <+> "<$>" <+> parens subPeek

  tDoc <- renderType (discTy ~> ConT ''Ptr :@ uTy ~> ConT ''IO :@ uTy)
  tellExport (ETerm fName)
  tellDoc $ vsep
    [ pretty fName <+> "::" <+> tDoc
    , pretty fName
    <+> discName
    <+> ptrName
    <+> "= case"
    <+> discName
    <+> "of"
    <>  line
    <>  indent 2 (vsep (toList cases))
    ]
