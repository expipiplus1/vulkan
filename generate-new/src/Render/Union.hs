module Render.Union
  where

import           Prettyprinter
import           Polysemy
import           Polysemy.Input
import           Polysemy.NonDet
import           Relude                  hiding ( State
                                                , Type
                                                )

import           Control.Exception              ( bracket )
import           Control.Monad.Trans.Cont       ( runContT )
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Language.Haskell.TH            ( mkName )

import           CType
import qualified Data.Text                     as T
import           Error
import           Haskell                       as H
import           Marshal.Scheme
import           Marshal.Scheme.Zero
import           Marshal.Struct
import           Render.Element
import           Render.Names
import           Render.Peek
import           Render.Scheme
import           Render.SpecInfo
import           Render.State
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Type
import           Render.Utils                   ( chooseAlign )
import           Spec.Parse

renderUnion
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     , HasRenderState r
     )
  => MarshaledStruct AUnion
  -> Sem r RenderElement
renderUnion marshaled@MarshaledStruct {..} = context (unCName msName) $ do
  RenderParams {..} <- input
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
    runInputConst lookupMember $ do

      toCStructInstance marshaled
      zeroInstance marshaled

      -- Check if this union contains any undiscriminated unions itself
      let isDiscriminated u =
            Spec.Parse.sName u `elem` (udUnionType <$> toList unionDiscriminators)
      descendentUnions <- filter (not . isDiscriminated) <$> containsUnion msName

      case
          [ d
          | d@UnionDiscriminator {..} <- toList unionDiscriminators
          , udUnionType == msName
          ]
        of
          _ | not (null descendentUnions) -> pure ()
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
  RenderParams {..} <- input
  let StructMember {..} = msmStructMember
  let con               = mkConName tyName smName
  case msmScheme of
    Tupled n elemScheme -> do
      t    <- note "Union member is elided" =<< schemeTypeNegative elemScheme
      tDoc <- renderTypeHighPrec t
      pure $ pretty con <+> hsep (replicate (fromIntegral n) tDoc)
    _ -> do
      t    <- note "Union member is elided" =<< schemeTypeNegative msmScheme
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
     , HasRenderState r
     )
  => MarshaledStruct AUnion
  -> Sem r ()
toCStructInstance MarshaledStruct {..} = do
  let addrVar = "p"
  RenderParams {..} <- input
  tellImportWithAll (TyConName "ToCStruct")
  let
    Struct {..} = msStruct
    n           = mkTyName sName
    aVar        = mkVar "a"
    structT     = ConT (typeName n)
    mkCase :: MarshaledStructMember -> Sem r (Doc ())
    mkCase MarshaledStructMember {..} = do
      unless (smOffset msmStructMember == 0)
        $ throw "Union has member with non-zero offset"
      let
        numMembers = case msmScheme of
          Tupled n _ -> n
          _          -> 1
        con   = mkConName sName (smName msmStructMember)
        ty    = mkTyName sName
        mVars = [ "v" <> show n | n <- [0 .. numMembers - 1] ]
        mVar  = if numMembers == 1
          then "v"
          else "(" <> T.intercalate ", " mVars <> ")"
        badNames = fromList (mVar : mVars)

      pokeVal <- renderStmts badNames $ do
        addrRef <- stmt Nothing Nothing $ do
          tellImport 'castPtr
          pTyDoc <- renderTypeHighPrec
            =<< cToHsTypeWithHoles DoPreserve (smType msmStructMember)
          pure
            .   Pure AlwaysInline
            .   AddrDoc
            $   "castPtr @_ @"
            <>  pTyDoc
            <+> addrVar
        ty       <- schemeTypeNegative msmScheme
        valueRef <-
          stmt ty Nothing . pure . Pure AlwaysInline . ValueDoc . pretty $ mVar
        getPokeIndirect msmStructMember msmScheme valueRef addrRef

      pokeDoc <- case pokeVal of
        IOStmts d -> do
          tellImport 'lift
          pure $ "lift $" <+> d
        ContTStmts d -> pure d

      tellImportWith ty con
      let matchVars =
            if numMembers == 1 then pretty mVar else hsep (pretty <$> mVars)
      pure $ pretty con <+> matchVars <+> "->" <+> pokeDoc

  cases           <- traverseV mkCase (toList msMembers)

  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)

  pokeZeroCStructTDoc <-
    let retVar = VarT (mkName "b")
    in  renderType
          (ConT ''Ptr :@ structT ~> ConT ''IO :@ retVar ~> ConT ''IO :@ retVar)

  tellImport 'runContT
  tellImport 'bracket
  tellImport 'callocBytes
  tellImport 'free

  (size, alignment) <- getTypeSize (TypeName msName)
  let (a, an, af) = chooseAlign sAlignment
  tellImport an
  tellDoc $ "instance ToCStruct" <+> pretty n <+> "where" <> line <> indent
    2
    (vsep
      [ "withCStruct x f ="
      <+> af (a <+> viaShow sSize)
      <+> "$ \\p -> pokeCStruct p x (f p)"
      , "pokeCStruct ::" <+> pokeCStructTDoc
      , "pokeCStruct"
      <+> addrVar
      <+> "= (. const) . runContT .  \\case"
      <>  line
      <>  indent 2 (vsep cases)
      , "pokeZeroCStruct ::" <+> pokeZeroCStructTDoc
      , "pokeZeroCStruct _ f = f"
      , "cStructSize =" <+> viaShow size
      , "cStructAlignment =" <+> viaShow alignment
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
  RenderParams {..} <- input
  let n = mkTyName msName
  -- Use the first member with size equal to the struct
  zeroableMembers <-
    fmap catMaybes
    . forV (toList msMembers)
    $ runNonDetMaybe
    . \MarshaledStructMember {..} -> do
        (unionSize, _) <- getTypeSize (TypeName msName)
        let (numElems, elemScheme) = case msmScheme of
              Tupled n e -> (n, e)
              e          -> (1, e)
        zero <- zeroScheme elemScheme >>= \case
          Nothing -> empty
          Just z  -> pure z
        let con = pretty (mkConName msName (smName msmStructMember))
        size <- case msmScheme of
          Normal        t     -> fst <$> getTypeSize t
          Preserve      t     -> fst <$> getTypeSize t
          WrappedStruct t     -> fst <$> getTypeSize (TypeName t)
          Tupled n (Normal e) -> do
            (tSize, _) <- getTypeSize e
            pure $ tSize * fromIntegral n
          Tupled n (Preserve e) -> do
            (tSize, _) <- getTypeSize e
            pure $ tSize * fromIntegral n
          _ -> empty
        guard (size == unionSize)
        pure (con <+> hsep (replicate (fromIntegral numElems) zero))
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
  RenderParams {..} <- input
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
    -- Should this be wrapped, or withHoles?
    ty    <- cToHsTypeWrapped DoPreserve (smType msmStructMember)
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
