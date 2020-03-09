module Render.Union
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , runReader
                                                , lift
                                                , State
                                                , Type
                                                )
import qualified Data.Vector                   as V
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State

import           Foreign.Ptr
import           Control.Monad.Trans.Cont       ( runContT )
import           Foreign.Marshal.Alloc
import           Control.Exception              ( bracket )

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type
import           Render.Poke             hiding ( Pure
                                                , Inline(..)
                                                )
import           Render.Peek
import           Render.Stmts
import           Marshal.Struct
import           Render.Scheme
import           Render.SpecInfo

renderUnion
  :: ( HasErr r
     , Member (Reader RenderParams) r
     , HasSpecInfo r
     )
  => MarshaledStruct AUnion
  -> Sem r RenderElement
renderUnion marshaled@MarshaledStruct {..} = context msName $ do
  RenderParams {..} <- ask
  let Struct {..} = msStruct
  genRe ("union " <> sName) $ do
    let n = mkTyName sName
    ms <- traverseV (renderUnionMember sName) msMembers
    tellExport (EData n)
    tellDoc $ "data" <+> pretty n <> line <> indent
      2
      (vsep $ zipWith (<+>) ("=" : repeat "|") (toList ms))

    let -- No useful information in the siblings in a union..
        lookupMember :: Text -> Maybe (SiblingInfo StructMember)
        lookupMember = const Nothing
    runReader lookupMember $ do

      toCStructInstance marshaled

      case
          [ d
          | d@UnionDiscriminator {..} <- toList unionDiscriminators
          , udUnionType == msName
          ]
        of
          []  -> pure ()
          [d] -> peekUnionFunction d marshaled
          _ -> throw ("Found multiple union discriminators for " <> n)

renderUnionMember
  :: ( HasErr r
     , MemberWithError (Reader RenderParams) r
     , MemberWithError (State RenderElement) r
     )
  => Text
  -- ^ union type name
  -> MarshaledStructMember
  -> Sem r (Doc ())
renderUnionMember tyName MarshaledStructMember {..} = do
  RenderParams {..} <- ask
  let StructMember {..} = msmStructMember
  let con                 = mkConName tyName smName
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
     )
  => MarshaledStruct AUnion
  -> Sem r ()
toCStructInstance MarshaledStruct {..} = do
  let addrVar = "p"
  pokes <- forV msMembers $ \MarshaledStructMember {..} -> do
    p <- getPoke msmStructMember msmScheme
    let p' =
          (\(Unassigned f) -> Assigned $ do
              t <- cToHsType DoPreserve (smType msmStructMember)
              unless (smOffset msmStructMember == 0)
                $ throw "Union has member with non-zero offset"
              tellImport 'castPtr
              let mVal = ValueDoc "v"
              f t (AddrDoc (parens ("castPtr" <+> addrVar))) mVal
            )
            <$> p
    pure p'

  RenderParams {..} <- ask
  tellImportWithAll (TyConName "ToCStruct")
  let Struct {..} = msStruct
      n           = mkTyName sName
      aVar        = mkVar "a"
      structT     = ConT (typeName n)
      mkCase :: StructMember -> AssignedPoke StructMember -> Sem r (Doc ())
      mkCase StructMember {..} poke = do
        let con = mkConName sName smName
            ty  = mkTyName sName
        pokeDoc <- renderPokesContT (V.singleton poke)
        tellImportWith (TyConName ty) (ConName con)
        pure $ pretty con <+> "v" <+> "->" <+> pokeDoc

  cases           <- zipWithM mkCase (toList sMembers) (toList pokes)

  pokeCStructTDoc <- renderType
    (ConT ''Ptr :@ structT ~> structT ~> ConT ''IO :@ aVar ~> ConT ''IO :@ aVar)

  withZeroCStructTDoc <-
    let retVar = VarT (typeName "b")
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

peekUnionFunction
  :: forall r
   . ( HasErr r
     , HasRenderParams r
     , HasRenderElem r
     , HasSiblingInfo StructMember r
     , HasSpecInfo r
     )
  => UnionDiscriminator
  -> MarshaledStruct AUnion
  -> Sem r ()
peekUnionFunction UnionDiscriminator {..} MarshaledStruct {..} = do
  RenderParams {..} <- ask
  let n        = mkTyName msName
      uTy      = ConT (typeName n)
      fName    = "peek" <> n
      discTy   = ConT (typeName (mkTyName udSiblingType))
      discName = "tag"
      ptrName  = "p"

  cases <- forV msMembers $ \MarshaledStructMember {..} -> do
    (pat, con) <-
      note "Unable to find union constructor in discriminant map"
        $ find ((== smName msmStructMember) . snd) udValueConstructorMap
    let pat' = mkPatternName pat
        con' = mkConName n con
    ty    <- cToHsType DoPreserve (smType msmStructMember)
    tyDoc <- renderTypeHighPrec ty
    tellImport 'castPtr
    let addr = AddrDoc ("castPtr @_ @" <> tyDoc <+> ptrName)
    tellImport (ConName pat')
    tellImportWith (TyConName n) (ConName con')
    let from   = smType msmStructMember
        scheme = msmScheme
    subPeek <- renderStmtsIO $ do
      addrRef <- pureStmt (ConT ''Ptr :@ ty) addr
      note "Nothing to peek to fill union with"
        =<< peekStmt msmStructMember addrRef scheme
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
