module Render.Element
  where

import           Relude                  hiding ( runState
                                                , State
                                                , modify'
                                                , Reader
                                                , asks
                                                , ask
                                                , runReader
                                                , Handle
                                                , Type
                                                )
import qualified Data.Vector.Extra             as V
import           Data.Vector.Extra              ( Vector
                                                , pattern Empty
                                                )
import           Data.Text                     as T
import           Data.Set                       ( insert )
import           Data.Text.Prettyprint.Doc
import           Data.Char                      ( isAlpha )
import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Language.Haskell.TH            ( Name
                                                , nameBase
                                                , nameModule
                                                , Type
                                                )
import           Language.Haskell.TH.Syntax
                                         hiding ( NameSpace
                                                , ModName
                                                , Module
                                                )

import           Render.Utils
import           Haskell.Name

data RenderElement = RenderElement
  { reName     :: Text
  , reDoc      :: Doc ()
  , reExports  :: Vector Export
  , reInternal :: Vector Export
    -- ^ Things which can only be "imported" from the same module, i.e.
    -- declared names which arent exported
  , reImports      :: Set (Import Name)
  , reLocalImports :: Set (Import HName)
  , reReexports    :: Set ModName
  }

data Export = Export
  { exportName    :: HName
  , exportWithAll :: Bool
  , exportWith    :: Vector Export
  }
  deriving (Show, Eq, Ord)

data NameSpace
  = Plain
  | Pattern
  | Module
  | Type
  deriving (Show, Eq, Ord)

data Import n = Import
  { importName :: n
  , importQualified :: Bool
  , importChildren :: Vector n
  , importWithAll :: Bool
  }
  deriving (Show, Eq, Ord)

newtype ModName = ModName { unModName :: Text }
  deriving (Eq, Ord, Show)

pattern ETerm :: Text -> Export
pattern ETerm n = Export (TermName n) False Empty

pattern EPat :: Text -> Export
pattern EPat n = Export (ConName n) False Empty

pattern EData :: Text -> Export
pattern EData n = Export (TyConName n) True Empty

pattern EClass :: Text -> Export
pattern EClass n = Export (TyConName n) True Empty

pattern EType :: Text -> Export
pattern EType n = Export (TyConName n) False Empty

exportDoc :: Export -> Doc ()
exportDoc Export {..} = renderExport
  (nameNameSpace exportName)
  (unName exportName)
  (  (unName . Render.Element.exportName <$> exportWith)
  <> (if exportWithAll then V.singleton ".." else V.empty)
  )

renderExport :: NameSpace -> Text -> Vector Text -> Doc ()
renderExport ns p cs =
  let spec       = nameSpacePrefix ns
      subExports = if V.null cs then "" else parenList . fmap wrapSymbol $ cs
  in  (spec <>) . (<> subExports) . wrapSymbol $ p

nameSpacePrefix :: NameSpace -> Doc ()
nameSpacePrefix = \case
  Plain   -> ""
  Pattern -> "pattern "
  Module  -> "module "
  Type    -> "type "

nameNameSpace :: HName -> NameSpace
nameNameSpace = \case
  ConName   _ -> Pattern
  TyConName _ -> Type
  _           -> Plain

thNameNamespace :: Name -> NameSpace
thNameNamespace n = case nameSpace n of
  Just TcClsName -> Type
  _              -> Plain

----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

type HasRenderParams r = MemberWithError (Reader RenderParams) r

data RenderParams = RenderParams
  { mkTyName          :: Text -> Text
  , mkConName         :: Text
                      -- ^ Parent union or enum name
                      -> Text -> Text
  , mkMemberName      :: Text -> Text
  , mkFunName         :: Text -> Text
  , mkParamName       :: Text -> Text
  , mkPatternName     :: Text -> Text
  , mkHandleName      :: Text -> Text
  , mkFuncPointerName :: Text -> Text
    -- ^ Should be distinct from mkTyName
  , mkFuncPointerMemberName :: Text -> Text
    -- ^ The name of function pointer members in the dynamic collection
  , alwaysQualifiedNames :: Vector Name
  , mkIdiomaticType   :: Type -> Maybe IdiomaticType
  }

data IdiomaticType = IdiomaticType
  { itType :: Type
  , itFrom :: forall r. (HasRenderElem r, HasRenderParams r) => Sem r (Doc ())
  , itTo :: forall r. (HasRenderElem r, HasRenderParams r) => Sem r (Either (Doc ()) (Doc ()))
    -- ^ Either a constructor for matching, or a term for applying
  }

----------------------------------------------------------------
-- Generating RenderElements
----------------------------------------------------------------

type HasRenderElem r = MemberWithError (State RenderElement) r

genRe :: Text -> Sem (State RenderElement : r) () -> Sem r RenderElement
genRe n m = do
  (o, _) <- runState
    RenderElement { reName         = n
                  , reDoc          = mempty
                  , reExports      = mempty
                  , reInternal     = mempty
                  , reImports      = mempty
                  , reLocalImports = mempty
                  , reReexports    = mempty
                  }
    m
  pure o

tellExport :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellExport e = modify' (\r -> r { reExports = reExports r <> V.singleton e })

tellInternal :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellInternal e =
  modify' (\r -> r { reInternal = reInternal r <> V.singleton e })

tellDoc :: MemberWithError (State RenderElement) r => Doc () -> Sem r ()
tellDoc d = modify' (\r -> r { reDoc = reDoc r <> hardline <> d })

class Importable a where
  addImport
    :: ( MemberWithError (State RenderElement) r
       , MemberWithError (Reader RenderParams) r
       )
    => Import a
    -> Sem r ()

-- TODO Reduce duplication here..
instance Importable Name where
  addImport (Import i qual children withAll) = case nameModule i of
    Just _ -> do
      RenderParams {..} <- ask
      let q = qual || V.elem i alwaysQualifiedNames
      modify' $ \r ->
        r { reImports = insert (Import i q children withAll) (reImports r) }
    Nothing -> do
      let mkLocalName = TyConName . T.pack . nameBase
      addImport (Import (mkLocalName i) qual (mkLocalName <$> children) withAll)

instance Importable HName where
  addImport i =
    modify' $ \r -> r { reLocalImports = insert i (reLocalImports r) }

tellImport
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> Sem r ()
tellImport a = addImport (Import a False Empty False)

tellImportWithAll
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> Sem r ()
tellImportWithAll a = addImport (Import a False Empty True)

tellQualImport
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> Sem r ()
tellQualImport a = addImport (Import a True Empty False)

tellQualImportWithAll
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> Sem r ()
tellQualImportWithAll a = addImport (Import a True Empty True)

tellConImport
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> a
  -> Sem r ()
tellConImport parent dat =
  addImport (Import parent False (V.singleton dat) False)

tellReexport :: MemberWithError (State RenderElement) r => ModName -> Sem r ()
tellReexport e = modify' (\r -> r { reReexports = insert e (reReexports r) })

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

wrapSymbol :: Text -> Doc ()
wrapSymbol s = if isSymbol s && s /= ".." then parens (pretty s) else pretty s
  where isSymbol = not . (\x -> isAlpha x || (x == '_')) . T.head

