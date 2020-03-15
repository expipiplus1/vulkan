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
import           Language.Haskell.TH.Syntax
                                         hiding ( NameSpace
                                                , ModName
                                                , Module
                                                , Stmt
                                                )

import           Error
import           Render.Utils
import           Haskell.Name
import           CType
import           Render.Type.Preserve
import {-# SOURCE #-} Render.Stmts

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

instance Semigroup RenderElement where
  r1 <> r2 = RenderElement
    { reName         = reName r1 <> " and " <> reName r2
    , reDoc          = reDoc r1 <> line <> reDoc r2
    , reExports      = reExports r1 <> reExports r2
    , reInternal     = reInternal r1 <> reInternal r2
    , reImports      = reImports r1 <> reImports r2
    , reLocalImports = reLocalImports r1 <> reLocalImports r2
    , reReexports    = reReexports r1 <> reReexports r2
    }

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
  let spec = nameSpacePrefix ns
      subExports =
        if V.null cs then "" else parenList . fmap (wrapSymbol ns) $ cs
  in  (spec <>) . (<> subExports) . wrapSymbol ns $ p

nameSpacePrefix :: NameSpace -> Doc ()
nameSpacePrefix = \case
  Plain   -> ""
  Pattern -> "pattern "
  Module  -> "module "
  -- hlint fails https://github.com/ndmitchell/hlint/issues/384
  Type    -> ""

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
  { mkTyName :: Text -> Text
  , mkConName
      :: Text
      -- ^ Parent union or enum name
      -> Text
      -> Text
  , mkMemberName                :: Text -> Text
  , mkFunName                   :: Text -> Text
  , mkParamName                 :: Text -> Text
  , mkPatternName               :: Text -> Text
  , mkHandleName                :: Text -> Text
  , mkFuncPointerName           :: Text -> Text
    -- ^ Should be distinct from mkTyName
  , mkFuncPointerMemberName     :: Text -> Text
    -- ^ The name of function pointer members in the dynamic collection
  , mkEmptyDataName             :: Text -> Text
    -- ^ The name of the empty data declaration used to tag pointer to opaque
    -- types
  , mkDispatchableHandlePtrName :: Text -> Text
    -- ^ The record member name for the pointer member in dispatchable handles
  , alwaysQualifiedNames        :: Vector Name
  , mkIdiomaticType             :: Type -> Maybe IdiomaticType
    -- ^ Overrides for using a different type than default on the Haskell side
    -- than the C side
  , mkHsTypeOverride            :: Preserve -> CType -> Maybe Type
    -- ^ Override for using a different type than default on the Haskell sied
  , unionDiscriminators         :: Vector UnionDiscriminator
  , successCodeType             :: CType
  , isSuccessCodeReturned       :: Text -> Bool
    -- ^ Is this success code returned from a function (failure codes are thrown)
    --
    -- use @const True@ to always return success codes
  , firstSuccessCode            :: Text
    -- Any code less than this is an error code
  , exceptionTypeName           :: Text
    -- The name for the exception wrapper
  , complexMemberLengthFunction
      :: forall r
       . (HasRenderElem r, HasRenderParams r)
      => Text
      -- ^ Sibling name
      -> Text
      -- ^ Member name
      -> Doc ()
      -- ^ What you must use to refer to the sibling, as it may be different from the name
      -> Maybe (Sem r (Doc ()))
    -- Sometimes vectors are sized with the length of a member in an sibling
    -- (other parameter or member). Sometimes also this isn't a trivial case of
    -- getting that member of the struct, so use this field for writing those
    -- complex overrides.
  }

data UnionDiscriminator = UnionDiscriminator
  { udUnionType           :: Text
    -- ^ The type of the union value
  , udSiblingType         :: Text
    -- ^ The (enum) type of the discriminator
  , udSiblingName         :: Text
    -- ^ The struct member which contains the discriminator
  , udValueConstructorMap :: [(Text, Text)]
    -- ^ A map of (enumerant, union member) name
  }

data IdiomaticType = IdiomaticType
  { itType :: Type
    -- Wrapped type, for example Float
  , itFrom
      :: forall r k (s :: k)
       . (HasRenderElem r, HasRenderParams r, HasErr r)
      => Stmt s r (Doc ())
    -- A function to apply to go from Float to CFloat
  , itTo
      :: forall r k (s :: k)
       . (HasRenderElem r, HasRenderParams r, HasErr r)
      => Stmt s r IdiomaticTypeTo
    -- ^ Either a constructor for matching, or a term for applying, to go from CFloat to Float
  }

data IdiomaticTypeTo
  = Constructor (Doc ())
  | PureFunction (Doc ())
  | IOFunction (Doc ())

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

tellImportWith
  :: ( MemberWithError (State RenderElement) r
     , MemberWithError (Reader RenderParams) r
     , Importable a
     )
  => a
  -> a
  -> Sem r ()
tellImportWith parent dat =
  addImport (Import parent False (V.singleton dat) False)

tellReexport :: MemberWithError (State RenderElement) r => ModName -> Sem r ()
tellReexport e = modify' (\r -> r { reReexports = insert e (reReexports r) })

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Add parens around operaors, and a "type" namespace specifier for type
-- operators
wrapSymbol :: NameSpace -> Text -> Doc ()
wrapSymbol ns s = if isSymbol s && s /= ".."
  then if ns == Type && T.head s /= ':'
    then "type" <> parens (pretty s)
    else parens (pretty s)
  else pretty s
  where isSymbol = not . (\x -> isAlpha x || (x == '_')) . T.head

