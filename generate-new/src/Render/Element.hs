module Render.Element
  ( genRe
  , emptyRenderElement
  , makeRenderElementInternal
  , HasRenderElem
  , HasRenderParams
  , RenderElement(..)
  , RenderParams(..)
  , Export(..)
  , ExportReexportable(..)
  , pattern ETerm
  , pattern EPat
  , pattern EData
  , pattern EClass
  , pattern EType
  , Import(..)
  , Name
  , NameSpace(..)
  , IdiomaticType(..)
  , IdiomaticTypeTo(..)
  , UnionDiscriminator(..)
  , LanguageExtension(..)
  , ModName(..)
  , Documentee(..)
  , tellDoc
  , tellDocWithHaddock
  , tellBoot
  , identicalBoot
  , tellExport
  , tellDataExport
  , tellExplicitModule
  , tellSourceImport
  , tellReexportMod
  , tellInternal
  , tellImportWithAll
  , tellQualImport
  , tellQualImportWithAll
  , tellImport
  , tellImportWith
  , tellNotReexportable
  , tellLanguageExtension
  , tellCanFormat
  , wrapSymbol
  , exportDoc
  , renderExport
  , thNameNamespace
  , nameNameSpace
  , nameSpacePrefix
  , mkModuleName
  , mkMkModuleName
  ) where

import           Data.Char                      ( isAlpha
                                                , isLower
                                                )
import           Data.Set                       ( insert )
import           Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector.Extra             as V
import           Data.Vector.Extra              ( pattern Empty
                                                , Vector
                                                )
import           Language.Haskell.TH.Syntax
                                         hiding ( ModName
                                                , Module
                                                , NameSpace
                                                , Stmt
                                                )
import           Polysemy
import           Polysemy.Input
import           Polysemy.State
import           Relude                  hiding ( Handle
                                                , State
                                                , Type
                                                , gets
                                                , modify'
                                                , runState
                                                )

import           CType
import           Documentation
import           Error
import           Haskell.Name
import {-# SOURCE #-} Render.Stmts
import           Render.Type.Preserve
import           Render.Utils
import           Spec.Name

-- It would be nice to distinguish the type of render element and boot to
-- prevent nesting, however without the polysemy plugin (doesn't work for me in
-- HIE) the type inference for this is a chore.
data RenderElement = RenderElement
  { reName              :: Text
  , reBoot              :: Maybe RenderElement
  , reDoc               :: (Documentee -> Doc ()) -> Maybe (Doc ())
  , reExports           :: Vector Export
  , reInternal          :: Vector Export
    -- ^ Things which can only be "imported" from the same module, i.e.
    -- declared names which arent exported
  , reImports           :: Set (Import Name)
  , reLocalImports      :: Set (Import HName)
  , reReexportedModules :: Set ModName
  , reReexportedNames   :: Set Export
  , reExplicitModule    :: Maybe ModName
  , reReexportable      :: All
  , reExtensions        :: Set LanguageExtension
  , reCanFormat         :: All
    -- Is this 'safe' to format with a code formatter
  }

data Export = Export
  { exportName         :: HName
  , exportWithAll      :: Bool
  , exportWith         :: Vector Export
  , exportReexportable :: ExportReexportable
  }
  deriving (Show, Eq, Ord)

data ExportReexportable = NotReexportable | Reexportable
  deriving (Show, Eq, Ord)

data NameSpace
  = Plain
  | Pattern
  | Module
  | Type
  deriving (Show, Eq, Ord)

data Import n = Import
  { importName      :: n
  , importQualified :: Bool
  , importChildren  :: Vector n
  , importWithAll   :: Bool
  , importSource    :: Bool
  }
  deriving (Show, Eq, Ord)

newtype LanguageExtension = LanguageExtension { unLanguageExtension :: Text }
  deriving (Eq, Ord, Show, Pretty)

newtype ModName = ModName { unModName :: Text }
  deriving (Eq, Ord, Show, Pretty)

instance Semigroup RenderElement where
  r1 <> r2 = RenderElement
    { reName              = reName r1 <> " and " <> reName r2
    , reBoot              = reBoot r1 <> reBoot r2
    , reDoc               = \h -> reDoc r1 h `lineMaybe` reDoc r2 h
    , reExports           = reExports r1 <> reExports r2
    , reInternal          = reInternal r1 <> reInternal r2
    , reImports           = reImports r1 <> reImports r2
    , reLocalImports      = reLocalImports r1 <> reLocalImports r2
    , reReexportedModules = reReexportedModules r1 <> reReexportedModules r2
    , reReexportedNames   = reReexportedNames r1 <> reReexportedNames r2
    , reExplicitModule    = reExplicitModule r1 <|> reExplicitModule r2
    , reReexportable      = reReexportable r1 <> reReexportable r2
    , reExtensions        = reExtensions r1 <> reExtensions r2
    , reCanFormat         = reCanFormat r1 <> reCanFormat r2
    }

lineMaybe :: Maybe (Doc ()) -> Maybe (Doc ()) -> Maybe (Doc ())
lineMaybe (Just d1) (Just d2) = Just (d1 <> line <> line <> d2)
lineMaybe d1        d2        = d1 <> d2

pattern ETerm :: HName -> Export
pattern ETerm n = Export n False Empty Reexportable

pattern EPat :: HName -> Export
pattern EPat n = Export n False Empty Reexportable

pattern EType :: HName -> Export
pattern EType n = Export n False Empty Reexportable

-- | Exports with (..)
pattern EData :: HName -> Export
pattern EData n = Export n True Empty Reexportable

-- | Exports with (..)
pattern EClass :: HName -> Export
pattern EClass n = Export n True Empty Reexportable

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
  Just DataName  -> Pattern
  _              -> Plain

----------------------------------------------------------------
-- Configuration
----------------------------------------------------------------

type HasRenderParams r = MemberWithError (Input RenderParams) r

data RenderParams = RenderParams
  { mkTyName :: CName -> HName
  , mkConName
      :: CName
      --  ^ Parent union or enum name
      -> CName
      -> HName
  , mkMemberName
      :: CName
      --  ^ Parent struct name
      -> CName
      -> HName
  , mkFunName                   :: CName -> HName
  , mkParamName                 :: CName -> HName
  , mkPatternName               :: CName -> HName
  , mkFuncPointerName           :: CName -> HName
    -- ^ Should be distinct from mkTyName
  , mkFuncPointerMemberName     :: CName -> HName
    -- ^ The name of function pointer members in the dynamic collection
  , mkEmptyDataName             :: CName -> HName
    -- ^ The name of the empty data declaration used to tag pointer to opaque
    -- types
  , mkDispatchableHandlePtrName :: CName -> HName
    -- ^ The record member name for the pointer member in dispatchable handles
  , camelPrefix                 :: Text
    -- ^ "Vk" or "Xr" or "Vma"
  , lowerPrefix                 :: Text
    -- ^ "vk" or "xr" or "vma"
  , upperPrefix                 :: Text
    -- ^ "VK" or "XR" or "VMA"
  , flagsTypeName               :: CName
    -- ^ "VkFlags" or "XrFlags64"
  , alwaysQualifiedNames        :: Vector Name
  , extraNewtypes               :: Vector Name
  , mkIdiomaticType             :: Type -> Maybe IdiomaticType
    -- ^ Overrides for using a different type than default on the Haskell side
    -- than the C side
  , mkHsTypeOverride
      :: forall r
       . ((Name -> ConstrainedVar) -> Sem r Name)
      --  ^ Get a fresh variable
      -> ExtensibleStructStyle r
      -> Preserve
      -> CType
      -> Maybe (Sem r Type)
    -- ^ Override for using a different type than default on the Haskell side
  , unionDiscriminators   :: Vector UnionDiscriminator
  , successCodeType       :: CType
  , isSuccessCodeReturned :: Text -> Bool
    -- ^ Is this success code returned from a function (failure codes are thrown)
    --
    -- use @const True@ to always return success codes
  , firstSuccessCode      :: CName
    -- Any code less than this is an error code
  , versionType           :: CType
  , exceptionTypeName     :: HName
    -- The name for the exception wrapper
  , complexMemberLengthFunction
      :: forall r
       . (HasRenderElem r, HasRenderParams r)
      => CName
      --  ^ Sibling name
      -> CName
      --  ^ Member name
      -> Doc ()
      --  ^ What you must use to refer to the sibling, as it may be different from the name
      -> Maybe (Sem r (Doc ()))
    -- Sometimes vectors are sized with the length of a member in an sibling
    -- (other parameter or member). Sometimes also this isn't a trivial case of
    -- getting that member of the struct, so use this field for writing those
    -- complex overrides.
  , isExternalName                 :: HName -> Maybe ModName
    -- ^ If you want to refer to something in another package without using
    -- template haskell ''quotes, put the module in here
  , externalDocHTML                :: Maybe Text
    -- ^ If we can't find a place in the generated source to link to, link to
    -- this documentation instead.
  , objectTypePattern              :: CName -> Maybe HName
    -- ^ An object type enumeration for a handle name, if any
  , extensibleStructTypeMemberName :: Maybe CName
    -- ^ "sType" or "type" or Nothing
  , extensibleStructTypeType       :: Maybe CName
    -- ^ "VkStructureType" or "XrStructureType"
  , modulePrefix                   :: Text
    -- ^ Where to put the generated module hierarchy
  , commandOverrides
      :: forall r
       . (HasRenderParams r, HasRenderElem r)
      => CName
      -> Maybe (Sem r ())
    -- ^ Instead of generating a command from the spec, use this definition
    -- instead
  }

data UnionDiscriminator = UnionDiscriminator
  { udUnionType           :: CName
    -- ^ The type of the union value
  , udSiblingType         :: CName
    -- ^ The (enum) type of the discriminator
  , udSiblingName         :: CName
    -- ^ The struct member which contains the discriminator
  , udValueConstructorMap :: [(CName, CName)]
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
  = PureFunction (Doc ())
  | IOFunction (Doc ())

----------------------------------------------------------------
-- Generating RenderElements
----------------------------------------------------------------

type HasRenderElem r = MemberWithError (State RenderElement) r

genRe :: Text -> Sem (State RenderElement : r) () -> Sem r RenderElement
genRe n m = do
  (o, _) <- runState (emptyRenderElement n) m
  pure o

tellBoot :: HasRenderElem r => Sem (State RenderElement : r) () -> Sem r ()
tellBoot m = do
  n      <- gets reName
  (b, _) <- runState (emptyRenderElement (n <> " boot")) m
  modify' (\r -> r { reBoot = reBoot r <> Just b })

identicalBoot :: RenderElement -> RenderElement
identicalBoot re = re { reBoot = Just re }

emptyRenderElement :: Text -> RenderElement
emptyRenderElement n = RenderElement { reName              = n
                                     , reBoot              = Nothing
                                     , reDoc               = mempty
                                     , reExports           = mempty
                                     , reInternal          = mempty
                                     , reImports           = mempty
                                     , reLocalImports      = mempty
                                     , reReexportedModules = mempty
                                     , reReexportedNames   = mempty
                                     , reExplicitModule    = Nothing
                                     , reReexportable      = mempty
                                     , reExtensions        = mempty
                                     , reCanFormat         = All False
                                     }

-- | Prevent any exports from being in the module export list
makeRenderElementInternal :: RenderElement -> RenderElement
makeRenderElementInternal re =
  re { reExports = mempty, reInternal = reExports re <> reInternal re }

tellExplicitModule :: (HasRenderElem r, HasErr r) => ModName -> Sem r ()
tellExplicitModule mod = gets reExplicitModule >>= \case
  Nothing -> modify' (\r -> r { reExplicitModule = Just mod })
  Just m | m == mod -> pure ()
  _ -> throw "Render element has been given two explicit modules"

tellExport :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellExport e = modify' (\r -> r { reExports = reExports r <> V.singleton e })

-- | A convenience for declaraing a data declaration and also putting it in the
-- hs boot file
tellDataExport :: MemberWithError (State RenderElement) r => HName -> Sem r ()
tellDataExport e = do
  let dat = EData e
  modify' (\r -> r { reExports = reExports r <> V.singleton dat })
  tellBoot $ do
    tellExport dat { exportWithAll = False }
    tellDoc $ "data" <+> pretty e

tellInternal :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellInternal e =
  modify' (\r -> r { reInternal = reInternal r <> V.singleton e })

tellCanFormat :: MemberWithError (State RenderElement) r => Sem r ()
tellCanFormat = modify' (\r -> r { reCanFormat = All True })

tellDoc :: MemberWithError (State RenderElement) r => Doc () -> Sem r ()
tellDoc d = modify' (\r -> r { reDoc = \h -> reDoc r h `lineMaybe` Just d })

tellDocWithHaddock
  :: MemberWithError (State RenderElement) r
  => ((Documentee -> Doc ()) -> Doc ())
  -> Sem r ()
tellDocWithHaddock d =
  modify' (\r -> r { reDoc = \h -> reDoc r h `lineMaybe` Just (d h) })

class Importable a where
  addImport
    :: ( HasRenderElem r
       , HasRenderParams r
       )
    => Import a
    -> Sem r ()

instance Importable Name where
  addImport import'@(Import i qual children withAll source) = do
    RenderParams {..} <- input
    -- TODO: Throw a an error here if we are not qual and i is in
    -- alwaysQualifiedNames
    let mkLocalName n =
          let b = T.pack . nameBase $ n
          in  case nameSpace n of
                Just TcClsName         -> TyConName b
                Just DataName          -> ConName b
                Just VarName           -> TermName b
                _ | isLower (T.head b) -> TermName b
                -- TODO: Defaults to TyCon over patterns
                _                      -> TyConName b
        localName = mkLocalName i
    withModule <- fromMaybe i <$> addModName localName
    case nameModule withModule of
      Just _ -> do
        -- This is an name in an external library, don't import with source
        RenderParams {..} <- input
        let q = qual || V.elem withModule alwaysQualifiedNames
        modify' $ \r -> r
          { reImports = insert (Import withModule q children withAll False)
                               (reImports r)
          }
      Nothing -> case isExternalName localName of
        Just (ModName modName) -> addImport import'
          { importName = mkName (T.unpack modName <> "." <> nameBase withModule)
          }
        Nothing -> addImport
          (Import localName qual (mkLocalName <$> children) withAll source)

instance Importable HName where
  addImport i = addModName (importName i) >>= \case
    Just n -> addImport i
      { importName     = n
      , importChildren = mkName . T.unpack . unName <$> importChildren i
      }
    Nothing ->
      modify' $ \r -> r { reLocalImports = insert i (reLocalImports r) }

addModName :: HasRenderParams r => HName -> Sem r (Maybe Name)
addModName n = do
  let mkName' m n = case n of
        TyConName s -> mkNameG_tc "" (T.unpack m) (T.unpack s)
        ConName   s -> mkNameG_d "" (T.unpack m) (T.unpack s)
        TermName  s -> mkNameG_v "" (T.unpack m) (T.unpack s)
  RenderParams {..} <- input
  pure $ isExternalName n <&> \(ModName modName) -> mkName' modName n

tellSourceImport, tellImportWithAll, tellQualImport, tellQualImportWithAll, tellImport
  :: (HasRenderElem r, HasRenderParams r, Importable a) => a -> Sem r ()
tellImport a = addImport (Import a False Empty False False)
tellSourceImport a = addImport (Import a False Empty False True)
tellImportWithAll a = addImport (Import a False Empty True False)
tellQualImport a = addImport (Import a True Empty False False)
tellQualImportWithAll a = addImport (Import a True Empty True False)

tellImportWith
  :: (HasRenderElem r, HasRenderParams r, Importable a) => a -> a -> Sem r ()
tellImportWith parent dat =
  addImport (Import parent False (V.singleton dat) False False)

tellReexportMod
  :: MemberWithError (State RenderElement) r => ModName -> Sem r ()
tellReexportMod e =
  modify' (\r -> r { reReexportedModules = insert e (reReexportedModules r) })

tellNotReexportable :: MemberWithError (State RenderElement) r => Sem r ()
tellNotReexportable = modify' (\r -> r { reReexportable = All False })

tellLanguageExtension
  :: MemberWithError (State RenderElement) r => LanguageExtension -> Sem r ()
tellLanguageExtension e =
  modify' (\r -> r { reExtensions = insert e (reExtensions r) })

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

mkModuleName :: HasRenderParams r => [Text] -> Sem r ModName
mkModuleName cs = do
  RenderParams {..} <- input
  pure . ModName . T.intercalate "." . (modulePrefix :) $ cs

mkMkModuleName :: HasRenderParams r => Sem r ([Text] -> ModName)
mkMkModuleName = do
  RenderParams {..} <- input
  pure $ ModName . T.intercalate "." . (modulePrefix :)
