module Render.Element.Write
  where

import           Relude                  hiding ( runState
                                                , State
                                                , modify'
                                                , Reader
                                                , asks
                                                , ask
                                                , runReader
                                                , Handle
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Vector.Extra             as V
import           Data.Vector.Extra              ( Vector )
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Set                       ( unions )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Directory
import           System.FilePath
import           Polysemy
import           Polysemy.Reader
import           Data.List                      ( lookup )
import           Foreign.Ptr
import           Language.Haskell.TH            ( Name
                                                , nameBase
                                                , nameModule
                                                , mkName
                                                )

import qualified Data.Vector.Generic           as VG

import           Render.Utils
import           Render.Element
import           Error
import           Write.Segment
import           Spec.Types
import           Haskell.Name


----------------------------------------------------------------
-- Segmenting
----------------------------------------------------------------

segmentRenderElements
  :: HasErr r
  => (i -> Text)
  -> Vector RenderElement
  -> Vector (SegmentGroup i HName)
  -> Sem r (V.Vector (SegmentedGroup i RenderElement))
segmentRenderElements debugI =
  let elementExports RenderElement {..} =
          reInternal <> reExports <> V.concatMap exportWith reExports
  in  segmentGraph
        reName
        show
        debugI
        (fmap exportName . V.toList . elementExports)
        (fmap importName . toList . reLocalImports)

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

renderSegments
  :: (HasErr r, MemberWithError (Embed IO) r, HasTypeInfo r)
  => FilePath
  -> [Segment ModName RenderElement]
  -> Sem r ()
renderSegments out segments = do
  let
    exportMap :: Map.Map HName ModName
    exportMap = Map.fromList
      [ (n, m)
      | Segment m rs <- segments
      , r            <- toList rs
      , e            <- toList (reExports r)
      , n <- exportName e : (exportName <$> V.toList (exportWith e))
      ]
    findLocalModule :: HasErr r => Import HName -> Sem r ModName
    findLocalModule (Import n _ _ _) =
      maybe (throw ("Unable to find import " <> show n)) pure
        $ Map.lookup n exportMap
    findModule :: HasErr r => Import Name -> Sem r ModName
    findModule (Import n _ _ _) = maybe
      (throw ("Unable to find import " <> show n))
      pure
      (ModName . T.pack <$> nameModule n)
  traverseV_ (renderModule out findModule findLocalModule) segments

renderModule
  :: (MemberWithError (Embed IO) r, HasErr r, HasTypeInfo r)
  => FilePath
  -- ^ out directory
  -> (Import Name -> Sem r ModName)
  -> (Import HName -> Sem r ModName)
  -> Segment ModName RenderElement
  -> Sem r ()
renderModule out findModule findLocalModule (Segment (ModName modName) es) = do
  let
    f = toString (out <> "/" <> T.unpack (T.replace "." "/" modName <> ".hs"))
    openImports = vsep
      ( fmap (\(ModName n) -> "import" <+> pretty n)
      . Set.toList
      . Set.unions
      $ (reReexports <$> V.toList es)
      )
    declaredNames = V.concatMap
      (\RenderElement {..} -> allExports (reExports <> reInternal))
      es
    importFilter =
      Relude.filter (\(Import n _ _ _) -> n `V.notElem` declaredNames)
  imports <- vsep <$> traverseV
    (renderImport findModule (T.pack . nameBase) thNameNamespace)
    ( mapMaybe
        (\i -> do
          n <- if V.null (importChildren i)
            then fixOddImport (importName i)
            else Just (importName i)
          pure i { importName = n }
        )
    . Relude.toList
    . unions
    $ (reImports <$> V.toList es)
    )
  let allLocalImports =
        Relude.toList . unions . fmap reLocalImports . V.toList $ es
  parentedImports <- traverse adoptConstructors allLocalImports
  localImports    <- vsep <$> traverseV
    (renderImport findLocalModule unName nameNameSpace)
    (importFilter parentedImports)
  let
    allReexports =
      V.fromList . Set.toList . Set.unions . fmap reReexports . toList $ es
    contents =
      vsep
        $   "module"
        <+> pretty modName
        <>  indent
              2
              (parenList
                (  (exportDoc <$> V.concatMap reExports es)
                <> (   (\(ModName m) -> renderExport Module m mempty)
                   <$> allReexports
                   )
                )
              )
        <+> "where"
        :   openImports
        :   imports
        :   localImports
        :   V.toList (reDoc <$> es)
  liftIO $ createDirectoryIfMissing True (takeDirectory f)
  liftIO $ withFile f WriteMode $ \h -> T.hPutStr h $ renderStrict
    (layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded } contents)

allExports :: Vector Export -> Vector HName
allExports =
  V.concatMap (\Export {..} -> exportName `V.cons` allExports exportWith)

renderImport
  :: (HasErr r, HasTypeInfo r)
  => (Import a -> Sem r ModName)
  -> (a -> Text)
  -> (a -> NameSpace)
  -> Import a
  -> Sem r (Doc ())
renderImport findModule getName getNameSpace i@(Import n qual children withAll)
  = do
    ModName mod' <- findModule i
    let qualDoc     = bool "" " qualified" qual
        base        = getName n
        baseP       = wrapSymbol base
        spec        = nameSpacePrefix (getNameSpace n)
        childrenDoc = if V.null children && not withAll
          then ""
          else parenList
            (  (wrapSymbol . getName <$> children)
            <> (if withAll then V.singleton ".." else V.empty)
            )
    pure $ "import" <> qualDoc <+> pretty mod' <+> parenList
      (V.singleton (spec <> baseP <> childrenDoc))

fixOddImport :: Name -> Maybe Name
fixOddImport n = fromMaybe (Just n) (lookup n fixes)
 where
  fixes =
    [ -- Prelude types
      (''Maybe     , Nothing)
    , (''Word      , Nothing)
    , (''()        , Nothing)
    , (''IO        , Nothing)
    , (''Integral  , Nothing)
    , (''Eq        , Nothing)
    , (''Float     , Nothing)
    , (''Double    , Nothing)
    , (''Int       , Nothing)
    ,
      -- Base
      (''Int8      , Just (mkName "Data.Int.Int8"))
    , (''Int16     , Just (mkName "Data.Int.Int16"))
    , (''Int32     , Just (mkName "Data.Int.Int32"))
    , (''Int64     , Just (mkName "Data.Int.Int64"))
    , (''Word8     , Just (mkName "Data.Word.Word8"))
    , (''Word16    , Just (mkName "Data.Word.Word16"))
    , (''Word32    , Just (mkName "Data.Word.Word32"))
    , (''Word64    , Just (mkName "Data.Word.Word64"))
    , (''Ptr       , Just (mkName "Foreign.Ptr.Ptr"))
    , (''FunPtr    , Just (mkName "Foreign.Ptr.FunPtr"))
    , ('nullPtr    , Just (mkName "Foreign.Ptr.nullPtr"))
    , ('castFunPtr, Just (mkName "Foreign.Ptr.castFunPtr"))
    , ('plusPtr    , Just (mkName "Foreign.Ptr.plusPtr"))
    ,
      -- Other
      (''ByteString, Just (mkName "Data.ByteString.ByteString"))
    , (''VG.Vector, Just (mkName "Data.Vector.Generic.Vector"))
    ]

----------------------------------------------------------------
--
----------------------------------------------------------------

-- Sometimes we need to lookup the type of a constructor or the level of a handle
data TypeInfo = TypeInfo
  { tiConMap :: HName -> Maybe HName
  , tiIsHandle :: HName -> Maybe Handle
  , tiIsCommand :: HName -> Maybe Command
  }

type HasTypeInfo r = MemberWithError (Reader TypeInfo) r

withTypeInfo :: Spec -> Sem (Reader TypeInfo ': r) a -> Sem r a
withTypeInfo Spec {..} =
  let
    tyMap :: Map HName HName
    tyMap = Map.fromList
      [ (ConName evName, TyConName eName)
      | Enum {..}      <- V.toList specEnums
      , EnumValue {..} <- V.toList eValues
      ]
    handleMap :: Map HName Handle
    handleMap = Map.fromList
      [ (TyConName hName, h) | h@Handle {..} <- V.toList specHandles ]
    commandMap :: Map HName Command
    commandMap = Map.fromList
      [ (TermName cName, c) | c@Command {..} <- V.toList specCommands ]
  in
    runReader
      (TypeInfo (`Map.lookup` tyMap)
                (`Map.lookup` handleMap)
                (`Map.lookup` commandMap)
      )

adoptConstructors :: HasTypeInfo r => Import HName -> Sem r (Import HName)
adoptConstructors = \case
  i@(Import n q cs _) -> getConParent n >>= pure . \case
    Just p  -> Import p q (V.singleton n <> cs) False
    Nothing -> i
  where getConParent n = asks (`tiConMap` n)

getHandle :: HasTypeInfo r => HName -> Sem r (Maybe Handle)
getHandle n = asks (`tiIsHandle` n)

getCommand :: HasTypeInfo r => HName -> Sem r (Maybe Command)
getCommand n = asks (`tiIsCommand` n)

----------------------------------------------------------------
--
----------------------------------------------------------------
