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
import           Data.List.Extra                ( nubOrd
                                                , groupOn
                                                , nubOrdOn
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Directory
import           System.FilePath
import           Polysemy
import           Polysemy.Reader
import           Data.List                      ( lookup )
import           Foreign.Ptr
import           Data.Version
import           Language.Haskell.TH            ( nameBase
                                                , nameModule
                                                , mkName
                                                )

import qualified Data.Vector.Generic           as VG
import           Type.Reflection

import           Render.Utils
import           Render.Names
import           Render.SpecInfo
import           Render.Element
import           Error
import           Write.Segment
import           Spec.Types
import           Haskell.Name
import           Bespoke.Seeds
import           Documentation
import           Documentation.Haddock

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

separateTypes
  :: Vector (SegmentedGroup ModulePlacement RenderElement)
  -> [Segment ModName RenderElement]
separateTypes segmented =
  [ ss'
  | SegmentedGroup segments extras <- toList segmented
  , ss                             <- extras : toList segments
  , ss'                            <- makeModName ss
  ]

makeModName
  :: Segment ModulePlacement RenderElement -> [Segment ModName RenderElement]
makeModName (Segment placement es) = case placement of
  BespokeMod mod        -> [Segment (ModName ("Graphics.Vulkan." <> mod)) es]
  CoreMod ver component ->
    let prefix = "Graphics.Vulkan.Core" <> (foldMap show (versionBranch ver)) <> "."
    in splitTypes prefix component es
  ExtensionMod component ->
    let prefix = "Graphics.Vulkan.Extensions."
    in splitTypes prefix component es

splitTypes
  :: Text -> Text -> Vector RenderElement -> [Segment ModName RenderElement]
splitTypes prefix component es = toList es <&> \re ->
  case mapMaybe getTyConName (exportName <$> toList (reExports re)) of
    []    -> Segment (ModName (prefix <> component)) (V.singleton re)
    x : _ -> Segment (ModName (prefix <> "Types." <> x)) (V.singleton re)

getTyConName :: HName -> Maybe Text
getTyConName = \case
  TyConName n -> Just n
  _ -> Nothing

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

renderSegments
  :: forall r
   . ( HasErr r
     , MemberWithError (Embed IO) r
     , HasSpecInfo r
     , HasTypeInfo r
     , HasRenderedNames r
     , HasRenderParams r
     )
  => (Documentee -> Maybe Documentation)
  -> FilePath
  -> [Segment ModName RenderElement]
  -> Sem r ()
renderSegments getDoc out segments = do
  let exportMap :: Map.Map HName ModName
      exportMap = Map.fromList
        [ (n, m)
        | Segment m rs <- segments
        , r            <- toList rs
        , e            <- toList (reExports r)
        , n <- exportName e : (exportName <$> V.toList (exportWith e))
        ]
      findLocalModule :: HName -> Maybe ModName
      findLocalModule n = Map.lookup n exportMap
      findModule :: Name -> Maybe ModName
      findModule n = ModName . T.pack <$> nameModule n

      --
      -- Boot module handling
      -- TODO, move this checking elsewhere
      --
      allBootSegments :: [Segment ModName RenderElement]
      allBootSegments =
        Relude.filter (\(Segment _ es) -> not (V.null es))
          $   segments
          <&> \(Segment m es) -> Segment m (V.mapMaybe reBoot es)

      findBootElems :: HName -> Sem r (ModName, RenderElement)
      findBootElems =
        let bootElemMap :: Map HName (ModName, RenderElement)
            bootElemMap = Map.fromList
              [ (n, (m, re))
              | Segment m res <- allBootSegments
              , re            <- toList res
              , e             <- toList (reExports re)
              , n <- exportName e : (exportName <$> V.toList (exportWith e))
              ]
        in  \n ->
              note @r ("Unable to find boot element for " <> show n)
                $ Map.lookup n bootElemMap

      sourceImportNames = nubOrd
        [ n
        | Segment _ res <- segments
        , re            <- toList res
        , Import { importName = n, importSource = True } <- toList
          (reLocalImports re <> maybe mempty reLocalImports (reBoot re))
        ]

  -- TODO: do this segmentation properly, nubbing here is nasty
  requiredBootElements <-
    nubOrdOn (reExports . snd) <$> forV sourceImportNames findBootElems

  let requiredBootSegments =
        fmap (\((m, re) : res) -> Segment m (fromList (re : (snd <$> res))))
          . groupOn fst
          . sortOn fst
          $ requiredBootElements

  --
  -- Write the files
  --
  traverseV_ (renderModule out False getDoc findModule findLocalModule) segments
  traverseV_ (renderModule out True getDoc findModule findLocalModule)
             requiredBootSegments

renderModule
  :: ( MemberWithError (Embed IO) r
     , HasErr r
     , HasSpecInfo r
     , HasTypeInfo r
     , HasRenderedNames r
     , HasRenderParams r
     )
  => FilePath
  -- ^ out directory
  -> Bool
  -- ^ Is a boot file
  -> (Documentee -> Maybe Documentation)
  -> (Name -> Maybe ModName)
  -> (HName -> Maybe ModName)
  -> Segment ModName RenderElement
  -> Sem r ()
renderModule out boot getDoc findModule findLocalModule (Segment modName es) =
  do
    RenderParams {..} <- ask
    let
      ext = bool ".hs" ".hs-boot" boot
      f =
        toString
          (out <> "/" <> T.unpack (T.replace "." "/" (unModName modName) <> ext)
          )
      openImports = vsep
        ( fmap (\(ModName n) -> "import" <+> pretty n)
        . Set.toList
        . Set.unions
        $ (reReexportedModules <$> V.toList es)
        )
      declaredNames = V.concatMap
        (\RenderElement {..} -> allExports (reExports <> reInternal))
        es
      importFilter =
        Relude.filter (\(Import n _ _ _ _) -> n `V.notElem` declaredNames)
      findModule' n =
        note ("Unable to find module for " <> show n) (findModule n)
      findLocalModule' n =
        note ("Unable to find module for " <> show n) (findLocalModule n)
    imports <- vsep <$> traverseV
      (renderImport findModule' (T.pack . nameBase) thNameNamespace id)
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
    resolveAlias    <- getResolveAlias
    parentedImports <- traverse adoptConstructors allLocalImports
    localImports    <- vsep <$> traverseV
      (renderImport findLocalModule' unName nameNameSpace resolveAlias)
      (importFilter parentedImports)
    let
      locate :: CName -> DocumenteeLocation
      locate n =
        let names = [mkTyName n, mkFunName n, mkPatternName n]
        in  case asum ((\n -> (n, ) <$> findLocalModule n) <$> names) of
              Just (n, m) | m == modName -> ThisModule n
              Just (n, m)                -> OtherModule m n
              Nothing                    -> Unknown

      getDocumentation :: Documentee -> Doc ()
      getDocumentation target = case getDoc target of
        Nothing -> "-- No documentation found for" <+> viaShow target
        Just d  -> case documentationToHaddock locate d of
          Left e ->
            "-- Error getting documentation for"
              <+> viaShow target
              <>  ":"
              <+> viaShow e
          Right (Haddock t) -> commentNoWrap t
      allReexports =
        V.fromList
          . Set.toList
          . Set.unions
          . fmap reReexportedModules
          . toList
          $ es
      contents =
        vsep
          $ "{-# language CPP #-}"
          : (   "module"
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
            )
          : openImports
          : imports
          : localImports
          : V.toList (($ getDocumentation) . reDoc <$> es)
    liftIO $ createDirectoryIfMissing True (takeDirectory f)
    liftIO $ withFile f WriteMode $ \h -> T.hPutStr h $ renderStrict
      (layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded }
                    contents
      )

allExports :: Vector Export -> Vector HName
allExports =
  V.concatMap (\Export {..} -> exportName `V.cons` allExports exportWith)

-- | If we are importing constructors of a type alias, resolve the alias and
-- import the constructors with the resolved name.
renderImport
  :: (HasErr r, HasSpecInfo r, Eq a)
  => (a -> Sem r ModName)
  -> (a -> Text)
  -> (a -> NameSpace)
  -> (a -> a)
  -> Import a
  -> Sem r (Doc ())
renderImport findModule getName getNameSpace resolveAlias i =
  let resolved          = resolveAlias (importName i)
      importsNoChildren = V.null (importChildren i) && not (importWithAll i)
  in  if importsNoChildren || resolved == importName i
        then renderImport' findModule getName getNameSpace i
        else do
          a <- renderImport'
            findModule
            getName
            getNameSpace
            i { importWithAll = False, importChildren = mempty }
          c <- renderImport' findModule
                             getName
                             getNameSpace
                             i { importName = resolved }
          pure $ vsep [a, c]


renderImport'
  :: (HasErr r, HasSpecInfo r, Eq a)
  => (a -> Sem r ModName)
  -> (a -> Text)
  -> (a -> NameSpace)
  -> Import a
  -> Sem r (Doc ())
renderImport' findModule getName getNameSpace (Import n qual children withAll source)
  = do
    ModName mod' <- findModule n
    let sourceDoc   = bool "" " {-# SOURCE #-}" source
        qualDoc     = bool "" " qualified" qual
        base        = getName n
        ns          = getNameSpace n
        baseP       = wrapSymbol ns base
        spec        = nameSpacePrefix ns
        childrenDoc = if V.null children && not withAll
          then ""
          else parenList
            (  (wrapSymbol (getNameSpace n) . getName <$> children)
            <> (if withAll then V.singleton ".." else V.empty)
            )
    pure $ "import" <> sourceDoc <> qualDoc <+> pretty mod' <+> parenList
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
    , (''Type      , Just (mkName "Data.Kind.Type"))
    , (''Constraint, Just (mkName "Data.Kind.Constraint"))
    , (''Typeable, Just (mkName "Data.Typeable.Typeable"))
    , ('typeRep, Just (mkName "Type.Reflection.typeRep"))
    , (''TypeRep, Just (mkName "Type.Reflection.TypeRep"))
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

withTypeInfo
  :: HasRenderParams r => Spec -> Sem (Reader TypeInfo ': r) a -> Sem r a
withTypeInfo Spec {..} a = do
  RenderParams {..} <- ask
  let
    tyMap :: Map HName HName
    tyMap = Map.fromList
      [ (mkConName eName evName, mkTyName eName)
      | Enum {..}      <- V.toList specEnums
      , EnumValue {..} <- V.toList eValues
      ]
    handleMap :: Map HName Handle
    handleMap = Map.fromList
      [ (mkTyName hName, h) | h@Handle {..} <- V.toList specHandles ]
    commandMap :: Map HName Command
    commandMap = Map.fromList
      [ (mkFunName cName, c) | c@Command {..} <- V.toList specCommands ]
  runReader
    (TypeInfo (`Map.lookup` tyMap)
              (`Map.lookup` handleMap)
              (`Map.lookup` commandMap)
    )
    a

adoptConstructors :: HasTypeInfo r => Import HName -> Sem r (Import HName)
adoptConstructors = \case
  i@(Import n q cs _ source) -> getConParent n >>= pure . \case
    Just p  -> Import p q (V.singleton n <> cs) False source
    Nothing -> i
  where getConParent n = asks (`tiConMap` n)


----------------------------------------------------------------
--
----------------------------------------------------------------
