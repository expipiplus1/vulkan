module Render.Element.Write
  where

import           Relude                  hiding ( runState
                                                , State
                                                , modify'
                                                , Handle
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Vector.Extra             as V
import           Data.Vector.Extra              ( Vector )
import           Data.Text                     as T
import           Data.Char                      ( isLower )
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
import           Polysemy.Input
import           Data.List                      ( lookup )
import           Foreign.Ptr
import           Language.Haskell.TH            ( nameBase
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
import           Documentation
import           Documentation.Haddock
import           Render.ImportLocation

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

renderSegments
  :: forall r
   . ( HasErr r
     , MemberWithError (Embed IO) r
     , HasTypeInfo r
     , HasRenderedNames r
     , HasRenderParams r
     )
  => (Documentee -> Maybe Documentation)
  -> FilePath
  -> ImportLocation
  -> [Segment ModName RenderElement]
  -> Sem r ()
renderSegments getDoc out ImportLocation{..} segments = do
  let
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
renderModule out boot getDoc findModule findLocalModule (Segment modName unsortedElements)
  = do
    let exportsType = V.any (isTyConName . exportName) . reExports
        es          = fromList . sortOn exportsType . toList $ unsortedElements
    RenderParams {..} <- input
    let
      ext = bool ".hs" ".hs-boot" boot
      f =
        toString
          (out <> "/" <> T.unpack (T.replace "." "/" (unModName modName) <> ext)
          )
      openImports = vsep
        ( fmap
            (\(ModName n, hidden) -> if Set.null hidden
              then "import" <+> pretty n
              else
                let renderHidden h =
                      nameSpacePrefix (nameNameSpace h) <> pretty h
                in  "import" <+> pretty n <+> "hiding" <+> tupled
                      (fmap renderHidden . Set.toList $ hidden)
            )
        . Map.toList
        . Map.unionsWith (<>)
        $ (reReexportedModules <$> V.toList es)
        )
      declaredNames = V.concatMap
        (\RenderElement {..} -> allExports (reExports <> reInternal))
        es
      importFilter = Relude.filter
        (\(Import n qual _ _ _) -> qual || n `V.notElem` declaredNames)
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
    let
      exportToImport (Export name withAll with _) =
        Import name False mempty (withAll || not (V.null with)) False
      allReexportImports =
        fmap exportToImport
          . Relude.toList
          . unions
          . fmap reReexportedNames
          . V.toList
          $ es
      allLocalImports =
        Relude.toList . unions . fmap reLocalImports . V.toList $ es
    resolveAlias    <- getResolveAlias
    parentedImports <- traverse adoptConstructors
                                (allLocalImports <> allReexportImports)
    localImports <- vsep <$> traverseV
      (renderImport findLocalModule' unName nameNameSpace resolveAlias)
      (importFilter parentedImports)
    let
      locate :: CName -> DocumenteeLocation
      locate n =
        let names = case n of
              CName "" -> []
              CName n' | isLower (T.head n') -> [mkFunName n]
              _ -> [mkTyName n, mkFunName n, mkPatternName n]
        in  case asum ((\n -> (n, ) <$> findLocalModule n) <$> names) of
              Just (n, m) | m == modName -> ThisModule n
              Just (n, m)                -> OtherModule m n
              Nothing                    -> Unknown

      getDocumentation :: Documentee -> Doc ()
      getDocumentation target = case getDoc target of
        Nothing -> "-- No documentation found for" <+> viaShow target
        Just d  -> case documentationToHaddock externalDocHTML locate d of
          Left e ->
            "-- Error getting documentation for"
              <+> viaShow target
              <>  ":"
              <+> viaShow e
          Right (Haddock t) -> commentNoWrap t
      allReexportedModules :: Vector ModName
      allReexportedModules =
        V.fromList
          . Map.keys
          . Map.unions
          . fmap reReexportedModules
          . toList
          $ es
      exports   = V.concatMap reExports es
      reexports = V.concatMap
        (\re -> V.fromList
          (   (\(Export name withAll with reexportable) ->
                Export name (withAll || not (V.null with)) mempty reexportable
              )
          <$> toList (reReexportedNames re)
          )
        )
        es
      contents =
        vsep
          $ "{-# language CPP #-}"
          : (   "module"
            <+> pretty modName
            <>  indent
                  2
                  (  parenList
                  $  ( fmap exportDoc
                     . nubOrdOnV exportName
                     $ (exports <> reexports)
                     )
                  <> (   (\(ModName m) -> renderExport Module m mempty)
                     <$> allReexportedModules
                     )
                  )
            <+> "where"
            )
          : openImports
          : imports
          : localImports
          : V.toList
              (   (<> (line <> line))
              <$> V.mapMaybe (($ getDocumentation) . reDoc) es
              )

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
  :: (HasErr r, Eq a)
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
  :: (HasErr r, Eq a)
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
    when (T.null mod') $ throw "Trying to render an import with no module!"
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
    , (''Bool      , Nothing)
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

newtype TypeInfo = TypeInfo
  { tiConMap :: HName -> Maybe HName
  }

type HasTypeInfo r = MemberWithError (Input TypeInfo) r

withTypeInfo
  :: HasRenderParams r => Spec -> Sem (Input TypeInfo ': r) a -> Sem r a
withTypeInfo spec a = do
  ti <- specTypeInfo spec
  runInputConst ti a

specTypeInfo :: HasRenderParams r => Spec -> Sem r TypeInfo
specTypeInfo Spec {..} = do
  RenderParams {..} <- input
  let tyMap :: Map HName HName
      tyMap = Map.fromList
        [ (mkConName eExportedName evName, mkTyName eExportedName)
        | Enum {..} <- V.toList specEnums
        , let eExportedName = case eType of
                AnEnum         -> eName
                ABitmask flags -> flags
        , EnumValue {..} <- V.toList eValues
        ]
  pure $ TypeInfo (`Map.lookup` tyMap)

adoptConstructors :: HasTypeInfo r => Import HName -> Sem r (Import HName)
adoptConstructors = \case
  i@(Import n q cs _ source) -> getConParent n <&> \case
    Just p  -> Import p q (V.singleton n <> cs) False source
    Nothing -> i
  where getConParent n = inputs (`tiConMap` n)

----------------------------------------------------------------
--
----------------------------------------------------------------

nubOrdOnV :: Ord b => (a -> b) -> Vector a -> Vector a
nubOrdOnV p = fromList . nubOrdOn p . toList
