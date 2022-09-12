module Main where

import           Control.Exception              ( mapException )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as V
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.ConstEval
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.Data
import           Language.C.Data.Ident
import           Language.C.Parser
import           Language.C.Syntax
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Fixpoint
import           Polysemy.Input
import           Polysemy.NonDet
import           Polysemy.State
import           Relude                  hiding ( Handle
                                                , State
                                                , Type
                                                )
import           Say
import           System.Process.Typed
import           System.TimeIt

import qualified Bespoke.MarshalParams         as Vk
import qualified Bespoke.RenderParams          as Vk
import           CType
import           CType.Size
import           Error
import           Marshal.Command
import           Marshal.Marshalable
import           Marshal.Scheme
import           Marshal.Struct
import           Render.Element                 ( ModName(..)
                                                , makeRenderElementInternal
                                                )
import           Render.Element.Write
import           Render.FuncPointer
import           Render.Names
import           Render.SpecInfo
import           Render.State
import           Spec.Parse
import           Write.Segment

import           VMA.Documentation
import           VMA.Render
import           VMA.RenderParams

vmaDir, vmaDocbookDir, vmaHeader :: FilePath
vmaDir = "../VulkanMemoryAllocator/VulkanMemoryAllocator"
vmaHeader = vmaDir <> "/include/vk_mem_alloc.h"
vmaDocbookDir = vmaDir <> "/docs/docbook"

main :: IO ()
main =
  (runFinal . embedToFinal @IO . fixpointToFinal @IO . runErr $ go) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <> " errors")
      exitFailure
    Right () -> pure ()
 where
  go :: Sem '[Err , Fixpoint , Embed IO , Final IO] ()
  go = do
    -- We need information on the Vulkan spec
    specText <- timeItNamed "Reading spec"
      $ readFileBS "./Vulkan-Docs/xml/vk.xml"
    (spec, specTypeSize) <- timeItNamed "Parsing spec"
      $ parseSpec @SpecVk specText

    getDocumentation <- loadAllDocumentation vmaDocbookDir

    (ds, state)      <- fileDecls DoNotIgnoreWarnings vmaHeader
    enums            <- unitEnums state ds
    structs          <- unitStructs state ds
    commands         <- unitCommands ds
    let handles = unitHandles ds
    funcPointers <- unitFuncPointers ds

    runInputConst (renderParams (specHandles spec)) $ do
      headerInfo <- fmap
        (specSpecInfo spec specTypeSize <>)
        (vmaSpecInfo (snd <$> enums)
                     (snd <$> structs)
                     (snd <$> handles)
                     (snd <$> commands)
        )

      (renderedNames, specTypeInfo) <-
        runInputConst (Vk.renderParams (specHandles spec)) $ do
          rn <- specRenderedNames spec
          ti <- specTypeInfo spec
          pure (rn, ti)

      runInputConst headerInfo
        . runInputConst specTypeInfo
        . runInputConst renderedNames
        . evalStateIO initialRenderState
        $ do
            vulkanFuncPointers                    <- vulkanFuncPointers
            specMarshalParams                     <- Vk.marshalParams spec
            ourMarshalParams <- marshalParams (snd <$> handles)
            (marshaledStructs, marshaledCommands) <-
              runInputConst (specMarshalParams <> ourMarshalParams) $ do
                ss <- traverseV (traverse marshalStruct) structs
                cs <- traverseV (traverse marshalCommand) commands
                pure (ss, cs)

            renderElems <- renderHeader enums
                                        marshaledStructs
                                        handles
                                        funcPointers
                                        marshaledCommands
            renderedVulkanFuncPointers <- traverseV
              (fmap makeRenderElementInternal . renderFuncPointer)
              vulkanFuncPointers
            let sortedRenderElems =
                  snd <$> V.modify (V.sortBy (comparing fst)) renderElems
                segments =
                  [ Segment (ModName "VulkanMemoryAllocator")
                            (sortedRenderElems <> renderedVulkanFuncPointers)
                  ]
            renderSegments getDocumentation "out-vma" segments

marshalParams :: Vector Handle -> Sem r MarshalParams
marshalParams handles =
  let
    handleNames = Set.fromList [ hName | Handle {..} <- toList handles ]
    isHandle    = \case
      TypeName n -> Set.member n handleNames
      _          -> False
    isDefaultable = isHandle
    isPassAsPointerType _ = False
    getBespokeScheme :: Marshalable a => CName -> a -> Maybe (MarshalScheme a)
    getBespokeScheme p a = case (p, name a) of
      ("VmaAllocatorCreateInfo", "pHeapSizeLimit") -> Just $ Preserve (type' a)
      ("VmaAllocatorCreateInfo", "pTypeExternalMemoryHandleTypes") -> Just $ Preserve (type' a)
      ("vmaGetHeapBudgets", "pBudgets") -> Just $ Preserve (type' a)
      ("vmaBuildStatsString", "ppStatsString") | Ptr _ p <- type' a ->
        Just $ Returned (Preserve p)
      ("vmaBuildVirtualBlockStatsString", "ppStatsString") | Ptr _ p <- type' a ->
        Just $ Returned (Preserve p)
      ("vmaGetPoolName", "ppName") | Ptr _ p <- type' a ->
        Just $ Returned (Preserve p)
      ("vmaFreeStatsString", "pStatsString") -> Just $ Preserve (type' a)
      ("vmaFreeVirtualBlockStatsString", "pStatsString") -> Just $ Preserve (type' a)
      _ -> Nothing
    isForeignStruct = const False
  in
    pure MarshalParams { .. }

----------------------------------------------------------------
-- Spec info
----------------------------------------------------------------

vmaSpecInfo
  :: Vector Enum'
  -> Vector Struct
  -> Vector Handle
  -> Vector Command
  -> Sem r SpecInfo
vmaSpecInfo enums structs handles commands = do
  let aliasMap = mempty
      resolveAlias :: CName -> CName
      resolveAlias n = maybe n resolveAlias (Map.lookup n aliasMap)
      mkLookup n f =
        let m = Map.fromList [ (n s, s) | s <- toList f ]
        in  (`Map.lookup` m) . resolveAlias
      siIsStruct          = mkLookup sName structs
      siIsUnion           = const Nothing
      siIsHandle          = mkLookup hName handles
      siIsCommand         = mkLookup cName commands
      siIsDisabledCommand = const Nothing
      siIsEnum            = mkLookup eName enums
      siContainsUnion     = const []
      siTypeSize =
        let sizeMap =
              Map.fromList
                $  [ (sName, (sSize, sAlignment))
                   | Struct {..} <- toList structs
                   ]
                <> [ (eName, (4, 4)) | Enum {..} <- toList enums ]
                <> [ (hName, (8, 8)) | Handle {..} <- toList handles ]
        in  \case
              TypeName n -> Map.lookup n sizeMap
              _          -> Nothing
      siAppearsInPositivePosition = const False
      siAppearsInNegativePosition = const False
      -- TODO: is there anything sensible to put in here
      siGetAliases                = const []
      siExtensionType             = const Nothing
      siExtensionDeps             = const []
  pure SpecInfo { .. }

----------------------------------------------------------------
--
----------------------------------------------------------------

unitEnums
  :: HasErr r
  => TravState Identity s
  -> GlobalDecls
  -> Sem r (Vector (NodeInfo, Enum'))
unitEnums state ds = do
  let ets = [ e | EnumDef e <- Map.elems (gTags ds) ]
  fmap fromList . forV ets $ \case
    EnumType (AnonymousRef _) _ _ _ -> throw "Enum without a name"
    EnumType (NamedRef (Ident n _ nodeInfo)) es _ _ -> do
      allValues <-
        fmap fromList . forV es $ \(Enumerator (Ident n _ _) expr _ _) -> do
          let evName        = CName (T.pack n)
              evIsExtension = False
          expr' <- runTrav_' DoNotIgnoreWarnings
                             state
                             (constEval x86_64 mempty expr)
          evValue <- exprValue expr'
          pure EnumValue { .. }

      let
        eName = CName (T.pack n)
        isMaxEnum EnumValue {..} = "_MAX_ENUM" `T.isSuffixOf` unCName evName
        eValues = V.filter (not . isMaxEnum) allValues
        eType   = if "FlagBits" `List.isSuffixOf` n
          then ABitmask (CName $ T.dropEnd 8 (T.pack n) <> "Flags") Bitmask32
          else AnEnum
      pure (nodeInfo, Enum { .. })

-- TODO: This may be a little fragile
unitHandles :: GlobalDecls -> Vector (NodeInfo, Handle)
unitHandles ds = fromList
  [ (nodeInfo, Handle (CName (T.pack n)) NonDispatchable NoHandleLevel)
  | (Ident n _ nodeInfo, TypeDef _ (PtrType (DirectType (TyComp (CompTypeRef (NamedRef (Ident nT _ _)) _ _)) _ _) _ _) _ _) <-
    Map.toList $ gTypeDefs ds
  , n <> "_T" == nT
  ]

unitFuncPointers
  :: HasErr r => GlobalDecls -> Sem r (Vector (NodeInfo, FuncPointer))
unitFuncPointers ds = fromList <$> sequenceV
  [ (nodeInfo, )
    .   FuncPointer (CName (T.pack n))
    .   (\(_, _, t) -> t)
    <$> typeToCType t
  | (Ident n _ nodeInfo, TypeDef _ t _ _) <- Map.toList $ gTypeDefs ds
  , "PFN_" `List.isPrefixOf` n
  ]

vulkanFuncPointers :: (HasSpecInfo r, HasErr r) => Sem r (Vector FuncPointer)
vulkanFuncPointers =
  fmap fromList
    . forV
        [ "vkAllocateMemory"
        , "vkBindBufferMemory"
        , "vkBindBufferMemory2KHR"
        , "vkBindImageMemory"
        , "vkBindImageMemory2KHR"
        , "vkCmdCopyBuffer"
        , "vkCreateBuffer"
        , "vkCreateImage"
        , "vkDestroyBuffer"
        , "vkDestroyImage"
        , "vkFlushMappedMemoryRanges"
        , "vkFreeMemory"
        , "vkGetBufferMemoryRequirements"
        , "vkGetBufferMemoryRequirements2KHR"
        , "vkGetImageMemoryRequirements"
        , "vkGetImageMemoryRequirements2KHR"
        , "vkGetPhysicalDeviceMemoryProperties"
        , "vkGetPhysicalDeviceMemoryProperties2KHR"
        , "vkGetPhysicalDeviceProperties"
        , "vkGetDeviceProcAddr"
        , "vkGetInstanceProcAddr"
        , "vkInvalidateMappedMemoryRanges"
        , "vkVoidFunction"
        , "vkMapMemory"
        , "vkUnmapMemory"
        , "vkGetDeviceBufferMemoryRequirements"
        , "vkGetDeviceImageMemoryRequirements"
        ]
    $ \case
        "vkVoidFunction" ->
          pure (FuncPointer "PFN_vkVoidFunction" (Ptr NonConst $ Proto Void []))
        n -> do
          Command {..} <- note ("Unable to find command " <> show n)
            =<< getCommand n
          pure $ FuncPointer
            (CName ("PFN_" <> unCName n))
            (Ptr NonConst $ Proto
              cReturnType
              (   (\Parameter {..} -> (Just (unCName pName), pType))
              <$> toList cParameters
              )
            )

unitCommands :: HasErr r => GlobalDecls -> Sem r (Vector (NodeInfo, Command))
unitCommands ds =
  fmap (fromList . catMaybes)
    . forV [ d | Declaration d <- toList (gObjs ds) ]
    $ \d -> runNonDetMaybe . failToNonDet $ do
        Decl (VarDecl (VarName (Ident name _ nodeInfo) _) attrs ty) _ <- pure d
        DeclAttrs _ (FunLinkage ExternalLinkage) _ <- pure attrs
        FunctionType (FunType ret params _) _ <- pure ty
        let cName         = CName (T.pack name)
            cSuccessCodes = fromList $ case cName of
              "vmaDefragmentationBegin"   -> ["VK_NOT_READY"]
              "vmaDefragmentationPassEnd" -> ["VK_NOT_READY"]
              _                           -> []
            cErrorCodes = fromList ["VK_ERROR_UNKNOWN"]
            cIsDynamic  = False
            cCanBlock   = False
        (_, _, cReturnType) <- typeToCType ret
        cParameters         <- fmap fromList . forV params $ \case
          ParamDecl (VarDecl (VarName (Ident name _ _) _) _ ty) _ -> do
            let pName = CName (T.pack name)
            (lengths, opts, pType) <- typeToCType ty
            let
              pLengths = fromList $ case (cName, pName) of
                ("vmaGetPoolName", "ppName") -> [NullTerminated]
                ("vmaSetPoolName", "pName" ) -> [NullTerminated]
                ("vmaSetAllocationName", "pName" ) -> [NullTerminated]
                ("VmaAllocationInfo", "pName" ) -> [NullTerminated]
                ("vmaGetBudget", "pBudget") ->
                  [NamedConstantLength "VK_MAX_MEMORY_HEAPS"]
                _ -> lengths
              pIsOptional = fromList $ case (cName, pName) of
                -- allocations can only be null when 'allocationCount' is zero
                ("vmaFlushAllocations", "allocations") -> [False]
                -- allocations can only be null when 'allocationCount' is zero
                ("vmaInvalidateAllocations", "allocations") -> [False]
                _ -> opts
            pure Parameter { .. }
          -- TODO: Make pName in Parameter optional
          ParamDecl (VarDecl NoName _ _) _ ->
            throw "Unhandled param with no name"
          AbstractParamDecl _ _ -> throw "Unhandled AbstractParamDecl"
        pure (nodeInfo, Command { .. })

unitStructs
  :: HasErr r
  => TravState Identity s
  -> GlobalDecls
  -> Sem r (Vector (NodeInfo, Struct))
unitStructs state ds = do
  let sts =
        [ s | CompDef s@(CompType _ StructTag _ _ _) <- Map.elems (gTags ds) ]
  fmap fromList . forV sts $ \case
    CompType (AnonymousRef _) _ _ _ _ -> throw "Struct without a name"
    t@(CompType (NamedRef (Ident n _ nodeInfo)) _ ms _ _) ->
      context (T.pack n) $ do
        let sName        = CName (T.pack n)
            sExtends     = mempty
            sExtendedBy  = mempty
            sInherits    = mempty
            sInheritedBy = mempty
        sizedMembers <- forV ms $ \case
          m@(MemberDecl (VarDecl (VarName (Ident n _ _) _) _ ty) Nothing _) ->
            do
              let smName = CName (T.pack n)
              (lengths, optionality, smType) <- case (sName, smName) of
                ("VmaTotalStatistics", "memoryType") -> pure
                  ( []
                  , []
                  , Array NonConst
                          (SymbolicArraySize "VK_MAX_MEMORY_TYPES")
                          (TypeName "VmaDetailedStatistics")
                  )
                ("VmaTotalStatistics", "memoryHeap") -> pure
                  ( []
                  , []
                  , Array NonConst
                          (SymbolicArraySize "VK_MAX_MEMORY_HEAPS")
                          (TypeName "VmaDetailedStatistics")
                  )
                _ -> typeToCType ty
              let smValues  = mempty
                  smLengths = if n == "pFilePath" || n == "pName"
                    then fromList [NullTerminated]
                    else fromList lengths
                  smIsOptional = fromList $ case (sName, smName) of
                    -- pPools can only be null when allocationCount is zero
                    ("VmaDefragmentationInfo2", "pPools") -> [False]
                    -- pAllocations can only be null when 'allocationCount' is zero
                    ("VmaDefragmentationInfo2", "pAllocations") -> [False]
                    _ -> optionality
                  smOffset = ()
              (size, alignment) <- runTrav_' DoNotIgnoreWarnings state $ do
                s <- fromIntegral <$> sizeofType x86_64 m ty
                a <- fromIntegral <$> alignofType x86_64 m ty
                pure (s, a)
              pure (StructMember { .. }, (size, alignment))
          MemberDecl (VarDecl NoName _ _) _ _ ->
            throw "Unhandled unnamed struct member"
          MemberDecl _ (Just _) _ -> throw "Unhandled bitfield member"
          AnonBitField{}          -> throw "Unhandled anonymous bitfield member"
        (size', align', Compose membersWithOffsets) <- scanOffsets
          CType.Size.roundToAlignment
          (\_ m o -> m + o)
          pure
          (Compose sizedMembers)
        let sMembers = fromList $ membersWithOffsets <&> \(s, o) ->
              s { smOffset = fromIntegral o }
        sSize <-
          fmap fromIntegral . runTrav_' DoNotIgnoreWarnings state $ sizeofType
            x86_64
            t
            (DirectType (typeOfCompDef t) noTypeQuals [])
        sAlignment <-
          fmap fromIntegral . runTrav_' DoNotIgnoreWarnings state $ alignofType
            x86_64
            t
            (DirectType (typeOfCompDef t) noTypeQuals [])
        unless (sSize == size')
          $ throw ("Size mismatch " <> show sSize <> " vs " <> show size')
        unless (sAlignment == align') $ throw
          ("Align mismatch " <> show sAlignment <> " vs " <> show align')
        pure (nodeInfo, Struct { .. })

-- TODO: Make optionality part of the Ptr constructor
typeToCType
  :: HasErr r
  => Type
  -- ^ to parse
  -> Sem r ([ParameterLength], [Bool], CType)
  -- ^ (type, optionality)
typeToCType = fmap (\(_, l, o, t) -> (l, o, t)) . typeToCType'

typeToCType'
  :: HasErr r => Type -> Sem r (Qualifier, [ParameterLength], [Bool], CType)
typeToCType' = \case
  DirectType t q _ -> (qual q, [], [], ) <$> case t of
    TyVoid                -> pure Void
    (TyIntegral TyChar  ) -> pure Char
    (TyIntegral TyInt   ) -> pure Int
    (TyFloating TyFloat ) -> pure Float
    (TyFloating TyDouble) -> pure Double
    (TyComp (CompTypeRef (NamedRef (Ident n _ _)) _ _)) ->
      pure . TypeName . CName . T.pack $ n
    _ -> throw $ "Unhandled DirectType: " <> show t
  ArrayType t (ArraySize False sizeExpr) q as -> do
    (elemQual, l, o, elemTy) <- typeToCType' t
    size                     <- case sizeExpr of
      CConst (CIntConst (CInteger n _ _) _) ->
        pure $ NumericArraySize (fromIntegral n)
      _ -> throw $ "Unhandled array size expression: " <> show sizeExpr
    pure (qual q, len as l, o, Array elemQual size elemTy)
  TypeDefType (TypeDefRef (Ident n _ _) _ _) q as ->
    pure (qual q, len as [], opt q [], TypeName . CName . T.pack $ n)
  PtrType t q as -> do
    (elemQual, ls, os, elemTy) <- typeToCType' t
    pure (qual q, len as ls, opt q os, Ptr elemQual elemTy)
  FunctionType (FunType ret params False) _ -> do
    (_, _, ret') <- typeToCType ret
    params'      <- forV params $ \case
      ParamDecl (VarDecl n _ t) _ -> do
        (_, _, t') <- typeToCType t
        let paramName = case n of
              NoName                  -> Nothing
              VarName (Ident n _ _) _ -> Just (T.pack n)
        pure (paramName, t')
      AbstractParamDecl _ _ -> throw "Unhandled AbstractParamDecl"
    pure (NonConst, [], [], Proto ret' params')
  t -> throw $ "Unhandled type to convert: " <> show t
 where
  qual = bool NonConst CType.Const . constant
  opt q t = if nullable q then True : t else if nonnull q then False : t else t
  len as t =
    [ l
    | Attr (Ident attrName _ _) [expr] _ <- as
    , attrName == lenAttrName
    , Just l <- pure $ case expr of
      CConst (CStrConst (CString x _) _) -> case T.splitOn "::" (T.pack x) of
        [x]    -> Just $ NamedLength (CName x)
        [x, y] -> Just $ NamedMemberLength (CName x) (CName y)
        _      -> error "bad len attribute"
      CVar (Ident v _ _) _ -> Just $ NamedLength (CName (T.pack v))
      _                    -> error "bad len attribute"
    ]
    ++ t

x86_64 :: MachineDesc
x86_64 =
  let iSize = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 8
        TyULong   -> 8
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> 16
        TyUInt128 -> 16
      fSize = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 16
        TyFloatN{} -> error "TyFloatN"
      builtinSize = \case
        TyVaList -> 24
        TyAny    -> error "TyAny"
      ptrSize  = 8
      voidSize = 1
      iAlign   = \case
        TyBool    -> 1
        TyChar    -> 1
        TySChar   -> 1
        TyUChar   -> 1
        TyShort   -> 2
        TyUShort  -> 2
        TyInt     -> 4
        TyUInt    -> 4
        TyLong    -> 8
        TyULong   -> 8
        TyLLong   -> 8
        TyULLong  -> 8
        TyInt128  -> 16
        TyUInt128 -> 16
      fAlign = \case
        TyFloat    -> 4
        TyDouble   -> 8
        TyLDouble  -> 16
        TyFloatN{} -> error "TyFloatN"
      builtinAlign = \case
        TyVaList -> 8
        TyAny    -> error "TyAny"
      ptrAlign  = 8
      voidAlign = 1
  in  MachineDesc { .. }

exprValue :: HasErr r => Expr -> Sem r Int64
exprValue = \case
  CConst (CIntConst (CInteger n _ _) _) -> pure $ fromIntegral n
  e -> throw $ "Unhandled expr type " <> show e

----------------------------------------------------------------
-- Parsing a file
----------------------------------------------------------------

-- | Extract the declarations defined in the given file. Declarations in
-- @#include@d files are not returned.
fileDecls
  :: (HasErr r, Member (Embed IO) r)
  => IgnoreWarnings
  -> FilePath
  -> Sem r (GlobalDecls, TravState Identity ())
fileDecls iw f = do
  preprocessed <- cpp f
  transUnit <- fromEitherShow $ parseC (toStrict preprocessed) (initPos f)
  (GlobalDecls objs tags typedefs, state) <- runTrav' iw
                                                      (initTravState ())
                                                      (analyseAST transUnit)
  let isLocalIdent (Ident _ _ i) =
        isSourcePos (posOfNode i) && f == posFile (posOfNode i)
  pure
    ( GlobalDecls
      (Map.filterWithKey (\k _ -> isLocalIdent k) objs)
      (Map.filterWithKey
        (\k _ -> case k of
          NamedRef     i -> isLocalIdent i
          AnonymousRef _ -> False
        )
        tags
      )
      (Map.filterWithKey (\k _ -> isLocalIdent k) typedefs)
    , state
    )

data IgnoreWarnings = DoIgnoreWarnings | DoNotIgnoreWarnings
  deriving (Eq)

-- | Read the preprocessed version of a file
cpp :: MonadIO m => FilePath -> m LByteString
cpp f = mapException (\e -> e { eceStdout = mempty }) $ readProcessStdout_
  (proc
    "cpp"
    [ f
    , "-DVMA_NOT_NULL=_Nonnull"
    , "-DVMA_NULLABLE=_Nullable"
    , "-DVMA_LEN_IF_NOT_NULL(len)=__attribute__((" <> lenAttrName <> "(len)))"
    ]
  )

lenAttrName :: String
lenAttrName = "len_if_not_null"

----------------------------------------------------------------
-- Trav to Sem
----------------------------------------------------------------

runTrav_'
  :: HasErr r => IgnoreWarnings -> TravState Identity s -> Trav s a -> Sem r a
runTrav_' iw s t = fst <$> runTrav' iw s t

runTrav'
  :: HasErr r
  => IgnoreWarnings
  -> TravState Identity s
  -> Trav s a
  -> Sem r (a, TravState Identity s)
runTrav' iw s t = case runIdentity $ runTravTWithTravState s t of
  Left es -> throwMany (show <$> es)
  Right (r, s) | DoNotIgnoreWarnings <- iw ->
    traverse_ (throw . show) (travErrors s) >> pure (r, s)
  Right (r, s) -> pure (r, s)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

evalStateIO :: Member (Embed IO) r => s -> Sem (State s ': r) a -> Sem r a
evalStateIO i = fmap snd . stateToIO i
