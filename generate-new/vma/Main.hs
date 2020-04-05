module VMA
  where

import           Relude                  hiding ( Type
                                                , Handle
                                                )
import qualified Data.Text                     as T
import qualified Data.List                     as List
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Language.C.Parser
import           Language.C.Data
import           Language.C.Data.Ident
import           Language.C.Syntax
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.ConstEval
import           System.Process.Typed
import           Polysemy
import           Polysemy.Input
import           Polysemy.Fixpoint
import           Say
import qualified Data.Map                      as Map
import           System.TimeIt

import           Error
import           CType
import           CType.Size
import           Spec.Parse
import           Render.SpecInfo
import           VMA.RenderParams
import           VMA.Render
import           Render.Element                 ( ModName(..) )
import           Render.Element.Write
import           Render.Names
import           Marshal.Struct
import           Marshal.Marshalable            ( ParameterLength(..) )
import           Write.Segment
import qualified Bespoke.MarshalParams         as Vk

main :: IO ()
main =
  (runFinal . embedToFinal @IO . fixpointToFinal @IO . runErr $ go) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <> " errors")
    Right () -> pure ()
 where
  go :: Sem '[Err , Fixpoint , Embed IO , Final IO] ()
  go = do
    -- We need information on the Vulkan spec
    specText <- timeItNamed "Reading spec"
      $ readFileBS "./Vulkan-Docs/xml/vk.xml"
    (spec, specTypeSize) <- timeItNamed "Parsing spec" $ parseSpec specText

    (ds  , state       ) <- fileDecls DoNotIgnoreWarnings vmaHeader
    enums                <- unitEnums state ds
    structs              <- unitStructs state ds
    let handles = unitHandles ds
    funcPointers <- unitFuncPointers ds

    runInputConst (renderParams (specHandles spec)) $ do
      headerInfo <- liftA2 (<>)
                           (specSpecInfo spec specTypeSize)
                           (vmaSpecInfo enums structs)

      renderedNames <- specRenderedNames spec
      runInputConst headerInfo
        . runInputConst (TypeInfo (const Nothing))
        . runInputConst renderedNames
        $ do
            specMarshalParams <- Vk.marshalParams spec
            marshaledStructs  <- runInputConst specMarshalParams
              $ traverseV marshalStruct structs

            renderElems <- renderHeader enums
                                        marshaledStructs
                                        handles
                                        funcPointers
            let
              segments =
                [Segment (ModName "Graphics.VulkanMemoryAllocator") renderElems]
            renderSegments (const Nothing) "out-vma" segments

vmaHeader :: FilePath
vmaHeader = "VulkanMemoryAllocator/src/vk_mem_alloc.h"

----------------------------------------------------------------
-- Spec info
----------------------------------------------------------------

vmaSpecInfo :: Vector Enum' -> Vector Struct -> Sem r SpecInfo
vmaSpecInfo enums structs = do
  let aliasMap = mempty
      resolveAlias :: CName -> CName
      resolveAlias n = maybe n resolveAlias (Map.lookup n aliasMap)
      mkLookup n f =
        let m = Map.fromList [ (n s, s) | s <- toList f ]
        in  (`Map.lookup` m) . resolveAlias
      siIsStruct          = mkLookup sName structs
      siIsUnion           = const Nothing
      siIsHandle          = const Nothing
      siIsCommand         = const Nothing
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
        in  \case
              TypeName n -> Map.lookup n sizeMap
              _          -> Nothing
      siAppearsInPositivePosition = const False
      siAppearsInNegativePosition = const False
  pure SpecInfo { .. }

----------------------------------------------------------------
--
----------------------------------------------------------------

unitEnums :: HasErr r => TravState s -> GlobalDecls -> Sem r (Vector Enum')
unitEnums state ds = do
  let ets = [ e | EnumDef e <- Map.elems (gTags ds) ]
  fmap fromList . forV ets $ \case
    EnumType (AnonymousRef _            ) _  _ _ -> throw "Enum without a name"
    EnumType (NamedRef     (Ident n _ _)) es _ _ -> do
      allValues <-
        fmap fromList . forV es $ \(Enumerator (Ident n _ _) expr _ _) -> do
          let evName        = CName (T.pack n)
              evIsExtension = False
          expr' <- runTrav_' DoNotIgnoreWarnings
                             state
                             (constEval x86_64 mempty expr)
          evValue <- exprValue expr'
          pure EnumValue { .. }

      let eName = CName (T.pack n)
          isMaxEnum EnumValue {..} = "_MAX_ENUM" `T.isSuffixOf` unCName evName
          eValues = V.filter (not . isMaxEnum) allValues
          eType   = if "FlagBits" `List.isSuffixOf` n
            then ABitmask (CName $ T.dropEnd 8 (T.pack n) <> "Flags")
            else AnEnum
      pure Enum { .. }

-- TODO: This may be a little fragile
unitHandles :: GlobalDecls -> Vector Handle
unitHandles ds = fromList
  [ Handle (CName (T.pack n)) NonDispatchable NoHandleLevel
  | (Ident n _ _, TypeDef _ (PtrType (DirectType (TyComp (CompTypeRef (NamedRef (Ident nT _ _)) _ _)) _ _) _ _) _ _) <-
    Map.toList $ gTypeDefs ds
  , n <> "_T" == nT
  ]

unitFuncPointers :: HasErr r => GlobalDecls -> Sem r (Vector FuncPointer)
unitFuncPointers ds = fromList <$> sequenceV
  [ FuncPointer (CName (T.pack n)) <$> typeToCType t
  | (Ident n _ _, TypeDef _ t _ _) <- Map.toList $ gTypeDefs ds
  , "PFN_" `List.isPrefixOf` n
  ]

unitStructs :: HasErr r => TravState s -> GlobalDecls -> Sem r (Vector Struct)
unitStructs state ds = do
  let sts =
        [ s | CompDef s@(CompType _ StructTag _ _ _) <- Map.elems (gTags ds) ]
  fmap fromList . forV sts $ \case
    CompType (AnonymousRef _) _ _ _ _ -> throw "Struct without a name"
    t@(CompType (NamedRef (Ident n _ _)) _ ms _ _) -> context (T.pack n) $ do
      let sName       = CName (T.pack n)
          sExtends    = mempty
          sExtendedBy = mempty
      sizedMembers <- forV ms $ \case
        m@(MemberDecl (VarDecl (VarName (Ident n _ _) _) _ ty) Nothing _) -> do
          smType <- typeToCType ty
          let
            smName   = CName (T.pack n)
            -- TODO: Extract these from comments or attributes
            -- https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator/issues/114
            smValues = mempty
            smLengths =
              if n == "pFilePath" then fromList [NullTerminated] else mempty
            smIsOptional =
              if n == "pHeapSizeLimit" then fromList [True] else mempty
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
      pure Struct { .. }

typeToCType :: HasErr r => Type -> Sem r CType
typeToCType = fmap snd . typeToCType'

typeToCType' :: HasErr r => Type -> Sem r (Qualifier, CType)
typeToCType' = \case
  DirectType t q _ -> (qual q, ) <$> case t of
    TyVoid                -> pure Void
    (TyIntegral TyChar  ) -> pure Char
    (TyIntegral TyInt   ) -> pure Int
    (TyFloating TyFloat ) -> pure Float
    (TyFloating TyDouble) -> pure Double
    (TyComp (CompTypeRef (NamedRef (Ident n _ _)) _ _)) ->
      pure . TypeName . CName . T.pack $ n
    _ -> throw $ "Unhandled DirectType: " <> show t
  ArrayType t (ArraySize False sizeExpr) q _ -> do
    (elemQual, elemTy) <- typeToCType' t
    size               <- case sizeExpr of
      CConst (CIntConst (CInteger n _ _) _) ->
        pure $ NumericArraySize (fromIntegral n)
      _ -> throw $ "Unhandled array size expression: " <> show sizeExpr
    pure (qual q, Array elemQual size elemTy)
  TypeDefType (TypeDefRef (Ident n _ _) _ _) q _ ->
    pure (qual q, TypeName . CName . T.pack $ n)
  PtrType t q _ -> do
    (elemQual, elemTy) <- typeToCType' t
    pure (qual q, Ptr elemQual elemTy)
  FunctionType (FunType ret params False) _ -> do
    ret'    <- typeToCType ret
    params' <- forV params $ \case
      ParamDecl (VarDecl n _ t) _ -> do
        t' <- typeToCType t
        let paramName = case n of
              NoName                  -> Nothing
              VarName (Ident n _ _) _ -> Just (T.pack n)
        pure (paramName, t')
      AbstractParamDecl _ _ -> throw "Unhandled AbstractParamDecl"
    pure (NonConst, Proto ret' params')
  t -> throw $ "Unhandled type to convert: " <> show t
  where qual = bool NonConst CType.Const . constant

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
  -> Sem r (GlobalDecls, TravState ())
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
cpp f = readProcessStdout_ (proc "cpp" [f])

----------------------------------------------------------------
-- Trav to Sem
----------------------------------------------------------------

runTrav_' :: HasErr r => IgnoreWarnings -> TravState s -> Trav s a -> Sem r a
runTrav_' iw s t = fst <$> runTrav' iw s t

runTrav'
  :: HasErr r
  => IgnoreWarnings
  -> TravState s
  -> Trav s a
  -> Sem r (a, TravState s)
runTrav' iw s t = case runTravWithTravState s t of
  Left es -> throwMany (show <$> es)
  Right (r, s) | DoNotIgnoreWarnings <- iw ->
    traverse_ (throw . show) (travErrors s) >> pure (r, s)
  Right (r, s) -> pure (r, s)
