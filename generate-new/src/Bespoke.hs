{-# LANGUAGE NamedFieldPuns #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskellQuotes #-}
module Bespoke
  ( forbiddenConstants
  , assignBespokeModules
  , bespokeStructsAndUnions
  , bespokeElements
  , bespokeSizes
  , bespokeOptionality
  , bespokeLengths
  , bespokeSchemes
  , BespokeScheme(..)
  , structChainVar
  , zeroNextPointer
  ) where

import qualified Data.List.Extra               as List
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Vector                    ( Vector )
import qualified Data.Vector.Extra             as V
import           Foreign.C.Types
import           Foreign.Ptr
import           Language.Haskell.TH            ( mkName )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Const )
import           Text.InterpolatedString.Perl6.Unindented

import           Control.Monad.Trans.Cont       ( ContT )
import           Data.Bits
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BS
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Numeric

import           CType
import           Error
import           Haskell                       as H
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.Peek                    ( storablePeek
                                                , vectorPeekWithLenRef
                                                )
import           Render.SpecInfo
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Stmts.Utils
import           Render.Type
import           Render.Utils
import           Spec.Types
import           VkModulePrefix

----------------------------------------------------------------
-- Changes to the spec
----------------------------------------------------------------

-- | These constants are defined elsewhere
forbiddenConstants :: [CName]
forbiddenConstants = ["VK_TRUE", "VK_FALSE"]

----------------------------------------------------------------
-- Module assignments
----------------------------------------------------------------

assignBespokeModules
  :: (HasErr r, HasRenderParams r, Traversable t)
  => t RenderElement
  -> Sem r (t RenderElement)
assignBespokeModules es = do
  bespokeModules <- bespokeModules
  forV es $ \case
    r@RenderElement {..}
      | exports <- fmap exportName . toList $ reExports
      , bespokeMods <-
        List.nubOrd . mapMaybe (`List.lookup` bespokeModules) $ exports
      -> case bespokeMods of
        []  -> pure r
        [x] -> case reExplicitModule of
          Just m | m /= x ->
            throw "Render element already has an explicit module"
          _ -> pure $ r { reExplicitModule = Just x }
        _ -> throw "Multiple bespoke module names found for render element"


bespokeModules :: HasRenderParams r => Sem r [(HName, ModName)]
bespokeModules = do
  RenderParams {..} <- input
  pure
    $  [ ( mkTyName "VkAllocationCallbacks"
         , vulkanModule ["Core10", "AllocationCallbacks"]
         )
       , (mkTyName "VkBaseInStructure" , vulkanModule ["CStruct", "Extends"])
       , (mkTyName "VkBaseOutStructure", vulkanModule ["CStruct", "Extends"])
       ]
    <> (   (, vulkanModule ["Core10", "FundamentalTypes"])
       <$> [ mkTyName "VkBool32"
           , TermName "boolToBool32"
           , TermName "bool32ToBool"
           ]
       )

----------------------------------------------------------------
-- Schemes
----------------------------------------------------------------

data BespokeScheme where
  BespokeScheme ::(forall a. Marshalable a => CName -> a -> Maybe (MarshalScheme a)) -> BespokeScheme
  --- ^ Parent name -> child -> scheme

bespokeSchemes :: Spec -> Sem r [BespokeScheme]
bespokeSchemes spec =
  pure
    $  [baseInOut, wsiScheme, dualPurposeBytestrings, nextPointers spec]
    <> difficultLengths
    <> [bitfields]
    <> [accelerationStructureGeometry]
    <> [buildingAccelerationStructures]

baseInOut :: BespokeScheme
baseInOut = BespokeScheme $ \case
  n | n `elem` ["VkBaseInStructure", "VkBaseOutStructure"] -> \case
    m | "pNext" <- name m -> Just $ Normal (type' m)
    _                     -> Nothing
  _ -> const Nothing


data NextType = NextElided | NextChain

nextPointers :: Spec -> BespokeScheme
nextPointers Spec {..} =
  let schemeMap :: Map (CName, CName) NextType
      schemeMap = Map.fromList
        [ ((sName s, "pNext"), scheme)
        | s <- toList specStructs
        , let scheme = case sExtendedBy s of
                V.Empty -> NextElided
                _       -> NextChain
        ]
  in  BespokeScheme $ \n c -> case Map.lookup (n, name c) schemeMap of
        Nothing         -> Nothing
        Just NextElided -> Just (ElidedUnivalued "nullPtr")
        Just NextChain  -> Just (Custom chainScheme)
 where
  chainVarT   = VarT (mkName structChainVar)
  chainT      = ConT (mkName "Chain") :@ chainVarT
  chainScheme = CustomScheme
    { csName       = "Chain"
    , csZero       = Just "()"
    , csZeroIsZero = False -- Pointer to chain
    -- , csType = pure $ ForallT [] [ConT (mkName "PokeChain") :@ chainVarT] chainT
    , csType       = pure $ ForallT [] [] chainT
    , csDirectPoke = APoke $ \chainRef ->
      stmt (Just (ConT ''Ptr :@ chainT)) (Just "pNext") $ do
        tellImportWithAll (TyConName "PokeChain")
        tellImport (TyConName "Chain")
        tellImport 'castPtr
        tellImportWithAll ''ContT
        ValueDoc chain <- use chainRef
        pure
          .   ContTAction
          .   ValueDoc
          $   "fmap castPtr . ContT $ withChain"
          <+> chain
    , csPeek       = \addrRef -> stmt (Just chainT) (Just "next") $ do
      chainPtr <- use
        =<< storablePeek "pNext" addrRef (Ptr Const (Ptr Const Void))
      tellImportWithAll (TyConName "PeekChain")
      tellImport (TyConName "Chain")
      tellImport 'castPtr
      pure $ IOAction . ValueDoc $ "peekChain" <+> parens
        ("castPtr" <+> chainPtr)
    }

-- | A special poke which writes a zero chain
zeroNextPointer
  :: forall r s
   . (HasRenderElem r, HasRenderParams r, HasErr r, HasStmts r)
  => Stmt s r (Ref s ValueDoc)
zeroNextPointer = do
  let chainVarT = VarT (mkName structChainVar)
      chainT    = ConT (mkName "Chain") :@ chainVarT
  varTDoc <- renderTypeHighPrec chainVarT
  stmt (Just (ConT ''Ptr :@ chainT)) (Just "pNext") $ do
    tellImportWithAll (TyConName "PokeChain")
    tellImport (TyConName "Chain")
    tellImport 'castPtr
    tellImportWithAll ''ContT
    pure
      .  ContTAction
      .  ValueDoc
      $  "fmap castPtr . ContT $ withZeroChain @"
      <> varTDoc

wsiScheme :: BespokeScheme
wsiScheme = BespokeScheme $ const $ \case
  a | t@(Ptr _ (TypeName "xcb_connection_t")) <- type' a -> Just (Normal t)
  a | t@(Ptr _ (TypeName "wl_display")) <- type' a -> Just (Normal t)
  a | t@(Ptr _ (TypeName "Display")) <- type' a -> Just (Normal t)
  _ -> Nothing

-- So we render the dual purpose command properly
dualPurposeBytestrings :: BespokeScheme
dualPurposeBytestrings = BespokeScheme $ \case
  c
    | c `elem` ["vkGetPipelineCacheData", "vkGetValidationCacheDataEXT"] -> \case
      a | (Ptr NonConst Void) <- type' a, "pData" <- name a ->
        Just (Returned ByteString)
      _ -> Nothing
    | c == "vkGetShaderInfoAMD" -> \case
      a | (Ptr NonConst Void) <- type' a, "pInfo" <- name a ->
        Just (Returned ByteString)
      _ -> Nothing
  _ -> const Nothing

difficultLengths :: [BespokeScheme]
difficultLengths =
  [ BespokeScheme $ \case
    "VkPipelineMultisampleStateCreateInfo" -> \case
      p | "rasterizationSamples" <- name p -> Just $ Normal (type' p)
      (p :: a) | "pSampleMask" <- name p   -> Just . Custom $ CustomScheme
        { csName       = "Sample mask array"
        , csZero       = Just "mempty"
        , csZeroIsZero = True
        , csType       = do
                           RenderParams {..} <- input
                           let TyConName sm = mkTyName "VkSampleMask"
                           pure $ ConT ''Vector :@ ConT (mkName (T.unpack sm))
        , csDirectPoke = APoke $ \vecRef -> do
          RenderParams {..} <- input
          stmt (Just (ConT ''Ptr :@ ConT ''Word32)) (Just "pSampleMask") $ do
            tellQualImport 'V.length
            tellQualImport 'nullPtr
            ValueDoc vec     <- use vecRef
            ValueDoc samples <- useViaName "rasterizationSamples"
            let
              sampleTy = mkTyName "VkSampleCountFlagBits"
              sampleCon =
                mkConName "VkSampleCountFlagBits" "VkSampleCountFlagBits"
              cond = parens "requiredLen == fromIntegral vecLen"
              err
                = "sampleMask must be either empty or contain enough bits to cover all the sample specified by 'rasterizationSamples'"
            tellImportWith sampleTy sampleCon
            throwErr <- renderSubStmtsIO (unitStmt (throwErrDoc err cond))
            vecPoke  <- renderSubStmts $ do
              vecRef' <- pureStmt =<< raise (use vecRef)
              getVectorPoke @a "pSampleMask"
                               (Ptr Const (TypeName "VkSampleMask"))
                               (Normal (TypeName "VkSampleMask"))
                               NotNullable
                               vecRef'
            vecPokeDoc <- case vecPoke of
              ContTStmts d -> pure d
              IOStmts    d -> do
                tellImportWithAll 'lift
                pure $ "lift $" <+> d
            pure
              .   ContTAction
              .   ValueDoc
              $   "case Data.Vector.length"
              <+> vec
              <+> "of"
              <>  line
              <>  indent
                    2
                    (vsep
                      [ "0      -> pure nullPtr"
                      , "vecLen ->" <+> doBlock
                        [ "let" <+> indent
                          0
                          (   "requiredLen ="
                          <+> "case"
                          <+> samples
                          <+> "of"
                          <>  line
                          <>  indent
                                2
                                (pretty sampleCon <+> "n -> (n + 31) `quot` 32")
                          )
                        , "lift $" <+> throwErr
                        , vecPokeDoc
                        ]
                      ]
                    )
        , csPeek       = \addrRef -> do
          RenderParams {..} <- input
          stmt (Just (ConT ''Vector :@ ConT ''Word32)) (Just "pSampleMask") $ do
            ptr <- use =<< storablePeek
              "pSampleMask"
              addrRef
              (Ptr Const (Ptr Const (TypeName "VkSampleMask")))
            vecPeek <- renderSubStmtsIO $ do
              addrRef          <- pureStmt (AddrDoc ptr)
              ValueDoc samples <- useViaName "rasterizationSamples"
              let sampleTy = mkTyName "VkSampleCountFlagBits"
                  sampleCon =
                    mkConName "VkSampleCountFlagBits" "VkSampleCountFlagBits"
              tellImportWith sampleTy sampleCon
              len <-
                pureStmt
                .   ValueDoc
                $   "case"
                <+> samples
                <+> "of"
                <>  line
                <>  indent
                      2
                      (   pretty sampleCon
                      <+> "n -> (fromIntegral n + 31) `quot` 32"
                      )
              -- TODO: pass Nullable here and don't reimplement that logic
              vectorPeekWithLenRef @a "sampleMask"
                                      (Normal (TypeName "VkSampleMask"))
                                      addrRef
                                      (TypeName "VkSampleMask")
                                      mempty
                                      len
                                      NotNullable
            pure
              .   IOAction
              .   ValueDoc
              $   "if"
              <+> ptr
              <+> "== nullPtr"
              <>  line
              <>  indent 2 (vsep ["then pure mempty", "else" <+> vecPeek])
        }
      _ -> Nothing
    _ -> const Nothing
  , BespokeScheme $ \case
    "VkShaderModuleCreateInfo" -> \case
      p | "codeSize" <- name p -> Just . ElidedCustom $ CustomSchemeElided
        { cseName       = "Shader code length"
        , cseDirectPoke = stmt (Just (ConT ''Int)) (Just "codeSizeBytes") $ do
                            tellQualImport 'BS.length
                            ValueDoc bs <- useViaName "pCode"
                            pure
                              .   Pure InlineOnce
                              .   ValueDoc
                              $   "fromIntegral $ Data.ByteString.length"
                              <+> bs
        , csePeek       = Just $ \addrRef ->
          storablePeek "codeSize" addrRef (Ptr Const (TypeName "size_t"))
        }
      p | "pCode" <- name p -> Just . Custom $ CustomScheme
        { csName       = "Shader code"
        , csZero       = Just "mempty"
        , csZeroIsZero = True
        , csType       = pure $ ConT ''ByteString
        , csDirectPoke = APoke $ \bsRef -> do
          assertMul4 <- unitStmt $ do
            ValueDoc bs <- use bsRef
            tellQualImport 'BS.length
            tellImport '(.&.)
            let err = "code size must be a multiple of 4"
                cond =
                  parens $ "Data.ByteString.length" <+> bs <+> ".&. 3 == 0"
            throwErrDoc err cond
          stmt (Just (ConT ''Ptr :@ ConT ''Word32)) (Just "pCode") $ do
            after assertMul4
            ValueDoc bs  <- use bsRef
            maybeAligned <- use =<< stmt
              Nothing
              (Just "unalignedCode")
              (do
                tellImportWithAll ''ContT
                tellImport 'BS.unsafeUseAsCString
                pure . ContTAction $ "ContT $ unsafeUseAsCString" <+> bs
              )
            tellImport 'ptrToWordPtr
            tellImport '(.&.)
            tellImport 'castPtr
            tellImport ''CChar
            tellImport ''Word32
            tellImportWithAll ''ContT
            tellImport 'allocaBytesAligned
            tellImport 'lift
            tellQualImport 'BS.length
            tellImport 'copyBytes
            let len = "Data.ByteString.length" <+> bs
            pure
              .   ContTAction
              .   ValueDoc
              $   "if ptrToWordPtr"
              <+> maybeAligned
              <+> ".&. 3 == 0"
              <>  line
              <>  indent
                    2
                    (vsep
                      [ "-- If this pointer is already aligned properly then use it"
                      , "then pure $ castPtr @CChar @Word32" <+> maybeAligned
                      , "-- Otherwise allocate and copy the bytes"
                      , "else" <+> doBlock
                        [ "let len =" <+> len
                        , "mem <- ContT $ allocaBytesAligned @Word32"
                        <+> "len"
                        <+> "4"
                        , "lift $ copyBytes mem (castPtr @CChar @Word32"
                        <+> maybeAligned
                        <>  ")"
                        <+> "len"
                        , "pure mem"
                        ]
                      ]
                    )
        , csPeek       = \addrRef ->
          stmt (Just (ConT ''ByteString)) (Just "code") $ do
            ValueDoc len <- useViaName "codeSize"
            let bytes = "fromIntegral $" <+> len <+> "* 4"
            ptr <- use =<< storablePeek
              "pCode"
              addrRef
              (Ptr Const (Ptr Const (TypeName "uint32_t")))
            tellImport 'castPtr
            tellImport ''Word32
            tellImport ''CChar
            let castPtr = "castPtr @Word32 @CChar" <+> ptr
            tellImport 'BS.packCStringLen
            pure . IOAction . ValueDoc $ "packCStringLen" <+> tupled
              [castPtr, bytes]
        }
      _ -> Nothing
    _ -> const Nothing
  , BespokeScheme $ \case
    structName
      | -- Handle before and after 1.2.162
        structName
        `elem` [ "VkAccelerationStructureVersionInfoKHR"
               , "VkAccelerationStructureVersionKHR"
               ]
      -> \case
        p
          | memberName <- name p
          , memberName `elem` ["pVersionData", "versionData"]
          , Ptr Const (TypeName "uint8_t") <- type' p
          , -- TODO, This should be a "MultipleLength" or something
            V.Singleton (NamedLength (CName len)) <- lengths p
          , len == "2*VK_UUID_SIZE"
          -> Just . Custom $ CustomScheme
            { csName       = "Acceleration structure version"
            , csZero       = Just "mempty"
            , csZeroIsZero = True
            , csType       = pure $ ConT ''ByteString
            , csDirectPoke = APoke $ \bsRef -> do
              assertCorrectLength <- unitStmt $ do
                RenderParams {..} <- input
                ValueDoc bs       <- use bsRef
                tellQualImport 'BS.length
                let
                  err =
                    "AccelerationStructureVersionKHR::versionData must be "
                      <> len
                      <> " bytes"
                  uuidSizeDoc = mkPatternName "VK_UUID_SIZE"
                  cond =
                    parens
                      $   "Data.ByteString.length"
                      <+> bs
                      <+> "== 2 *"
                      <+> pretty uuidSizeDoc
                tellImport uuidSizeDoc
                throwErrDoc err cond
              stmt (Just (ConT ''Ptr :@ ConT ''Word8)) (Just "versionData'")
                $ do
                    after assertCorrectLength
                    ValueDoc bs <- use bsRef
                    tellImportWithAll ''ContT
                    tellImport 'BS.unsafeUseAsCString
                    tellImport 'castPtr
                    tellImport ''Word8
                    tellImport ''CChar
                    pure
                      .   ContTAction
                      .   ValueDoc
                      $ "fmap (castPtr @CChar @Word8) . ContT $ unsafeUseAsCString"
                      <+> bs
            , csPeek       = \addrRef ->
              stmt (Just (ConT ''ByteString)) (Just "versionData") $ do
                RenderParams {..} <- input
                let uuidSizeDoc = mkPatternName "VK_UUID_SIZE"
                    bytes       = "2 *" <+> pretty uuidSizeDoc
                tellImport uuidSizeDoc
                ptr <- use =<< storablePeek
                  "versionData"
                  addrRef
                  (Ptr Const (Ptr Const (TypeName "uint8_t")))
                tellImport 'castPtr
                tellImport ''Word8
                tellImport ''CChar
                let castPtr = "castPtr @Word8 @CChar" <+> ptr
                tellImport 'BS.packCStringLen
                pure . IOAction . ValueDoc $ "packCStringLen" <+> tupled
                  [castPtr, bytes]
            }
        _ -> Nothing
    _ -> const Nothing
  ]

-- | Bitfields at the moment are handled by writing both fields when the first
-- (lower bits) one is written and not doing anything for the second one.
bitfields :: BespokeScheme
bitfields = BespokeScheme $ \case
  "VkAccelerationStructureInstanceKHR" -> \case
    p
      | "instanceCustomIndex" <- name p -> Just $ bitfieldMaster p ("mask", 8)
      | "mask" <- name p -> Just $ bitfieldSlave 24 p
      | "instanceShaderBindingTableRecordOffset" <- name p -> Just
      $ bitfieldMaster p ("flags", 8)
      | "flags" <- name p -> Just $ bitfieldSlave 24 p
    _ -> Nothing
  _ -> const Nothing
 where
  peekBitfield
    :: (HasRenderElem r, HasErr r, HasSpecInfo r, HasRenderParams r)
    => CName
    -> CType
    -> Int
    -> Int
    -> Ref s AddrDoc
    -> Stmt s r (Ref s ValueDoc)
  peekBitfield name ty bitSize bitShift addr = do
    tyH     <- cToHsType DoNotPreserve ty
    base    <- storablePeek name addr (Ptr Const ty)
    shifted <- if bitShift == 0
      then pure base
      else stmt Nothing Nothing $ do
        ValueDoc base <- use base
        tellImport 'shiftR
        pure . Pure InlineOnce . ValueDoc $ parens
          (base <+> "`shiftR`" <+> viaShow bitShift)
    masked <- if bitSize == 32
      then pure shifted
      else stmt Nothing Nothing $ do
        ValueDoc shifted <- use shifted
        tellImport '(.&.)
        tellImport 'coerce
        let mask = "coerce @Word32 0x"
              <> pretty (showHex ((1 `shiftL` bitSize :: Int) - 1) "")
        pure . Pure InlineOnce . ValueDoc $ parens (shifted <+> ".&." <+> mask)
    stmt (Just tyH) (Just (unCName name)) $ do
      masked <- use masked
      pure . Pure NeverInline $ masked

  bitfieldSlave :: Marshalable a => Int -> a -> MarshalScheme a
  bitfieldSlave bitShift = \case
    p
      | Bitfield ty bitSize <- type' p -> Custom CustomScheme
        { csName       = "bitfield slave " <> unCName (name p)
        , csZero       = Just "zero"
        , csZeroIsZero = True
        , csType       = cToHsType DoNotPreserve ty
        , csDirectPoke = NoPoke
        , csPeek       = peekBitfield (name p) ty bitSize bitShift
        }
      | otherwise -> error "bitfield slave type isn't a bitfield "

  bitfieldMaster :: Marshalable a => a -> (CName, Int) -> MarshalScheme a
  bitfieldMaster master (slaveName, _slaveBitSize) = case type' master of
    Bitfield ty masterBitSize -> Custom CustomScheme
      { csName       = "bitfield master " <> unCName (name master)
      , csZero       = Just "zero"
      , csZeroIsZero = True
      , csType       = cToHsType DoNotPreserve ty
      , csDirectPoke = APoke $ \masterRef -> do
                         tyH <- cToHsType DoPreserve ty
                         stmt (Just tyH) Nothing $ do
                           ValueDoc slaveDoc  <- useViaName (unCName slaveName)
                           ValueDoc masterDoc <- use masterRef
                           tellImport 'shiftL
                           tellImport '(.|.)
                           tellImport 'coerce
                           pure
                             .   Pure InlineOnce
                             .   ValueDoc
                             $   parens
                                   (   parens ("coerce @_ @Word32" <+> slaveDoc)
                                   <+> "`shiftL`"
                                   <+> viaShow masterBitSize
                                   )
                             <+> ".|."
                             <+> masterDoc
      , csPeek       = peekBitfield (name master) ty masterBitSize 0
      }
    _ -> error "bitfield master isn't a bitfield"

accelerationStructureGeometry :: BespokeScheme
accelerationStructureGeometry = BespokeScheme $ \case
  "VkAccelerationStructureBuildGeometryInfoKHR" -> \case
    (p :: a) | "ppGeometries" <- name p, Ptr Const (Ptr Const _) <- type' p ->
      Just $ ElidedUnivalued "nullPtr"
    _ -> Nothing
  _ -> const Nothing

-- TODO: Select this when compiling an older spec
_accelerationStructureGeometryPre1_2_162 :: BespokeScheme
_accelerationStructureGeometryPre1_2_162 = BespokeScheme $ \case
  "VkAccelerationStructureBuildGeometryInfoKHR" -> \case
    (p :: a)
      | "geometryArrayOfPointers" <- name p
      -> Just . ElidedCustom $ CustomSchemeElided
        { cseName       = "geometry array type"
        , cseDirectPoke = do
          RenderParams {..} <- input
          let t = mkPatternName "VK_FALSE"
          tellImport t
          tyH <- cToHsType DoNotPreserve (type' p)
          stmt (Just tyH) Nothing $ pure . Pure AlwaysInline . ValueDoc $ pretty
            t
        , csePeek       = Nothing -- TODO assert it's VK_FALSE here
        }
      | "geometryCount" <- name p
      -> Just . ElidedCustom $ CustomSchemeElided
        { cseName       = "geometryCount"
        , cseDirectPoke = elidedLengthPoke @_ @a (name p)
                                                 (type' p)
                                                 mempty
                                                 (V.fromList ["ppGeometries"])
        , csePeek       = Just $ \addr -> storablePeek (name p) addr (type' p)
        }
      | "ppGeometries" <- name p, Ptr Const unPtrTy@(Ptr Const elemTy) <- type'
        p
      -> Just . Custom $ CustomScheme
        { csName       = "ppGeometries"
        , csZero       = Just "mempty"
        , csZeroIsZero = True -- Pointer to empty array
        , csType       = (ConT ''Vector :@) <$> cToHsType DoNotPreserve elemTy
        , csDirectPoke = APoke $ \vecRef -> do
          ptrRef <- getPokeDirect' @a (name p)
                                      unPtrTy
                                      (Vector NotNullable (Normal elemTy))
                                      vecRef
          tyH <- cToHsType DoPreserve (Ptr Const unPtrTy)
          stmt (Just tyH) (Just "ppGeometries") $ do
            ValueDoc ptr <- use ptrRef
            tellImportWithAll ''ContT
            tellImport 'with
            pure . ContTAction . ValueDoc $ "ContT $ with" <+> ptr
        , csPeek       = error "unused csPeek for ppGeometries"
        }
    _ -> Nothing
  _ -> const Nothing

-- TODO: These should have length annotations which check that they match their
-- siblings
buildingAccelerationStructures :: BespokeScheme
buildingAccelerationStructures = BespokeScheme $ \case
  commandName
    | commandName
      `elem` [ "vkCmdBuildAccelerationStructuresKHR"
             , "vkBuildAccelerationStructuresKHR"
             ]
    -> \case
      (p :: a)
        | "ppBuildRangeInfos" <- name p, Ptr Const (Ptr Const elemTy) <- type' p
        -> Just $ Vector NotNullable (Vector NotNullable (Normal elemTy))
      _ -> Nothing
  "vkCmdBuildAccelerationStructuresIndirectKHR" -> \case
    (p :: a)
      | "ppMaxPrimitiveCounts" <- name p, Ptr Const (Ptr Const elemTy) <- type'
        p
      -> Just $ Vector NotNullable (Vector NotNullable (Normal elemTy))
    _ -> Nothing

  _ -> const Nothing

structChainVar :: String
structChainVar = "es"

----------------------------------------------------------------
-- Things which are easier to write by hand
----------------------------------------------------------------

-- | These override the description in the spec, make sure they're correct!
bespokeStructsAndUnions :: [StructOrUnion a WithoutSize WithoutChildren]
bespokeStructsAndUnions =
  [ Struct
      { sName       = "VkTransformMatrixKHR"
      , sMembers    = V.fromList
                        [ StructMember
                          { smName       = "matrixRow0"
                          , smType = Array NonConst (NumericArraySize 4) Float
                          , smValues     = mempty
                          , smLengths    = mempty
                          , smIsOptional = mempty
                          , smOffset     = ()
                          }
                        , StructMember
                          { smName       = "matrixRow1"
                          , smType = Array NonConst (NumericArraySize 4) Float
                          , smValues     = mempty
                          , smLengths    = mempty
                          , smIsOptional = mempty
                          , smOffset     = ()
                          }
                        , StructMember
                          { smName       = "matrixRow2"
                          , smType = Array NonConst (NumericArraySize 4) Float
                          , smValues     = mempty
                          , smLengths    = mempty
                          , smIsOptional = mempty
                          , smOffset     = ()
                          }
                        ]
      , sSize       = ()
      , sAlignment  = ()
      , sExtends    = mempty
      , sExtendedBy = mempty
      }
  ]

bespokeSizes :: [(CName, (Int, Int))]
bespokeSizes =
  (fst <$> concat [win32 @'[Input RenderParams], x11, xcb2, zircon, ggp])
    <> [ ("VkSampleMask"   , (4, 4))
       , ("VkFlags"        , (4, 4))
       , ("VkDeviceSize"   , (8, 8))
       , ("VkDeviceAddress", (8, 8))
       ]

bespokeOptionality :: CName -> CName -> Maybe (Vector Bool)
bespokeOptionality = \case
  -- These are optional depending on the value of `descriptorType`, treat them
  -- as unconditionally optional and rely on the programmer (and validation
  -- layers) to keep it safe
  "VkWriteDescriptorSet" -> \case
    "pImageInfo"       -> Just (fromList [True])
    "pBufferInfo"      -> Just (fromList [True])
    "pTexelBufferView" -> Just (fromList [True])
    _                  -> Nothing
  _ -> const Nothing

bespokeLengths :: CName -> CName -> Maybe (Vector ParameterLength)
bespokeLengths = \case
  -- Work around https://github.com/KhronosGroup/Vulkan-Docs/issues/1414
  "VkDescriptorSetAllocateInfo" -> \case
    "pSetLayouts" -> Just (fromList [NamedLength "descriptorSetCount"])
    _             -> Nothing
  _ -> const Nothing

bespokeElements
  :: (HasErr r, HasRenderParams r) => Vector (Sem r RenderElement)
bespokeElements =
  fromList
    $  [ namedType
       , baseType "VkSampleMask"    ''Word32
       , baseType "VkFlags"         ''Word32
       , baseType "VkDeviceSize"    ''Word64
       , baseType "VkDeviceAddress" ''Word64
       , nullHandle
       , boolConversion
       ]
    <> wsiTypes

boolConversion :: HasRenderParams r => Sem r RenderElement
boolConversion = genRe "Bool conversion" $ do
  RenderParams {..} <- input
  tellNotReexportable
  let true   = mkPatternName "VK_TRUE"
      false  = mkPatternName "VK_FALSE"
      bool32 = mkTyName "VkBool32"
  tellExport (ETerm (TermName "boolToBool32"))
  tellExport (ETerm (TermName "bool32ToBool"))
  tellImport 'bool
  tellDoc [qqi|
    boolToBool32 :: Bool -> {bool32}
    boolToBool32 = bool {false} {true}

    bool32ToBool :: {bool32} -> Bool
    bool32ToBool = \\case
      {false} -> False
      {true}  -> True
  |]


wsiTypes :: (HasErr r, HasRenderParams r) => [Sem r RenderElement]
wsiTypes = (snd <$> concat [win32, x11, xcb2, zircon, ggp])
  <> concat [win32', xcb1, wayland, metal, android, directfb]

namedType :: HasErr r => Sem r RenderElement
namedType = genRe "namedType" $ do
  tellExplicitModule (vulkanModule ["NamedType"])
  tellNotReexportable
  tellExport (EType (TyConName ":::"))
  tellDoc "-- | Annotate a type with a name\ntype (name :: k) ::: a = a"

baseType
  :: (HasRenderParams r, HasErr r) => CName -> Name -> Sem r RenderElement
baseType n t = fmap identicalBoot . genRe ("base type " <> unCName n) $ do
  RenderParams {..} <- input
  let n' = mkTyName n
  tellExplicitModule (vulkanModule ["Core10", "FundamentalTypes"])
  tellExport (EType n')
  tDoc <- renderType (ConT t)
  tellDocWithHaddock $ \getDoc ->
    vsep [getDoc (TopLevel n), "type" <+> pretty n' <+> "=" <+> tDoc]

----------------------------------------------------------------
-- Base Vulkan stuff
----------------------------------------------------------------

nullHandle :: (HasErr r, HasRenderParams r) => Sem r RenderElement
nullHandle = genRe "null handle" $ do
  RenderParams {..} <- input
  let patName = mkPatternName "VK_NULL_HANDLE"
  tellExplicitModule (vulkanModule ["Core10", "APIConstants"])
  tellNotReexportable
  tellExport (EPat patName)
  tellExport (EType (TyConName "IsHandle"))
  tellImportWithAll (TyConName "Zero")
  tellImport ''Word64
  tellDocWithHaddock $ \getDoc -> [qqi|
    {getDoc (TopLevel "VK_NULL_HANDLE")}
    pattern {patName} :: IsHandle a => a
    pattern {patName} <- ((== zero) -> True)
      where {patName} = zero

    -- | A class for things which can be created with '{patName}'.
    class (Eq a, Zero a) => IsHandle a where
  |]

----------------------------------------------------------------
-- Platform specific nonsense
----------------------------------------------------------------

type BespokeAlias r = ((CName, (Int, Int)), Sem r RenderElement)
-- C name, size, alignment, render element

win32 :: HasRenderParams r => [BespokeAlias r]
win32 =
  [ alias (APtr ''())     "HINSTANCE"
  , alias (APtr ''())     "HWND"
  , alias (APtr ''())     "HMONITOR"
  , alias (APtr ''())     "HANDLE"
  , alias AWord32         "DWORD"
  , alias (APtr ''CWchar) "LPCWSTR"
  ]

win32' :: HasRenderParams r => [Sem r RenderElement]
win32' = [voidData "SECURITY_ATTRIBUTES"]

x11 :: HasRenderParams r => [BespokeAlias r]
x11 =
  [ alias (APtr ''()) "Display"
  , alias AWord64     "VisualID"
  , alias AWord64     "Window"
  , alias AWord64     "RROutput"
  ]

xcb1 :: HasRenderParams r => [Sem r RenderElement]
xcb1 = [voidData "xcb_connection_t"]

xcb2 :: HasRenderParams r => [BespokeAlias r]
xcb2 = [alias AWord32 "xcb_visualid_t", alias AWord32 "xcb_window_t"]

ggp :: HasRenderParams r => [BespokeAlias r]
ggp = [alias AWord32 "GgpStreamDescriptor", alias AWord32 "GgpFrameToken"]

metal :: HasRenderParams r => [Sem r RenderElement]
metal = [voidData "CAMetalLayer"]

wayland :: HasRenderParams r => [Sem r RenderElement]
wayland = [voidData "wl_display", voidData "wl_surface"]

zircon :: HasRenderParams r => [BespokeAlias r]
zircon = [alias AWord32 "zx_handle_t"]

android :: HasRenderParams r => [Sem r RenderElement]
android = [voidData "AHardwareBuffer", voidData "ANativeWindow"]

directfb :: HasRenderParams r => [Sem r RenderElement]
directfb = [voidData "IDirectFB", voidData "IDirectFBSurface"]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data AType = AWord32 | AWord64 | APtr Name

aTypeSize :: AType -> (Int, Int)
aTypeSize = \case
  AWord32 -> (4, 4)
  AWord64 -> (8, 8)
  APtr _  -> (8, 8)

aTypeType :: AType -> H.Type
aTypeType = \case
  AWord32 -> ConT ''Word32
  AWord64 -> ConT ''Word64
  APtr n  -> ConT ''Ptr :@ ConT n

voidData :: HasRenderParams r => CName -> Sem r RenderElement
voidData n = fmap identicalBoot . genRe ("data " <> unCName n) $ do
  RenderParams {..} <- input
  let n' = mkTyName n
  tellExport (EType n')
  tellDoc $ "data" <+> pretty n'

alias :: HasRenderParams r => AType -> CName -> BespokeAlias r
alias t n =
  ( (n, aTypeSize t)
  , fmap identicalBoot . genRe ("alias " <> unCName n) $ do
    RenderParams {..} <- input
    let n' = mkTyName n
    tDoc <- renderType (aTypeType t)
    tellExport (EType n')
    tellDoc $ "type" <+> pretty n' <+> "=" <+> tDoc
  )
