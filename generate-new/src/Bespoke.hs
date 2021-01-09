{-# LANGUAGE NamedFieldPuns #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Bespoke
  ( forbiddenConstants
  , forceDisabledExtensions
  , assignBespokeModules
  , bespokeStructsAndUnions
  , bespokeElements
  , bespokeSizes
  , bespokeOptionality
  , bespokeLengths
  , bespokeZeroInstances
  , bespokeZeroCStruct
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
import qualified Language.Haskell.TH.Syntax    as TH
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
import           Data.List                      ( lookup )
import           Error
import           Foreign.C.String               ( CString )
import           Foreign.Storable               ( Storable(poke) )
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

----------------------------------------------------------------
-- Changes to the spec
----------------------------------------------------------------

-- | These constants are defined elsewhere
forbiddenConstants :: [CName]
forbiddenConstants = ["VK_TRUE", "VK_FALSE", "XR_TRUE", "XR_FALSE"]

forceDisabledExtensions :: [ByteString]
forceDisabledExtensions = ["XR_EXT_conformance_automation"]

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
  mod'              <- mkMkModuleName
  pure
    $  [ ( mkTyName "VkAllocationCallbacks"
         , mod' ["Core10", "AllocationCallbacks"]
         )
       , (mkTyName "VkBaseInStructure" , mod' ["CStruct", "Extends"])
       , (mkTyName "VkBaseOutStructure", mod' ["CStruct", "Extends"])
       , (mkTyName "XrBaseInStructure" , mod' ["CStruct", "Extends"])
       , (mkTyName "XrBaseOutStructure", mod' ["CStruct", "Extends"])
       , (mkTyName "XrFovf"            , mod' ["Core10", "OtherTypes"])
       , (mkTyName "XrPosef"           , mod' ["Core10", "Space"])
       ]
    <> (   (, mod' ["Core10", "FundamentalTypes"])
       <$> [ mkTyName "VkBool32"
           , TermName "boolToBool32"
           , TermName "bool32ToBool"
           , mkTyName "XrBool32"
           , mkTyName "XrOffset2Df"
           , mkTyName "XrExtent2Df"
           , mkTyName "XrRect2Df"
           , mkTyName "XrOffset2Di"
           , mkTyName "XrExtent2Di"
           , mkTyName "XrRect2Di"
           ]
       )

----------------------------------------------------------------
-- Schemes
----------------------------------------------------------------

data BespokeScheme where
  BespokeScheme ::(forall a. Marshalable a => CName -> a -> Maybe (MarshalScheme a)) -> BespokeScheme
  --- ^ Parent name -> child -> scheme

bespokeSchemes :: KnownSpecFlavor t => Spec t -> Sem r [BespokeScheme]
bespokeSchemes spec =
  pure
    $  [baseInOut, wsiScheme, dualPurposeBytestrings, nextPointers spec]
    <> difficultLengths
    <> [bitfields]
    <> [accelerationStructureGeometry]
    <> [buildingAccelerationStructures]
    <> openXRSchemes

baseInOut :: BespokeScheme
baseInOut = BespokeScheme $ \case
  n | n `elem` ["VkBaseInStructure", "VkBaseOutStructure"] -> \case
    m | "pNext" <- name m -> Just $ Normal (type' m)
    _                     -> Nothing
  n | n `elem` ["XrBaseInStructure", "XrBaseOutStructure"] -> \case
    m | "next" <- name m -> Just $ Normal (type' m)
    _                    -> Nothing
  _ -> const Nothing

data NextType = NextElided | NextChain

nextPointers :: forall t . KnownSpecFlavor t => Spec t -> BespokeScheme
nextPointers Spec {..} =
  let schemeMap :: Map (CName, CName) NextType
      schemeMap = Map.fromList
        [ ((sName s, nextName), scheme)
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
  nextName :: CName
  nextName = case specFlavor @t of
    SpecVk -> "pNext"
    SpecXr -> "next"
  chainVarT   = VarT (mkName structChainVar)
  chainT      = ConT (mkName "Chain") :@ chainVarT
  chainScheme = CustomScheme
    { csName       = "Chain"
    , csZero       = Just "()"
    , csZeroIsZero = False -- Pointer to chain
    -- , csType = pure $ ForallT [] [ConT (mkName "PokeChain") :@ chainVarT] chainT
    , csType       = pure $ ForallT [] [] chainT
    , csDirectPoke = APoke $ \chainRef ->
      stmt (Just (ConT ''Ptr :@ chainT)) (Just (unCName nextName)) $ do
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
        =<< storablePeek nextName addrRef (Ptr Const (Ptr Const Void))
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
      { sName        = "VkTransformMatrixKHR"
      , sMembers     = V.fromList
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
      , sSize        = ()
      , sAlignment   = ()
      , sExtends     = mempty
      , sExtendedBy  = mempty
      , sInherits    = mempty
      , sInheritedBy = mempty
      }
  ]

bespokeSizes :: SpecFlavor -> [(CName, (Int, Int))]
bespokeSizes t =
  let
    xrSizes =
      [ ("XrFlags64"                , (8, 8))
        , ("XrTime"                   , (8, 8))
        , ("XrDuration"               , (8, 8))
        , ("XrVersion"                , (8, 8))
        , ("timespec"                 , (16, 8))
        -- TODO: Can these be got elsewhere?
        , ("VkResult"                 , (4, 4))
        , ("VkFormat"                 , (4, 4))
        , ("VkInstance"               , (8, 8))
        , ("VkPhysicalDevice"         , (8, 8))
        , ("VkImage"                  , (8, 8))
        , ("VkDevice"                 , (8, 8))
        , ("PFN_vkGetDeviceProcAddr"  , (8, 8))
        , ("PFN_vkGetInstanceProcAddr", (8, 8))
        ]
        <> (fst <$> concat
             [win32Xr @'[Input RenderParams], x11Shared, xcb2Xr, egl, gl, d3d]
           )
    vkSizes =
      [ ("VkSampleMask"   , (4, 4))
        , ("VkFlags"        , (4, 4))
        , ("VkDeviceSize"   , (8, 8))
        , ("VkDeviceAddress", (8, 8))
        ]
        <> (fst <$> concat
             [win32 @'[Input RenderParams], x11Shared, x11, xcb2, zircon, ggp]
           )
    sharedSizes = []
  in
    sharedSizes <> case t of
      SpecVk -> vkSizes
      SpecXr -> xrSizes

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
  -- Because we don't marshal ppGeometries, this is not actually optional
  -- See https://github.com/expipiplus1/vulkan/issues/239
  "VkAccelerationStructureBuildGeometryInfoKHR" -> \case
    "pGeometries" -> Just mempty
    _             -> Nothing
  _ -> const Nothing

bespokeLengths :: CName -> CName -> Maybe (Vector ParameterLength)
bespokeLengths = \case
  -- Work around https://github.com/KhronosGroup/Vulkan-Docs/issues/1414
  "VkDescriptorSetAllocateInfo" -> \case
    "pSetLayouts" -> Just (fromList [NamedLength "descriptorSetCount"])
    _             -> Nothing
  _ -> const Nothing

bespokeZeroInstances
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => HasRenderElem r => CName -> Maybe (Sem r ())
bespokeZeroInstances = flip
  lookup
  [ ( "VkTransformMatrixKHR"
    , do
      tellImportWithAll (TyConName "Zero")
      tellDoc [qqi|
        instance Zero TransformMatrixKHR where
         zero = TransformMatrixKHR
                  (1,0,0,0)
                  (0,1,0,0)
                  (0,0,1,0)
      |]
    )
  ]

bespokeZeroCStruct
  :: ( HasErr r
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo StructMember r
     , HasStmts r
     )
  => HasRenderElem r => CName -> Maybe (Sem r (Doc ()))
bespokeZeroCStruct = flip
  lookup
  [ ( "VkTransformMatrixKHR"
    , do
      tellImport ''CFloat
      tellImport 'plusPtr
      tellImportWith ''Storable 'poke
      pure [qqi|
        pokeZeroCStruct p f = do
          poke (p `plusPtr` 0) (CFloat 1)
          poke (p `plusPtr` 20) (CFloat 1)
          poke (p `plusPtr` 40) (CFloat 1)
          f
      |]
    )
  ]

bespokeElements
  :: (HasErr r, HasRenderParams r) => SpecFlavor -> Vector (Sem r RenderElement)
bespokeElements = \case
  SpecVk ->
    fromList
      $  shared
      <> [ baseType "VkSampleMask"    ''Word32
         , baseType "VkFlags"         ''Word32
         , baseType "VkDeviceSize"    ''Word64
         , baseType "VkDeviceAddress" ''Word64
         ]
      <> wsiTypes SpecVk
  SpecXr ->
    fromList
      $  shared
      <> [ baseType "XrFlags64"  ''Word64
         , baseType "XrTime"     ''Int64
         , baseType "XrDuration" ''Int64
         ]
      <> wsiTypes SpecXr
      <> [resultMatchers]
  where shared = fromList [namedType, nullHandle, boolConversion]

boolConversion :: HasRenderParams r => Sem r RenderElement
boolConversion = genRe "Bool conversion" $ do
  RenderParams {..} <- input
  tellNotReexportable
  let true   = mkPatternName (CName $ upperPrefix <> "_TRUE")
      false  = mkPatternName (CName $ upperPrefix <> "_FALSE")
      bool32 = mkTyName (CName $ camelPrefix <> "Bool32")
  tellExport (ETerm (TermName "boolToBool32"))
  tellExport (ETerm (TermName "bool32ToBool"))
  tellImport 'bool
  tellImportWithAll bool32
  tellDoc [qqi|
    boolToBool32 :: Bool -> {bool32}
    boolToBool32 = bool {false} {true}

    bool32ToBool :: {bool32} -> Bool
    bool32ToBool = \\case
      {false} -> False
      {true}  -> True
  |]

wsiTypes
  :: (HasErr r, HasRenderParams r) => SpecFlavor -> [Sem r RenderElement]
wsiTypes = \case
  SpecVk -> (snd <$> concat [win32, x11Shared, x11, xcb2, zircon, ggp])
    <> concat [win32', xcb1, waylandShared, wayland, metal, android, directfb]
  SpecXr -> (snd <$> concat [win32Xr, x11Shared, xcb2Xr, egl, gl, d3d])
    <> concat [win32Xr', xcb1, waylandShared, d3d', jni, timespec]

namedType :: (HasRenderParams r, HasErr r) => Sem r RenderElement
namedType = genRe "namedType" $ do
  tellExplicitModule =<< mkModuleName ["NamedType"]
  tellNotReexportable
  tellExport (EType (TyConName ":::"))
  tellDoc "-- | Annotate a type with a name\ntype (name :: k) ::: a = a"

baseType
  :: (HasRenderParams r, HasErr r) => CName -> Name -> Sem r RenderElement
baseType n t = fmap identicalBoot . genRe ("base type " <> unCName n) $ do
  RenderParams {..} <- input
  let n' = mkTyName n
  tellExplicitModule =<< mkModuleName ["Core10", "FundamentalTypes"]
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
  tellExplicitModule =<< mkModuleName ["Core10", "APIConstants"]
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
-- Base XR stuff
----------------------------------------------------------------

resultMatchers :: (HasErr r, HasRenderParams r) => Sem r RenderElement
resultMatchers = genRe "xr result matchers" $ do
  RenderParams {..} <- input
  let succName = mkPatternName "XR_SUCCEEDED"
      unquName = mkPatternName "XR_UNQUALIFIED_SUCCESS"
      failName = mkPatternName "XR_FAILED"
  tellExplicitModule =<< mkModuleName ["Core10", "Enums", "Result"]
  tellNotReexportable
  tellExport (EPat succName)
  tellExport (EPat unquName)
  tellExport (EPat failName)
  tellDocWithHaddock $ \getDoc -> [qqi|
    {getDoc (TopLevel "XR_SUCCEEDED")}
    pattern SUCCEEDED :: Result
    pattern SUCCEEDED <- ((SUCCESS <=) -> True)

    {getDoc (TopLevel "XR_UNQUALIFIED_SUCCESS")}
    pattern UNQUALIFIED_SUCCESS :: Result
    pattern UNQUALIFIED_SUCCESS <- ((SUCCESS ==) -> True)

    {getDoc (TopLevel "XR_FAILED")}
    pattern FAILED :: Result
    pattern FAILED <- ((SUCCESS >) -> True)

    \{-# complete SUCCEEDED, FAILED #-}
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

win32Xr :: HasRenderParams r => [BespokeAlias r]
win32Xr =
  [ alias (APtr ''()) "HDC" -- TODO: should be an alias for HANDLE
  , alias (APtr ''()) "HGLRC" -- TODO: check this
  , alias AWord64     "LUID"
  , alias AWord64     "LARGE_INTEGER"
  ]

win32Xr' :: HasRenderParams r => [Sem r RenderElement]
win32Xr' = [voidData "IUnknown"]

x11Shared :: HasRenderParams r => [BespokeAlias r]
x11Shared = [alias (APtr ''()) "Display"]

x11 :: HasRenderParams r => [BespokeAlias r]
x11 =
  [alias AWord64 "VisualID", alias AWord64 "Window", alias AWord64 "RROutput"]

xcb1 :: HasRenderParams r => [Sem r RenderElement]
xcb1 = [voidData "xcb_connection_t"]

xcb2 :: HasRenderParams r => [BespokeAlias r]
xcb2 = [alias AWord32 "xcb_visualid_t", alias AWord32 "xcb_window_t"]

xcb2Xr :: HasRenderParams r => [BespokeAlias r]
xcb2Xr =
  [ alias AWord32 "xcb_visualid_t"
  , alias AWord32 "xcb_glx_fbconfig_t"
  , alias AWord32 "xcb_glx_drawable_t"
  , alias AWord32 "xcb_glx_context_t"
  ]


ggp :: HasRenderParams r => [BespokeAlias r]
ggp = [alias AWord32 "GgpStreamDescriptor", alias AWord32 "GgpFrameToken"]

metal :: HasRenderParams r => [Sem r RenderElement]
metal = [voidData "CAMetalLayer"]

waylandShared :: HasRenderParams r => [Sem r RenderElement]
waylandShared = [voidData "wl_display"]

wayland :: HasRenderParams r => [Sem r RenderElement]
wayland = [voidData "wl_surface"]

zircon :: HasRenderParams r => [BespokeAlias r]
zircon = [alias AWord32 "zx_handle_t"]

android :: HasRenderParams r => [Sem r RenderElement]
android = [voidData "AHardwareBuffer", voidData "ANativeWindow"]

directfb :: HasRenderParams r => [Sem r RenderElement]
directfb = [voidData "IDirectFB", voidData "IDirectFBSurface"]

----------------------------------------------------------------
-- OpenXR platform stuff
----------------------------------------------------------------

egl :: HasRenderParams r => [BespokeAlias r]
egl =
  [ alias (AFunPtr $(TH.lift =<< [t|CString -> IO (FunPtr (IO ()))|]))
          "PFNEGLGETPROCADDRESSPROC"
  , alias (APtr ''()) "EGLDisplay"
  , alias (APtr ''()) "EGLConfig"
  , alias (APtr ''()) "EGLContext"
  ]

gl :: HasRenderParams r => [BespokeAlias r]
gl =
  [ alias (APtr ''()) "GLXFBConfig"
  , alias AWord64     "GLXDrawable"
  , alias (APtr ''()) "GLXContext"
  ]

d3d :: HasRenderParams r => [BespokeAlias r]
d3d = [alias AWord32 "D3D_FEATURE_LEVEL"]

d3d' :: HasRenderParams r => [Sem r RenderElement]
d3d' =
  [ voidData "ID3D11Device"
  , voidData "ID3D11Texture2D"
  , voidData "ID3D12CommandQueue"
  , voidData "ID3D12Device"
  , voidData "ID3D12Resource"
  ]

jni :: HasRenderParams r => [Sem r RenderElement]
jni = [voidData "jobject"]

timespec :: HasRenderParams r => [Sem r RenderElement]
timespec = pure $ genRe "timespec" $ do
  tellImport ''CTime
  tellImport ''Int64
  tellImport ''Typeable
  tellImport ''Generic
  tellImportWithAll ''Storable
  tellImport 'castPtr
  tellImport 'plusPtr
  tellDataExport (TyConName "Timespec")
  tellDoc [qqi|
    data Timespec = Timespec
      \{ tv_sec :: CTime
      , tv_nsec :: Int64
      }
      deriving (Typeable, Generic, Read, Show, Eq, Ord)

    instance Storable Timespec where
      sizeOf ~_ = 16
      alignment ~_ = 8
      peek p = Timespec
        <$> peek (castPtr @Timespec @CTime p)
        <*> peek (plusPtr @Timespec @Int64 p 8)
      poke p (Timespec s n) = do
        poke (castPtr @Timespec @CTime p) s
        poke (plusPtr @Timespec @Int64 p 8) n
  |]

----------------------------------------------------------------
-- OpenXR stuff
----------------------------------------------------------------

openXRSchemes :: [BespokeScheme]
openXRSchemes =
  [ BespokeScheme $ \case
      "XrEventDataBuffer" -> \case
        a | "varying" <- name a -> Just ByteString
        _                       -> Nothing
      "XrSpatialGraphNodeSpaceCreateInfoMSFT" -> \case
        a | "nodeId" <- name a -> Just ByteString
        _                      -> Nothing
      -- Work around https://github.com/KhronosGroup/OpenXR-Docs/issues/69
      f
        | f
          `elem` [ "xrPathToString"
                 , "xrGetInputSourceLocalizedName"
                 , "xrGetVulkanInstanceExtensionsKHR"
                 , "xrGetVulkanDeviceExtensionsKHR"
                 ]
        -> \case
          a | "buffer" <- name a -> Just (Returned ByteString)
          _                      -> Nothing

      _ -> const Nothing
  ]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data AType = AWord32 | AWord64 | APtr Name | AFunPtr H.Type

aTypeSize :: AType -> (Int, Int)
aTypeSize = \case
  AWord32   -> (4, 4)
  AWord64   -> (8, 8)
  APtr    _ -> (8, 8)
  AFunPtr _ -> (8, 8)

aTypeType :: AType -> H.Type
aTypeType = \case
  AWord32   -> ConT ''Word32
  AWord64   -> ConT ''Word64
  APtr    n -> ConT ''Ptr :@ ConT n
  AFunPtr t -> ConT ''FunPtr :@ t

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
