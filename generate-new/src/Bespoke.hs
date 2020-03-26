{-# language QuasiQuotes #-}
{-# language TemplateHaskellQuotes #-}
module Bespoke
  ( forbiddenConstants
  , assignBespokeModules
  , bespokeElements
  , bespokeSizes
  , bespokeSchemes
  , BespokeScheme(..)
  , structChainVar
  , zeroNextPointer
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                , Const
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import qualified Data.Text                     as T
import qualified Data.List.Extra               as List
import           Data.Vector                    ( Vector )
import qualified Data.Vector.Extra             as V
import qualified Data.Map                      as Map
import           Foreign.Ptr
import           Foreign.C.Types
import           Text.InterpolatedString.Perl6.Unindented
import           Language.Haskell.TH            ( mkName )

import           Foreign.Marshal.Alloc
import           Data.Bits
import           Foreign.Marshal.Utils
import           Control.Monad.Trans.Cont       ( ContT )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BS

import           Haskell                       as H
import           Spec.Types
import           Render.Element
import           Render.Utils
import           Render.Stmts
import           Render.Stmts.Utils
import           Render.Stmts.Poke              ( getVectorPoke )
import           Render.Peek                    ( storablePeek
                                                , vectorPeekWithLenRef
                                                )
import           Error
import           Marshal.Scheme
import           Marshal.Marshalable
import           CType

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
  RenderParams {..} <- ask
  let core10Base n = (mkTyName n, ModName "Graphics.Vulkan.Core10.SharedTypes")
  pure
    $  [ ( mkTyName "VkAllocationCallbacks"
         , ModName "Graphics.Vulkan.Core10.AllocationCallbacks"
         )
       , ( mkTyName "VkBaseInStructure"
         , ModName "Graphics.Vulkan.CStruct.Extends"
         )
       , ( mkTyName "VkBaseOutStructure"
         , ModName "Graphics.Vulkan.CStruct.Extends"
         )
       ]
    <> (   core10Base
       <$> [ "VkExtent2D"
           , "VkExtent3D"
           , "VkOffset2D"
           , "VkOffset3D"
           , "VkImageSubresourceLayers"
           , "VkImageSubresourceRange"
           , "VkClearValue"
           , "VkClearColorValue"
           , "VkClearDepthStencilValue"
           ]
       )
    <> (   (, ModName "Graphics.Vulkan.Core10.BaseType")
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
  -- ^ Parent name -> child -> scheme

bespokeSchemes :: Spec -> Sem r [BespokeScheme]
bespokeSchemes spec =
  pure
    $  [baseInOut, wsiScheme, dualPurposeBytestrings, nextPointers spec]
    <> difficultLengths

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
        [ ((sName s, m), scheme)
        | s <- toList specStructs
        , let m = "pNext"
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
    -- , csType = pure $ ForallT [] [ConT (mkName "PokeChain") :@ chainVarT] chainT
    , csType       = pure $ ForallT [] [] chainT
    , csDirectPoke = \chainRef ->
      stmt (Just (ConT ''Ptr :@ chainT)) (Just "pNext") $ do
        tellImportWithAll (TyConName "PokeChain")
        tellImport (TyConName "Chain")
        tellImport 'castPtr
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
        , csType       = do
                           RenderParams {..} <- ask
                           let TyConName sm = mkTyName "VkSampleMask"
                           pure $ ConT ''Vector :@ ConT (mkName (T.unpack sm))
        , csDirectPoke = \vecRef -> do
          RenderParams {..} <- ask
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
                      , "vecLen -> " <+> doBlock
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
          RenderParams {..} <- ask
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
              vectorPeekWithLenRef @a "sampleMask"
                                      (Normal (TypeName "VkSampleMask"))
                                      addrRef
                                      (TypeName "VkSampleMask")
                                      mempty
                                      len
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
        , csType       = pure $ ConT ''ByteString
        , csDirectPoke = \bsRef -> do
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
  ]

structChainVar :: String
structChainVar = "es"

----------------------------------------------------------------
-- Things which are easier to write by hand
----------------------------------------------------------------

bespokeSizes :: [(CName, (Int, Int))]
bespokeSizes =
  (fst <$> concat [win32 @'[Reader RenderParams], x11, xcb2, zircon, ggp])
    <> [ ("VkSampleMask"   , (4, 4))
       , ("VkFlags"        , (4, 4))
       , ("VkDeviceSize"   , (8, 8))
       , ("VkDeviceAddress", (8, 8))
       ]


bespokeElements :: (HasErr r, HasRenderParams r) => Vector (Sem r RenderElement)
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
  RenderParams {..} <- ask
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


wsiTypes :: (HasErr r, Member (Reader RenderParams) r) => [Sem r RenderElement]
wsiTypes =
  putInWSI <$> (snd <$> concat [win32, x11, xcb2, zircon, ggp]) <> concat
    [win32', xcb1, wayland, metal, android]
 where
  putInWSI = fmap $ \re -> re
    { reExplicitModule = reExplicitModule re
      <|> Just (ModName "Graphics.Vulkan.Extensions.WSITypes")
    }


namedType :: HasErr r => Sem r RenderElement
namedType = genRe "namedType" $ do
  tellExplicitModule (ModName "Graphics.Vulkan.NamedType")
  tellNotReexportable
  tellExport (EType (TyConName ":::"))
  tellDoc "-- | Annotate a type with a name\ntype (name :: k) ::: a = a"

baseType
  :: (HasRenderParams r, HasErr r) => CName -> Name -> Sem r RenderElement
baseType n t = fmap identicalBoot . genRe ("base type " <> unCName n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
  tellExplicitModule (ModName "Graphics.Vulkan.Core10.BaseType")
  tellExport (EType n')
  tDoc <- renderType (ConT t)
  tellDocWithHaddock $ \getDoc ->
    vsep [getDoc (TopLevel n), "type" <+> pretty n' <+> "=" <+> tDoc]

----------------------------------------------------------------
-- Base Vulkan stuff
----------------------------------------------------------------

nullHandle :: Member (Reader RenderParams) r => Sem r RenderElement
nullHandle = genRe "null handle" $ do
  RenderParams {..} <- ask
  let patName = mkPatternName "VK_NULL_HANDLE"
  tellExport (EPat patName)
  tellImport 'nullPtr
  tDoc <- renderType (ConT ''Ptr :@ VarT (mkName "a"))
  tellDoc [qqi|
    pattern {patName} :: {tDoc}
    pattern {patName} <- ((== nullPtr) -> True)
      where {patName} = nullPtr
  |]

----------------------------------------------------------------
-- Platform specific nonsense
----------------------------------------------------------------

type BespokeAlias r = ((CName, (Int, Int)), Sem r RenderElement)
-- C name, size, alignment, render element

win32 :: Member (Reader RenderParams) r => [BespokeAlias r]
win32 =
  [ alias (APtr ''())     "HINSTANCE"
  , alias (APtr ''())     "HWND"
  , alias (APtr ''())     "HMONITOR"
  , alias (APtr ''())     "HANDLE"
  , alias AWord32         "DWORD"
  , alias (APtr ''CWchar) "LPCWSTR"
  ]

win32' :: Member (Reader RenderParams) r => [Sem r RenderElement]
win32' = [voidData "SECURITY_ATTRIBUTES"]

x11 :: Member (Reader RenderParams) r => [BespokeAlias r]
x11 =
  [ alias (APtr ''()) "Display"
  , alias AWord64 "VisualID"
  , alias AWord64 "Window"
  , alias AWord64 "RROutput"
  ]

xcb1 :: Member (Reader RenderParams) r => [Sem r RenderElement]
xcb1 = [voidData "xcb_connection_t"]

xcb2 :: Member (Reader RenderParams) r => [BespokeAlias r]
xcb2 = [alias AWord32 "xcb_visualid_t", alias AWord32 "xcb_window_t"]

ggp :: Member (Reader RenderParams) r => [BespokeAlias r]
ggp =
  [ alias AWord32 "GgpStreamDescriptor"
  , alias AWord32 "GgpFrameToken"
  ]

metal :: Member (Reader RenderParams) r => [Sem r RenderElement]
metal =
  [ voidData "CAMetalLayer"
  ]

wayland :: Member (Reader RenderParams) r => [Sem r RenderElement]
wayland =
  [ voidData "wl_display"
  , voidData "wl_surface"
  ]

zircon :: Member (Reader RenderParams) r => [BespokeAlias r]
zircon =
  [alias AWord32 "zx_handle_t"]

android :: Member (Reader RenderParams) r => [Sem r RenderElement]
android =
  [voidData "AHardwareBuffer", voidData "ANativeWindow"]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data AType = AWord32 | AWord64 | APtr Name

aTypeSize :: AType -> (Int, Int)
aTypeSize = \case
  AWord32  -> (4, 4)
  AWord64  -> (8, 8)
  APtr _ -> (8, 8)

aTypeType :: AType -> H.Type
aTypeType = \case
  AWord32 -> ConT ''Word32
  AWord64 -> ConT ''Word64
  APtr n  -> ConT ''Ptr :@ ConT n

voidData :: Member (Reader RenderParams) r => CName -> Sem r RenderElement
voidData n = fmap identicalBoot . genRe ("data " <> unCName n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
  tellExport (EType n')
  tellDoc $ "data" <+> pretty n'

alias :: Member (Reader RenderParams) r => AType -> CName -> BespokeAlias r
alias t n =
  ( (n, aTypeSize t)
  , fmap identicalBoot . genRe ("alias " <> unCName n) $ do
    RenderParams {..} <- ask
    let n' = mkTyName n
    tDoc <- renderType (aTypeType t)
    tellExport (EType n')
    tellDoc $ "type" <+> pretty n' <+> "=" <+> tDoc
  )
