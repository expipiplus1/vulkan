{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Write.Wrapper
  ( commandWrapper
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Category                         ((>>>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict              hiding ((<>))
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Monoid                              (Endo (..))
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Savvy.Type.Haskell                  as H

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Struct
import           Write.Util

commandWrapper
  :: (Text -> Bool)
  -- ^ Is this the name of a handle type
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> Command
  -> Either Text WriteElement
commandWrapper isHandle isBitmask command = do
  let weName = cName command T.<+> "wrapper"
      isDefaultable t =
        maybe False (isHandle <||> isBitmask) (simpleTypeName t)
  (weDoc, (weImports, weProvides, weDepends, weExtensions)) <- either
    (throwError . (cName command T.<+>))
    (pure . first (const . vcat))
    (runWrap $ wrapCommand isDefaultable command)
  pure WriteElement {..}

----------------------------------------------------------------
-- Monad for wrapping
----------------------------------------------------------------

type WrapM = WriterT ([Import], [Guarded Export], [Guarded HaskellName], [Text]) (Except Text)

runWrap
  :: WrapM a
  -> Either Text (a, ([Import], [Guarded Export], [Guarded HaskellName], [Text]))
runWrap w = runExcept . runWriterT $ w

tellImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellImport m v = tell ([Import m [v]], [], [], [])

tellQualifiedImport
  :: Text
  -- ^ Module
  -> Text
  -- ^ Value
  -> WrapM ()
tellQualifiedImport m v = tell ([QualifiedImport m [v]], [], [], [])

tellImports
  :: [Import]
  -> WrapM ()
tellImports is = tell (is, [], [], [])

tellExport
  :: Guarded Export
  -> WrapM ()
tellExport e = tell ([], [e], [], [])

tellDepend
  :: Guarded HaskellName
  -> WrapM ()
tellDepend d = tell ([], [], [d], [])

tellDepends
  :: [Guarded HaskellName]
  -> WrapM ()
tellDepends ds = tell ([], [], ds, [])

tellExtension
  :: Text
  -> WrapM ()
tellExtension e = tell ([], [], [], [e])

toHsType :: Type -> WrapM (Doc ())
toHsType t = case H.toHsType t of
  Left es -> throwError ("Failure making Haskell type: " <> T.tShow es)
  Right (d, (is, es)) -> do
    tell (is, [], [], es)
    -- TODO: This is a bit of a hack
    tellDepends (Unguarded <$> typeDepends t)
    pure d

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapCommand
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> Command
  -> WrapM [Doc ()]
wrapCommand isDefaultable Command {..} = do
  let lengthPairs :: [(Parameter, Maybe Text, [Parameter])]
      lengthPairs = getLengthPointerPairs cParameters
      makeWts usage =
        parametersToWrappingTypes isDefaultable usage lengthPairs cParameters
  let printTypes makeName wts = do
        sig <- wtsToSig cReturnType wts
        pure $ line <> [qci|
          -- | Wrapper for {cName}
          {makeName cName} :: {sig}
          {makeName cName} = {wrap wts (pretty cName) }|]
  ds <- if isDualUseCommand lengthPairs
    then do
      wts1 <- printTypes funGetLengthName =<< makeWts (Just CommandGetLength)
      wts2 <- printTypes funName =<< makeWts (Just CommandGetValues)
      pure [wts1, wts2]
    else fmap pure $ printTypes funName =<< makeWts Nothing
  tellDepend (Unguarded (TermName cName))
  pure ds


isDualUseCommand :: [(Parameter, Maybe Text, [Parameter])] -> Bool
isDualUseCommand = \case
  -- For simplicity, only handle these commands if they have one array parameter
  -- The length must not be a member
  -- There must only be one return array
  [(length, Nothing, [vector])]
    | -- The length parameter must be a valid pointer, but it can be
      -- unspecified if the vector pointer is null, i.e. we are querying the size.
      Just [False, True] <- pIsOptional length
      -- The vector pointer can be null, i.e. we are querying the size.
    , Just [True] <- pIsOptional vector
    -> True
  _ -> False

wtsToSig :: Type -> [WrappingType] -> WrapM (Doc ())
wtsToSig returnType ts = do
  returnTypeDoc <- toHsType returnType
  let outputs = [ t | WrappingType _ (Just (Output t _)) _ <- ts ]
      ret     = tupled (returnTypeDoc : outputs)
  pure $ intercalateArrows (mapMaybe (fmap iType . wtInput) ts <> ["IO" <+> ret])

parametersToWrappingTypes
  :: (Type -> Bool)
  -- ^ Is this type defaultable (a handle or bitmask)
  -> Maybe CommandUsage
  -- ^ If this is a dual use command, which usage shall we generate
  -> [(Parameter, Maybe Text, [Parameter])]
  -- ^ (parameter containing length, name of member representing length,
  -- vector parameters which must be this length)
  -> [Parameter]
  -> WrapM [WrappingType]
parametersToWrappingTypes isDefaultable maybeCommandUsage lengthPairs ps =
  let
    -- Go from a length name to a list of vectors having that length
    -- TODO: Rename
    nonMemberLengthMap :: Text -> Maybe [Parameter]
    nonMemberLengthMap =
      (`Map.lookup` Map.fromList
        [ (pName l, vs) | (l, Nothing, vs) <- lengthPairs ]
      )

    -- For an output vector: Get the input vectors which determine the length
    inputVectorLengthMap :: Text -> [Parameter]
    inputVectorLengthMap outVectorName =
      [ p
      | (_, Nothing, vs) <- lengthPairs
      , outVectorName `elem` (pName <$> vs)
      , p@(Parameter _ (Ptr Const _) _ _) <- vs
      ]

    isLengthPairList :: Text -> Bool
    isLengthPairList =
      (`elem` [ pName v | (_, Nothing, vs) <- lengthPairs, v <- vs ])

    parameterToWrappingType :: Parameter -> WrapM WrappingType
    parameterToWrappingType p
      | Just vectors <- nonMemberLengthMap (pName p)
      , length [ () | Parameter _ (Ptr Const _) _ _ <- vectors ] > 1
      = throwError "More then two input vectors with same length specifier"
      | otherwise
      = let name = pName p
        in
          case pType p of
            Void -> throwError "void parameter"
            Array Const (NumericArraySize n) t
              | n >= 2, isSimpleType t, Nothing <- pIsOptional p -> do
                tyName <- toHsType t
                (w, _) <- tupleWrap n (pName p)
                pure $ InputType (Input name (tupleTy n tyName)) w
            Array{}   -> throwError "array parameter"
            Proto _ _ -> throwError "proto paramter"
            t
              | -- The length of an array of typed values, length is given in
                -- number of values
                [vector] <-
                [ v
                | Just vs <- pure $nonMemberLengthMap (pName p)
                , v@(Parameter _ (Ptr Const _) _ _) <- vs
                ]
              , isSimpleType t
              , Ptr _ vType <- pType vector
              -> do
                tyName <- toHsType t
                w      <- lengthWrap (dropPointer (pName vector)) name
                pure $ InferredType w
              | -- The length of an array of untyped (void*) values, length is
                -- given in bytes.
                Just [vector] <- nonMemberLengthMap (pName p)
              , isSimpleType t
              , Ptr _ Void <- pType vector
              -> InferredType <$> lengthBytesWrap (dropPointer (pName vector))
              | isSimpleType t
              , isNothing (pIsOptional p) || isDefaultable t
              -> do
                tyName <- toHsType t
                pure $ InputType (Input name tyName) (simpleWrap (pName p))
            Ptr Const t
              | Nothing <- pLength p, isSimpleType t, Nothing <- pIsOptional p
              -> do
                tyName <- toHsType t
                w      <- allocaWrap (pName p)
                pure $ InputType (Input name tyName) w
              | Nothing <- pLength p, isSimpleType t, Just [True] <- pIsOptional
                p
              -> do
                tyName <- toHsType t
                w      <- optionalAllocaWrap (pName p)
                pure $ InputType (Input name ("Maybe " <> tyName)) w
              | isLengthPairList (pName p), isSimpleType t, Nothing <-
                pIsOptional p
              -> do
                tyName <- toHsType t
                w      <- vecWrap (pName p)
                pure $ InputType (Input name ("Vector " <> tyName)) w
              | -- A const void pointer, represent as a 'Vector a' where a is
                -- storable
                isLengthPairList (pName p), Void <- t, Nothing <- pIsOptional p
              -> do
                tellImport "Data.Vector.Storable" "Vector"
                w <- voidVecWrap (pName p)
                pure $ InputType (Input name ("Vector " <> "a")) w
              | -- A non optional string
                Char <- t, Just NullTerminated <- pLength p, Nothing <-
                pIsOptional p
              -> do
                w <- cStringWrap (pName p)
                tellImport "Data.ByteString" "ByteString"
                pure $ InputType (Input name "ByteString") w
              | -- An optional string
                Char <- t, Just NullTerminated <- pLength p, Just [True] <-
                pIsOptional p
              -> do
                w <- optionalCStringWrap (pName p)
                tellImport "Data.ByteString" "ByteString"
                pure $ InputType (Input name "Maybe ByteString") w
              | otherwise
              -> throwError "array ptr parameter"
            Ptr NonConst t
              | -- A pointer to a non-optional return value
                Nothing <- pLength p
              , isSimpleType t
              , Nothing <- pIsOptional p
              -> do
                -- This is probaby the most fishy case, it assumes that the
                -- pointers being sent in are for returning variables only.
                tyName   <- toHsType t
                (w, ptr) <- simpleAllocaOutputWrap (pName p)
                tellImport "Foreign.Storable" "peek"
                pure $ OutputType (Output tyName ("peek" <+> ptr)) w
              | -- A pointer to an optional return value
                Nothing <- pLength p
              , isSimpleType t
              , Just [False, True] <- pIsOptional p
              , isDefaultable t
              -> do
                tyName   <- toHsType t
                (w, ptr) <- simpleAllocaOutputWrap (pName p)
                tellImport "Foreign.Storable" "peek"
                pure $ OutputType (Output tyName ("peek" <+> ptr)) w
              | -- A vector to read which will be made as long as a member of a
                -- struct being passed in g
                Just (NamedMemberLength s m) <- pLength p
              , isSimpleType t
              , Nothing <- pIsOptional p
              -> do
                tyName <- toHsType t
                tellExtension "DuplicateRecordFields"
                let lengthExpression = toRecordMemberName m <> " " <> dropPointer s
                peek <- peekForeignPtrOutput (pName p) lengthExpression
                w    <- vectorForeignPtrOutputWrap (pName p) lengthExpression
                pure $ OutputType (Output ("Vector " <> tyName) peek) w
              | -- A vector to read which will be made as long as an input parameter
                Just (NamedLength lengthParamName) <- pLength p
              , isSimpleType t
              , Nothing <- pIsOptional p
              , [inputVector] <- inputVectorLengthMap (pName p)
              -> do
                tyName <- toHsType t
                -- TODO: Make sure that this is always correct, the output
                -- length is sometimes determined by a parameter, should only
                -- *peek* that much
                let lengthExpression =
                      "(Data.Vector.Storable.length "
                        <> dropPointer (pName inputVector)
                        <> ")"
                peek <- peekForeignPtrOutput (pName p) lengthExpression
                w    <- vectorForeignPtrOutputWrap (pName p) lengthExpression
                pure $ OutputType (Output ("Vector " <> tyName) peek) w
              | -- The length dual use command output value
                -- It is not an array
                Nothing <- pLength p
              , -- It has uint32_t type
                Just "uint32_t" <- simpleTypeName t
              , -- Is is a required pointer to an optional value
                Just [False, True] <- pIsOptional p
              , -- We are generating a dual use command
                Just commandUsage <- maybeCommandUsage
              -> do
                tyName <- toHsType t
                case commandUsage of
                  CommandGetLength -> do
                    (w, ptr) <- simpleAllocaOutputWrap (pName p)
                    pure $ OutputType (Output tyName ("peek" <+> ptr)) w
                  CommandGetValues -> do
                    w <- allocaWrap (pName p)
                    pure $ InputType (Input name tyName) w
              | -- The value dual use command output
                -- It is an array with a parameter length
                isLengthPairList (pName p)
              , -- It is not required
                Just [True] <- pIsOptional p
              , -- We are generating a dual use command
                Just commandUsage <- maybeCommandUsage
              , isSimpleType t
              , Just (NamedLength lengthParamName) <- pLength p
              -> do
                tyName <- toHsType t
                case commandUsage of
                  CommandGetLength -> InferredType <$> passNullPtrWrap
                  CommandGetValues -> do
                    peek <- peekForeignPtrOutputPeekLength
                      (pName p)
                      (ptrName (dropPointer lengthParamName))
                    w <- vectorForeignPtrOutputWrap
                      (pName p)
                      (dropPointer lengthParamName)
                    pure $ OutputType (Output ("Vector " <> tyName) peek) w
            t -> throwError ("unhandled type: " <> T.tShow t)
  in
    traverse parameterToWrappingType ps

-- | Returns (parameter containing length, name of member representing length,
-- vector parameters)
getLengthPointerPairs :: [Parameter] -> [(Parameter, Maybe Text, [Parameter])]
getLengthPointerPairs parameters =
  let
    -- A list of all const arrays with lengths
    arrays :: [((Text, Maybe Text), Parameter)]
    arrays =
      [ (lm, p)
      | p@(Parameter _ (Ptr _ _) (Just lenSpecifier) _) <- parameters
      , lm <- case lenSpecifier of
        NamedLength l         -> pure (l, Nothing)
        NamedMemberLength l m -> pure (l, Just m)
        _                     -> []
      ]
    -- We have a pair if the length is determined by exactly one array
    uniqueLengthPairs :: [(Text, Maybe Text, [Parameter])]
    uniqueLengthPairs =
      [ (length, m, ps)
      | ((length, m), ps) <- MultiMap.assocs (MultiMap.fromList arrays)
      ]
    -- A map from name to parameters
    parameterMap = (`Map.lookup` Map.fromList ((pName &&& id) <$> parameters))
  in
    -- Only return pairs where the length is another parameter
    mapMaybe (\(length, m, ps) -> (, m, ps) <$> parameterMap length)
             uniqueLengthPairs

data WrappingType = WrappingType
  { wtInput  :: Maybe Input
    -- TODO: would wtWrapSig work better?
  , wrOutput :: Maybe Output
  , wtWrap   :: Wrapper
    -- ^ A function taking a continuation to wrap the wrapped value and
    -- returning a wrapper
    -- TODO: Make better types!
  }

type Wrapper = (Doc () -> Doc ()) -> Doc () -> Doc ()

data Input = Input
  { iName :: Text
  , iType :: Doc ()
  }

data Output = Output
  { oType :: Doc ()
  , oPeek :: Doc ()
    -- ^ An expression of type "IO oType"
  }

pattern InputType i w = WrappingType (Just i) Nothing w
pattern OutputType o w = WrappingType Nothing (Just o) w
pattern InferredType w = WrappingType Nothing Nothing w

----------------------------------------------------------------
-- Parameter Wrappers
----------------------------------------------------------------

simpleWrap
  :: Text
  -- ^ Parameter name
  -> Wrapper
simpleWrap paramName cont e =
  [qci|\\{pretty (unKeyword paramName)} -> {cont (e <+> pretty (unKeyword paramName))}|]

cStringWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
cStringWrap paramName = do
  tellImport "Data.ByteString" "useAsCString"
  pure $ \cont e ->
    let -- TODO: Use unkeyword everywhere
        param    = pretty (unKeyword $ dropPointer paramName)
        paramPtr = pretty (unKeyword $ ptrName (dropPointer paramName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|useAsCString {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|\\{param} -> {cont withPtr})|]

optionalCStringWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
optionalCStringWrap paramName = do
  tellImport "Data.ByteString"       "useAsCString"
  tellImport "Foreign.Marshal.Utils" "maybeWith"
  pure $ \cont e ->
    let -- TODO: Use unkeyword everywhere
      param    = pretty (unKeyword $ dropPointer paramName)
      paramPtr = pretty (unKeyword $ ptrName (dropPointer paramName))
      -- Note the bracket opened here is closed after cont!
      withPtr =
        [qci|maybeWith useAsCString {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in
      [qci|\\{param} -> {cont withPtr})|]

simpleAllocaOutputWrap
  :: Text
  -- ^ Parameter name
  -> WrapM (Wrapper, Doc ())
  -- ^ The wrapper and the name of the pointer used
simpleAllocaOutputWrap paramName = do
  tellImport "Foreign.Marshal.Alloc" "alloca"
  let paramPtr = pretty (ptrName (dropPointer paramName))
  pure . (,paramPtr) $ \cont e ->
    let withPtr  = [qci|alloca (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|{cont withPtr})|]

allocaWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
allocaWrap paramName = do
  tellImport "Foreign.Marshal.Utils" "with"
  pure $ \cont e ->
    let param    = pretty (dropPointer paramName)
        paramPtr = pretty (ptrName (dropPointer paramName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|with {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|\\{param} -> {cont withPtr})|]

optionalAllocaWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
optionalAllocaWrap paramName = do
  tellImport "Foreign.Marshal.Utils" "with"
  tellImport "Foreign.Marshal.Utils" "maybeWith"
  pure $ \cont e ->
    let
      param    = dropPointer paramName
      paramPtr = ptrName (dropPointer paramName)
      -- Note the bracket opened here is closed after cont!
      withPtr  = [qci|maybeWith with {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in
      [qci|\\{param} -> {cont withPtr})|]

lengthWrap
  :: Text
  -- ^ Vector name
  -> Text
  -- ^ Parameter name
  -> WrapM Wrapper
lengthWrap vec paramName = do
  tellQualifiedImport "Data.Vector.Storable" "length"
  pure $ \cont e ->
    cont [qci|{e} (fromIntegral $ Data.Vector.Storable.length {unKeyword vec})|]
    -- let withLength = cont [qci|{e} {paramName}|]
    -- in
    --   [qci|let {paramName} = (fromIntegral $ Data.Vector.Storable.length {unKeyword vec}) in {withLength}|]

lengthBytesWrap
  :: Text
  -- ^ Vector name
  -> WrapM Wrapper
lengthBytesWrap vec = do
  tellImport          "Foreign.Storable"     "sizeOf"
  tellQualifiedImport "Data.Vector.Storable" "length"
  pure $ \cont e ->
    cont
      [qci|{e} (fromIntegral $ sizeOf (head {unKeyword vec}) * Data.Vector.Storable.length {unKeyword vec})|]

passNullPtrWrap :: WrapM Wrapper
passNullPtrWrap = do
  tellImport "Foreign.Ptr" "nullPtr"
  pure $ \cont e -> cont [qci|{e} nullPtr|]

vecWrap
  :: Text
  -- ^ vector name
  -> WrapM Wrapper
vecWrap vecName = do
  tellImport "Data.Vector.Storable" "unsafeWith"
  pure $ \cont e ->
    let param    = pretty (unKeyword $ dropPointer vecName)
        paramPtr = pretty (ptrName (dropPointer vecName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|unsafeWith {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|\\{param} -> {cont withPtr})|]

voidVecWrap
  :: Text
  -- ^ vector name
  -> WrapM Wrapper
voidVecWrap vecName = do
  tellImport "Data.Vector.Storable" "unsafeWith"
  tellImport "Foreign.Ptr"          "castPtr"
  pure $ \cont e ->
    let
      param    = pretty (unKeyword $ dropPointer vecName)
      paramPtr = pretty (ptrName (dropPointer vecName))
      -- Note the bracket opened here is closed after cont!
      withPtr =
        [qci|unsafeWith {param} (\\{paramPtr} -> {e} (castPtr {paramPtr})|]
    in
      [qci|\\{param} -> {cont withPtr})|]

peekForeignPtrOutput
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM (Doc ())
peekForeignPtrOutput paramName len = do
  tellImport "Data.Vector.Storable" "unsafeFromForeignPtr0"
  tellImport "Foreign.Storable" "peek"
  pure
    [qci|pure (unsafeFromForeignPtr0 {"f" <> ptrName (dropPointer paramName)} (fromIntegral ({len})))|]

peekForeignPtrOutputPeekLength
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get a pointer to the length
  -> WrapM (Doc ())
peekForeignPtrOutputPeekLength paramName lenPtr = do
  tellImport "Data.Vector.Storable" "unsafeFromForeignPtr0"
  tellImport "Foreign.Storable" "peek"
  pure
    [qci|(unsafeFromForeignPtr0 {"f" <> ptrName (dropPointer paramName)} . fromIntegral <$> peek {lenPtr})|]


vectorForeignPtrOutputWrap
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM Wrapper
vectorForeignPtrOutputWrap vecName len = do
  tellImport "Foreign.ForeignPtr" "mallocForeignPtrArray"
  tellImport "Foreign.ForeignPtr" "withForeignPtr"
  pure $ \cont e ->
   let
     param     = pretty (dropPointer vecName)
     paramPtr  = pretty (ptrName (dropPointer vecName))
     paramFPtr = pretty ("f" <> ptrName (dropPointer vecName))
     -- Note the brackets opened here are closed after cont!
     withPtr
       = [qci|mallocForeignPtrArray (fromIntegral ({len})) >>= (\\{paramFPtr} -> withForeignPtr {paramFPtr} (\\{paramPtr} -> {e} {paramPtr}|]
   in
     [qci|{cont withPtr}))|]

tupleTy :: Word -> Doc () -> Doc ()
tupleTy n t = tupled (replicate (fromIntegral n) t)

tupleWrap :: Word -> Text
  -> WrapM (Wrapper, Doc ())
  -- ^ Returns the wrapper and the pointer name
tupleWrap n paramName = do
   let paramPtr  = pretty (ptrName (dropPointer paramName))
       paramNames = (pretty paramName <>) . pretty . show <$> [0..n-1]
   pokes <- writePokes paramPtr paramNames
   tellImport "Foreign.Marshal.Array" "allocaArray"
   pure . (,paramPtr) $ \ cont e ->
     let withPtr =
           [qci|\\{tupled paramNames} -> allocaArray {n} (\\{paramPtr} -> {pokes} *> {e} {paramPtr}|]
     in [qci|{cont withPtr})|]

writePokes :: Doc () -> [Doc ()] -> WrapM (Doc ())
writePokes ptr ds = do
  let writePoke n d = [qci|pokeElemOff {ptr} {n} {d}|]
  tellImport "Foreign.Storable" "pokeElemOff"
  pure . hsep $ punctuate "*>" (zipWith writePoke [0..] ds)


wrap :: [WrappingType] -> Doc () -> Doc ()
wrap wts = go . fmap wtWrap $ wts
  where
    go :: [(Doc () -> Doc ()) -> Doc () -> Doc ()] -> Doc () -> Doc ()
    go = ($makeOutput) . appEndo . fold . fmap Endo

    tupleA :: [Doc ()] -> Doc ()
    tupleA = \case
      []  -> "()"
      [x] -> x
      xs ->
        "(" <> hcat (replicate (length xs - 1) ",") <> ")" <+> "<$>" <+> hcat
          (punctuate "<*>" xs)

    makeOutput :: Doc () -> Doc ()
    makeOutput =
      let outputs = [ o | WrappingType _ (Just o) _ <- wts ]
      in  case outputs of
            [] -> id
            xs -> \e -> [qci|{e} >>= (\\r -> {tupleA ("pure r" : (oPeek <$> xs))})|]

isPassByValue :: Parameter -> Bool
isPassByValue = pType >>> \case
  Float      -> True
  Void       -> True
  Char       -> True
  Int        -> True
  Ptr _ _    -> False
  Array{}    -> False
  TypeName _ -> True
  Proto _ _  -> False

isPassByConstPointer :: Parameter -> Bool
isPassByConstPointer = \case
  Parameter _ (Ptr Const _) Nothing Nothing -> True
  _ -> False

intercalateArrows = hsep . punctuate (space <> "->" <> space)

funName :: Text -> Text
funName = T.lowerCaseFirst . dropVk

funGetLengthName :: Text -> Text
funGetLengthName =
  ("getNum" <>) . T.dropPrefix' "Get" . dropVk

ptrName :: Text -> Text
ptrName = ("p" <>) . T.upperCaseFirst

dropVk :: Text -> Text
dropVk = T.lowerCaseFirst . T.dropPrefix' "vk"

dropPointer :: Text -> Text
dropPointer = T.lowerCaseFirst . T.dropPrefix' "p"

unKeyword :: Text -> Text
unKeyword t = if t `elem` keywords then t <> "'" else t
  where
    keywords =
      [ "as"
      , "case"
      , "class"
      , "data family"
      , "data instance"
      , "data"
      , "default"
      , "deriving"
      , "do"
      , "else"
      , "family"
      , "forall"
      , "foreign"
      , "hiding"
      , "if"
      , "import"
      , "in"
      , "infix"
      , "infixl"
      , "infixr"
      , "instance"
      , "let"
      , "mdo"
      , "module"
      , "newtype"
      , "of"
      , "proc"
      , "qualified"
      , "rec"
      , "then"
      , "type"
      , "where"
      ]

simpleTypeName :: Type -> Maybe Text
simpleTypeName = \case
  Float      -> pure "Float"
  Void       -> Nothing
  Char       -> pure "CChar"
  Int        -> pure "CInt"
  Ptr _ _    -> Nothing
  Array{}    -> Nothing
  TypeName n -> pure n
  Proto _ _  -> Nothing

isSimpleType :: Type -> Bool
isSimpleType = \case
  Float      -> True
  Void       -> False
  Char       -> True
  Int        -> True
  Ptr _ _    -> False
  Array{}    -> False
  TypeName n -> True
  Proto _ _  -> False

-- | Several commands such as `vkEnumerateInstanceExtensionProperties` have a dual usage
--
-- In one configuration when the output array is NULL an `inout` parameter is
-- filled with the number of values to fetch, in another configuration this
-- `inout` parameter is used to represent the size of the returned value array.
--
-- We expose these functionalities separately, and this type is used to select
-- which command to generate
data CommandUsage
  = CommandGetLength
  | CommandGetValues
