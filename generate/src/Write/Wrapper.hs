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
import           Write.Marshal.Monad
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Struct
import           Write.Util

commandWrapper
  :: (Text -> Bool)
  -- ^ Is this the name of a handle type
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> (Text -> Bool)
  -- ^ Is this a struct
  -> Command
  -> Either [SpecError] WriteElement
commandWrapper isHandle isBitmask isStruct command = do
  let weName = cName command T.<+> "wrapper"
      isDefaultable t =
        maybe False (isHandle <||> isBitmask) (simpleTypeName t)
      isTypeStruct t =
        maybe False isStruct (simpleTypeName t)
  (weDoc, (weImports, weProvides, weDepends, weExtensions, _)) <- either
    (throwError . fmap (WithContext (cName command)))
    (pure . first (const . vcat))
    (runWrap $ wrapCommand isDefaultable isTypeStruct command)
  pure WriteElement {..}

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

wrapCommand
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this is a struct
  -> Command
  -> WrapM [Doc ()]
wrapCommand isDefaultable isStruct Command {..} = do
  let lengthPairs :: [(Parameter, Maybe Text, [Parameter])]
      lengthPairs = getLengthPointerPairs cParameters
      makeWts :: Maybe CommandUsage -> WrapM ([WrappingType], [Constraint])
      makeWts usage = listens getConstraints $
        parametersToWrappingTypes isDefaultable isStruct usage lengthPairs cParameters
  let printTypes makeName (wts, constraints) = do
        sig <- wtsToSig cReturnType wts
        pure $ line <> [qci|
          -- | Wrapper for {cName}
          {makeName cName} :: {if not (null constraints) then tupled (pretty <$> constraints) <+> "=> " else mempty}{sig}
          {makeName cName} = {wrap cReturnType wts (pretty cName) }|]
  ds <- if isDualUseCommand lengthPairs
    then do
      wts1 <- do
        tellExport (Unguarded (Term (funGetLengthName cName)))
        printTypes funGetLengthName =<< makeWts (Just CommandGetLength)
      wts2 <- do
        tellExport (Unguarded (Term (funName cName)))
        printTypes funName =<< makeWts (Just CommandGetValues)
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
  let outputs = [ t | WrappingType _ (Just (Output t _)) _ <- ts ]
  ret <- tupled <$> if returnType == Void
    then pure outputs
    else do
      returnTypeDoc <- toHsType returnType
      pure (returnTypeDoc : outputs)
  pure
    $ intercalateArrows (mapMaybe (fmap iType . wtInput) ts <> ["IO" <+> ret])

parametersToWrappingTypes
  :: (Type -> Bool)
  -- ^ Is this type defaultable (a handle or bitmask)
  -> (Type -> Bool)
  -- ^ Is this a struct
  -> Maybe CommandUsage
  -- ^ If this is a dual use command, which usage shall we generate
  -> [(Parameter, Maybe Text, [Parameter])]
  -- ^ (parameter containing length, name of member representing length,
  -- vector parameters which must be this length)
  -> [Parameter]
  -> WrapM [WrappingType]
parametersToWrappingTypes isDefaultable isStruct maybeCommandUsage lengthPairs ps
  = let
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

      getParameter :: Text -> Maybe Parameter
      getParameter = (`Map.lookup` Map.fromList [ (pName p, p) | p <- ps ])

      -- TODO: don't use this ad-hoc functions, make things more principled!
      isMarshalledStruct :: Type -> Bool
      isMarshalledStruct = \case
        TypeName n -> isStruct (TypeName ("Vk" <> n))
        _          -> False

      marshalledType :: Type -> Type
      marshalledType t
        | Just tyName <- simpleTypeName t, isStruct t = TypeName
          (dropVkType tyName)
        | otherwise = t

      deepMarshalledType :: Type -> Type
      deepMarshalledType = \case
        Ptr cv t        -> Ptr cv (deepMarshalledType t)
        Array cv size t -> Array cv size (deepMarshalledType t)
        Proto t ps ->
          Proto (deepMarshalledType t) (second deepMarshalledType <$> ps)
        t -> marshalledType t

      parameterToWrappingType :: Parameter -> WrapM WrappingType
      parameterToWrappingType p
        | Just vectors <- nonMemberLengthMap (pName p)
        , length [ () | Parameter _ (Ptr Const _) _ _ <- vectors ] > 1
        = throwError
          [Other "More then two input vectors with same length specifier"]
        | otherwise
        = let
            name = pName p
            getAlloc t = if isMarshalledStruct t
              then do
                tellDepend (Unguarded (TermName "withCStructPtr"))
                pure "withCStructPtr"
              else do
                tellImport "Foreign.Marshal.Utils" "with"
                pure "with"
            getNonPtrAlloc t = if isMarshalledStruct t
              then do
                tellDepend (Unguarded (WE.TypeName "ToCStruct"))
                pure "withCStruct"
              else do
                tellImport "Data.Function" "(&)"
                pure "(&)"
            getPeek t = if isMarshalledStruct t
              then do
                tellDepend (Unguarded (TermName "fromCStructPtr"))
                pure "fromCStructPtr"
              else do
                tellImport "Foreign.Storable" "peek"
                pure "peek"
            getPeekElemOff t = if isMarshalledStruct t
              then do
                tellDepend (Unguarded (TermName "fromCStructPtrElem"))
                pure "fromCStructPtrElem"
              else do
                tellImport "Foreign.Storable" "peekElemOff"
                pure "peekElemOff"
          in
            case deepMarshalledType (pType p) of
              Void -> throwError [Other "void parameter"]
              Array Const (NumericArraySize n) unmarshalledType
                | t <- marshalledType unmarshalledType
                , n >= 2
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  tyName <- toHsType t
                  (w, _) <- tupleWrap n (pName p)
                  pure $ InputType (Input name (tupleTy n tyName)) w
              Array{}   -> throwError [Other "array parameter"]
              Proto _ _ -> throwError [Other "proto paramter"]
              t
                | -- The length of an array of typed values, length is given in
                  -- number of values
                  [vector] <-
                  [ v
                  | Just vs <- pure $ nonMemberLengthMap (pName p)
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
              ptrType@(Ptr Const t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> do
                  tyName <- toHsType ptrType
                  pure $ InputType (Input (pName p) tyName)
                                   (simpleWrap (pName p))
                | Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  tyName <- toHsType t
                  alloc  <- getAlloc t
                  w      <- allocaWrap alloc (pName p)
                  pure $ InputType (Input name tyName) w
                | Nothing <- pLength p
                , isSimpleType t
                , Just [True] <- pIsOptional p
                -> do
                  tyName <- toHsType t
                  alloc  <- getAlloc t
                  w      <- optionalAllocaWrap alloc (pName p)
                  pure $ InputType (Input name ("Maybe " <> tyName)) w
                | isLengthPairList (pName p)
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  tyName <- toHsType t
                  alloc  <- getNonPtrAlloc t
                  w      <- vecWrap alloc (pName p)
                  pure $ InputType (Input name ("Vector " <> tyName)) w
                | -- A const void pointer, represent as a 'Vector a' where a is
                  -- storable
                  isLengthPairList (pName p)
                , Void <- t
                , Nothing <- pIsOptional p
                -> do
                  tellImport "Data.Vector" "Vector"
                  tellImport "Foreign.Marshal.Utils" "with"
                  let alloc = "(flip ($))"
                  w <- voidVecWrap alloc (pName p)
                  tellImport "Foreign.Storable" "Storable"
                  tellConstraint "Storable a"
                  pure $ InputType (Input name ("Vector " <> "a")) w
                | -- A non optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Nothing <- pIsOptional p
                -> do
                  w <- cStringWrap (pName p)
                  tellImport "Data.ByteString" "ByteString"
                  pure $ InputType (Input name "ByteString") w
                | -- An optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Just [True] <- pIsOptional p
                -> do
                  w <- optionalCStringWrap (pName p)
                  tellImport "Data.ByteString" "ByteString"
                  pure $ InputType (Input name "Maybe ByteString") w
                | otherwise
                -> throwError [Other "array ptr parameter"]
              ptrType@(Ptr NonConst t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> do
                  tyName <- toHsType ptrType
                  pure $ InputType (Input (pName p) tyName)
                                   (simpleWrap (pName p))
                | -- A pointer to a non-optional return value
                  Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  -- This is probaby the most fishy case, it assumes that the
                  -- pointers being sent in are for returning variables only.
                  tyName   <- toHsType t
                  (w, ptr) <- simpleAllocaOutputWrap (pName p)
                  peek     <- getPeek t
                  pure $ OutputType (Output tyName (peek <+> ptr)) w
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
                  structType <- case getParameter s of
                    Nothing -> throwError [Other "Cant find length parameter"]
                    Just p' -> case pType p' of
                      Ptr _ structType -> toHsType structType
                      structType       -> toHsType structType
                  let lengthExpression =
                        toRecordMemberName m
                          <> " "
                          <> "("
                          <> dropPointer s
                          <> " :: "
                          <> T.tShow structType
                          <> ")"
                  peekElemOff <- getPeekElemOff t
                  peek <- peekVectorOutput peekElemOff (pName p) lengthExpression
                  w    <- vectorOutputWrap (pName p) lengthExpression
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
                  tellQualifiedImport "Data.Vector" "length"
                  let lengthExpression =
                        "(Data.Vector.length "
                          <> dropPointer (pName inputVector)
                          <> ")"
                  peekElemOff <- getPeekElemOff t
                  peek <- peekVectorOutput peekElemOff (pName p) lengthExpression
                  w    <- vectorOutputWrap (pName p) lengthExpression
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
                      peek     <- if isMarshalledStruct t
                        then do
                          tellDepend (Unguarded (TermName "fromCStructPtr"))
                          pure "fromCStructPtr"
                        else do
                          tellImport "Foreign.Storable" "peek"
                          pure "peek"
                      pure $ OutputType (Output tyName (peek <+> ptr)) w
                    CommandGetValues -> do
                      alloc <- getAlloc t
                      w     <- allocaWrap alloc (pName p)
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
                      peekElemOff <- getPeekElemOff t
                      peek <- peekVectorOutputPeekLength
                        peekElemOff
                        (pName p)
                        (ptrName (dropPointer lengthParamName))
                      w <- vectorOutputWrap
                        (pName p)
                        (dropPointer lengthParamName)
                      pure $ OutputType (Output ("Vector " <> tyName) peek) w
              t -> throwError [Other ("unhandled type: " <> T.tShow t)]
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
  -- ^ allocator
  -> Text
  -- ^ Parameter name
  -> WrapM Wrapper
allocaWrap alloc paramName = do
  pure $ \cont e ->
    let param    = pretty (dropPointer paramName)
        paramPtr = pretty (ptrName (dropPointer paramName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|{alloc} {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|\\{param} -> {cont withPtr})|]

optionalAllocaWrap
  :: Text
  -- ^ allocator
  -> Text
  -- ^ Parameter name
  -> WrapM Wrapper
optionalAllocaWrap alloc paramName = do
  tellImport "Foreign.Marshal.Utils" "maybeWith"
  pure $ \cont e ->
    let
      param    = dropPointer paramName
      paramPtr = ptrName (dropPointer paramName)
      -- Note the bracket opened here is closed after cont!
      withPtr  = [qci|maybeWith {alloc} {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in
      [qci|\\{param} -> {cont withPtr})|]

lengthWrap
  :: Text
  -- ^ Vector name
  -> Text
  -- ^ Parameter name
  -> WrapM Wrapper
lengthWrap vec paramName = do
  tellQualifiedImport "Data.Vector" "length"
  pure $ \cont e ->
    cont [qci|{e} (fromIntegral $ Data.Vector.length {unKeyword vec})|]
    -- let withLength = cont [qci|{e} {paramName}|]
    -- in
    --   [qci|let {paramName} = (fromIntegral $ Data.Vector.length {unKeyword vec}) in {withLength}|]

lengthBytesWrap
  :: Text
  -- ^ Vector name
  -> WrapM Wrapper
lengthBytesWrap vec = do
  tellImport          "Foreign.Storable"     "sizeOf"
  tellQualifiedImport "Data.Vector" "length"
  pure $ \cont e ->
    cont
      [qci|{e} (fromIntegral $ sizeOf (head {unKeyword vec}) * Data.Vector.length {unKeyword vec})|]

passNullPtrWrap :: WrapM Wrapper
passNullPtrWrap = do
  tellImport "Foreign.Ptr" "nullPtr"
  pure $ \cont e -> cont [qci|{e} nullPtr|]

vecWrap
  :: Text
  -- ^ Non pointer Allocator
  -> Text
  -- ^ vector name
  -> WrapM Wrapper
vecWrap nonPtrAlloc vecName = do
  tellDepend (Unguarded (TermName "withVec"))
  pure $ \cont e ->
    let param    = pretty (unKeyword $ dropPointer vecName)
        paramPtr = pretty (ptrName (dropPointer vecName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|withVec {nonPtrAlloc} {param} (\\{paramPtr} -> {e} {paramPtr}|]
    in  [qci|\\{param} -> {cont withPtr})|]

voidVecWrap
  :: Text
  -- ^ Non Pointer Allocator
  -> Text
  -- ^ vector name
  -> WrapM Wrapper
voidVecWrap nonPtrAlloc vecName = do
  tellImport "Foreign.Ptr"          "castPtr"
  tellDepend (Unguarded (TermName "withVec"))
  pure $ \cont e ->
    let
      param    = pretty (unKeyword $ dropPointer vecName)
      paramPtr = pretty (ptrName (dropPointer vecName))
      -- Note the bracket opened here is closed after cont!
      withPtr =
        [qci|withVec {nonPtrAlloc} {param} (\\{paramPtr} -> {e} (castPtr {paramPtr})|]
    in
      [qci|\\{param} -> {cont withPtr})|]

peekVectorOutput
  :: Text
  -- ^ peekElemOff
  -> Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM (Doc ())
peekVectorOutput peekElemOff paramName len = do
  tellQualifiedImport "Data.Vector" "generateM"
  pure [qci|(Data.Vector.generateM (fromIntegral {len}) ({peekElemOff} {ptrName (dropPointer paramName)}))|]

peekVectorOutputPeekLength
  :: Text
  -- ^ peekElemOff
  -> Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get a pointer to the length
  -> WrapM (Doc ())
peekVectorOutputPeekLength peekElemOff paramName lenPtr = do
  tellImport "Foreign.Storable" "peek"
  tellQualifiedImport "Data.Vector" "generateM"
  pure [qci|(flip Data.Vector.generateM ({peekElemOff} {ptrName (dropPointer paramName)}) =<< (fromIntegral <$> (peek {lenPtr})))|]


vectorOutputWrap
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM Wrapper
vectorOutputWrap vecName len = do
  tellImport "Foreign.Marshal.Array" "allocaArray"
  pure $ \cont e ->
   let
     param     = pretty (dropPointer vecName)
     paramPtr  = pretty (ptrName (dropPointer vecName))
     -- Note the bracket opened here is closed after cont!
     withPtr
       = [qci|allocaArray (fromIntegral ({len})) (\\{paramPtr} -> {e} {paramPtr}|]
   in
     [qci|{cont withPtr})|]

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

wrap :: Type -> [WrappingType] -> Doc () -> Doc ()
wrap returnType wts = ($makeOutput) . foldWrappers . fmap wtWrap $ wts
  where

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
      in
        case outputs of
          [] -> id
          xs -> case returnType of
            Void -> \e -> [qci|{e} *> ({tupleA (oPeek <$> xs)})|]
            _    -> \e ->
              [qci|{e} >>= (\\r -> {tupleA ("pure r" : (oPeek <$> xs))})|]

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

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"
