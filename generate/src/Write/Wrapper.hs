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
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict              hiding ((<>))
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor.Extra
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Type

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Marshal.Monad
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Struct

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

commandWrapper
  :: (Text -> Bool)
  -- ^ Is this the name of a handle type
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> (Text -> Bool)
  -- ^ Is this a struct
  -> Command
  -> Either [SpecError] [WriteElement]
commandWrapper isHandle isBitmask isStruct command = do
  let
    weName              = cName command T.<+> "wrapper"
    weBootElement       = Nothing
    isMarshalledHandle  = isHandle . ("Vk" <>)
    isMarshalledBitmask = isBitmask . ("Vk" <>)
    isDefaultable t = maybe
      False
      (isHandle <||> isMarshalledHandle <||> isMarshalledBitmask <||> isBitmask)
      (simpleTypeName t)
    isTypeStruct t = maybe False isStruct (simpleTypeName t)
  ((weDoc, aliasWriteElements), (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext (cName command)))
           (pure . first (first (const . vcat)))
           (runWrap $ wrapCommand isDefaultable isTypeStruct command)
  pure (WriteElement {..} : aliasWriteElements)

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

-- | Take a command and return the 'Doc' for the command's wrapper
-- Also return 'WriteElement's for any command aliases
wrapCommand
  :: (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this is a struct
  -> Command
  -> WrapM ([Doc ()], [WriteElement])
wrapCommand isDefaultable isStruct c@Command {..} = do
  let lengthPairs :: [(Parameter, Maybe Text, [Parameter])]
      lengthPairs = getLengthPointerPairs cParameters
      makeWts :: Maybe CommandUsage -> WrapM ([WrappingType], [Constraint])
      makeWts usage = listens getConstraints $
        parametersToWrappingTypes isDefaultable isStruct usage lengthPairs cParameters
  let printWrapped makeName wts constraints = do
        wrapped <- wrap c wts
        t <- makeType wts constraints
        pure $ line <> [qci|
          -- | Wrapper for {cName}
          {makeName cName} :: {t}
          {makeName cName} = {wrapped (pretty cName)}|]
      makeType wts constraints = wtsToSig c constraints wts
  (ds, aliases) <- if isDualUseCommand lengthPairs
    then do
      (wts1, as1) <- do
        (wts, constraints) <- makeWts (Just CommandGetLength)
        tellExport (Unguarded (Term (funGetLengthName cName)))
        (,) <$> printWrapped funGetLengthName wts constraints
            <*> pure [] -- TODO: write aliases for get length names
      (wts2, as2) <- do
        (wts, constraints) <- makeWts (Just CommandGetValues)
        tellExport (Unguarded (Term (funName cName)))
        (,) <$> printWrapped funName wts constraints
            <*> traverse (writeAlias wts constraints c) cAliases
      pure ([wts1, wts2], as1 ++ as2)
    else do
      tellExport (Unguarded (Term (funName cName)))
      (wts, constraints) <- makeWts Nothing
      d <- printWrapped funName wts constraints
      as <- traverse (writeAlias wts constraints c) cAliases
      pure ([d], as)
  tellDepend (Unguarded (TermName cName))
  pure (ds, aliases)

isDualUseCommand :: [(Parameter, Maybe Text, [Parameter])] -> Bool
isDualUseCommand = \case
  -- For simplicity, only handle these commands if they have one array parameter
  -- The length must not be a member
  -- There must only be one return array
  [(length', Nothing, [vector])]
    | -- The length parameter must be a valid pointer, but it can be
      -- unspecified if the vector pointer is null, i.e. we are querying the size.
      Just [False, True] <- pIsOptional length'
      -- The vector pointer can be null, i.e. we are querying the size.
    , Just [True] <- pIsOptional vector
    -> True
  _ -> False

wtsToSig :: Command -> [Constraint] -> [WrappingType] -> WrapM (Doc ())
wtsToSig c@Command {..} constraints ts = do
  outputs <- sequenceA [ t | WrappingType _ (Just (Output t _)) _ <- ts ]
  ret     <- tupled <$> if commandReturnsResult c
    then do
      returnTypeDoc <- toHsType cReturnType
      pure (returnTypeDoc : outputs)
    else pure outputs
  let inputs = mapMaybe wtInput ts
  inputTypes <- traverse iType inputs
  let h  = intercalateArrows (inputTypes <> ["IO" <+> ret])
      cs = if not (null constraints)
        then tupled (pretty <$> constraints) <+> "=> "
        else mempty
  pure $ cs <> h

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
        | Just tyName <- simpleTypeName t
        , isStruct t
        = TypeName (dropVkType tyName)
        | Just tyName <- simpleTypeName t
        , Just n <- T.dropPrefix "Vk" tyName
        , tyName /= "VkBool32"
        = TypeName n
        | otherwise
        = t

      deepMarshalledType :: Type -> Type
      deepMarshalledType = \case
        Ptr cv t        -> Ptr cv (deepMarshalledType t)
        Array cv size t -> Array cv size (deepMarshalledType t)
        Proto t ps' ->
          Proto (deepMarshalledType t) (second deepMarshalledType <$> ps')
        t -> marshalledType t

      elidedLengthVector
        :: Type
        -- ^ Struct type
        -> Text
        -- ^ Length member name
        -> Maybe Text
        -- ^ The name of the vector member which has this length
      elidedLengthVector structType lengthName =
        -- TODO: Don't hard code this
        if structType
             == TypeName "DescriptorSetAllocateInfo"
             && lengthName
             == "descriptorSetCount"
          then Just "pSetLayouts"
          else Nothing

      parameterToWrappingType :: Parameter -> WrapM WrappingType
      parameterToWrappingType p
        = let
            name = pName p
            getAlloc t = if isMarshalledStruct t
              then do
                Just tyName <- pure $ simpleTypeName t
                tellDepend (Unguarded (TermName ("withCStruct" <> tyName)))
                tellImport "Foreign.Marshal.Utils" "with"
                pure [qci|(\\a -> withCStruct{tyName} a . flip with)|]
              else do
                tellImport "Foreign.Marshal.Utils" "with"
                pure "with"
            getNonPtrAlloc t = if isMarshalledStruct t
              then do
                Just tyName <- pure $ simpleTypeName t
                tellDepend (Unguarded (TermName ("withCStruct" <> tyName)))
                pure ("withCStruct" <> tyName)
              else do
                tellImport "Data.Function" "(&)"
                pure "(&)"
            getPeek t = if isMarshalledStruct t
              then do
                Just tyName <- pure $ simpleTypeName t
                tellDepend (Unguarded (TermName ("fromCStruct" <> tyName)))
                tellImport "Foreign.Storable" "peek"
                tellImport "Control.Monad"    "(<=<)"
                pure [qci|(fromCStruct{tyName} <=< peek)|]
              else do
                tellImport "Foreign.Storable" "peek"
                pure "peek"
            getPeekElemOff t = if isMarshalledStruct t
              then do
                Just tyName <- pure $ simpleTypeName t
                tellDepend (Unguarded (TermName ("fromCStruct" <> tyName)))
                tellImport "Foreign.Storable" "peekElemOff"
                tellImport "Control.Monad"    "(<=<)"
                pure [qci|(\\p -> fromCStruct{tyName} <=< peekElemOff p)|]
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
                  (w, _) <- tupleWrap n (pName p)
                  pure $ InputType (Input name (tupleTy n <$> toHsType t)) w
              Array{}   -> throwError [Other "array parameter"]
              Proto _ _ -> throwError [Other "proto paramter"]
              t
                | -- The length of an array of untyped (void*) values, length is
                  -- given in bytes.
                  Just [vector] <- nonMemberLengthMap (pName p)
                , isSimpleType t
                , Ptr Const Void <- pType vector
                -> InferredType <$> lengthBytesWrap (dropPointer (pName vector))
                | -- The length of an array of untyped (void*) values, length is
                  -- given in bytes. This is an output vector and we must
                  -- supply the length of the output
                  Just [vector] <- nonMemberLengthMap (pName p)
                , isSimpleType t
                , Ptr NonConst Void <- pType vector
                -> do
                  let w = simpleWrap (pName p)
                  pure $ InputType (Input name (toHsType t)) w
                | -- The length of an array of typed values, length is given in
                  -- number of values
                  vectors <-
                  [ v
                  | Just vs <- pure $ nonMemberLengthMap (pName p)
                  , v@(Parameter _ (Ptr Const _) _ _) <- vs
                  ]
                , not (null vectors)
                , isSimpleType t
                , all (isPtrType . pType) vectors
                -> do
                  w <- lengthWrap (dropPointer . pName <$> vectors)
                  pure $ InferredType w
                | -- A Bool argument
                  Just "VkBool32" <- simpleTypeName t
                , Nothing <- pIsOptional p
                -> InputType (Input name (pure "Bool")) <$> boolWrap (pName p)
                | -- A ordinary boring argument
                  isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure
                  $ InputType (Input name (toHsType t)) (simpleWrap (pName p))
              ptrType@(Ptr Const t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure $ InputType (Input (pName p) (toHsType ptrType))
                                    (simpleWrap (pName p))
                | -- This is a pointer without any length associated with it
                  Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getAlloc t
                  w     <- allocaWrap alloc (pName p)
                  pure $ InputType (Input name (toHsType t)) w
                | -- An optional pointer without any length
                  Nothing <- pLength p
                , isSimpleType t
                , Just [True] <- pIsOptional p
                -> do
                  alloc <- getAlloc t
                  w     <- optionalAllocaWrap alloc (pName p)
                  pure $ InputType (Input name (("Maybe " <>) <$> toHsType t)) w
                | -- A vector of values with a length
                  isLengthPairList (pName p)
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getNonPtrAlloc t
                  w     <- vecWrap alloc (pName p)
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input name vType) w
                | -- A vector of pointers with a length
                  isLengthPairList (pName p)
                , Ptr Const t' <- t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getAlloc t'
                  w     <- vecWrap alloc (pName p)
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t'
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input name vType) w
                | -- A const void pointer, represent as a 'Vector a' where a is
                  -- storable
                  -- TODO: Can't have more than one of these
                  isLengthPairList (pName p)
                , Void <- t
                , Nothing <- pIsOptional p
                -> do
                  tellImport "Foreign.Marshal.Utils" "with"
                  tellImport "Data.Function"         "(&)"
                  let alloc = "(&)"
                  w <- voidVecWrap alloc (pName p)
                  tellImport "Foreign.Storable" "Storable"
                  tellConstraint "Storable a"
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        let tyName = "a"
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input name vType) w
                | -- A non optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Nothing <- pIsOptional p
                -> do
                  w <- cStringWrap (pName p)
                  let t' = do
                        tellImport "Data.ByteString" "ByteString"
                        pure "ByteString"
                  pure $ InputType (Input name t') w
                | -- An optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Just [True] <- pIsOptional p
                -> do
                  w <- optionalCStringWrap (pName p)
                  let t' = do
                        tellImport "Data.ByteString" "ByteString"
                        pure "Maybe ByteString"
                  pure $ InputType (Input name t') w
                | -- A const void pointer with no length
                  -- Pass as 'Ptr ()'
                  Void <- t
                , Nothing <- pLength p
                , isNothing (pIsOptional p) || Just [False] == pIsOptional p
                -> pure $ InputType (Input (pName p) (toHsType ptrType))
                                    (simpleWrap (pName p))
                | otherwise
                -> throwError [Other "array ptr parameter"]
              ptrType@(Ptr NonConst t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure $ InputType (Input (pName p) (toHsType ptrType))
                                    (simpleWrap (pName p))
                | -- Is another pointer being returned
                  Ptr NonConst t' <- t
                , -- There is no optional attribute on the return value from
                  -- vkGetMemoryAndroidHardwareBufferANDROID
                  (Just [False, True] == pIsOptional p) || t' == TypeName
                  "AHardwareBuffer"
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap (pName p)
                  tellImport "Foreign.Storable" "peek"
                  pure $ OutputType (Output (toHsType t) ("peek" <+> ptr)) w
                | -- A pointer to a non-optional return bool
                  Nothing <- pLength p
                , Just "VkBool32" <- simpleTypeName t
                , Nothing <- pIsOptional p
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap (pName p)
                  peek     <- getPeek t
                  tellDepend (Unguarded (TermName "bool32ToBool"))
                  pure $ OutputType
                    (Output
                      (pure "Bool")
                      ("bool32ToBool" <+> "<$>" <+> parens (peek <+> ptr))
                    )
                    w
                | -- A pointer to a non-optional return value
                  Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  -- This is probaby the most fishy case, it assumes that the
                  -- pointers being sent in are for returning variables only.
                  (w, ptr) <- simpleAllocaOutputWrap (pName p)
                  peek     <- getPeek t
                  pure $ OutputType (Output (toHsType t) (peek <+> ptr)) w
                | -- A pointer to an optional return value
                  Nothing <- pLength p
                , isSimpleType t
                , Just [False, True] <- pIsOptional p
                , isDefaultable t
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap (pName p)
                  tellImport "Foreign.Storable" "peek"
                  pure $ OutputType (Output (toHsType t) ("peek" <+> ptr)) w
                | -- A vector to read which will be made as long as a member of a
                  -- struct being passed in g
                  Just (NamedMemberLength s m) <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  tellExtension "DuplicateRecordFields"
                  structType <- case getParameter s of
                    Nothing -> throwError [Other "Cant find length parameter"]
                    Just p' -> pure $ case deepMarshalledType (pType p') of
                      Ptr _ structType -> structType
                      structType       -> structType
                  structTypeDoc    <- toHsType structType
                  lengthExpression <- case elidedLengthVector structType m of
                    Nothing ->
                      pure
                        $  "fromIntegral ("
                        <> toRecordMemberName m
                        <> " "
                        <> "("
                        <> dropPointer s
                        <> " :: "
                        <> T.tShow structTypeDoc
                        <> "))"
                    Just vec -> do
                      tellQualifiedImport "Data.Vector" "length"
                      pure
                        $  "Data.Vector.length ("
                        <> toRecordMemberName vec
                        <> " ("
                        <> dropPointer s
                        <> " :: "
                        <> T.tShow structTypeDoc
                        <> "))"
                  peekElemOff <- getPeekElemOff t
                  peek        <- peekVectorOutput peekElemOff
                                                  (pName p)
                                                  lengthExpression
                  w <- vectorOutputWrap (pName p) lengthExpression
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ OutputType (Output vType peek) w
                | -- A vector to read which will be made as long as an input parameter
                  Just (NamedLength _lengthParamName) <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                , [inputVector] <- inputVectorLengthMap (pName p)
                -> do
                  -- TODO: Make sure that this is always correct, the output
                  -- length is sometimes determined by a parameter, should only
                  -- *peek* that much
                  tellQualifiedImport "Data.Vector" "length"
                  let lengthExpression =
                        "(Data.Vector.length "
                          <> dropPointer (pName inputVector)
                          <> ")"
                  peekElemOff <- getPeekElemOff t
                  peek        <- peekVectorOutput peekElemOff
                                                  (pName p)
                                                  lengthExpression
                  w <- vectorOutputWrap (pName p) lengthExpression
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ OutputType (Output vType peek) w
                | -- The length dual use command output value
                  -- It is not an array
                  Nothing <- pLength p
                , -- It has uint32_t or size_t type
                  ((Just "uint32_t" ==) <||> (Just "size_t" ==))
                  (simpleTypeName t)
                , -- Is is a required pointer to an optional value
                  Just [False, True] <- pIsOptional p
                , -- We are generating a dual use command
                  Just commandUsage <- maybeCommandUsage
                -> case commandUsage of
                  CommandGetLength -> do
                    (w, ptr) <- simpleAllocaOutputWrap (pName p)
                    peek     <- do
                      tellImport "Foreign.Storable" "peek"
                      pure "peek"
                    pure $ OutputType (Output (toHsType t) (peek <+> ptr)) w
                  CommandGetValues -> do
                    alloc <- getAlloc t
                    w     <- allocaWrap alloc (pName p)
                    pure $ InputType (Input name (toHsType t)) w
                | -- The value dual use command output
                  -- It is an array with a parameter length in bytes
                  -- Returned as a ByteString
                  isLengthPairList (pName p)
                , -- It is not required
                  Just [True] <- pIsOptional p
                , -- We are generating a dual use command
                  Just commandUsage <- maybeCommandUsage
                , Void <- t
                , Just (NamedLength lengthParamName) <- pLength p
                -> case commandUsage of
                  CommandGetLength -> InferredType <$> passNullPtrWrap
                  CommandGetValues -> do
                    peek <- peekByteStringOutputPeekLength
                      (pName p)
                      (ptrName (dropPointer lengthParamName))
                    w <- byteStringOutputWrap
                      (pName p)
                      ("fromIntegral" T.<+> dropPointer lengthParamName)
                    let
                      vType =
                        tellImport "Data.ByteString" "ByteString"
                          $> "ByteString"
                    pure $ OutputType (Output vType peek) w
                | -- The value dual use command output
                  -- It is an array with a parameter length
                  isLengthPairList (pName p)
                , -- It is not required
                  Just [True] <- pIsOptional p
                , -- We are generating a dual use command
                  Just commandUsage <- maybeCommandUsage
                , isSimpleType t
                , Just (NamedLength lengthParamName) <- pLength p
                -> case commandUsage of
                  CommandGetLength -> InferredType <$> passNullPtrWrap
                  CommandGetValues -> do
                    peekElemOff <- getPeekElemOff t
                    peek        <- peekVectorOutputPeekLength
                      peekElemOff
                      (pName p)
                      (ptrName (dropPointer lengthParamName))
                    w <- vectorOutputWrap
                      (pName p)
                      ("fromIntegral" T.<+> dropPointer lengthParamName)
                    let vType = do
                          tellImport "Data.Vector" "Vector"
                          tyName <- toHsType t
                          pure ("Vector" <+> tyName)
                    pure $ OutputType (Output vType peek) w
                | -- This is an array with a parameter length in bytes
                  -- Returned as a ByteString
                  isLengthPairList (pName p)
                , -- It is not required
                  Nothing <- pIsOptional p
                , Void <- t
                , Just (NamedLength lengthParamName) <- pLength p
                -> do
                  peek <- peekByteStringOutput (pName p)
                                               (dropPointer lengthParamName)
                  w <- byteStringOutputWrap
                    (pName p)
                    ("fromIntegral" T.<+> dropPointer lengthParamName)
                  let
                    vType =
                      tellImport "Data.ByteString" "ByteString" $> "ByteString"
                  pure $ OutputType (Output vType peek) w
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
      [ (length', m, ps)
      | ((length', m), ps) <- MultiMap.assocs (MultiMap.fromList arrays)
      ]
    -- A map from name to parameters
    parameterMap = (`Map.lookup` Map.fromList ((pName &&& id) <$> parameters))
  in
    -- Only return pairs where the length is another parameter
    mapMaybe (\(length', m, ps) -> (, m, ps) <$> parameterMap length')
             uniqueLengthPairs

data WrappingType = WrappingType
  { wtInput  :: Maybe Input
    -- TODO: would wtWrapSig work better?
  , _wrOutput :: Maybe Output
  , wtWrap   :: Wrapper
    -- ^ A function taking a continuation to wrap the wrapped value and
    -- returning a wrapper
    -- TODO: Make better types!
  }

data Input = Input
  { _iName :: Text
  , iType :: WrapM (Doc ())
  }

data Output = Output
  { _oType :: WrapM (Doc ())
  , oPeek :: Doc ()
    -- ^ An expression of type "IO oType"
  }

pattern InputType :: Input -> Wrapper -> WrappingType
pattern InputType i w = WrappingType (Just i) Nothing w

pattern OutputType :: Output -> Wrapper -> WrappingType
pattern OutputType o w = WrappingType Nothing (Just o) w

pattern InferredType :: Wrapper -> WrappingType
pattern InferredType w = WrappingType Nothing Nothing w

----------------------------------------------------------------
-- Parameter Wrappers
----------------------------------------------------------------

simpleWrap
  :: Text
  -- ^ Parameter name
  -> Wrapper
simpleWrap paramName cont e =
  let w = pretty $ unReservedWord paramName
  in [qci|\\{w} -> {cont (e <+> w)}|]

boolWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
boolWrap paramName = do
  tellDepend (Unguarded (TermName "boolToBool32"))
  let paramDoc = pretty $ unReservedWord paramName
      w = parens $ "boolToBool32" <+> paramDoc
  pure $ \cont e -> [qci|\\{paramDoc} -> {cont (e <+> w)}|]

cStringWrap
  :: Text
  -- ^ Parameter name
  -> WrapM Wrapper
cStringWrap paramName = do
  tellImport "Data.ByteString" "useAsCString"
  pure $ \cont e ->
    let -- TODO: Use unReservedWord everywhere
        param    = pretty (unReservedWord $ dropPointer paramName)
        paramPtr = pretty (unReservedWord $ ptrName (dropPointer paramName))
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
    let -- TODO: Use unReservedWord everywhere
      param    = pretty (unReservedWord $ dropPointer paramName)
      paramPtr = pretty (unReservedWord $ ptrName (dropPointer paramName))
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
allocaWrap alloc paramName =
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
  :: [Text]
  -- ^ Vector names
  -> WrapM Wrapper
lengthWrap vecs = do
  tellQualifiedImport "Data.Vector" "length"
  let lengthExpressions = vecs <&> \n -> [qci|Data.Vector.length {unReservedWord n}|]
      minLength = hsep (punctuate " `min`" lengthExpressions)
  pure $ \cont e ->
    cont [qci|{e} (fromIntegral $ {minLength})|]

lengthBytesWrap
  :: Text
  -- ^ Vector name
  -> WrapM Wrapper
lengthBytesWrap vec = do
  tellImport          "Foreign.Storable" "sizeOf"
  tellQualifiedImport "Data.Vector"      "length"
  tellQualifiedImport "Data.Vector"      "head"
  pure $ \cont e ->
    cont
      [qci|{e} (fromIntegral $ sizeOf (Data.Vector.head {unReservedWord vec}) * Data.Vector.length {unReservedWord vec})|]

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
    let param    = pretty (unReservedWord $ dropPointer vecName)
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
      param    = pretty (unReservedWord $ dropPointer vecName)
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
  pure [qci|(Data.Vector.generateM ({len}) ({peekElemOff} {ptrName (dropPointer paramName)}))|]

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
     paramPtr  = pretty (ptrName (dropPointer vecName))
     -- Note the bracket opened here is closed after cont!
     withPtr
       = [qci|allocaArray ({len}) (\\{paramPtr} -> {e} {paramPtr}|]
   in
     [qci|{cont withPtr})|]

peekByteStringOutput
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM (Doc ())
peekByteStringOutput paramName len = do
  tellImport "Data.ByteString" "packCStringLen"
  pure [qci|(packCStringLen ({ptrName (dropPointer paramName)}, (fromIntegral {len})))|]

peekByteStringOutputPeekLength
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get a pointer to the length
  -> WrapM (Doc ())
peekByteStringOutputPeekLength paramName lenPtr = do
  tellImport "Data.ByteString" "packCStringLen"
  tellImport "Foreign.Storable" "peek"
  pure [qci|(curry packCStringLen {ptrName (dropPointer paramName)} =<< (fromIntegral <$> (peek {lenPtr})))|]

byteStringOutputWrap
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> WrapM Wrapper
byteStringOutputWrap bsName len = do
  tellImport "Foreign.Marshal.Array" "allocaArray"
  tellImport "Foreign.Ptr" "castPtr"
  pure $ \cont e ->
   let
     paramPtr  = pretty (ptrName (dropPointer bsName))
     -- Note the bracket opened here is closed after cont!
     withPtr
       = [qci|allocaArray ({len}) (\\{paramPtr} -> {e} (castPtr {paramPtr})|]
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

wrap :: Command -> [WrappingType] -> WrapM (Doc () -> Doc ())
wrap c wts = do
  makeOutput <-
    let outputs = [ o | WrappingType _ (Just o) _ <- wts ]
    in
      case cReturnType c of
        Void -> pure $ \e -> [qci|{e} *> ({tupleA (oPeek <$> outputs)})|]
        TypeName "VkResult" -> do
          tellImport "Control.Monad"     "when"
          tellImport "Control.Exception" "throwIO"
          tellDepend (Unguarded (PatternName "VK_SUCCESS"))
          tellDepend (Unguarded (WE.TypeName "VulkanException"))
          let returnsResult = commandReturnsResult c
          pure
            $ \e ->
                [qci|{e} >>= (\\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ({tupleA (["pure r"| returnsResult] ++ (oPeek <$> outputs))}))|]
        _ ->
          pure $ \e ->
            [qci|{e} >>= (\\r -> {tupleA ("pure r" : (oPeek <$> outputs))})|]
  pure $ ($ makeOutput) . foldWrappers . fmap wtWrap $ wts

-- | Does this command have an interesting result
commandReturnsResult :: Command -> Bool
commandReturnsResult Command {..} = case cReturnType of
  Void                -> False
  TypeName "VkResult" -> ["VK_SUCCESS"] /= fromMaybe [] cSuccessCodes
  _                   -> True

-- | Create a tuple from some arguments in an Applicative Functor
tupleA :: [Doc ()] -> Doc ()
tupleA = \case
  []  -> "pure ()"
  [x] -> x
  xs ->
    "(" <> hcat (replicate (length xs - 1) ",") <> ")" <+> "<$>" <+> hcat
      (punctuate "<*>" xs)


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

----------------------------------------------------------------
-- Aliases
----------------------------------------------------------------

writeAlias
  :: MonadError [SpecError] m
  => [WrappingType]
  -- ^ The types
  -> [Constraint]
  -- ^ constraints
  -> Command
  -- ^ The original command
  -> Text
  -- ^ The alias name
  -> m WriteElement
writeAlias wts constraints c@Command{..} name = do
  let weName = name T.<+> "alias" T.<+> cName
      weBootElement          = Nothing
      aliasDoc = do
        t <- wtsToSig c constraints wts
        tellExport (Unguarded (Term (dropVk name)))
        tellDepend (Unguarded (TermName (dropVk cName)))
        pure $ \_ -> [qci|
          {dropVk name} :: {t}
          {dropVk name} = {dropVk cName}
        |]
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <- either
    (throwError . fmap (WithContext cName))
    pure
    (runWrap aliasDoc)
  pure WriteElement {..}
