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


import           Control.Arrow                            ( (&&&) )
import           Control.Bool
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Strict       hiding ( (<>) )
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor
import qualified Data.Map                      as Map
import           Data.List                                ( partition )
import           Data.Maybe
import qualified Data.MultiMap                 as MultiMap
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Handle
import           Spec.Savvy.Error
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Marshal.Monad
import           Write.Util                               ( document, Documentee(..) )
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Marshal.Struct
import           Write.Struct

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

commandWrapper
  :: (Text -> Maybe Handle)
  -- ^ Lookup a handle
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> (Text -> Bool)
  -- ^ Is this a struct
  -> (Text -> Maybe Handle)
  -- ^ Get the dispatchable handle in a struct
  -> (Text -> Text)
  -- ^ Alias resolver
  -> (Text -> Maybe HaskellName)
  -- ^ Get brackets for this command
  -> Command
  -> Either [SpecError] [WriteElement]
commandWrapper getHandle isBitmask isStruct structDispatchableHandle resolveAlias getBrackets command
  = do
    let weName        = cName command T.<+> "wrapper"
        weBootElement = Nothing
        isNonDispatchableHandle n = case getHandle n of
          Just h  -> hHandleType h == NonDispatchable
          Nothing -> True
        isMarshalledNonDispatchableHandle = isNonDispatchableHandle . ("Vk" <>)
        isMarshalledBitmask               = isBitmask . ("Vk" <>)
        isDefaultable t = maybe
          False
          (    isNonDispatchableHandle
          <||> isMarshalledNonDispatchableHandle
          <||> isMarshalledBitmask
          <||> isBitmask
          )
          (simpleTypeName t)
        isTypeStruct t = maybe False isStruct (simpleTypeName t)
        getTypeHandle h = getHandle . ("Vk" <>) =<< simpleTypeName h
        structContainsDispatchableHandle s =
          isJust $ structDispatchableHandle . ("Vk" <>) =<< simpleTypeName s
    ((weDoc, aliasWriteElements), (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
      either
        (throwError . fmap (WithContext (cName command)))
        -- (pure . first (first (const . vcat)))
        (pure . first (first (fmap vcat . sequence)))
        (runWrap $ wrapCommand getTypeHandle
                               isDefaultable
                               isTypeStruct
                               structContainsDispatchableHandle
                               resolveAlias
                               getBrackets
                               command
        )
    let rs = WriteElement { .. } : aliasWriteElements
        -- TODO: Remove this
        r  = case cName command of
          "vkGetDeviceGroupSurfacePresentModes2EXT" -> fromJust
            (traverse (guardWriteElement (Guard "VK_USE_PLATFORM_WIN32")) rs)
          _ -> rs
    pure r

----------------------------------------------------------------
-- The wrapping commands
----------------------------------------------------------------

-- | Take a command and return the 'Doc' for the command's wrapper
-- Also return 'WriteElement's for any command aliases
wrapCommand
  :: (Type -> Maybe Handle)
  -- ^ gethandle
  -> (Type -> Bool)
  -- ^ Is a defaultable type
  -> (Type -> Bool)
  -- ^ Is this is a struct
  -> (Type -> Bool)
  -- ^ Struct contains dispatchable handle
  -> (Text -> Text)
  -- ^ Alias resolver
  -> (Text -> Maybe HaskellName)
  -- ^ Get brackets for this command
  -> Command
  -> WrapM ([DocMap -> Doc ()], [WriteElement])
wrapCommand getHandle isDefaultable isStruct structContainsDispatchableHandle resolveAlias getBrackets c@Command {..} = do
  let lengthPairs :: [(Parameter, Maybe Text, [Parameter])]
      lengthPairs = getLengthPointerPairs cParameters
      makeWts :: Maybe CommandUsage -> WrapM ([WrappingType], [Constraint])
      makeWts usage = listens getConstraints $
        parametersToWrappingTypes isDefaultable isStruct getHandle structContainsDispatchableHandle resolveAlias usage lengthPairs cParameters
  let printWrapped ::  Text -> [WrappingType] -> [Constraint] -> WriterT WriteState (Except [SpecError]) (DocMap -> Doc ann)
      printWrapped n wts constraints = do
        wrapped <- wrap c wts
        t <- makeType wts constraints
        tellDepend (Unguarded (TermName cName))
        pure $ \getDoc -> line <> [qci|
          {document getDoc (TopLevel cName)}
          {n} :: {t :: Doc ()}
          {n} = {wrapped (pretty cName) :: Doc ()}|]
      makeType wts constraints = wtsToSig KeepVkResult c constraints wts
  (ds, aliases) <- if isDualUseCommand lengthPairs
    then do
      (lengthWts, lengthConstraints) <- makeWts (Just CommandGetLength)
      (valueWts, valueConstraints) <- makeWts (Just CommandGetValues)
      (wts1, as1) <- do
        n <- maybe
          (throwError [Other $ "Failed to get wrapped function name for" T.<+> cName]) pure
          (funGetLengthName cName)
        tellExport (Unguarded (Term n ))
        (,) <$> printWrapped n lengthWts lengthConstraints
            <*> pure [] -- TODO: write aliases for get length names
      (wts2, as2) <- do
        let n = funName cName
        tellExport (Unguarded (Term n))
        (,) <$> printWrapped n valueWts valueConstraints
            <*> traverse (writeAlias valueWts valueConstraints c) cAliases
      (wts3, as3) <-
        (,) <$> writeGetAllCommand lengthWts valueWts c
            <*> pure [] -- TODO: write aliases for get length names
      pure ([wts1, wts2, wts3], as1 ++ as2 ++ as3)
    else do
      tellExport (Unguarded (Term (funName cName)))
      (wts, constraints) <- makeWts Nothing
      let n = funName cName
      d <- printWrapped n wts constraints
      as <- traverse (writeAlias wts constraints c) cAliases
      pure ([d], as)
  traverse_ (tellDepend . Unguarded) (getBrackets (dropVk cName))
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

data ReturnValueHandling
  = KeepVkResult
  | IgnoreVkResult
  deriving (Eq)

wtsToSig
  :: ReturnValueHandling
  -> Command
  -> [Constraint]
  -> [WrappingType]
  -> WrapM (Doc ())
wtsToSig returnValueHandling c@Command {..} constraints ts = do
  outputs <- sequenceA [ t | WrappingType _ (Just (Output t _)) _ <- ts ]
  ret     <-
    tupled <$> if commandReturnsResult c && returnValueHandling == KeepVkResult
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
  -> (Type -> Maybe Handle)
  -- ^ Get handle
  -> (Type -> Bool)
  -- ^ Struct contains dispatchable handle
  -> (Text -> Text)
  -- ^ Alias resolver
  -> Maybe CommandUsage
  -- ^ If this is a dual use command, which usage shall we generate
  -> [(Parameter, Maybe Text, [Parameter])]
  -- ^ (parameter containing length, name of member representing length,
  -- vector parameters which must be this length)
  -> [Parameter]
  -> WrapM [WrappingType]
parametersToWrappingTypes isDefaultable isStruct getHandle structContainsDispatchableHandle resolveAlias maybeCommandUsage lengthPairs ps
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
        = TypeName (dropVkType (resolveAlias tyName))
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
            paramName = makeParamName name
            -- Used to avoid stepping on the record member names
            makeParamName = (<> "'")
            getAlloc t = if isMarshalledStruct t
              then do
                let Just tyName = simpleTypeName t
                tellDepend (Unguarded (TermName ("withCStruct" <> tyName)))
                tellImport "Foreign.Marshal.Utils" "with"
                pure [qci|(\\marshalled -> withCStruct{tyName} marshalled . flip with)|]
              else do
                tellImport "Foreign.Marshal.Utils" "with"
                pure "with"
            getNonPtrAlloc t = if isMarshalledStruct t
              then do
                let Just tyName = simpleTypeName t
                tellDepend (Unguarded (TermName ("withCStruct" <> tyName)))
                pure ("withCStruct" <> tyName)
              else do
                tellImport "Data.Function" "(&)"
                pure $ case getHandle t of
                  Just h
                    | Dispatchable <- hHandleType h
                    -> [qci|((&) . {T.lowerCaseFirst . dropVkType . hName $ h}Handle)|]
                  _ -> [qci|(&)|]
            getPeek t = if isMarshalledStruct t
              then do
                let Just tyName = simpleTypeName t
                tellDepend (Unguarded (TermName ("fromCStruct" <> tyName)))
                tellImport "Foreign.Storable" "peek"
                tellImport "Control.Monad"    "(<=<)"
                pure $ if structContainsDispatchableHandle t
                  then [qci|(fromCStruct{tyName} commandTable <=< peek)|]
                  else [qci|(fromCStruct{tyName} <=< peek)|]
              else do
                tellImport "Foreign.Storable" "peek"
                pure "peek"
            getPeekElemOff t = case getHandle t of
              Just h | Dispatchable <- hHandleType h -> do
                getCommandTable <- case hLevel h of
                  Nothing ->
                    throwError
                      [ Other
                          "Getting the command table for a handle without a level"
                      ]
                  Just Instance ->
                    throwError
                      [ Other
                          "Getting the command table for vector of instances"
                      ]
                  Just PhysicalDevice ->
                    pure ("pure commandTable" :: Doc ())
                  Just Device ->
                    pure ("pure commandTable" :: Doc ())
                tellImport "Foreign.Storable" "peekElemOff"
                pure
                  [qci|(\\p i -> {dropVkType (hName h)} <$> peekElemOff p i <*> {getCommandTable})|]
              _ | isMarshalledStruct t
                -> do
                  let Just tyName = simpleTypeName t
                  tellDepend (Unguarded (TermName ("fromCStruct" <> tyName)))
                  tellImport "Foreign.Storable" "peekElemOff"
                  tellImport "Control.Monad"    "(<=<)"
                  pure $ if structContainsDispatchableHandle t
                    then [qci|(\\p -> fromCStruct{tyName} commandTable <=< peekElemOff p)|]
                    else [qci|(\\p -> fromCStruct{tyName} <=< peekElemOff p)|]
              _ -> do
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
                  (w, _) <- tupleWrap n paramName
                  pure $ InputType (Input paramName (tupleTy n <$> toHsType t)) w
              Array{}   -> throwError [Other "array parameter"]
              Proto _ _ -> throwError [Other "proto paramter"]
              t
                | -- The length of an array of untyped (void*) values, length is
                  -- given in bytes.
                  Just [vector] <- nonMemberLengthMap name
                , isSimpleType t
                , Ptr Const Void <- pType vector
                -> InferredType <$> lengthBytesWrap (makeParamName (dropPointer (pName vector)))
                | -- The length of an array of untyped (void*) values, length is
                  -- given in bytes. This is an output vector and we must
                  -- supply the length of the output
                  Just [vector] <- nonMemberLengthMap name
                , isSimpleType t
                , Ptr NonConst Void <- pType vector
                -> do
                  let w = simpleWrap paramName
                  pure $ InputType (Input paramName (toHsType t)) w
                | -- The length of an array of typed values, length is given in
                  -- number of values
                  vectors <-
                  [ v
                  | Just vs <- pure $ nonMemberLengthMap name
                  , v@(Parameter _ (Ptr Const _) _ _) <- vs
                  ]
                , not (null vectors)
                , isSimpleType t
                , all (isPtrType . pType) vectors
                -> do
                  let isRequired v = case pIsOptional v of
                                       Just (True:_) -> True
                                       _             -> False
                      (optionals, required) = partition isRequired vectors
                  w <- lengthWrap
                    (makeParamName . dropPointer . pName <$> required)
                    (makeParamName . dropPointer . pName <$> optionals)
                  pure $ InferredType w
                | -- A Bool argument
                  Just "VkBool32" <- simpleTypeName t
                , Nothing <- pIsOptional p
                -> InputType (Input paramName (pure "Bool")) <$> boolWrap paramName
                | -- A dispatchable Handle argument
                  Just h <- getHandle t
                , Dispatchable <- hHandleType h
                -> InputType (Input paramName (toHsType t))
                  <$> handleWrap paramName h
                | -- A ordinary boring argument
                  isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure
                  $ InputType (Input paramName (toHsType t)) (simpleWrap paramName)
              ptrType@(Ptr Const t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure $ InputType (Input paramName (toHsType ptrType))
                                    (simpleWrap paramName)
                | -- This is a pointer without any length associated with it
                  Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getAlloc t
                  w     <- allocaWrap alloc paramName
                  pure $ InputType (Input paramName (toHsType t)) w
                | -- An optional pointer without any length
                  Nothing <- pLength p
                , isSimpleType t
                , Just [True] <- pIsOptional p
                -> do
                  alloc <- getAlloc t
                  w     <- optionalAllocaWrap alloc name
                  pure $ InputType (Input paramName (("Maybe " <>) <$> toHsType t)) w
                | -- A vector of values with a length
                  isLengthPairList name
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getNonPtrAlloc t
                  w     <- vecWrap alloc paramName
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input paramName vType) w
                | -- An optional vector of values with a length
                  isLengthPairList name
                , isSimpleType t
                , Just [True] <- pIsOptional p
                -> do
                  alloc <- getNonPtrAlloc t
                  w     <- optionalVecWrap alloc paramName
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Maybe (Vector" <+> tyName <> ")")
                  pure $ InputType (Input paramName vType) w
                | -- A vector of pointers with a length
                  isLengthPairList name
                , Ptr Const t' <- t
                , Nothing <- pIsOptional p
                -> do
                  alloc <- getAlloc t'
                  w     <- vecWrap alloc paramName
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t'
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input paramName vType) w
                | -- A const void pointer, represent as a 'Vector a' where a is
                  -- storable
                  -- TODO: Can't have more than one of these
                  isLengthPairList name
                , Void <- t
                , Nothing <- pIsOptional p
                -> do
                  tellImport "Foreign.Marshal.Utils" "with"
                  tellImport "Data.Function"         "(&)"
                  let alloc = "(&)"
                  w <- voidVecWrap alloc paramName
                  tellImport "Foreign.Storable" "Storable"
                  tellConstraint "Storable a"
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        let tyName = "a"
                        pure ("Vector" <+> tyName)
                  pure $ InputType (Input paramName vType) w
                | -- A non optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Nothing <- pIsOptional p
                -> do
                  w <- cStringWrap paramName
                  let t' = do
                        tellImport "Data.ByteString" "ByteString"
                        pure "ByteString"
                  pure $ InputType (Input paramName t') w
                | -- An optional string
                  Char <- t
                , Just NullTerminated <- pLength p
                , Just [True] <- pIsOptional p
                -> do
                  w <- optionalCStringWrap paramName
                  let t' = do
                        tellImport "Data.ByteString" "ByteString"
                        pure "Maybe ByteString"
                  pure $ InputType (Input paramName t') w
                | -- A const void pointer with no length
                  -- Pass as 'Ptr ()'
                  Void <- t
                , Nothing <- pLength p
                , isNothing (pIsOptional p) || Just [False] == pIsOptional p
                -> pure $ InputType (Input paramName (toHsType ptrType))
                                    (simpleWrap paramName)
                | otherwise
                -> throwError [Other "array ptr parameter"]
              ptrType@(Ptr NonConst t)
                | -- Is this a type which we should pass using the pointer
                  -- without any marshaling
                  isPassAsPointerType t
                , isSimpleType t
                , isNothing (pIsOptional p) || isDefaultable t
                -> pure $ InputType (Input paramName (toHsType ptrType))
                                    (simpleWrap paramName)
                | -- Is another pointer being returned
                  Ptr NonConst t' <- t
                , -- There is no optional attribute on the return value from
                  -- vkGetMemoryAndroidHardwareBufferANDROID
                  (Just [False, True] == pIsOptional p) || t' == TypeName
                  "AHardwareBuffer"
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap paramName
                  tellImport "Foreign.Storable" "peek"
                  pure $ OutputType (Output (toHsType t) ("peek" <+> ptr)) w
                | -- A pointer to a non-optional return bool
                  Nothing <- pLength p
                , Just "VkBool32" <- simpleTypeName t
                , Nothing <- pIsOptional p
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap paramName
                  peek     <- getPeek t
                  tellDepend (Unguarded (TermName "bool32ToBool"))
                  pure $ OutputType
                    (Output
                      (pure "Bool")
                      ("bool32ToBool" <+> "<$>" <+> parens (peek <+> ptr))
                    )
                    w
                | -- A pointer to a non-optional return handle
                  Just h <- getHandle t
                , Nothing <- pIsOptional p
                , Nothing <- pLength p
                -> do
                  (w, ptr) <- handleOutputWrap paramName
                  con      <- constructHandle h ptr
                  pure $ OutputType (Output (toHsType t) con) w
                | -- A pointer to a non-optional return value
                  Nothing <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                -> do
                  -- This is probaby the most fishy case, it assumes that the
                  -- pointers being sent in are for returning variables only.
                  (w, ptr) <- simpleAllocaOutputWrap paramName
                  peek     <- getPeek t
                  pure $ OutputType (Output (toHsType t) (peek <+> ptr)) w
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
                    (w, ptr) <- simpleAllocaOutputWrap paramName
                    peek     <- do
                      tellImport "Foreign.Storable" "peek"
                      pure "peek"
                    pure $ OutputType (Output (toHsType t) (peek <+> ptr)) w
                  CommandGetValues -> do
                    alloc <- getAlloc t
                    w     <- allocaWrap alloc paramName
                    pure $ InputType (Input paramName (toHsType t)) w
                | -- A pointer to an optional return value
                  Nothing <- pLength p
                , isSimpleType t
                , Just [False, True] <- pIsOptional p
                , isDefaultable t
                -> do
                  (w, ptr) <- simpleAllocaOutputWrap paramName
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
                        <> toMemberName m
                        <> " "
                        <> "("
                        <> makeParamName (dropPointer s)
                        <> " :: "
                        <> T.tShow structTypeDoc
                        <> "))"
                    Just vec -> do
                      tellQualifiedImport "Data.Vector" "length"
                      pure
                        $  "Data.Vector.length ("
                        <> toMemberName vec
                        <> " ("
                        <> makeParamName (dropPointer s)
                        <> " :: "
                        <> T.tShow structTypeDoc
                        <> "))"
                  peekElemOff <- getPeekElemOff t
                  peek        <- peekVectorOutput peekElemOff
                                                  paramName
                                                  lengthExpression
                  w <- vectorOutputWrap paramName lengthExpression
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ OutputType (Output vType peek) w
                | -- A vector to read which will be made as long as an input parameter
                  Just (NamedLength _lengthParamName) <- pLength p
                , isSimpleType t
                , Nothing <- pIsOptional p
                , [inputVector] <- inputVectorLengthMap name
                -> do
                  -- TODO: Make sure that this is always correct, the output
                  -- length is sometimes determined by a parameter, should only
                  -- *peek* that much
                  tellQualifiedImport "Data.Vector" "length"
                  let lengthExpression =
                        "(Data.Vector.length "
                          <> (makeParamName (dropPointer (pName inputVector)))
                          <> ")"
                  peekElemOff <- getPeekElemOff t
                  peek        <- peekVectorOutput peekElemOff
                                                  paramName
                                                  lengthExpression
                  w <- vectorOutputWrap paramName lengthExpression
                  let vType = do
                        tellImport "Data.Vector" "Vector"
                        tyName <- toHsType t
                        pure ("Vector" <+> tyName)
                  pure $ OutputType (Output vType peek) w
                | -- The value dual use command output
                  -- It is an array with a parameter length in bytes
                  -- Returned as a ByteString
                  isLengthPairList name
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
                      paramName
                      (makeParamName (ptrName (dropPointer lengthParamName)))
                    w <- byteStringOutputWrap
                      paramName
                      ("fromIntegral" T.<+> makeParamName (dropPointer lengthParamName))
                    let
                      vType =
                        tellImport "Data.ByteString" "ByteString"
                          $> "ByteString"
                    pure $ OutputType (Output vType peek) w
                | -- The value dual use command output
                  -- It is an array with a parameter length
                  isLengthPairList name
                , -- It is not required
                  Just [True] <- pIsOptional p
                , -- We are generating a dual use command
                  Just commandUsage <- maybeCommandUsage
                , isSimpleType t
                , Just (NamedLength lengthParamName) <- pLength p
                -> case commandUsage of
                  CommandGetLength -> InferredType <$> passNullPtrWrap
                  CommandGetValues -> do
                    peekElemOff <- case getHandle t of
                      Just h | Dispatchable <- hHandleType h -> do
                        getCommandTable <- case hLevel h of
                          Nothing ->
                            throwError
                              [ Other
                                  "Getting the command table for a handle without a level"
                              ]
                          Just Instance ->
                            throwError
                              [ Other
                                  "Getting the command table for vector of instances"
                              ]
                          Just PhysicalDevice ->
                            pure ("pure commandTable" :: Doc ())
                          Just Device ->
                            throwError
                              [ Other
                                  "Getting the command table for vector of devices"
                              ]
                        tellImport "Foreign.Storable" "peekElemOff"
                        pure
                          [qci|(\\p i -> {dropVkType (hName h)} <$> peekElemOff p i <*> {getCommandTable})|]
                      _ -> getPeekElemOff t
                    peek <- peekVectorOutputPeekLength
                      peekElemOff
                      paramName
                      (ptrName (makeParamName (dropPointer lengthParamName)))
                    w <- vectorOutputWrap
                      paramName
                      ("fromIntegral" T.<+> makeParamName (dropPointer lengthParamName))
                    let vType = do
                          tellImport "Data.Vector" "Vector"
                          tyName <- toHsType t
                          pure ("Vector" <+> tyName)
                    pure $ OutputType (Output vType peek) w
                | -- This is an array with a parameter length in bytes
                  -- Returned as a ByteString
                  isLengthPairList name
                , -- It is not required
                  Nothing <- pIsOptional p
                , Void <- t
                , Just (NamedLength lengthParamName) <- pLength p
                -> do
                  peek <- peekByteStringOutput paramName
                                               (makeParamName (dropPointer lengthParamName))
                  w <- byteStringOutputWrap
                    paramName
                    ("fromIntegral" T.<+> makeParamName (dropPointer lengthParamName))
                  let
                    vType =
                      tellImport "Data.ByteString" "ByteString" $> "ByteString"
                  pure $ OutputType (Output vType peek) w
              t -> throwError [Other ("unhandled type: " <> T.tShow t)]
    in
      traverse parameterToWrappingType ps

-- | Returns (parameter containing length, name of member representing length,)
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
  { iName :: Text
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

handleWrap
  :: Text
  -- ^ Parameter name
  -> Handle
  -> WrapM Wrapper
handleWrap paramName handle = do
  let w = pretty $ unReservedWord paramName
      pat = parens [qci|{dropVkType (hName handle)} {w} commandTable|]
  pure $ \cont e -> [qci|\\{pat} -> {cont (e <+> "commandTable" <+> w)}|]

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

handleOutputWrap
  :: Text
  -- ^ Parameter name
  -> WrapM (Wrapper, Doc ())
  -- ^ The wrapper and the name of the pointer used
handleOutputWrap = simpleAllocaOutputWrap

constructHandle
  :: Handle
  -> Doc ()
  -- ^ The name of the pointer to the C handle
  -> WrapM (Doc ())
constructHandle h ptr = case hHandleType h of
  NonDispatchable -> do
    tellImport "Foreign.Storable" "peek"
    pure [qci|peek {ptr}|]
  Dispatchable -> case hName h of
    "VkInstance" -> do
      tellDepend (Unguarded (WE.TypeName "Instance"))
      tellDepend (Unguarded (TermName "initInstanceCmds"))
      tellImport "Foreign.Storable" "peek"
      pure
        [qci|peek {ptr} >>= (\instanceH -> Instance instanceH <$> initInstanceCmds instanceH)|]
    "VkDevice" -> do
      tellDepend (Unguarded (WE.TypeName "Device"))
      tellDepend (Unguarded (TermName "initDeviceCmds"))
      tellImport "Foreign.Storable" "peek"
      pure
        [qci|peek {ptr} >>= (\deviceH -> Device deviceH <$> initDeviceCmds commandTable deviceH)|]
    _ -> case hLevel h of
      Just _ -> do
        let con = dropVkType (hName h)
        tellImport "Foreign.Storable" "peek"
        tellDepend (Unguarded (WE.TypeName con))
        pure [qci|flip {pretty con} commandTable <$> peek {ptr}|]
      Nothing ->
        throwError [Other "constructing dispatchable handle with no level"]

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
  -- ^ Required vector names
  -> [Text]
  -- ^ Optional vector names
  -> WrapM Wrapper
lengthWrap vecs optionals = do
  when (null vecs) $
    throwError [Other "lengthWrap has no required vectors"]
  tellQualifiedImport "Data.Vector" "length"
  let lengthExpressions =
        (vecs <&> \n -> [qci|Data.Vector.length {unReservedWord n}|])
          ++ (   optionals
             <&> \n -> [qci|maybe maxBound Data.Vector.length {unReservedWord n}|]
             )
      minLength = hsep (punctuate " `min`" lengthExpressions)
  pure $ \cont e -> cont [qci|{e} (fromIntegral $ {minLength})|]

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
      [qci|{e} (fromIntegral $ sizeOf (Data.Vector.head {vec}) * Data.Vector.length {vec})|]

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

optionalVecWrap
  :: Text
  -- ^ Non pointer Allocator
  -> Text
  -- ^ vector name
  -> WrapM Wrapper
optionalVecWrap nonPtrAlloc vecName = do
  tellDepend (Unguarded (TermName "withVec"))
  tellImport "Foreign.Marshal.Utils" "maybeWith"
  pure $ \cont e ->
    let param    = pretty (unReservedWord $ dropPointer vecName)
        paramPtr = pretty (ptrName (dropPointer vecName))
        -- Note the bracket opened here is closed after cont!
        withPtr  = [qci|maybeWith (withVec {nonPtrAlloc}) {param} (\\{paramPtr} -> {e} {paramPtr}|]
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
                [qci|{e} >>= (\\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ({tupleA (["pure ret"| returnsResult] ++ (oPeek <$> outputs))}))|]
        _ ->
          pure $ \e ->
            [qci|{e} >>= (\\ret -> {tupleA ("pure ret" : (oPeek <$> outputs))})|]
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
        t <- wtsToSig KeepVkResult c constraints wts
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

----------------------------------------------------------------
-- Getting all values using a dual use command
----------------------------------------------------------------

writeGetAllCommand
  :: [WrappingType]
  -- ^ The wrapping types for the GetLength command
  -> [WrappingType]
  -- ^ The wrapping types for the GetValues command
  -> Command
  -> WrapM (DocMap -> Doc ())
writeGetAllCommand getLengthTypes getValuesTypes c@Command {..} = do
  getAll <- maybe
    (throwError [Other $ "Failed to get 'getAll' function name for" T.<+> cName]
    )
    pure
    (funGetAllName cName)
  getLength <- maybe
    (throwError [Other $ "Failed to get 'getAll' function name for" T.<+> cName]
    )
    pure
    (funGetLengthName cName)
  let get     = funName cName
      inputs  = [ i | WrappingType (Just i) _ _ <- getLengthTypes ]
      outputs = [ o | WrappingType _ (Just o) _ <- getValuesTypes ]
      args    = pretty . unReservedWord . iName <$> inputs
  case outputs of
    []  -> throwError [Other ("getAll command has no outputs:" T.<+> cName)]
    [_] -> pure ()
    _   -> throwError
      [Other ("getAll command has more than one output:" T.<+> cName)]
  case cSuccessCodes of
    -- TODO: why does vkGetPhysicalDeviceQueueFamilyProperties behave like this
    Nothing | Void <- cReturnType -> pure ()
    Nothing                       -> throwError
      [Other ("getAll command doesn't return VK_INCOMPLETE:" T.<+> cName)]
    Just ["VK_SUCCESS"] -> throwError
      [Other ("getAll command doesn't return VK_INCOMPLETE:" T.<+> cName)]
    Just ["VK_SUCCESS", "VK_INCOMPLETE"] -> pure ()
    Just cs                              -> throwError
      [Other ("getAll returns unexpected success codes:" T.<+> T.tShow cs)]
  type' <- wtsToSig
    IgnoreVkResult
    c
    []
    (  ((\i -> WrappingType (Just i) Nothing id) <$> inputs)
    <> ((\o -> WrappingType Nothing (Just o) id) <$> outputs)
    )
  let takeResult = if cReturnType == Void then "" else "snd <$> " :: Doc ()
  tellExport (Unguarded (Term getAll))
  pure $ \_ -> [qci|
    -- | Returns all the values available from '{get}'.
    {getAll} :: {type'}
    {getAll} {hsep args} =
      {takeResult}{getLength} {hsep args}
        >>= \num -> {takeResult}{get} {hsep args} num

  |]
