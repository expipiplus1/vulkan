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
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
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
import           Spec.Savvy.Type.Haskell

import           Write.Util

commandWrapper
  :: (Text -> Bool)
  -- ^ Is this the name of a handle type
  -> (Text -> Bool)
  -- ^ Is this the name of a bitmask type
  -> Command
  -> Doc ()
commandWrapper isHandle isBitmask command = either
  ((pretty (cName command) <+>) . pretty)
  vcat
  (wrapCommand isHandle isBitmask command)

wrapCommand
  :: (Text -> Bool) -> (Text -> Bool) -> Command -> Either Text [Doc ()]
wrapCommand isHandle isBitmask Command {..} = do
  let
    lengthPairs :: [(Parameter, Maybe Text, [Parameter])]
    lengthPairs = getLengthPointerPairs cParameters
    makeWts usage =
      parametersToWrappingTypes isHandle isBitmask usage lengthPairs cParameters
  wrappingTypes <- if isDualUseCommand lengthPairs
    then do
      wts1 <- makeWts (Just CommandGetLength)
      wts2 <- makeWts (Just CommandGetValues)
      pure [wts1, wts2]
    else pure <$> makeWts Nothing
  let printTypes wts =
        [qci|{funName cName} :: {wtsToSig wts}|] <> line <>
         [qci|{funName cName} = {wrap wts (pretty cName) }|]
  pure $ printTypes <$> wrappingTypes


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

wtsToSig :: [WrappingType] -> Doc ()
wtsToSig ts =
  let outputs = [ t | WrappingType _ (Just (Output t _)) _ <- ts ]
      ret     = tupled (pretty <$> outputs)
  in  intercalateArrows
        (fmap pretty (mapMaybe (fmap iType . wtInput) ts) <> ["IO" <+> ret])

-- | A simple function is one which takes all its arguments by value or by
-- const pointer with no length and returns 'VkResult'.
-- simpleFunction :: Command -> Maybe (Doc ())
-- simpleFunction c@Command {..} = do
--   guard (cReturnType == TypeName "VkResult")
--   guard (all (isPassByValue <||> isPassByConstPointer) cParameters)
--   pure ([qci|simple: {cName} : {commandType c}|] <> line <> simpleFunctionSig c)

parametersToWrappingTypes
  :: (Text -> Bool)
  -- ^ Is this name a handle
  -> (Text -> Bool)
  -- ^ Is this name a bitmask
  -> Maybe CommandUsage
  -- ^ If this is a dual use command, which usage shall we generate
  -> [(Parameter, Maybe Text, [Parameter])]
  -- ^ (parameter containing length, name of member representing length,
  -- vector parameters which must be this length)
  -> [Parameter]
  -> Either Text [WrappingType]
parametersToWrappingTypes isHandle isBitmask maybeCommandUsage lengthPairs ps =
  let
    -- Go from a length name to a list of vectors having that length
    nonMemberLengthMap :: Text -> Maybe [Parameter]
    nonMemberLengthMap =
      (`Map.lookup` Map.fromList
        [ (pName l, vs) | (l, Nothing, vs) <- lengthPairs ]
      )

    isLengthPairList :: Text -> Bool
    isLengthPairList =
      (`elem` [ pName v | (_, Nothing, vs) <- lengthPairs, v <- vs ])

    parameterToWrappingType :: Parameter -> Either Text WrappingType
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
              | n >= 2, Just tyName <- simpleTypeName t, Nothing <- pIsOptional
                p
              -> pure $ InputType (Input name (tupleTy n tyName))
                                  (tupleWrap n (pName p))
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
              , Just tyName <- simpleTypeName t
              , Ptr _ vType <- pType vector
              , Just _ <- simpleTypeName vType
              -> pure $ InferredType (lengthWrap (dropPointer (pName vector)) name)
              | -- The length of an array of untyped (void*) values, length is
                -- given in bytes.
                Just [vector] <- nonMemberLengthMap (pName p)
              , Just tyName <- simpleTypeName t
              , Ptr _ Void <- pType vector
              -> pure
                $ InferredType (lengthBytesWrap (dropPointer (pName vector)))
              | Just tyName <- simpleTypeName t
              , isNothing (pIsOptional p)
                -- Handles can always be null
                                          || isHandle tyName
                -- Bitmasks can always be 0
                                                             || isBitmask tyName
              -> pure $ InputType (Input name tyName) (simpleWrap (pName p))
            Ptr Const t
              | Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ InputType (Input name tyName) (allocaWrap (pName p))
              | Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Just [True] <- pIsOptional p
              -> pure $ InputType (Input name ("Maybe " <> tyName))
                                  (optionalAllocaWrap (pName p))
              | isLengthPairList (pName p)
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ InputType (Input name ("Vector " <> tyName))
                                  (vecWrap (pName p))
              | -- A const void pointer, represent as a 'Vector a' where a is
                -- storable
                isLengthPairList (pName p)
              , Void <- t
              , Nothing <- pIsOptional p
              -> pure $ InputType (Input name ("Vector " <> "a"))
                                  (vecWrap (pName p))
              | -- A non optional string
                Char <- t
              , Just NullTerminated <- pLength p
              , Nothing <- pIsOptional p
              -> pure
                $ InputType (Input name "ByteString") (cStringWrap (pName p))
              | -- An optional string
                Char <- t
              , Just NullTerminated <- pLength p
              , Just [True] <- pIsOptional p
              -> pure $ InputType (Input name "ByteString")
                                  (optionalCStringWrap (pName p))
              | otherwise
              -> throwError "array ptr parameter"
            Ptr NonConst t
              | -- A pointer to a non-optional return value
                Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> -- This is probaby the most fishy case, it assumes that the
                 -- pointers being sent in are for returning variables only.
                 pure $ OutputType
                (Output tyName ("peek" <+> pretty (pName p)))
                (simpleAllocaOutputWrap (pName p))
              | -- A pointer to an optional return value
                Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Just [False, True] <- pIsOptional p
              , isHandle tyName || isBitmask tyName
              -> pure $ OutputType
                (Output tyName ("peek" <+> pretty (pName p)))
                (simpleAllocaOutputWrap (pName p))
              | -- A vector to read which will be made as long as a member of a
                -- struct being passed in g
                Just (NamedMemberLength s m) <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ OutputType
                (Output ("Vector " <> tyName)
                        (peekForeignPtrOutput (pName p) (m <> " " <> s))
                )
                (vectorForeignPtrOutputWrap (pName p) (m <> " " <> s))
              | -- A vector to read which will be made as long as an input parameter
                Just (NamedLength lengthParamName) <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ OutputType
                (Output ("Vector " <> tyName)
                        (peekForeignPtrOutput (pName p) lengthParamName)
                )
                (vectorForeignPtrOutputWrap (pName p) lengthParamName)
              | -- The length dual use command output value
                -- It is not an array
                Nothing <- pLength p
              , -- It has uint32_t type
                Just "uint32_t" <- simpleTypeName t
              , -- Is is a required pointer to an optional value
                Just [False, True] <- pIsOptional p
              , -- We are generating a dual use command
                Just commandUsage <- maybeCommandUsage
              -> case commandUsage of
                CommandGetLength -> pure $ OutputType
                  (Output "uint32_t" ("peek" <+> pretty (pName p)))
                  (simpleAllocaOutputWrap (pName p))
                CommandGetValues -> pure
                  $ InputType (Input name "uint32_t") (allocaWrap (pName p))
              | -- The value dual use command output
                -- It is an array with a parameter length
                isLengthPairList (pName p)
              , -- It is not required
                Just [True] <- pIsOptional p
              , -- We are generating a dual use command
                Just commandUsage <- maybeCommandUsage
              , Just tyName <- simpleTypeName t
              , Just (NamedLength lengthParamName) <- pLength p
              -> case commandUsage of
                CommandGetLength -> pure $ InferredType passNullPtrWrap
                CommandGetValues -> pure $ OutputType
                  (Output
                    ("Vector " <> tyName)
                    (peekForeignPtrOutputPeekLength (pName p)
                                                    (ptrName lengthParamName)
                    )
                  )
                  (vectorForeignPtrOutputWrap (pName p)
                                              (dropPointer lengthParamName)
                  )
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
  , wtWrap   :: (Doc () -> Doc ()) -> Doc () -> Doc ()
    -- ^ A function taking a continuation to wrap the wrapped value and
    -- returning a wrapper
    -- TODO: Make better types!
  }

data Input = Input
  { iName :: Text
  , iType :: Text
  }

data Output = Output
  { oType :: Text
  , oPeek :: Doc ()
    -- ^ An expression of type "IO oType"
  }

pattern InputType i w = WrappingType (Just i) Nothing w
pattern OutputType o w = WrappingType Nothing (Just o) w
pattern InferredType w = WrappingType Nothing Nothing w

simpleWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
simpleWrap paramName cont e =
  [qci|\\{pretty (unKeyword paramName)} -> {cont (e <+> pretty (unKeyword paramName))}|]

cStringWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
cStringWrap paramName cont e =
  let -- TODO: Use unkeyword everywhere
      param = pretty (unKeyword $ dropPointer paramName)
      paramPtr = pretty (unKeyword $ ptrName (dropPointer paramName))
      -- Note the bracket opened here is closed after cont!
      withPtr = [qci|useAsCString {param} (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

optionalCStringWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
optionalCStringWrap paramName cont e =
  let -- TODO: Use unkeyword everywhere
      param = pretty (unKeyword $ dropPointer paramName)
      paramPtr = pretty (unKeyword $ ptrName (dropPointer paramName))
      -- Note the bracket opened here is closed after cont!
      withPtr = [qci|useAsCStringMaybe {param} (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

simpleAllocaOutputWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc () -> Doc ()
simpleAllocaOutputWrap paramName cont e =
  let param = pretty (dropPointer paramName)
      paramPtr = pretty (ptrName (dropPointer paramName))
      withPtr = [qci|alloca (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|{cont withPtr})|]

allocaWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
allocaWrap paramName cont e =
  let param = pretty (dropPointer paramName)
      paramPtr = pretty (ptrName (dropPointer paramName))
      -- Note the bracket opened here is closed after cont!
      withPtr = [qci|withAlloca {param} (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

optionalAllocaWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
optionalAllocaWrap paramName cont e =
  let param = (dropPointer paramName)
      paramPtr = (ptrName (dropPointer paramName))
      -- Note the bracket opened here is closed after cont!
      withPtr = [qci|withAllocaMaybe {param} (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

{-
withAlloca :: Storable a => a -> (Ptr a -> IO b) -> IO b
withAlloca x f = alloca (\p -> poke p x *> f p)
{-# inline withAlloca #-}

withAllocaMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withAllocaMaybe m f = case m of
  Nothing -> f nullPtr
  Just x  -> withAlloca x f
{-# inline withAllocaMaybe #-}

useAsCStringMaybe :: Maybe ByteString -> (CString -> IO b) -> IO b
useAsCStringMaybe m f = case m of
  Nothing -> f nullPtr
  Just x  -> useAsCString x f
{-# inline withAllocaMaybe #-}
-}

lengthWrap
  :: Text
  -- ^ Vector name
  -> Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
lengthWrap vec paramName cont e =
  let withLength = cont [qci|{e} {paramName}|]
  in [qci|let {paramName} = (length {vec}) in {withLength}|]

lengthBytesWrap
  :: Text
  -- ^ Vector name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
lengthBytesWrap vec cont e = cont [qci|{e} (sizeOf (head {vec}) * length {vec})|]

passNullPtrWrap
  :: (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
passNullPtrWrap cont e = cont [qci|{e} nullPtr|]

vecWrap
  :: Text
  -- ^ vector name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
vecWrap vecName cont e =
  let param = pretty (dropPointer vecName)
      paramPtr = pretty (ptrName (dropPointer vecName))
      -- Note the bracket opened here is closed after cont!
      withPtr = [qci|unsafeWith {param} (\\{paramPtr} -> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

peekForeignPtrOutput
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> Doc ()
peekForeignPtrOutput paramName len =
  [qci|pure (unsafeFromForeignPtr0 {"f" <> ptrName (dropPointer paramName)} (fromIntegral ({len})))|]

peekForeignPtrOutputPeekLength
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get a pointer to the length
  -> Doc ()
peekForeignPtrOutputPeekLength paramName lenPtr =
  [qci|(unsafeFromForeignPtr0 {"f" <> ptrName (dropPointer paramName)} . fromIntegral <$> peek {lenPtr})|]


vectorForeignPtrOutputWrap
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Expression to get the length
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
vectorForeignPtrOutputWrap vecName len cont e =
  let
    param     = pretty (dropPointer vecName)
    paramPtr  = pretty (ptrName (dropPointer vecName))
    paramFPtr = pretty ("f" <> ptrName (dropPointer vecName))
    -- Note the brackets opened here are closed after cont!
    withPtr
      = [qci|mallocForeignPtrArray (fromIntegral ({len})) >>= (\\{paramFPtr} -> withForeignPtr {paramFPtr} (\\{paramPtr} -> {e} {paramPtr}|]
  in
    [qci|{cont withPtr}))|]

tupleTy :: Word -> Text -> Text
tupleTy n t = T.pack . show $ tupled (replicate (fromIntegral n) (pretty t))

tupleWrap :: Word -> Text
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
tupleWrap n paramName cont e =
  -- let paramNames = (pretty paramName <>) . pretty . show <$> [0..n-1]
  --     paramPtr  = pretty (ptrName (dropPointer paramName))
  --     withPtr =
  --       [qci|\\{tupled paramNames} -> withArray {list paramNames} (\\{paramPtr} -> {e} {paramPtr}|]
  -- in [qci|{cont withPtr})|]
  let paramNames = (pretty paramName <>) . pretty . show <$> [0..n-1]
      paramPtr  = pretty (ptrName (dropPointer paramName))
      withPtr =
        [qci|\\{tupled paramNames} -> allocaArray {n} (\\{paramPtr} -> {writePokes paramPtr paramNames} *> {e} {paramPtr}|]
  in [qci|{cont withPtr})|]

-- This opens a bracket
withTupleArray :: [Doc ()] -> Doc ()
withTupleArray ds = [qci|allocaArray {length ds} (\\|]

writePokes :: Doc () -> [Doc ()] -> Doc ()
writePokes ptr ds = hsep $ punctuate "*>" (zipWith writePoke [0..] ds)
  where
    writePoke n d = [qci|pokeElemOff {ptr} {n} {d}|]


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
            xs -> \e -> [qci|{e} *> ({tupleA (oPeek <$> xs)})|]

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
