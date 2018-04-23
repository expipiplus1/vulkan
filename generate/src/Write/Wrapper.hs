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
import           Debug.Trace
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
  id
  (wrapCommand isHandle isBitmask command)

wrapCommand
  :: (Text -> Bool) -> (Text -> Bool) -> Command -> Either Text (Doc ())
wrapCommand isHandle isBitmask Command{..} = do
  wts <- parametersToWrappingTypes isHandle isBitmask cParameters
  pure ([qci|{funName cName} :: {wtsToSig wts}|] <> line <>
        [qci|{funName cName} = {wrap wts (pretty cName) }|])

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
  -> (Text -> Bool)
  -> [Parameter]
  -> Either Text [WrappingType]
parametersToWrappingTypes isHandle isBitmask ps =
  let
    lengthPairs :: [(Parameter, Maybe Text, Parameter)]
    lengthPairs = getLengthPointerPairs ps

    nonMemberLengthMap :: Text -> Maybe Parameter
    nonMemberLengthMap =
      (`Map.lookup` Map.fromList
        [ (pName l, v) | (l, Nothing, v) <- lengthPairs ]
      )

    isLengthPairList :: Text -> Bool
    isLengthPairList = (`elem` [ pName v | (_, Nothing, v) <- lengthPairs ])

    parameterToWrappingType :: Parameter -> Either Text WrappingType
    parameterToWrappingType p
      | Just vector <- nonMemberLengthMap (pName p)
      = pure $ InferredType (lengthWrap (dropPointer (pName vector)))
      | otherwise
      = let name = pName p
        in
          case pType p of
            Void      -> throwError "void parameter"
            Array{}   -> throwError "array parameter"
            Proto _ _ -> throwError "proto paramter"
            t
              | Just tyName <- simpleTypeName t, isNothing (pIsOptional p)
                -- Handles can always be null
                || isHandle tyName
                -- Bitmasks can always be 0
                || isBitmask tyName
              -> pure
                $ InputType (Input name tyName) (simpleWrap (pName p))
            Ptr Const t
              | Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure
                $ InputType (Input name tyName) (allocaWrap (pName p))
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
              | otherwise
              -> throwError "array ptr parameter"
            Ptr NonConst t
              | Nothing <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ OutputType
                (Output tyName ("peek" <+> pretty (pName p)))
                (simpleAllocaOutputWrap (pName p))
              | Just (NamedMemberLength s m) <- pLength p
              , Just tyName <- simpleTypeName t
              , Nothing <- pIsOptional p
              -> pure $ OutputType
                (Output ("Vector " <> tyName) (peekForeignPtrOutput (pName p) s m))
                (vectorForeignPtrOutputWrap (pName p) s m)
            t -> throwError ("unhandled type: " <> T.tShow t)
  in
    traverse parameterToWrappingType ps

-- | Returns (parameter containing length, name of member representing length,
-- vector parameter)
getLengthPointerPairs :: [Parameter] -> [(Parameter, Maybe Text, Parameter)]
getLengthPointerPairs parameters =
  let
    -- A list of all const arrays with lengths
    arrays :: [((Text, Maybe Text), Parameter)]
    arrays =
      [ (lm, p)
      | p@(Parameter _ (Ptr Const _) (Just lenSpecifier) Nothing) <- parameters
      , lm <- case lenSpecifier of
        NamedLength l         -> pure (l, Nothing)
        NamedMemberLength l m -> pure (l, Just m)
        _                     -> []
      ]
    -- We have a pair if the length is determined by exactly one array
    uniqueLengthPairs :: [(Text, Maybe Text, Parameter)]
    uniqueLengthPairs =
      [ (length, m, p)
      | ((length, m), [p]) <- MultiMap.assocs (MultiMap.fromList arrays)
      ]
    -- A map from name to parameters
    parameterMap = (`Map.lookup` Map.fromList ((pName &&& id) <$> parameters))
  in
    -- Only return pairs where the length is another parameter
    mapMaybe (\(length, m, p) -> (, m, p) <$> parameterMap length)
             uniqueLengthPairs

getMemberLengthPointerPairs :: [Parameter] -> [(Parameter, Text, Parameter)]
getMemberLengthPointerPairs parameters
  = let
      -- A list of all const arrays with lengths
      arrays :: [((Text, Text), Parameter)]
      arrays =
        [ ((length, member), p)
        | p@(Parameter _ (Ptr Const _) (Just (NamedMemberLength length member)) Nothing) <-
          parameters
        ]
      -- We have a pair if the length is determined by exactly one array
      uniqueLengthPairs :: [(Text, Text, Parameter)]
      uniqueLengthPairs =
        [ (length, member, p)
        | ((length, member), [p]) <- MultiMap.assocs (MultiMap.fromList arrays)
        ]
      -- A map from name to parameters
      parameterMap =
        (`Map.lookup` Map.fromList ((pName &&& id) <$> parameters))
    in
      -- Only return pairs where the length is another parameter
      mapMaybe (\(length, member, p) -> (, member, p) <$> parameterMap length)
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
  [qci|\\{pretty paramName} -> {cont (e <+> pretty paramName)}|]

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
  let param = pretty (dropPointer paramName)
      paramPtr = pretty (ptrName (dropPointer paramName))
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
-}

lengthWrap
  :: Text
  -- ^ Vector name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
lengthWrap vec cont e = cont [qci|{e} (length {vec})|]

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
  -- ^ Struct containing the length member
  -> Text
  -- ^ Member of struct containing the length
  -> Doc ()
peekForeignPtrOutput paramName struct lenMember =
  [qci|pure (unsafeFromForeignPtr0 {"f" <> ptrName (dropPointer paramName)} (fromIntegral ({lenMember} {struct})))|]

vectorForeignPtrOutputWrap
  :: Text
  -- ^ Parameter name
  -> Text
  -- ^ Struct containing the length member
  -> Text
  -- ^ Member of struct containing the length
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
vectorForeignPtrOutputWrap vecName struct lenMember cont e =
  let
    param     = pretty (dropPointer vecName)
    paramPtr  = pretty (ptrName (dropPointer vecName))
    paramFPtr = pretty ("f" <> ptrName (dropPointer vecName))
    -- Note the brackets opened here are closed after cont!
    withPtr
      = [qci|mallocForeignPtrArray (fromIntegral ({lenMember} {struct})) >>= (\\{paramFPtr} -> withForeignPtr {paramFPtr} (\\{paramPtr} -> {e} {paramPtr}|]
  in
    [qci|{cont withPtr}))|]

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

