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

commandWrapper :: Command -> Doc ()
commandWrapper command =
  either ((pretty (cName command) <+>) . pretty) id (wrapCommand command)

wrapCommand :: Command -> Either Text (Doc ())
wrapCommand Command{..} = do
  wts <- parametersToWrappingTypes cParameters
  pure ([qci|{funName cName} :: {wtsToSig wts}|] <> line <>
        [qci|{funName cName} = {wrap wts (pretty cName) }|])

wtsToSig :: [WrappingType] -> Doc ()
wtsToSig ts =
  let outputs = [ t | WrappingType _ (Just (Output t)) _ <- ts ]
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

parametersToWrappingTypes :: [Parameter] -> Either Text [WrappingType]
parametersToWrappingTypes ps =
  let
    lengthPairs :: [(Parameter, Parameter)]
    lengthPairs = getLengthPointerPairs ps

    lengthMap :: Text -> Maybe Parameter
    lengthMap = (`Map.lookup` Map.fromList (first pName <$> lengthPairs))

    isLengthPairList :: Text -> Bool
    isLengthPairList = (`elem` (pName . snd <$> lengthPairs))

    parameterToWrappingType :: Parameter -> Either Text WrappingType
    parameterToWrappingType p
      | Just vector <- lengthMap (pName p)
      = pure $ InferredType (lengthWrap (dropPointer (pName vector)))
      | otherwise
      = let name = pName p
        in
          case pType p of
            Void      -> throwError "void parameter"
            Array{}   -> throwError "array parameter"
            Proto _ _ -> throwError "proto paramter"
            t | Just n <- simpleTypeName t ->
              pure $ InputType (Just (Input n name)) (simpleWrap (pName p))
            Ptr Const t
              | Nothing <- pLength p, Just n <- simpleTypeName t -> pure
              $ InputType (Just (Input n name)) (allocaWrap (pName p))
              | isLengthPairList (pName p), Just n <- simpleTypeName t -> pure
              $ InputType (Just (Input ("Vector " <> n) name))
                          (vecWrap (pName p))
              | otherwise -> throwError "array ptr parameter"
            t -> throwError ("unhandled type: " <> T.tShow t)
  in
    traverse parameterToWrappingType ps

getLengthPointerPairs :: [Parameter] -> [(Parameter, Parameter)]
getLengthPointerPairs parameters
  = let
      -- A list of all const arrays with lengths
      arrays :: [(Text, Parameter)]
      arrays =
        [ (length, p)
        | p@(Parameter _ (Ptr Const _) (Just (NamedLength length)) Nothing) <-
          parameters
        ]
      -- We have a pair if the length is determined by exactly one array
      uniqueLengthPairs :: [(Text, Parameter)]
      uniqueLengthPairs =
        [ (length, p)
        | (length, [p]) <- MultiMap.assocs (MultiMap.fromList arrays)
        ]
      -- A map from name to parameters
      parameterMap =
        (`Map.lookup` Map.fromList ((pName &&& id) <$> parameters))
    in
      -- Only return pairs where the length is another parameter
      mapMaybe (\(length, p) -> (, p) <$> parameterMap length) uniqueLengthPairs

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

newtype Output = Output
  { oType :: Text
  }

pattern InputType i w = WrappingType i Nothing w
pattern OutputType o w = WrappingType Nothing o w
pattern InferredType w = WrappingType Nothing Nothing w

simpleWrap
  :: Text
  -- ^ Parameter name
  -> (Doc () -> Doc ())
  -> Doc ()
  -> Doc ()
simpleWrap paramName cont e =
  [qci|\\{pretty paramName} -> {cont (e <+> pretty paramName)}|]

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
      withPtr = [qci|alloca (\\{paramPtr} -> poke {paramPtr} {param} *> {e} {paramPtr}|]
  in [qci|\\{param} -> {cont withPtr})|]

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

wrap :: [WrappingType] -> Doc () -> Doc ()
wrap = go . fmap wtWrap
  where
    go :: [(Doc () -> Doc ()) -> Doc () -> Doc ()] -> Doc () -> Doc ()
    go = ($id) . appEndo . fold . fmap Endo

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
