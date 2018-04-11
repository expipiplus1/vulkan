{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Spec.Savvy.Type
  ( Type(..)
  , TypeParseContext
  , stringToType
  , specParserContext
  ) where

import           Control.Monad
import           Data.Either.Validation
import qualified Data.HashSet           as HashSet
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Traversable
import qualified Language.C.Types       as C
import           Spec.Savvy.Error
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data Type
  = Float
  | Void
  | Char
  | Int
    -- ^ Signed int
  | Ptr Type
  | Array ArraySize Type
  | TypeName Text
  deriving (Show)

data ArraySize
  = NumericArraySize Word
  | SymbolicArraySize Text
  deriving (Show)

type CType = C.Type C.CIdentifier

type TypeParseContext = C.CParserContext C.CIdentifier

stringToType :: TypeParseContext -> Text -> Either [SpecError] Type
stringToType pc = cTypeToType <=< stringToCType pc

cTypeToType :: CType -> Either [SpecError] Type
cTypeToType = \case
  C.TypeSpecifier (C.Specifiers [] []        []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [] [C.CONST] []) t -> nameToType t
  C.Ptr           []                             t -> Ptr <$> cTypeToType t
  C.Ptr           [C.CONST]                      t -> Ptr <$> cTypeToType t
  C.Array         s                              t -> Array <$> arraySize s <*> cTypeToType t
  c -> Left [UnhandledCType (showText c)]

arraySize :: C.ArrayType C.CIdentifier -> Either [SpecError] ArraySize
arraySize = \case
  C.SizedByInteger i
    | i >= 0
    -> pure $ NumericArraySize (fromInteger i)
    | otherwise
    -> Left [NegativeCArraySize i]
  C.SizedByIdentifier t -> pure $ SymbolicArraySize (T.pack (C.unCIdentifier t))
  s -> Left [UnhandledCArraySize (showText s)]

nameToType :: C.TypeSpecifier -> Either [SpecError] Type
nameToType = \case
  (C.TypeName (C.unCIdentifier -> n)) -> pure $ TypeName (T.pack n)
  C.Float -> pure Float
  C.Void -> pure Void
  C.Char Nothing -> pure Char
  C.Int C.Signed -> pure Int
  t -> Left [UnhandledCTypeSpecifier (showText t)]

stringToCType
  :: TypeParseContext -> Text -> Either [SpecError] CType
stringToCType parseContext s =
  let r = C.runCParser parseContext "typestring" (typeStringWorkarounds s) C.parseType
  in  case r of
        Left  parseError -> Left [TypeParseError s (showText parseError)]
        Right t          -> Right t

showText :: Show a => a -> Text
showText = T.pack . show

specParserContext :: P.Spec -> Either [SpecError] TypeParseContext
specParserContext P.Spec {..} = validationToEither $ do
  let disallowedNames = ["int", "void", "char", "float"]
  let specTypeNames = filter (`notElem` disallowedNames)
        $ catMaybes (P.typeDeclTypeName <$> sTypes)
  specCTypeNames <- for specTypeNames $ \tn ->
    case C.cIdentifierFromString (T.unpack tn) of
      Left  e -> Failure [TypeNameParseError tn (T.pack e)]
      Right c -> pure c
  pure $ C.cCParserContext . HashSet.fromList $ specCTypeNames

-- | Drop the 'struct' keyword, it confuses our C type parser.
typeStringWorkarounds :: Text -> Text
typeStringWorkarounds = T.unwords . filter (/= "struct") . T.words
