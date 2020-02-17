module CType
  where

import           Relude                  hiding ( Const
                                                , Reader
                                                , ask
                                                )
import           Language.C.Types.Parse  hiding ( Proto
                                                , Array
                                                , TypeName
                                                )
import qualified Language.C.Types              as C
import           Data.ByteString.Char8         as BS
import           Polysemy
import           Polysemy.Reader
import qualified Text.ParserCombinators.Parsec.Combinator as Parsec
import qualified Text.Parsec.Char as Parsec


import           Error

data CType
  = Float
  | Double
  | Void
  | Char
  | Int
    -- ^ Signed int
  | Ptr Qualifier CType
    -- ^ Qualifies the pointed to type
  | Array Qualifier ArraySize CType
    -- ^ Qualifies the pointed to type
  | TypeName Text
  | Proto CType [(Maybe Text, CType)]
  deriving (Show, Eq, Ord)

data ArraySize
  = NumericArraySize Word
  | SymbolicArraySize Text
  deriving (Show, Eq, Ord)

data Qualifier
  = NonConst
  | Const
  deriving (Show, Eq, Ord)

type C = C.Type C.CIdentifier

parseCType
  :: (MemberWithError (Reader TypeNames) r, HasErr r)
  => ByteString
  -> Sem r CType
parseCType bs = do
  parseContext <- cCParserContext <$> ask
  let -- Drop the 'struct' keyword, it confuses our C type parser.
      typeStringWorkarounds :: ByteString -> ByteString
      typeStringWorkarounds =
        BS.unwords . Relude.filter (/= "struct") . BS.words . removeSubString
          "VKAPI_PTR"
  case
      runCParser
        parseContext
        "no source"
        (typeStringWorkarounds bs)
        (  C.parseParameterDeclaration
        <* (ReaderT (const (optional (Parsec.char ';') >> Parsec.eof)))
        )
    of
      Left  err                          -> throw (show err)
      Right (C.ParameterDeclaration _ t) -> cTypeToType t

cTypeToType :: HasErr r => C -> Sem r CType
cTypeToType = \case
  C.TypeSpecifier (C.Specifiers [] [] []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [] [C.CONST] []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [C.TYPEDEF] [] []) t -> nameToType t
  C.Ptr           []        t  -> Ptr <$> typeQualifier t <*> cTypeToType t
  C.Ptr           [C.CONST] t  -> Ptr <$> typeQualifier t <*> cTypeToType t
  C.Array s t -> Array <$> typeQualifier t <*> arraySize s <*> cTypeToType t
  C.Proto ret ps -> Proto <$> cTypeToType ret <*> traverse cParamDeclToType ps
  _                            -> throw "Unhandled C Type"

typeQualifier :: HasErr r => C -> Sem r Qualifier
typeQualifier = \case
  C.TypeSpecifier (C.Specifiers [] [] []) _ -> pure NonConst
  C.TypeSpecifier (C.Specifiers [] [C.CONST] []) _ -> pure Const
  C.TypeSpecifier (C.Specifiers [C.TYPEDEF] [] []) _ -> pure NonConst
  C.Ptr           []        _ -> pure NonConst
  C.Ptr           [C.CONST] _ -> pure Const
  C.Array         _         _ -> pure NonConst
  C.Proto         _         _ -> pure NonConst
  _                           -> throw "Unhandled C Type"

cParamDeclToType
  :: HasErr r
  => C.ParameterDeclaration C.CIdentifier
  -> Sem r (Maybe Text, CType)
cParamDeclToType (C.ParameterDeclaration cId t) =
  (,) (fromString . unCIdentifier <$> cId) <$> cTypeToType t

arraySize :: HasErr r => C.ArrayType C.CIdentifier -> Sem r ArraySize
arraySize = \case
  C.SizedByInteger i | i >= 0    -> pure $ NumericArraySize (fromInteger i)
                     | otherwise -> throw "Negative C array size"
  C.SizedByIdentifier t ->
    pure $ SymbolicArraySize (fromString (unCIdentifier t))
  _ -> throw "Unhandled C Array size"

nameToType :: HasErr r => C.TypeSpecifier -> Sem r CType
nameToType = \case
  C.TypeName n    -> pure $ TypeName (fromString $ unCIdentifier n)
  C.Float         -> pure Float
  C.Double        -> pure Double
  C.Void          -> pure Void
  C.Char Nothing  -> pure Char
  C.Int  C.Signed -> pure Int
  _               -> throw "Unhandled C type specifier"

isPtrType :: CType -> Bool
isPtrType = \case
  Ptr _ _ -> True
  _       -> False

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Just removes the first occurrence...
removeSubString :: ByteString -> ByteString -> ByteString
removeSubString n h =
  let (b, a) = BS.breakSubstring n h in b <> (BS.drop (BS.length n) a)

