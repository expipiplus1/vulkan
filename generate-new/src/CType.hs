module CType
  where

import Relude hiding (Const)
import Language.C.Types.Parse hiding (Proto, Array, TypeName)
import qualified Language.C.Types as C
import Data.ByteString.Char8 as BS

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

parseCType :: TypeNames -> ByteString -> Either Text CType
parseCType typeNames bs =
  let context = cCParserContext typeNames
      -- Drop the 'struct' keyword, it confuses our C type parser.
      typeStringWorkarounds :: ByteString -> ByteString
      typeStringWorkarounds =
            -- dropWhileEnd (== ';')
          BS.unwords . Relude.filter (/= "struct") . BS.words
  in  case
          runCParser context
                     "no source"
                     (typeStringWorkarounds bs)
                     C.parseParameterDeclaration
        of
          Left  err                          -> Left (show err)
          Right (C.ParameterDeclaration _ t) -> cTypeToType t

cTypeToType :: C -> Either Text CType
cTypeToType = \case
  C.TypeSpecifier (C.Specifiers [] [] []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [] [C.CONST] []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [C.TYPEDEF] [] []) t -> nameToType t
  C.Ptr           []        t  -> Ptr <$> typeQualifier t <*> cTypeToType t
  C.Ptr           [C.CONST] t  -> Ptr <$> typeQualifier t <*> cTypeToType t
  C.Array s t -> Array <$> typeQualifier t <*> arraySize s <*> cTypeToType t
  C.Proto ret ps -> Proto <$> cTypeToType ret <*> traverse cParamDeclToType ps
  _                            -> Left "Unhandled C Type"

typeQualifier :: C -> Either Text Qualifier
typeQualifier = \case
  C.TypeSpecifier (C.Specifiers [] [] []) _ -> pure NonConst
  C.TypeSpecifier (C.Specifiers [] [C.CONST] []) _ -> pure Const
  C.TypeSpecifier (C.Specifiers [C.TYPEDEF] [] []) _ -> pure NonConst
  C.Ptr           []        _ -> pure NonConst
  C.Ptr           [C.CONST] _ -> pure Const
  C.Array         _         _ -> pure NonConst
  C.Proto         _         _ -> pure NonConst
  _                           -> Left "Unhandled C Type"

cParamDeclToType
  :: C.ParameterDeclaration C.CIdentifier -> Either Text (Maybe Text, CType)
cParamDeclToType (C.ParameterDeclaration cId t) =
  (,) (fromString . unCIdentifier <$> cId) <$> cTypeToType t

arraySize :: C.ArrayType C.CIdentifier -> Either Text ArraySize
arraySize = \case
  C.SizedByInteger i | i >= 0    -> pure $ NumericArraySize (fromInteger i)
                     | otherwise -> Left "Negative C array size"
  C.SizedByIdentifier t -> pure $ SymbolicArraySize (fromString (unCIdentifier t))
  _                     -> Left "Unhandled C Array size"

nameToType :: C.TypeSpecifier -> Either Text CType
nameToType = \case
  C.TypeName n    -> pure $ TypeName (fromString $ unCIdentifier n)
  C.Float         -> pure Float
  C.Double        -> pure Double
  C.Void          -> pure Void
  C.Char Nothing  -> pure Char
  C.Int  C.Signed -> pure Int
  _               -> Left "Unhandled C type specifier"

