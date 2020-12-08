module CType where

import qualified Data.ByteString.Char8         as BS
import           Data.List.Extra               as List
import qualified Language.C.Types              as C
import           Language.C.Types.Parse  hiding ( Array
                                                , Proto
                                                , TypeName
                                                )
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Const )
import           Spec.Name
import qualified Text.Parsec.Char              as Parsec
import qualified Text.Parser.Token             as Parser
import qualified Text.ParserCombinators.Parsec.Combinator
                                               as Parsec


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
  | TypeName CName
  | Proto CType [(Maybe Text, CType)]
  | Bitfield CType Int
    -- ^ A type and some number of bits
  deriving (Show, Eq, Ord)

data ArraySize
  = NumericArraySize Word
  | SymbolicArraySize CName
  | MultipleArraySize Word ArraySize
  deriving (Show, Eq, Ord)

data Qualifier
  = NonConst
  | Const
  deriving (Show, Eq, Ord)

type C = C.Type C.CIdentifier

parseCType
  :: (MemberWithError (Input TypeNames) r, HasErr r)
  => ByteString
  -> Sem r CType
parseCType bs = do
  parseContext <- cCParserContext False <$> input
  let -- Drop the 'struct' keyword, it confuses our C type parser.
      typeStringWorkarounds :: ByteString -> ByteString
      typeStringWorkarounds =
        BS.unwords
          . Relude.filter (/= "struct")
          . BS.words
          . removeSubString "VKAPI_PTR"
          . removeSubString "XRAPI_PTR"
  case
      runCParser
        parseContext
        "no source"
        (typeStringWorkarounds bs)
        (do
          t    <- C.parseParameterDeclaration
          bits <- ReaderT (const (optional (Parsec.char ':' *> Parser.natural)))
          ReaderT (const (optional (Parsec.char ';') >> Parsec.eof))
          pure (t, bits)
        )
    of
      Left  err -> throw (show err)
      Right (C.ParameterDeclaration _ t, bits) -> do
        r <- cTypeToType t
        pure $ case bits of
          Nothing -> r
          Just b  -> Bitfield r (fromIntegral b)

unBitfield :: CType -> CType
unBitfield = \case
  Bitfield ty _ -> ty
  ty            -> ty

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
  C.SizedByIdentifier t
    | xs <- List.splitOn "*" . unCIdentifier $ t
    , length xs >= 2
    , Just ms <- traverse readMaybe (List.init xs)
    -> let end' = case List.last xs of
             l | Just i <- readMaybe l -> NumericArraySize i
               | otherwise -> SymbolicArraySize . CName . fromString $ l
       in  pure $ Relude.foldr MultipleArraySize end' ms
    | otherwise
    -> pure $ SymbolicArraySize (CName . fromString . unCIdentifier $ t)
  _ -> throw "Unhandled C Array size"

nameToType :: HasErr r => C.TypeSpecifier -> Sem r CType
nameToType = \case
  C.TypeName n    -> pure $ TypeName (CName . fromString . unCIdentifier $ n)
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

getAllTypeNames :: CType -> [CName]
getAllTypeNames = \case
  Float       -> []
  Double      -> []
  Void        -> []
  Char        -> []
  Int         -> []
  Ptr _ t     -> getAllTypeNames t
  Array _ _ t -> getAllTypeNames t
  TypeName n  -> [n]
  Proto t ts ->
    getAllTypeNames t <> Relude.concatMap (getAllTypeNames . snd) ts
  Bitfield t _ -> getAllTypeNames t

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Just removes the first occurrence...
removeSubString :: ByteString -> ByteString -> ByteString
removeSubString n h =
  let (b, a) = BS.breakSubstring n h in b <> BS.drop (BS.length n) a

----------------------------------------------------------------
-- Sizing
----------------------------------------------------------------

cTypeSize
  :: (CName -> Maybe Int)
  -> (CType -> Maybe (Int, Int))
  -> CType
  -> Maybe (Int, Int)
cTypeSize constantValue nonBaseSize = \case
  TypeName "uint8_t"             -> Just (1, 1)
  TypeName "uint16_t"            -> Just (2, 2)
  TypeName "uint32_t"            -> Just (4, 4)
  TypeName "uint64_t"            -> Just (8, 8)
  TypeName "int8_t"              -> Just (1, 1)
  TypeName "int16_t"             -> Just (2, 2)
  TypeName "int32_t"             -> Just (4, 4)
  TypeName "int64_t"             -> Just (8, 8)
  TypeName "size_t"              -> Just (8, 8)
  Char                           -> Just (1, 1)
  Int                            -> Just (4, 4)
  Float                          -> Just (4, 4)
  Double                         -> Just (8, 8)
  Ptr _ _                        -> Just (8, 8)
  --- ^ TODO: 32 bit support
  Array _ (NumericArraySize n) t -> do
    ~(es, ea) <- cTypeSize constantValue nonBaseSize t
    Just (es * fromIntegral n, ea)
  Array _ (SymbolicArraySize s) t -> do
    n         <- constantValue s
    ~(es, ea) <- cTypeSize constantValue nonBaseSize t
    Just (es * fromIntegral n, ea)
  t -> nonBaseSize t
