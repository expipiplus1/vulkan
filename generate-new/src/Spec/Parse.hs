{-# LANGUAGE QuantifiedConstraints #-}

module Spec.Parse where

import           Relude
import           Xeno.DOM
import           Data.Vector                    ( Vector )
import           Language.C.Types               ( cIdentifierFromString
                                                , TypeNames
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.List                      ( dropWhileEnd
                                                , lookup
                                                )
import           Data.Char
import           Polysemy
import           Data.Text.Extra                ( (<+>) )

import           CType
import           Error
import           Marshal.Name
import qualified Marshal.Marshalable         as M
import           Marshal.Marshalable          ( ParameterLength(..) )

data Spec = Spec
  { specStructs :: Vector Struct
  }
  deriving (Show)

data Struct = Struct
  { structName :: Text
  , structMembers :: Vector StructMember
  }
  deriving (Show)

data StructMember = StructMember
  { smName :: Text
  , smType :: CType
  , smValues :: Vector Text
  , smLengths :: Vector M.ParameterLength
  , smIsOptional :: Vector Bool
  }
  deriving (Show)

instance M.Marshalable StructMember where
  name       = Name . smName
  type'      = smType
  values     = smValues
  lengths    = smLengths
  isOptional = smIsOptional

type P a = forall r . HasErr r => Sem r a

parseSpec :: ByteString -> P Spec
parseSpec bs = do
  n <- fromEither (first show (parse bs))
  case name n of
    "registry" -> do
      ts <-
        expectOne "types"
          $ [ contents e | Element e <- contents n, name e == "types" ]
      parseTypes ts
    _ -> throw "This spec isn't a registry node"

parseTypes :: [Content] -> P Spec
parseTypes es = do
  typeNames   <- allTypeNames es
  specStructs <-
    several
    . fromList
    $ [ parseStruct typeNames n
      | Element n <- es
      , name n == "type"
      , Just "struct" <- pure (getAttr "category" n)
      ]
  pure Spec { .. }

parseStruct :: TypeNames -> Node -> P Struct
parseStruct typeNames n = do
  structName <- decode =<< note "Unable to get struct name" (getAttr "name" n)
  structMembers <-
    several
    . fromList
    $ [ parseStructMember typeNames m
      | Element m <- contents n
      , name m == "member"
      ]
  pure Struct { .. }

parseStructMember :: TypeNames -> Node -> P StructMember
parseStructMember typeNames m = do
  smName <-
    decode
      =<< (maybe (throw "struct member without name") pure $ elemText "name" m)
  let typeString = allText m
  smType       <- parseCType typeNames typeString
  smIsOptional <- boolListAttr "optional" m
  smLengths    <- lenListAttr "len" m
  smValues     <- listAttr decode "values" m
  pure StructMember { .. }

----------------------------------------------------------------
-- Getting all the type names
----------------------------------------------------------------

allTypeNames :: [Content] -> P TypeNames
allTypeNames es = do
  let nameText :: Node -> P ByteString
      nameText n =
        note "Unable to get type name" (getAttr "name" n <|> elemText "name" n)
  categoryTypeNames <- several
    [ nameText n
    | Element n <- es
    , name n == "type"
    , Just c <- pure (getAttr "category" n)
    , c `notElem` ["include", "define"]
    ]
  requiresTypeNames <- several
    [ nameText n | Element n <- es, name n == "type", hasAttr "requires" n ]
  fromList <$> several
    ( fmap
        ( fromEither
        . first fromList
        . cIdentifierFromString
        . dropWhileEnd isSpace
        . dropWhile isSpace
        . BS.unpack
        )
    . filter (`notElem` reservedTypeNames)
    $ (extraTypeNames <> toList (categoryTypeNames <> requiresTypeNames))
    )

reservedTypeNames :: [ByteString]
reservedTypeNames = ["void", "char", "float", "double"]

extraTypeNames :: [ByteString]
extraTypeNames = ["ANativeWindow", "AHardwareBuffer", "CAMetalLayer"]

----------------------------------------------------------------
-- XML
----------------------------------------------------------------

decode :: ByteString -> P Text
decode bs = case decodeUtf8' bs of
  Left  e -> throw $ show e
  Right t -> pure t

hasAttr :: ByteString -> Node -> Bool
hasAttr a n = any ((== a) . fst) (attributes n)

getAttr :: ByteString -> Node -> Maybe ByteString
getAttr a n = lookup a (attributes n)

allText :: Node -> ByteString
allText =
  BS.unwords
    . fmap
        (\case
          Text    t -> t
          Element n -> allText n
          CData   t -> t
        )
    . contents

elemText :: ByteString -> Node -> Maybe ByteString
elemText elemName node =
  let r =
          [ m
          | Element a <- contents node
          , name a == elemName
          , [Text m] <- pure (contents a)
          ]
  in  case r of
        [x] -> Just x
        _   -> Nothing

-- | Empty list if there is no such attribute
listAttr :: (ByteString -> P a) -> ByteString -> Node -> P (Vector a)
listAttr p a n = case getAttr a n of
  Nothing -> pure mempty
  Just bs -> do
    traverse p . fromList . BS.split ',' $ bs

boolListAttr :: ByteString -> Node -> P (Vector Bool)
boolListAttr = listAttr $ \case
  "true"  -> pure True
  "false" -> pure False
  b       -> throw $ "Can't parse bool:" <+> show b

lenListAttr :: ByteString -> Node -> P (Vector ParameterLength)
lenListAttr = listAttr $ \case
  "null-terminated" -> pure NullTerminated
  l | [param, member] <- tokenise "::" l ->
    NamedMemberLength <$> decode param <*> decode member
  l -> NamedLength <$> decode l

tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y = h
  : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
  where (h, t) = BS.breakSubstring x y


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

expectOne :: Text -> [a] -> P a
expectOne m = \case
  []  -> throw ("No " <> m <> " found")
  [x] -> pure x
  _   -> throw ("More than one " <> m <> " found")
