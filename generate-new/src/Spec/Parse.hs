{-# LANGUAGE QuantifiedConstraints #-}

module Spec.Parse where

import           Relude                  hiding ( Reader
                                                , runReader
                                                )
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
import           Polysemy.Reader
import           Data.Text.Extra                ( (<+>) )

import           CType
import           Error
import           Marshal.Name
import qualified Marshal.Marshalable           as M
import           Marshal.Marshalable            ( ParameterLength(..) )

data Spec = Spec
  { specStructs :: Vector Struct
  , specCommands :: Vector Command
  }
  deriving (Show)

--
-- Structs
--

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

--
-- Commands
--

data Command = Command
  { cName :: Text
  , cReturnType :: CType
  , cParameters :: Vector Parameter
  -- , cCommandLevel :: HandleLevel
  , cSuccessCodes :: Vector Text
  , cErrorCodes :: Vector Text
  }
  deriving (Show, Eq)

-- | The "level" of a handle, related to what it is descended from.
data HandleLevel
  = Instance
  | PhysicalDevice
  | Device
  | NoHandleLevel
  deriving (Show, Eq)

data Parameter = Parameter
  { pName       :: Text
  , pType       :: CType
  , pLengths    :: Vector M.ParameterLength
  , pIsOptional :: Vector Bool
  }
  deriving (Show, Eq)

instance M.Marshalable Parameter where
  name       = Name . pName
  type'      = pType
  values     = const mempty
  lengths    = pLengths
  isOptional = pIsOptional

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

type P a
  = forall r . (MemberWithError (Reader TypeNames) r, HasErr r) => Sem r a

parseSpec :: HasErr r => ByteString -> Sem r Spec
parseSpec bs = do
  n <- fromEither (first show (parse bs))
  case name n of
    "registry" -> do
      types     <- contents <$> oneChild "types" n
      typeNames <- allTypeNames types
      runReader typeNames $ do
        specStructs  <- parseStructs types
        specCommands <- parseCommands . contents =<< oneChild "commands" n
        pure Spec { .. }
    _ -> throw "This spec isn't a registry node"

parseStructs :: [Content] -> P (Vector Struct)
parseStructs es = fromList <$> traverse
  parseStruct
  [ n
  | Element n <- es
  , name n == "type"
  , Just "struct" <- pure (getAttr "category" n)
  ]

parseStruct :: Node -> P Struct
parseStruct n = do
  structName <- decode =<< note "Unable to get struct name" (getAttr "name" n)
  structMembers <-
    fmap fromList
    . traverseV parseStructMember
    $ [ m | Element m <- contents n, name m == "member" ]
  pure Struct { .. }

parseStructMember :: Node -> P StructMember
parseStructMember m = do
  smName <- decode
    =<< maybe (throw "struct member without name") pure (elemText "name" m)
  let typeString = allText m
  smType       <- parseCType typeString
  smIsOptional <- boolListAttr "optional" m
  smLengths    <- lenListAttr "len" m
  smValues     <- listAttr decode "values" m
  pure StructMember { .. }

parseCommands :: [Content] -> P (Vector Command)
parseCommands es =
  fmap fromList
    . traverseV parseCommand
    $ [ n
      | Element n <- es
      , name n == "command"
      , all ((/= "alias") . fst) (attributes n)
      ]

parseCommand :: Node -> P Command
parseCommand n = do
  cSuccessCodes <- listAttr decode "successcodes" n
  cErrorCodes   <- listAttr decode "errorcodes" n
  proto         <- oneChild "proto" n
  cName         <- decode =<< note "Command has no name" (elemText "name" proto)
  cReturnType   <- parseCType (allText proto)
  cParameters <- fromList <$> traverseV parseParameter (manyChildren "param" n)
  pure Command { .. }

parseParameter :: Node -> P Parameter
parseParameter m = do
  pName <- decode
    =<< maybe (throw "struct member without name") pure (elemText "name" m)
  let typeString = allText m
  pType       <- parseCType typeString
  pIsOptional <- boolListAttr "optional" m
  pLengths    <- lenListAttr "len" m
  pure Parameter { .. }

----------------------------------------------------------------
-- Getting all the type names
----------------------------------------------------------------

allTypeNames :: forall r . HasErr r => [Content] -> Sem r TypeNames
allTypeNames es = do
  let nameText :: Node -> Sem r ByteString
      nameText n =
        note "Unable to get type name" (getAttr "name" n <|> elemText "name" n)
  categoryTypeNames <- traverseV
    nameText
    [ n
    | Element n <- es
    , name n == "type"
    , Just c <- pure (getAttr "category" n)
    , c `notElem` ["include", "define"]
    ]
  requiresTypeNames <- traverseV
    nameText
    [ n | Element n <- es, name n == "type", hasAttr "requires" n ]
  fromList <$> traverseV
    ( fromEither
    . first fromList
    . cIdentifierFromString
    . dropWhileEnd isSpace
    . dropWhile isSpace
    . BS.unpack
    )
    ( filter (`notElem` reservedTypeNames)
    $ extraTypeNames <> toList (categoryTypeNames <> requiresTypeNames)
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
  Just bs -> traverse p . fromList . BS.split ',' $ bs

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

manyChildren :: ByteString -> Node -> [Node]
manyChildren childName n = [ e | Element e <- contents n, name e == childName ]

oneChild :: HasErr r => ByteString -> Node -> Sem r Node
oneChild childName = expectOne (decodeUtf8 childName) . manyChildren childName

expectOne :: HasErr r => Text -> [a] -> Sem r a
expectOne m = \case
  []  -> throw ("No " <> m <> " found")
  [x] -> pure x
  _   -> throw ("More than one " <> m <> " found")
