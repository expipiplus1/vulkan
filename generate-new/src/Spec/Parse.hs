{-# LANGUAGE QuantifiedConstraints #-}

module Spec.Parse where

import           Relude                  hiding ( Reader
                                                , runReader
                                                , Handle
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
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Char
import           Polysemy
import           Polysemy.Reader
import           Data.Text.Extra                ( (<+>) )

import           CType
import           Error
import           Marshal.Name
import qualified Marshal.Marshalable           as M
import           Marshal.Marshalable            ( ParameterLength(..) )
import           Data.Bits

data Spec = Spec
  { specHandles      :: Vector Handle
  , specFuncPointers :: Vector FuncPointer
  , specStructs      :: Vector Struct
  , specUnions       :: Vector Union
  , specCommands     :: Vector Command
  , specEnums        :: Vector Enum'
  , specAliases      :: Vector Alias
  }
  deriving (Show)

--
-- Aliases
--

data Alias = Alias
  { aName :: Text
  , aTarget :: Text
  , aType :: AliasType
  }
  deriving (Show)

data AliasType
  = TypeAlias
  | TermAlias
  | PatternAlias
  deriving (Show)

--
-- Function Pointers
--

data FuncPointer = FuncPointer
  { fpName :: Text
  , fpType :: CType
  }
  deriving (Show)

--
-- Handles
--

data Handle = Handle
  { hName :: Text
  , hDispatchable :: Dispatchable
  , hLevel :: HandleLevel
  }
  deriving (Show)

-- | The "level" of a handle, related to what it is descended from.
data HandleLevel
  = Instance
  | PhysicalDevice
  | Device
  | NoHandleLevel
  deriving (Show, Eq)

data Dispatchable = Dispatchable | NonDispatchable
  deriving (Show)

--
-- Structs
--


type Struct = StructOrUnion AStruct
type Union = StructOrUnion AUnion
data StructOrUnionType = AStruct | AUnion

data StructOrUnion (t :: StructOrUnionType) = Struct
  { sName :: Text
  , sMembers :: Vector StructMember
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

--
-- Enums
--

data Enum' = Enum
  { eName :: Text
  , eValues :: Vector EnumValue
  , eType :: EnumType
  }
  deriving (Show, Eq)

data EnumValue = EnumValue
  { evName  :: Text
  , evValue :: Int64
  }
  deriving (Show, Eq)

data EnumType = AnEnum | ABitmask
  deriving (Show, Eq)

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
        specHandles      <- parseHandles types
        specFuncPointers <- parseFuncPointers types
        specStructs      <- parseStructs types
        specUnions       <- parseUnions types
        specCommands     <- parseCommands . contents =<< oneChild "commands" n
        emptyBitmasks    <- parseEmptyBitmasks types
        nonEmptyEnums    <- parseEnums . contents $ n
        bitmaskAliases   <- parseBitmaskAliases types
        typeAliases      <- parseAliases ["bitmask", "struct"] types
        let specEnums   = extraEnums <> emptyBitmasks <> nonEmptyEnums
            specAliases = bitmaskAliases <> typeAliases
        pure Spec { .. }
    _ -> throw "This spec isn't a registry node"

----------------------------------------------------------------
-- Aliases
----------------------------------------------------------------

parseBitmaskAliases :: [Content] -> P (Vector Alias)
parseBitmaskAliases es =
  fmap fromList
    .  sequenceV
    $  [ do
           aName   <- nameElem "bitmask alias" n
           aTarget <- decode requires
           let aType = TypeAlias
           pure Alias { .. }
       | Element n <- es
       , "type" == name n
       , Just requires  <- pure $ getAttr "requires" n
       , Just "bitmask" <- pure $ getAttr "category" n
       ]

parseAliases :: [ByteString] -> [Content] -> P (Vector Alias)
parseAliases categories es =
  fmap fromList
    .  sequenceV
    $ [ do
           aName   <- nameAttr "struct alias" n
           aTarget <- decode alias
           let aType = TypeAlias
           pure Alias { .. }
       | Element n <- es
       , "type" == name n
       , Just alias     <- pure $ getAttr "alias" n
       , Just c <- pure $ getAttr "category" n
       , c `elem` categories
       ]

----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

parseHandles :: [Content] -> P (Vector Handle)
parseHandles = onTypes "handle" parseHandle
 where

  parseHandle :: Node -> P Handle
  parseHandle n = do
    hName <- nameElem "handle" n
    context hName $ do
      typeString    <- note "Handle has no type" (elemText "type" n)
      hDispatchable <- case typeString of
        "VK_DEFINE_HANDLE" -> pure Dispatchable
        "VK_DEFINE_NON_DISPATCHABLE_HANDLE" -> pure NonDispatchable
        _                  -> throw "Unable to parse handle type"
      hLevel <- case getAttr "parent" n of
        Nothing -> pure NoHandleLevel
        Just "VkInstance" -> pure Instance
        Just "VkPhysicalDevice" -> pure PhysicalDevice
        Just "VkDevice" -> pure Device
        Just "VkCommandPool" -> pure Device
        Just "VkDescriptorPool" -> pure Device
        Just "VkPhysicalDevice,VkDisplayKHR" -> pure PhysicalDevice
        Just "VkSurfaceKHR" -> pure Instance
        _       -> throw "Unknown handle level"
      pure Handle { .. }

parseFuncPointers :: [Content] -> P (Vector FuncPointer)
parseFuncPointers = onTypes "funcpointer" parseFuncPointer
 where
  parseFuncPointer :: Node -> P FuncPointer
  parseFuncPointer n = do
    fpName <- nameElem "funcpointer" n
    context fpName $ do
      let typeString = allTextOn ((`notElem` ["name", "comment"]) . name) n
      fpType <- parseCType typeString
      pure FuncPointer { .. }

----------------------------------------------------------------
-- Enums
----------------------------------------------------------------

parseEmptyBitmasks :: [Content] -> P (Vector Enum')
parseEmptyBitmasks es = fromList <$> traverseV
  parseEmptyBitmask
  [ n
  | Element n <- es
  , "type" == name n
  , not (isAlias n)
  , Nothing        <- pure $ getAttr "requires" n
  , Just "bitmask" <- pure $ getAttr "category" n
  ]
 where
  parseEmptyBitmask :: Node -> P Enum'
  parseEmptyBitmask n = do
    eName <- nameElem "bitmask" n
    pure Enum { eValues = mempty, eType = ABitmask, .. }

parseEnums :: [Content] -> P (Vector Enum')
parseEnums es = fromList <$> traverseV
  (uncurry parseEnum)
  [ (bool AnEnum ABitmask isBitmask, n)
  | Element n <- es
  , name n == "enums"
  , Just t <- pure (getAttr "type" n)
  , let isBitmask = t == "bitmask"
        isEnum    = t == "enum"
  , isBitmask || isEnum
  ]

 where
  parseEnum :: EnumType -> Node -> P Enum'
  parseEnum eType n = do
    eName   <- nameAttr "enum" n
    eValues <- fromList <$> traverseV
      (context eName . parseValue)
      [ e | Element e <- contents n, name e == "enum", not (isAlias e) ]
    pure Enum { .. }

  parseValue :: Node -> P EnumValue
  parseValue v = do
    evName  <- nameAttr "enum value" v
    evValue <- case getAttr "value" v of
      Just b  -> readP b
      Nothing -> (0x1 `shiftL`) <$> readAttr "bitpos" v
    pure EnumValue { .. }

----------------------------------------------------------------
-- Structs
----------------------------------------------------------------

parseStructs :: [Content] -> P (Vector Struct)
parseStructs = onTypes "struct" parseStruct

parseUnions :: [Content] -> P (Vector Union)
parseUnions = onTypes "union" parseStruct

parseStruct :: Node -> P (StructOrUnion a)
parseStruct n = do
  sName <- nameAttr "struct" n
  context sName $ do
    sMembers <-
      fmap fromList
      . traverseV parseStructMember
      $ [ m | Element m <- contents n, name m == "member" ]
    pure Struct { .. }
 where

  parseStructMember :: Node -> P StructMember
  parseStructMember m = do
    smName <- nameElem "struct member" m
    let typeString = allNonCommentText m
    smType       <- parseCType typeString
    smIsOptional <- boolListAttr "optional" m
    smLengths    <- lenListAttr "len" m
    smValues     <- listAttr decode "values" m
    pure StructMember { .. }

----------------------------------------------------------------
-- Commands
----------------------------------------------------------------

parseCommands :: [Content] -> P (Vector Command)
parseCommands es =
  fmap fromList
    . traverseV parseCommand
    $ [ n | Element n <- es, name n == "command", not (isAlias n) ]
 where

  parseCommand :: Node -> P Command
  parseCommand n = do
    cSuccessCodes <- listAttr decode "successcodes" n
    cErrorCodes   <- listAttr decode "errorcodes" n
    proto         <- oneChild "proto" n
    cName         <- nameElem "command" proto
    cReturnType   <- parseCType (allNonCommentText proto)
    cParameters   <- fromList
      <$> traverseV parseParameter (manyChildren "param" n)
    pure Command { .. }

  parseParameter :: Node -> P Parameter
  parseParameter m = do
    pName <- nameElem "parameter" m
    let typeString = allNonCommentText m
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
    (filter (`notElem` reservedTypeNames) $ extraTypeNames <> toList
      (categoryTypeNames <> requiresTypeNames)
    )

reservedTypeNames :: [ByteString]
reservedTypeNames = ["void", "char", "int", "float", "double"]

----------------------------------------------------------------
-- Vulkan
----------------------------------------------------------------

extraEnums :: Vector Enum'
extraEnums = V.singleton Enum
  { eName   = "VkBool32"
  , eValues = V.fromList [EnumValue "VK_FALSE" 0, EnumValue "VK_TRUE" 1]
  , eType   = AnEnum
  }

onTypes
  :: HasErr r
  => ByteString
  -> (Node -> Sem r a)
  -> [Content]
  -> Sem r (Vector a)
onTypes t f es = fromList <$> traverseV
  f
  [ n
  | Element n <- es
  , name n == "type"
  , not (isAlias n)
  , Just t' <- pure (getAttr "category" n)
  , t' == t
  ]


isAlias :: Node -> Bool
isAlias n = any ((== "alias") . fst) (attributes n)

extraTypeNames :: [ByteString]
extraTypeNames = ["ANativeWindow", "AHardwareBuffer", "CAMetalLayer"]

----------------------------------------------------------------
-- XML
----------------------------------------------------------------

decode :: ByteString -> P Text
decode bs = case decodeUtf8' bs of
  Left  e -> throw $ show e
  Right t -> pure t

readP :: Read a => ByteString -> P a
readP b = do
  t <- decode b
  case readMaybe (T.unpack t) of
    Nothing -> throw ("Unable to read: " <> t)
    Just r  -> pure r

readAttr :: Read a => ByteString -> Node -> P a
readAttr a n = case getAttr a n of
  Nothing -> throw ("No such attribute: " <> decodeUtf8 a)
  Just b  -> readP b

hasAttr :: ByteString -> Node -> Bool
hasAttr a n = any ((== a) . fst) (attributes n)

getAttr :: ByteString -> Node -> Maybe ByteString
getAttr a n = lookup a (attributes n)

allText :: Node -> ByteString
allText = allTextOn (const True)

allNonCommentText :: Node -> ByteString
allNonCommentText = allTextOn ((/= "comment") . name)

allTextOn :: (Node -> Bool) -> Node -> ByteString
allTextOn p =
  BS.unwords
    . fmap
        (\case
          Text    t -> t
          Element n -> if p n then allText n else mempty
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

nameElem :: Text -> Node -> P Text
nameElem debug n =
  decode =<< note (debug <> " has no name") (elemText "name" n)

nameAttr :: Text -> Node -> P Text
nameAttr debug n =
  decode =<< note (debug <> " has no name") (getAttr "name" n)

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

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&> -- same as (&&)
