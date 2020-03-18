module Spec.Parse
  ( module Spec.Types
  , parseSpec
  )
where

import           Relude                  hiding ( Reader
                                                , runReader
                                                , Handle
                                                , State
                                                , get
                                                , put
                                                , runState
                                                , evalState
                                                , modify'
                                                )
import           Xeno.DOM
import           Data.Vector                    ( Vector )
import           Data.List.Extra                ( nubOrd )
import           Text.ParserCombinators.ReadP
                                         hiding ( get )
import           Data.Version
import           Language.C.Types               ( cIdentifierFromString
                                                , TypeNames
                                                )
import qualified Data.ByteString.Char8         as BS
import qualified Data.Map                      as Map
import           Polysemy.State
import           Data.List                      ( dropWhileEnd
                                                , lookup
                                                )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Char
import           Polysemy
import           Polysemy.Reader
import           Data.Text.Extra                ( (<+>) )
import           Data.Bits

import           Spec.APIConstant
import           CType
import           CType.Size
import           Error
import           Bespoke
import           Marshal.Marshalable            ( ParameterLength(..) )
import           Spec.Types

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

type P a
  = forall r . (MemberWithError (Reader TypeNames) r, HasErr r) => Sem r a

parseSpec
  :: HasErr r
  => ByteString
  -- ^ The spec xml
  -> Sem r (Spec, CType -> Maybe (Int, Int))
  -- ^ Return the map from CType to size and alignment because it's useful later
parseSpec bs = do
  n <- fromEither (first show (parse bs))
  case name n of
    "registry" -> do
      types     <- contents <$> oneChild "types" n
      typeNames <- allTypeNames types
      runReader typeNames $ do
        specHandles      <- parseHandles types
        specFuncPointers <- parseFuncPointers types
        unsizedStructs   <- parseStructs types
        unsizedUnions    <- parseUnions types
        specCommands     <- parseCommands . contents =<< oneChild "commands" n
        emptyBitmasks    <- parseEmptyBitmasks types
        nonEmptyEnums    <- parseEnums . contents $ n
        requires         <- allRequires . contents $ n
        enumExtensions   <- parseEnumExtensions requires
        constantAliases  <- parseConstantAliases (contents n)
        bitmaskAliases   <- parseBitmaskAliases types
        typeAliases      <- parseTypeAliases
          ["handle", "enum", "bitmask", "struct"]
          types
        enumAliases    <- parseEnumAliases requires
        commandAliases <-
          parseCommandAliases . contents =<< oneChild "commands" n
        let specEnums = appendEnumExtensions
              enumExtensions
              (extraEnums <> emptyBitmasks <> nonEmptyEnums)
            specAliases =
              bitmaskAliases
                <> typeAliases
                <> enumAliases
                <> commandAliases
                <> constantAliases
        specFeatures           <- parseFeatures (contents n)
        specExtensions <- parseExtensions . contents =<< oneChild "extensions" n
        specAPIConstants       <- parseAPIConstants (contents n)
        specExtensionConstants <- parseExtensionConstants (contents n)
        let
          sizeMap :: Map.Map CName (Int, Int)
          sizeMap =
            Map.fromList
              $  bespokeSizes
              <> [ (eName, (4, 4)) | Enum {..} <- V.toList specEnums ]
              <> [ (aName, (4, 4)) | Alias {..} <- V.toList bitmaskAliases ]
              <> [ (hName, (8, 8)) | Handle {..} <- V.toList specHandles ]
              <> [ (fpName, (8, 8))
                 | FuncPointer {..} <- V.toList specFuncPointers
                 ]
          constantMap :: Map.Map CName Int
          constantMap = Map.fromList
            [ (n, fromIntegral v)
            | Constant n (IntegralValue v) <- V.toList
              (specAPIConstants <> specExtensionConstants)
            ]
          constantValue v = Map.lookup v constantMap
        (specUnions, specStructs, getSize) <- sizeAll sizeMap
                                                      constantValue
                                                      unsizedUnions
                                                      unsizedStructs
        pure (Spec { .. }, getSize)
    _ -> throw "This spec isn't a registry node"

----------------------------------------------------------------
-- Sizes, alignments and offsets
----------------------------------------------------------------

sizeAll
  :: forall r
   . HasErr r
  => Map.Map CName (Int, Int)
  -> (CName -> Maybe Int)
  -> Vector (StructOrUnion AUnion WithoutSize)
  -> Vector (StructOrUnion AStruct WithoutSize)
  -> Sem
       r
       (Vector Union, Vector Struct, CType -> Maybe (Int, Int))
sizeAll typeSizes constantMap unions structs = do
  let
    both    = (Left <$> unions) <> (Right <$> structs)
    initial = typeSizes
    try
      :: Either
           (StructOrUnion AUnion WithoutSize)
           (StructOrUnion AStruct WithoutSize)
      -> Sem
           (State (Map.Map CName (Int, Int)) ': r)
           (Maybe (Either Union Struct))
    getSize m =
      let getLocalSize = \case
            TypeName n -> Map.lookup n m
            _          -> Nothing
      in  cTypeSize constantMap getLocalSize
    try s = do
      m <- get
      let g = getSize m
      case s of
        Left u -> for (sizeUnion g u) $ \u' -> do
          modify' (Map.insert (sName u') (sSize u', sAlignment u'))
          pure (Left u')
        Right s -> for (sizeStruct g s) $ \s' -> do
          modify' (Map.insert (sName s') (sSize s', sAlignment s'))
          pure (Right s')
  (m, r) <- runState initial $ tryTwice both try

  let (failed, succeeded) = partitionEithers . V.toList $ r
  forV_ failed $ \s -> throw
    ("Unable to calculate size for " <> (unCName . either sName sName) s)
  let (us, ss) = bimap V.fromList V.fromList $ partitionEithers succeeded
  pure (us, ss, getSize m)

sizeStruct
  :: Monad m
  => (CType -> m (Int, Int))
  -- ^ Size and alignment
  -> StructOrUnion AStruct WithoutSize
  -> m Struct
sizeStruct = sizeGeneric roundToAlignment (\_ m o -> m + o)

sizeUnion
  :: Monad m
  => (CType -> m (Int, Int))
  -- ^ Size and alignment
  -> StructOrUnion AUnion WithoutSize
  -> m Union
sizeUnion = sizeGeneric (\_ _ -> 0) (\c m _ -> max c m)

sizeGeneric
  :: Monad m
  => (Int -> Int -> Int)
  -- ^ Member offset given member alignment and current size
  -> (Int -> Int -> Int -> Int)
  -- ^ Next size given current size, member size and offset
  -> (CType -> m (Int, Int))
  -- ^ Size and alignment
  -> StructOrUnion t WithoutSize
  -> m (StructOrUnion t 'WithSize)
sizeGeneric getOffset getSize getTypeSize Struct {..} = do
  ((newSize, newAlign), memberOffsets) <- scanOffsets getOffset
                                                      getSize
                                                      (getTypeSize . smType)
                                                      sMembers
  pure Struct
    { sSize      = roundToAlignment newAlign newSize
    , sAlignment = newAlign
    , sMembers   = V.zipWith (\o m -> m { smOffset = o }) memberOffsets sMembers
    , ..
    }


----------------------------------------------------------------
-- Features and extensions
----------------------------------------------------------------

parseFeatures :: [Content] -> P (Vector Feature)
parseFeatures es = V.fromList
  <$> sequenceV [ parseFeature e | Element e <- es, "feature" == name e ]
 where
  parseFeature :: Node -> P Feature
  parseFeature n = do
    fName <- nameAttr "feature" n
    context (unCName fName) $ do
      fVersion <- runReadP parseVersion
        =<< note "feature has no version" (getAttr "number" n)
      fRequires <- parseRequires n
      pure Feature { .. }

parseExtensions :: [Content] -> P (Vector Extension)
parseExtensions es = V.fromList <$> sequenceV
  [ parseExtension e | Element e <- es, "extension" == name e, notDisabled e ]
 where
  parseExtension :: Node -> P Extension
  parseExtension n = do
    exName      <- decode =<< note "extension has no name" (getAttr "name" n)
    exNumber    <- readAttr "number" n
    exRequires  <- parseRequires n
    exSupported <- decode
      =<< note "extension has no supported attr" (getAttr "supported" n)
    pure Extension { .. }

parseRequires :: Node -> P (Vector Require)
parseRequires n = V.fromList <$> traverseV
  parseRequire
  [ r | Element r <- contents n, "require" == name r ]
 where
  parseRequire :: Node -> P Require
  parseRequire r = do
    rComment   <- traverse decode (getAttr "comment" r)
    rTypeNames <- V.fromList <$> sequenceV
      [ nameAttr "require type" t | Element t <- contents r, "type" == name t ]
    rCommandNames <- V.fromList <$> sequenceV
      [ nameAttr "require commands" t
      | Element t <- contents r
      , "command" == name t
      ]
    rEnumValueNames <- V.fromList <$> sequenceV
      [ nameAttr "require enum" t | Element t <- contents r, "enum" == name t ]
    pure Require { .. }

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

parseAPIConstants :: [Content] -> P (Vector Constant)
parseAPIConstants es = V.fromList <$> sequenceV
  [ do
      n' <- decode n
      context n' (Constant (CName n') <$> parseConstant v)
  | Element e <- es
  , "enums" == name e
  , Just "API Constants" <- pure $ getAttr "name" e
  , (n, v)               <- someConstants e
  ]

parseExtensionConstants :: [Content] -> P (Vector Constant)
parseExtensionConstants es = V.fromList <$> sequenceV
  [ do
      n' <- decode n
      context n' $ (Constant (CName n') <$> parseConstant v)
  | Element e <- es
  , "extensions" == name e
  , Element ex <- contents e
  , "extension" == name ex
  , notDisabled ex
  , Element r <- contents ex
  , "require" == name r
  , (n, v) <- someConstants r
  ]

someConstants :: Node -> [(ByteString, ByteString)]
someConstants r =
  [ (n, v)
  | Element ee <- contents r
  , "enum" == name ee
  , isNothing (getAttr "extends" ee)
  , Just n <- pure (getAttr "name" ee)
  , Just v <- pure (getAttr "value" ee)
  ]

---------------------------------------------------------------
-- Aliases
----------------------------------------------------------------

parseTypeAliases :: [ByteString] -> [Content] -> P (Vector Alias)
parseTypeAliases categories es =
  fmap fromList
    .  sequenceV
    $ [ do
           aName   <- nameAttr "struct alias" n
           aTarget <- decodeName alias
           let aType = TypeAlias
           pure Alias { .. }
       | Element n <- es
       , "type" == name n
       , Just alias     <- pure $ getAttr "alias" n
       , Just c <- pure $ getAttr "category" n
       , c `elem` categories
       ]

parseBitmaskAliases :: [Content] -> P (Vector Alias)
parseBitmaskAliases es =
  fmap fromList
    . sequenceV
    $ [ do
          aName   <- nameElem "bitmask alias" n
          aTarget <- decodeName requires
          let aType = TypeAlias
          pure Alias { .. }
      | Element n <- es
      , "type" == name n
      , Just requires  <- pure $ getAttr "requires" n
      , Just "bitmask" <- pure $ getAttr "category" n
      ]

parseEnumAliases :: Vector (Node, Maybe Int) -> P (Vector Alias)
parseEnumAliases rs =
  fmap V.fromList
    . sequenceV
    $ [ do
          aName   <- nameAttr "enum alias" ee
          aTarget <- decodeName alias
          let aType = PatternAlias
          pure Alias { .. }
      | (r, _)     <- toList rs
      , Element ee <- contents r
      , "enum" == name ee
      , Just alias <- pure $ getAttr "alias" ee
      ]

parseCommandAliases
  :: [Content] -> P (Vector Alias)
parseCommandAliases es =
  fmap V.fromList
    . sequenceV
    $ [ do
          aName   <- nameAttr "alias" ee
          aTarget <- decodeName alias
          let aType = TermAlias
          pure Alias { .. }
      | Element ee <- es
      , "command" == name ee
      , Just alias <- pure $ getAttr "alias" ee
      ]

parseConstantAliases :: [Content] -> P (Vector Alias)
parseConstantAliases es =
  fmap V.fromList
    . sequenceV
    $ [ do
          aName   <- nameAttr "enum alias" ee
          aTarget <- decodeName alias
          pure Alias { .. }
      | Element e <- es
      , "enums" == name e
      , Just    "API Constants" <- pure $ getAttr "name" e
      , Element ee              <- contents e
      , "enum" == name ee
      , Just alias <- pure $ getAttr "alias" ee
      , aType <- [TypeAlias, PatternAlias]
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
    context (unCName hName) $ do
      typeString    <- note "Handle has no type" (elemText "type" n)
      hDispatchable <- case typeString of
        "VK_DEFINE_HANDLE" -> pure Dispatchable
        "VK_DEFINE_NON_DISPATCHABLE_HANDLE" -> pure NonDispatchable
        _                  -> throw "Unable to parse handle type"
      hLevel <- case hName of
        "VkInstance" -> pure Instance
        "VkDevice"   -> pure Device
        _            -> case getAttr "parent" n of
          Nothing -> pure NoHandleLevel
          Just "VkInstance" -> pure Instance
          Just "VkPhysicalDevice" -> pure Instance
          Just "VkDevice" -> pure Device
          Just "VkCommandPool" -> pure Device
          Just "VkDescriptorPool" -> pure Device
          Just "VkPhysicalDevice,VkDisplayKHR" -> pure Instance
          Just "VkSurfaceKHR" -> pure Instance
          _       -> throw "Unknown handle level"
      pure Handle { .. }

parseFuncPointers :: [Content] -> P (Vector FuncPointer)
parseFuncPointers = onTypes "funcpointer" parseFuncPointer
 where
  parseFuncPointer :: Node -> P FuncPointer
  parseFuncPointer n = do
    fpName <- nameElem "funcpointer" n
    context (unCName fpName) $ do
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
  (uncurry (parseEnum False))
  [ (bool AnEnum ABitmask isBitmask, n)
  | Element n <- es
  , name n == "enums"
  , Just t <- pure (getAttr "type" n)
  , let isBitmask = t == "bitmask"
        isEnum    = t == "enum"
  , isBitmask || isEnum
  ]

 where
  parseEnum :: Bool -> EnumType -> Node -> P Enum'
  parseEnum evIsExtension eType n = do
    eName   <- nameAttr "enum" n
    eValues <- fromList <$> traverseV
      (context (unCName eName) . parseValue)
      [ e | Element e <- contents n, name e == "enum", not (isAlias e) ]
    pure Enum { .. }
   where
    parseValue :: Node -> P EnumValue
    parseValue v = do
      evName  <- nameAttr "enum value" v
      evValue <- case getAttr "value" v of
        Just b  -> readP b
        Nothing -> (0x1 `shiftL`) <$> readAttr "bitpos" v
      pure EnumValue { .. }

parseEnumExtensions :: Vector (Node, Maybe Int) -> P (Vector (CName, EnumValue))
parseEnumExtensions rs =
  fmap V.fromList
    . sequenceV
    $ [ do
          v        <- enumValue number ee
          extends' <- decodeName extends
          pure (extends', v)
      | (r, number) <- toList rs
      , Element ee  <- contents r
      , "enum" == name ee
      , not (isAlias ee)
      , Just extends <- pure (getAttr "extends" ee)
      ]
 where

  enumValue :: Maybe Int -> Node -> P EnumValue
  enumValue inheritedExNum ee = do
    evName <- nameAttr "enum extension" ee
    context (unCName evName) $ do
      evValue <- case getAttr "value" ee of
        Just bs -> readP bs
        Nothing -> case getAttr "bitpos" ee of
          Just bs -> do
            v <- readP bs
            pure (0x1 `shiftL` v)
          Nothing -> do
            offset <- readAttr "offset" ee
            sign   <- case getAttr "dir" ee of
              Nothing  -> pure id
              Just "-" -> pure negate
              _        -> throw "Unhandled enum extension direction"
            extNum <- case getAttr "extnumber" ee of
              Just n  -> readP n
              Nothing -> case inheritedExNum of
                Just n  -> pure n
                Nothing -> throw "couldn't find extension number"
            pure $ enumExtensionValue extNum offset sign
      let evIsExtension = True
      pure EnumValue { .. }

  enumExtensionValue
    :: Int
    -- ^ The extension number
    -> Int
    -- ^ Offset
    -> (Int -> Int)
    -- ^ The offset direction
    -> Int64
    -- ^ The enum value
  enumExtensionValue extNumber offset sign =
    let
      extBlockSize = 1000
      extBase      = 1000000000
      n =
        extBase
          + pred (fromIntegral extNumber)
          * extBlockSize
          + fromIntegral offset
    in
      fromIntegral (sign n)

appendEnumExtensions :: Vector (CName, EnumValue) -> Vector Enum' -> Vector Enum'
appendEnumExtensions extensions =
  let
    extensionMap :: Map.Map CName (Vector EnumValue)
    extensionMap =
      Map.fromListWith (<>) (toList (fmap V.singleton <$> extensions))
    getExtensions n = Map.findWithDefault V.empty n extensionMap
    vNubOrd = V.fromList . nubOrd . V.toList
  in
    fmap
      (\e@Enum {..} -> e { eValues = vNubOrd $ eValues <> getExtensions eName })

----------------------------------------------------------------
-- Structs
----------------------------------------------------------------

parseStructs :: [Content] -> P (Vector  (StructOrUnion AStruct WithoutSize))
parseStructs = onTypes "struct" parseStruct

parseUnions :: [Content] -> P (Vector (StructOrUnion AUnion WithoutSize))
parseUnions = onTypes "union" parseStruct

parseStruct :: Node -> P (StructOrUnion a WithoutSize)
parseStruct n = do
  sName <- nameAttr "struct" n
  context (unCName sName) $ do
    sMembers <-
      fmap fromList
      . traverseV parseStructMember
      $ [ m | Element m <- contents n, name m == "member" ]
    let sSize      = ()
        sAlignment = ()
    pure Struct { .. }
 where

  parseStructMember :: Node -> P (StructMember' WithoutSize)
  parseStructMember m = do
    smName <- nameElem "struct member" m
    let typeString = allNonCommentText m
    smType       <- parseCType typeString
    smIsOptional <- boolListAttr "optional" m
    smLengths    <- lenListAttr "len" m
    smValues     <- listAttr decode "values" m
    let smOffset = ()
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
    . cIdentifierFromString False
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
-- Helpers
----------------------------------------------------------------

allRequires :: [Content] -> P (Vector (Node, Maybe Int))
allRequires es =
  fmap V.fromList
    .  sequenceV
    $  [ pure (r, Nothing)
       | Element f <- es
       , "feature" == name f
       , Element r <- contents f
       , "require" == name r
       ]
    <> [ do
           n <- readAttr "number" ex
           pure (r, Just n)
       | Element e <- es
       , "extensions" == name e
       , Element ex <- contents e
       , "extension" == name ex
       , Element r <- contents ex
       , "require" == name r
       ]

----------------------------------------------------------------
-- Vulkan
----------------------------------------------------------------

extraEnums :: Vector Enum'
extraEnums = V.singleton Enum
  { eName   = "VkBool32"
  , eValues = V.fromList
                [EnumValue "VK_FALSE" 0 False, EnumValue "VK_TRUE" 1 False]
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

notDisabled :: Node -> Bool
notDisabled e =
  Just "disabled" /= getAttr "supported" e

----------------------------------------------------------------
-- XML
----------------------------------------------------------------

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

nameElem :: Text -> Node -> P CName
nameElem debug n =
  fmap CName $ decode =<< note (debug <> " has no name") (elemText "name" n)

nameAttr :: Text -> Node -> P CName
nameAttr debug n =
  fmap CName $ decode =<< note (debug <> " has no name") (getAttr "name" n)

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
  "null-terminated"                      -> pure NullTerminated
  l | [param, member] <- tokenise "::" l -> do
    s <- decode param
    m <- decode member
    pure $ NamedMemberLength (CName s) (CName m)
  l -> NamedLength <$> decodeName l

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

runReadP :: ReadP a -> ByteString -> P a
runReadP p s = case filter (null . snd) (readP_to_S p (BS.unpack s)) of
  []       -> throw "no parse"
  [(x, _)] -> pure x
  _        -> throw "ambiguous parse"

----------------------------------------------------------------
--
----------------------------------------------------------------

tryTwice
  :: forall f a m b
   . (Traversable f, Monad m)
  => f a
  -> (a -> m (Maybe b))
  -> m (f (Either a b))
tryTwice xs f =
  let xs' = Left <$> xs
      go :: f (Either a b) -> m (f (Either a b))
      go = traverse
        (\case
          Left x -> f x <&> \case
            Nothing -> Left x
            Just r  -> Right r
          Right r -> pure $ Right r
        )
  in  go =<< go xs'

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = flip traverse
