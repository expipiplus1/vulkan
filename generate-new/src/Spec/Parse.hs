module Spec.Parse
  where

import           Relude
import           Xeno.DOM
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Relude.Extra.Validation
import           Language.C.Types               ( cIdentifierFromString
                                                , TypeNames
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.List                      ( dropWhileEnd
                                                , lookup
                                                )
import           Data.Char

import           CType

type P = Either Text

parseSpec :: ByteString -> P Spec
parseSpec bs = do
  n <- first show (parse bs)
  case name n of
    "registry" -> do
      [ts] <- pure [ contents e | Element e <- contents n, name e == "types" ]
      parseTypes ts
    _ -> Left "This spec isn't a registry node"


parseTypes :: [Content] -> P Spec
parseTypes es = do
  typeNames <- allTypeNames es
  specStructs <- several
    [ parseStruct typeNames n
    | Element n <- es
    , name n == "type"
    , Just "struct" <- pure (lookup "category" (attributes n))
    ]
  pure Spec { .. }

parseStruct :: TypeNames ->  Node -> P Struct
parseStruct typeNames n = do
  let attrs = attributes n
  structName    <- note "Unable to get struct name" (lookup "name" attrs)
  structMembers <- several
    [ parseStructMember typeNames m | Element m <- contents n, name m == "member" ]
  pure Struct { .. }

parseStructMember :: TypeNames -> Node -> P StructMember
parseStructMember typeNames m = do
  smName <- expectOne
    "struct member name"
    [ n
    | Element a <- contents m
    , name a == "name"
    , [Text n] <- pure (contents a)
    ]
  let typeString = allText m
  smType <- parseCType typeNames typeString
  pure StructMember { .. }

data Spec = Spec
  { specStructs :: Vector Struct
  }
  deriving (Show)

data Struct = Struct
  { structName :: ByteString
  , structMembers :: Vector StructMember
  }
  deriving (Show)

data StructMember = StructMember
  { smName :: ByteString
  , smType :: CType
  }
  deriving (Show)

----------------------------------------------------------------
-- Getting all the type names
----------------------------------------------------------------

allTypeNames :: [Content] -> P TypeNames
allTypeNames es = do
  let nameText n = note
        "Unable to get type name"
        (lookup "name" (attributes n) <|> hush
          (expectOne
            "struct member name"
            [ m
            | Element a <- contents n
            , name a == "name"
            , [Text m] <- pure (contents a)
            ]
          )
        )
  categoryTypeNames <- several
    [ nameText n
    | Element n <- es
    , name n == "type"
    , Just c <- pure (lookup "category" (attributes n))
    , c `notElem` ["include", "define"]
    ]
  requiresTypeNames <- several
    [ nameText n
    | Element n <- es
    , name n == "type"
    , Just _ <- pure (lookup "requires" (attributes n))
    ]
  fromList . toList <$> several
    ( fmap
        ( first fromList
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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

several :: [P a] -> P (Vector a)
several = fmap V.fromList . validationToEither . traverse eitherToValidation

note = maybeToRight
hush = rightToMaybe

expectOne :: Text -> [a] -> P a
expectOne m = \case
  [] -> Left ("No " <> m <> " found")
  [x] -> pure x
  _ -> Left ("More than one " <> m <> " found")
