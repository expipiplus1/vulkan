{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Write.Element
  ( WriteElement(..)
  , Export(..)
  , HaskellName(..)
  , Import(..)
  , isQualifiedImport
  , Guarded(..)
  , Guard(..)
  , unGuarded
  , guardWriteElement
  , guardCPPGuard
  , pattern Pattern
  , pattern Term
  , pattern TypeConstructor
  , pattern TypeAlias
  , DocMap
  ) where

import           Data.Text
import           Data.Text.Prettyprint.Doc

import           Documentation
import           Documentation.Haddock
import Write.Util(guarded)

data WriteElement = WriteElement
  { weName                 :: Text
    -- ^ For debug purposes
  , weExtensions           :: [Text]
  , weImports              :: [Guarded Import]
    -- ^ "system" imports
  , weDoc                  :: DocMap -> Doc ()
  , weProvides             :: [Guarded Export]
    -- ^ The names this element declares
  , weUndependableProvides :: [Guarded Export]
    -- ^ Names this write element exposes which are ignored when calculating
    -- module dependencies
  , weSourceDepends        :: [Guarded HaskellName]
    -- ^ Things to import with a SOURCE pragma
  , weDepends              :: [Guarded HaskellName]
    -- ^ Other internal library names required by this element
  , weBootElement          :: Maybe WriteElement
    -- ^ An element used to write a hs-boot file
  }

data Export
  = WithConstructors { unExport :: HaskellName }
  | WithoutConstructors { unExport :: HaskellName }
  deriving (Show, Eq, Ord)

data HaskellName
  = TypeName { unHaskellName :: Text }
  | TermName { unHaskellName :: Text }
  | PatternName { unHaskellName :: Text }
  deriving (Show, Eq, Ord)

-- TODO: Remove invguarded
data Guarded a
  = Guarded Guard a
  | Unguarded a
  deriving (Show, Eq, Ord, Functor)

data Guard
  = Guard Text
  | InvGuard Text
  deriving (Show, Eq, Ord)

unGuarded :: Guarded a -> a
unGuarded = \case
  Guarded _ n -> n
  Unguarded  n -> n

-- | Returns nothing if the write element has already got any guarded things
guardWriteElement :: Guard -> WriteElement -> Maybe WriteElement
guardWriteElement g we = do
  let addGuard :: Guarded a -> Maybe (Guarded a)
      addGuard = \case
        Unguarded x -> pure $ Guarded g x
        _           -> Nothing
  imports              <- traverse addGuard (weImports we)
  provides             <- traverse addGuard (weProvides we)
  undependableProvides <- traverse addGuard (weUndependableProvides we)
  sourceDepends        <- traverse addGuard (weSourceDepends we)
  depends              <- traverse addGuard (weDepends we)
  let doc = guarded (Just (guardCPPGuard g)) <$> (weDoc we)
  pure we { weImports              = imports
          , weProvides             = provides
          , weUndependableProvides = undependableProvides
          , weSourceDepends        = sourceDepends
          , weDepends              = depends
          , weDoc                  = doc
          }

-- Get the CPP guard for a Guarded value
guardCPPGuard :: Guard -> Text
guardCPPGuard = \case
  Guard    g -> "defined(" <> g <> ")"
  InvGuard g -> "!defined(" <> g <> ")"

pattern Pattern :: Text -> Export
pattern Pattern n = WithoutConstructors (PatternName n)

pattern Term :: Text -> Export
pattern Term n = WithoutConstructors (TermName n)

pattern TypeConstructor :: Text -> Export
pattern TypeConstructor n = WithConstructors (TypeName n)

pattern TypeAlias :: Text -> Export
pattern TypeAlias n = WithoutConstructors (TypeName n)

data Import
  = Import
    { iModule  :: Text
    , iImports :: [Text]
    }
  | QualifiedImport
    { iModule  :: Text
    , iImports :: [Text]
    }
  deriving (Show, Eq, Ord)

isQualifiedImport :: Import -> Bool
isQualifiedImport = \case
  Import{}          -> False
  QualifiedImport{} -> True

type DocMap = Documentee -> Maybe Haddock

instance Semigroup WriteElement where
  we1 <> we2 = WriteElement
    { weName                 = weName we1 <> " and " <> weName we2
    , weDoc                  = \d -> vcat [weDoc we1 d, line, weDoc we2 d]
    , weExtensions           = weExtensions we1 <> weExtensions we2
    , weImports              = weImports we1 <> weImports we2
    , weProvides             = weProvides we1 <> weProvides we2
    , weDepends              = weDepends we1 <> weDepends we2
    , weUndependableProvides = weUndependableProvides we1
      <> weUndependableProvides we2
    , weSourceDepends        = weSourceDepends we1 <> weSourceDepends we2
    , weBootElement          = weBootElement we1 <> weBootElement we2
    }
