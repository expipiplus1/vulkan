{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Write.Element
  where

import           Data.List.Extra
import           Data.Semigroup
import           Data.Text
import           Data.Text.Prettyprint.Doc

data WriteElement = WriteElement
  { weName       :: Text
    -- ^ For debug purposes
  , weExtensions :: [Text]
  , weImports    :: [Import]
    -- ^ "system" imports
  , weDoc        :: Doc ()
  , weProvides   :: [Export]
    -- ^ The names this element declares
  , weDepends    :: [HaskellName]
    -- ^ Other Vulkan names to expose
  }
  deriving (Show)

data Export
  = WithConstructors { unExport :: HaskellName }
  | WithoutConstructors { unExport :: HaskellName }
  deriving (Show, Eq, Ord)

data HaskellName
  = TypeName Text
  | TermName Text
  | PatternName Text
  deriving (Show, Eq, Ord)

pattern Pattern n = WithoutConstructors (PatternName n)
pattern Term n = WithoutConstructors (TermName n)
pattern TypeConstructor n = WithConstructors (TypeName n)
pattern TypeAlias n = WithoutConstructors (TypeName n)

data Import = Import
  { iModule  :: Text
  , iImports :: [Text]
  }
  deriving (Show, Eq, Ord)

instance Semigroup WriteElement where
  we1 <> we2 = WriteElement
    { weName       = weName we1 <> " and " <> weName we2
    , weDoc        = vcat [weDoc we1, line, weDoc we2]
    , weExtensions = weExtensions we1 <> weExtensions we2
    , weImports    = weImports we1 <> weImports we2
    , weProvides   = weProvides we1 <> weProvides we2
    , weDepends    = weDepends we1 <> weDepends we2
    }
