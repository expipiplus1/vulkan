{-# LANGUAGE OverloadedStrings #-}

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
  , weProvides   :: [HaskellName]
    -- ^ The names this element declares
  , weDepends    :: [HaskellName]
    -- ^ Other Vulkan names to expose
  }
  deriving (Show)

data HaskellName
  = Type Text
  | Term Text
  | Pattern Text
  deriving (Show, Eq, Ord)

data Import = Import
  { iModule  :: Text
  , iImports :: [Text]
  }
  deriving (Show, Eq, Ord)

instance Semigroup WriteElement where
  we1 <> we2 = WriteElement
    { weName       = weName we1 <> " and " <> weName we2
    , weDoc        = vcat [weDoc we1, weDoc we2]
    , weExtensions = nubOrd $ weExtensions we1 <> weExtensions we2
    , weImports    = nubOrd $ weImports we1 <> weImports we2
    , weProvides   = nubOrd $ weProvides we1 <> weProvides we2
    , weDepends    = nubOrd $ weDepends we1 <> weDepends we2
    }
