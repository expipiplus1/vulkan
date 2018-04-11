module Write.Element
  where

import           Data.Text
import           Data.Text.Prettyprint.Doc

data WriteElement = WriteElement
  { weExtensions :: [Text]
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
  deriving (Show)

data Import = Import
  { iModule  :: Text
  , iImports :: [Text]
  }
  deriving (Show)
