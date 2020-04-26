module Render.ImportLocation
  ( ImportLocation(..)
  , makeImportLocation
  ) where

import           Relude
import qualified Data.Map                      as Map
import qualified Data.Vector.Extra             as V
import           Data.Text                     as T
import           Language.Haskell.TH            ( nameModule )

import           Render.Element
import           Write.Segment
import           Haskell.Name

data ImportLocation = ImportLocation
  { findLocalModule :: HName -> Maybe ModName
  , findModule      :: Name -> Maybe ModName
  }

instance Semigroup ImportLocation where
  i1 <> i2 = ImportLocation (first findLocalModule) (first findModule)
   where
    first :: (ImportLocation -> (a -> Maybe b)) -> a -> Maybe b
    first f = liftA2 (<|>) (f i1) (f i2)

instance Monoid ImportLocation where
  mempty = ImportLocation (const Nothing) (const Nothing)

makeImportLocation
  :: Foldable f => f (Segment ModName RenderElement) -> ImportLocation
makeImportLocation segments =
  let exportMap :: Map.Map HName (Export, ModName)
      exportMap = Map.fromList
        [ (n, (e, m))
        | Segment m rs <- toList segments
        , r            <- toList rs
        , e            <- toList (reExports r)
        , n <- exportName e : (exportName <$> V.toList (exportWith e))
        ]
      findLocalModule :: HName -> Maybe ModName
      findLocalModule n = snd <$> Map.lookup n exportMap
      findModule :: Name -> Maybe ModName
      findModule n = ModName . T.pack <$> nameModule n
  in  ImportLocation { .. }
