module Haskell
  ( Type(..)
  , renderType
  , pattern (:@)
  , mkName
  -- , conName
  )
  where

import           Relude                  hiding ( Type )
import           Language.Haskell.TH
import           Data.Text.Prettyprint.Doc
import           Data.Generics.Uniplate.Data

renderType :: Type -> Doc ()
renderType = pretty . pprint . removeModules
  where removeModules = transformBi (mkName . nameBase)

pattern (:@) :: Type -> Type -> Type
pattern a :@ b = AppT a b

-- conName :: Text -> Name
-- conName n = mkNameG_d "" (toString m) (toString n)

-- tt :: QuasiQuoter
-- tt = QuasiQuoter { quoteExp = \s -> quoteExp t s }
