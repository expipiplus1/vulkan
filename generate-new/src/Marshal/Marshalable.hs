module Marshal.Marshalable
  ( module Marshal.Marshalable
  , CName(..)
  ) where

import           Data.Vector                   as V
                                         hiding ( empty )
import           Relude

import           CType
import           Spec.Name

data ParameterLength
  = NullTerminated
  | NamedLength CName
  | NamedMemberLength CName CName
    -- ^ The length is specified by a member of another (struct) parameter, an
    -- example is vkAllocateCommandBuffers
  deriving (Show, Eq, Ord)

class Marshalable a where
  name       :: a -> CName
  type'      :: a -> CType
  values     :: a -> Vector Text
  lengths    :: a -> Vector ParameterLength
  isOptional :: a -> Vector Bool

singleValue :: (Alternative f, Marshalable a) => a -> f Text
singleValue x =
  let vs = values x in if V.length vs == 1 then pure $ unsafeHead vs else empty

