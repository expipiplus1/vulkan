module Marshal.Marshalable
  where

import           Relude
import           Data.Vector                   as V
                                         hiding ( empty )

import           Marshal.Name
import           CType

data ParameterLength
  = NullTerminated
  | NamedLength Text
  | NamedMemberLength Text Text
    -- ^ The length is specified by a member of another (struct) parameter, an
    -- example is vkAllocateCommandBuffers
  deriving (Show, Eq)

class Marshalable a where
  name       :: a -> Name Unmarshaled
  type'      :: a -> CType
  values     :: a -> Vector Text
  lengths    :: a -> Vector ParameterLength
  isOptional :: a -> Vector Bool

singleValue :: (Alternative f, Marshalable a) => a -> f Text
singleValue x =
  let vs = values x in if V.length vs == 1 then pure $ unsafeHead vs else empty

