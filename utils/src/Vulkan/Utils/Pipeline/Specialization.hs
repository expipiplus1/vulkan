{-|
Specialization constants, normalized to stacks of 32-bit units.

32 bits is both the minimal and the maximal size of a Vulkan specialization
constant (they may only be scalar @int@/@uint@/@float@/@bool@; @double@ is the
lone exception and is not supported here), so every constant maps to exactly one
'Word32' slot. That makes the layout trivial: the @n@th value packs into
@constantID = n@ at @offset = n * 4@ with @size = 4@. Shaders therefore declare
their constants with matching @constant_id@s counting from zero, in the same
order as the Haskell tuple/list passed here.

@
data MySpec = MySpec { width :: Word32, height :: Word32, scale :: Float }
instance Specialization MySpec where -- ... pack the fields

withSpecialization sp \\mSpec ->
  -- build a 'Vulkan.Core10.PipelineShaderStageCreateInfo' with
  --   specializationInfo = mSpec
  ...
@
-}
module Vulkan.Utils.Pipeline.Specialization
  ( withSpecialization
  , allocateSpecialization
  , Specialization (..)
  , SpecializationConst (..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Bool (bool)
import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import Data.Word (Word32)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Ptr (castPtr)
import GHC.Float (castFloatToWord32)
import qualified Vulkan.Core10 as Vk

{- | Provide a 'Vk.SpecializationInfo' describing @spec@ to the callback.

The info (and the buffer its @data'@ pointer references) is valid only for the
duration of the callback, which is exactly the window in which it needs to live:
pipeline creation copies the constant values out. Build and create the pipeline
inside the continuation.

An empty specialization (e.g. @()@ or an empty list) yields 'Nothing', so the
shader stage's @specializationInfo@ stays unset.
-}
withSpecialization
  :: (Specialization spec, MonadUnliftIO m)
  => spec
  -> (Maybe Vk.SpecializationInfo -> m a)
  -> m a
withSpecialization spec action =
  if Storable.null specData
    then
      action Nothing
    else withRunInIO $ \run ->
      Storable.unsafeWith specData $ \specPtr ->
        run . action $
          Just
            Vk.SpecializationInfo
              { Vk.mapEntries = mapEntries
              , Vk.dataSize = fromIntegral $ Storable.length specData * 4
              , Vk.data' = castPtr specPtr
              }
  where
    specData :: Storable.Vector Word32
    specData = Storable.fromList (specializationData spec)

    mapEntries :: Vector Vk.SpecializationMapEntry
    mapEntries = specializationMapEntries (Storable.length specData)

{- | Pack a specialization into a buffer tied to the current resource scope,
yielding the 'Vk.SpecializationInfo' to embed in a shader stage's
@specializationInfo@.

Unlike 'withSpecialization' this is not continuation-scoped: the backing buffer
lives until the surrounding 'Control.Monad.Trans.Resource.ResourceT' block ends,
so it survives a later pipeline creation. An empty specialization (e.g. @()@)
yields 'Nothing'.
-}
allocateSpecialization
  :: (Specialization spec, MonadResource m)
  => spec
  -> m (Maybe Vk.SpecializationInfo)
allocateSpecialization spec =
  case specializationData spec of
    [] ->
      pure Nothing
    ws -> do
      let n = length ws
      -- A C-malloc'd buffer is pointer-stable and freed at scope end; the
      -- pointer must stay valid until pipeline creation copies the values.
      (_key, ptr) <- allocate (mallocArray n) free
      liftIO $ pokeArray ptr ws
      pure $
        Just
          Vk.SpecializationInfo
            { Vk.mapEntries = specializationMapEntries n
            , Vk.dataSize = fromIntegral (n * 4)
            , Vk.data' = castPtr ptr
            }

-- | One 32-bit @constantID = offset/4@ entry per slot, counting from zero.
specializationMapEntries :: Int -> Vector Vk.SpecializationMapEntry
specializationMapEntries n =
  Vector.generate n $ \ix ->
    Vk.SpecializationMapEntry
      { Vk.constantID = fromIntegral ix
      , Vk.offset = fromIntegral (ix * 4)
      , Vk.size = 4
      }

{- | A value that flattens to a stack of 32-bit specialization constants, in
@constant_id@ order starting from zero.
-}
class Specialization a where
  specializationData :: a -> [Word32]

instance Specialization () where
  specializationData _ = []

-- | Pre-packed constants, used as-is.
instance Specialization [Word32] where
  specializationData = id

instance Specialization Word32 where
  specializationData x = [packConstData x]

instance Specialization Int32 where
  specializationData x = [packConstData x]

instance Specialization Float where
  specializationData x = [packConstData x]

instance Specialization Bool where
  specializationData x = [packConstData x]

{- | A single scalar specialization constant, reinterpreted into its 32-bit
representation.

Per the @GL_KHR_vulkan_glsl@ spec a @constant_id@ may only decorate a scalar
@int@, @float@ or @bool@; @uint@ works in practice too. All of these are 32 bits
wide.
-}
class SpecializationConst a where
  packConstData :: a -> Word32

instance SpecializationConst Word32 where
  packConstData = id

-- | Two's-complement bit pattern, preserved by 'fromIntegral' at the same width.
instance SpecializationConst Int32 where
  packConstData = fromIntegral

instance SpecializationConst Float where
  packConstData = castFloatToWord32

instance SpecializationConst Bool where
  packConstData = bool 0 1

instance
  ( SpecializationConst a
  , SpecializationConst b
  )
  => Specialization (a, b)
  where
  specializationData (a, b) =
    [ packConstData a
    , packConstData b
    ]

instance
  ( SpecializationConst a
  , SpecializationConst b
  , SpecializationConst c
  )
  => Specialization (a, b, c)
  where
  specializationData (a, b, c) =
    [ packConstData a
    , packConstData b
    , packConstData c
    ]

instance
  ( SpecializationConst a
  , SpecializationConst b
  , SpecializationConst c
  , SpecializationConst d
  )
  => Specialization (a, b, c, d)
  where
  specializationData (a, b, c, d) =
    [ packConstData a
    , packConstData b
    , packConstData c
    , packConstData d
    ]

instance
  ( SpecializationConst a
  , SpecializationConst b
  , SpecializationConst c
  , SpecializationConst d
  , SpecializationConst e
  )
  => Specialization (a, b, c, d, e)
  where
  specializationData (a, b, c, d, e) =
    [ packConstData a
    , packConstData b
    , packConstData c
    , packConstData d
    , packConstData e
    ]

instance
  ( SpecializationConst a
  , SpecializationConst b
  , SpecializationConst c
  , SpecializationConst d
  , SpecializationConst e
  , SpecializationConst f
  )
  => Specialization (a, b, c, d, e, f)
  where
  specializationData (a, b, c, d, e, f) =
    [ packConstData a
    , packConstData b
    , packConstData c
    , packConstData d
    , packConstData e
    , packConstData f
    ]
