{-# language QuasiQuotes #-}
module Bespoke.Utils
  ( marshalUtils
  , zeroClass
  , hasObjectTypeClass
  ) where

import           Data.Text.Prettyprint.Doc
import           Foreign.C.Types
import           Foreign.Ptr
import           Polysemy
import           Polysemy.Input
import           Relude
import           Text.InterpolatedString.Perl6.Unindented

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BS
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           GHC.TypeNats

import           Error
import           Haskell.Name
import           Render.Element
import           VkModulePrefix

hasObjectTypeClass :: (HasErr r, HasRenderParams r) => Sem r RenderElement
hasObjectTypeClass = genRe "HasObjectType class" $ do
  RenderParams {..} <- input
  tellExport (EClass (TyConName "HasObjectType"))
  tellExplicitModule (vulkanModule ["Core10", "APIConstants"])
  tellImport ''Word64
  tellNotReexportable
  let objectType = mkTyName "VkObjectType"
  tellImport objectType

  tellDoc $ "class HasObjectType a where" <> line <> indent
    2
    ("objectTypeAndHandle :: a ->" <+> tupled [pretty objectType, "Word64"])

zeroClass :: (HasErr r, HasRenderParams r) => Sem r RenderElement
zeroClass = genRe "zero class" $ do
  traverseV_
    tellImport
    [ 'nullPtr
    , 'nullFunPtr
    , ''CFloat
    , ''CChar
    , ''CSize
    , ''CInt
    , ''Int8
    , ''Int16
    , ''Int32
    , ''Int64
    , ''Word8
    , ''Word16
    , ''Word32
    , ''Word64
    , ''KnownNat
    , ''Storable
    , ''FunPtr
    , ''Ptr
    ]

  tellExport (EClass (TyConName "Zero"))
  tellExplicitModule (vulkanModule ["Zero"])
  tellNotReexportable

  tellDoc [qi|
    -- | A class for initializing things with all zero data
    --
    -- Any instance should satisfy the following law:
    --
    -- @ new zero = calloc @ or @ with zero = withZeroCStruct @
    --
    -- i.e. Marshaling @zero@ to memory yeilds only zero-valued bytes, except
    -- for structs which require a "type" tag
    --
    class Zero a where
      zero :: a

    instance Zero Bool where
      zero = False

    instance Zero (FunPtr a) where
      zero = nullFunPtr

    instance Zero (Ptr a) where
      zero = nullPtr

    instance Zero Int8 where
      zero = 0

    instance Zero Int16 where
      zero = 0

    instance Zero Int32 where
      zero = 0

    instance Zero Int64 where
      zero = 0

    instance Zero Word8 where
      zero = 0

    instance Zero Word16 where
      zero = 0

    instance Zero Word32 where
      zero = 0

    instance Zero Word64 where
      zero = 0

    instance Zero Float where
      zero = 0

    instance Zero CFloat where
      zero = 0

    instance Zero CChar where
      zero = 0

    instance Zero CSize where
      zero = 0

    instance Zero CInt where
      zero = 0
    |]

marshalUtils :: (HasErr r, HasRenderParams r) => Sem r RenderElement
marshalUtils = genRe "marshal utils" $ do
  tellExplicitModule (vulkanModule ["CStruct", "Utils"])
  tellNotReexportable
  traverseV_ tellImportWithAll [''Proxy, ''CChar]
  traverseV_
    tellImport
    [ ''Word8
    , ''BS.ByteString
    , 'BS.take
    , 'BS.unpack
    , 'BS.packCString
    , 'BS.packCStringLen
    , 'BS.unsafeUseAsCString
    , ''V.Vector
    , 'V.ifoldr
    , 'allocaArray
    , 'copyBytes
    , ''Ptr
    , 'castPtr
    , ''Storable
    , 'pokeElemOff
    , 'peekElemOff
    , 'natVal
    , ''KnownNat
    , ''(<=)
    , 'natVal
    , 'plusPtr
    , ''Nat
    , ''Type
    ]

  traverseV_
    tellQualImport
    [ 'V.length
    , 'VG.length
    , 'VG.take
    , 'VG.replicate
    , 'VG.fromList
    , ''VG.Vector
    , '(VG.++)
    , 'VG.snoc
    , 'VG.empty
    , 'BS.length
    ]

  traverseV_
    (tellExport . ETerm . TermName)
    [ "pokeFixedLengthByteString"
    , "pokeFixedLengthNullTerminatedByteString"
    , "peekByteStringFromSizedVectorPtr"
    , "lowerArrayPtr"
    , "advancePtrBytes"
    ]
  tellExport (EType (TyConName "FixedArray"))

  tellDoc [qi|
    -- | An unpopulated type intended to be used as in @'Ptr' (FixedArray n a)@ to
    -- indicate that the pointer points to an array of @n@ @a@s
    data FixedArray (n :: Nat) (a :: Type)

    -- | Store a 'ByteString' in a fixed amount of space inserting a null
    -- character at the end and truncating if necessary.
    --
    -- If the 'ByteString' is not long enough to fill the space the remaining
    -- bytes are unchanged
    --
    -- Note that if the 'ByteString' is exactly long enough the last byte will
    -- still be replaced with 0
    pokeFixedLengthNullTerminatedByteString
      :: forall n
       . KnownNat n
      => Ptr (FixedArray n CChar)
      -> ByteString
      -> IO ()
    pokeFixedLengthNullTerminatedByteString to bs =
      unsafeUseAsCString bs $ \from -> do
        let maxLength = fromIntegral (natVal (Proxy @n))
            len       = min maxLength (Data.ByteString.length bs)
            end       = min (maxLength - 1) len
        -- Copy the entire string into the buffer
        copyBytes (lowerArrayPtr to) from len
        -- Make the last byte (the one following the string, or the
        -- one at the end of the buffer)
        pokeElemOff (lowerArrayPtr to) end 0

    -- | Store a 'ByteString' in a fixed amount of space, truncating if necessary.
    --
    -- If the 'ByteString' is not long enough to fill the space the remaining
    -- bytes are unchanged
    pokeFixedLengthByteString
      :: forall n
       . KnownNat n
      => Ptr (FixedArray n Word8)
      -> ByteString
      -> IO ()
    pokeFixedLengthByteString to bs = unsafeUseAsCString bs $ \from -> do
      let maxLength = fromIntegral (natVal (Proxy @n))
          len       = min maxLength (Data.ByteString.length bs)
      copyBytes (lowerArrayPtr to) (castPtr @CChar @Word8 from) len

    -- | Peek a 'ByteString' from a fixed sized array of bytes
    peekByteStringFromSizedVectorPtr
      :: forall n
       . KnownNat n
      => Ptr (FixedArray n Word8)
      -> IO ByteString
    peekByteStringFromSizedVectorPtr p = packCStringLen (castPtr p, fromIntegral (natVal (Proxy @n)))

    -- | Get the pointer to the first element in the array
    lowerArrayPtr
      :: forall a n
       . Ptr (FixedArray n a)
      -> Ptr a
    lowerArrayPtr = castPtr

    -- | A type restricted 'plusPtr'
    advancePtrBytes :: Ptr a -> Int -> Ptr a
    advancePtrBytes = plusPtr
  |]

