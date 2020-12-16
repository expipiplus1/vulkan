{-# language QuasiQuotes #-}
module Bespoke.Utils
  ( marshalUtils
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
import           Foreign.Marshal.Alloc          ( callocBytes )
import           Haskell.Name
import           Render.Element
import           Spec.Name                      ( CName(CName) )

hasObjectTypeClass :: (HasErr r, HasRenderParams r) => Sem r RenderElement
hasObjectTypeClass = genRe "HasObjectType class" $ do
  RenderParams {..} <- input
  tellExport (EClass (TyConName "HasObjectType"))
  tellExplicitModule =<< mkModuleName ["Core10", "APIConstants"]
  tellImport ''Word64
  tellNotReexportable
  let objectType = mkTyName (CName $ camelPrefix <> "ObjectType")
  tellImport objectType

  tellDoc $ "class HasObjectType a where" <> line <> indent
    2
    ("objectTypeAndHandle :: a ->" <+> tupled [pretty objectType, "Word64"])

marshalUtils :: (HasErr r, HasRenderParams r) => Sem r RenderElement
marshalUtils = genRe "marshal utils" $ do
  tellExplicitModule =<< mkModuleName ["CStruct", "Utils"]
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
    , 'callocBytes
    , 'sizeOf
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
    , "callocFixedArray"
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

    -- | Allocate a zero array with the size specified by the 'FixedArray'
    -- return type. Make sure to release the memory with 'free'
    callocFixedArray
      :: forall n a . (KnownNat n, Storable a) => IO (Ptr (FixedArray n a))
    callocFixedArray = callocBytes
      ( sizeOf (error "sizeOf evaluated its argument" :: a)
      * fromIntegral (natVal (Proxy @n))
      )

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

