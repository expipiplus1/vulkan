{-# language QuasiQuotes #-}
{-# language TemplateHaskellQuotes #-}
module Render.CStruct
  ( cStructDocs
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Control.Exception              ( bracket )

import           Error
import           Render.Element
import           Haskell.Name

cStructDocs :: (HasErr r, HasRenderParams r) => Vector (Sem r RenderElement)
cStructDocs = V.fromList [toCStruct, fromCStruct]

toCStruct :: (HasErr r, HasRenderParams r) => Sem r RenderElement
toCStruct = do
  RenderParams {..} <- ask
  genRe "ToCStruct" $ do
    tellExplicitModule (ModName "Graphics.Vulkan.CStruct")
    tellNotReexportable
    tellExport (EClass (TyConName "ToCStruct"))
    tellImport ''Ptr
    tellImport 'allocaBytesAligned
    tellImport 'callocBytes
    tellImport 'free
    tellImport 'bracket
    tellDoc [qi|
      -- | A class for types which can be marshalled into a C style
      -- structure.
      class ToCStruct a where
        -- | Allocates a C type structure and all dependencies and passes
        -- it to a continuation. The space is deallocated when this
        -- continuation returns and the C type structure must not be
        -- returned out of it.
        withCStruct :: a -> (Ptr a -> IO b) -> IO b
        withCStruct x f = allocaBytesAligned (cStructSize @a) (cStructAlignment @a)
          $ \p -> pokeCStruct p x (f p)

        -- | Write a C type struct into some existing memory and run a
        -- continuation. The pointed to structure is not necessarily valid
        -- outside the continuation as additional allocations may have been
        -- made.
        pokeCStruct :: Ptr a -> a -> IO b -> IO b

        -- | Allocate space for an "empty" @a@ and populate any univalued
        -- members with their value.
        withZeroCStruct :: (Ptr a -> IO b) -> IO b
        withZeroCStruct f =
          bracket (callocBytes @a (cStructSize @a)) free $ \p -> pokeZeroCStruct p (f p)

        -- | And populate any univalued members with their value, run a
        -- function and then clean up any allocated resources.
        pokeZeroCStruct :: Ptr a -> IO b -> IO b

        -- | The size of this struct, note that this doesn't account for any
        -- extra pointed-to data
        cStructSize :: Int

        -- | The required memory alignment for this type
        cStructAlignment :: Int
    |]

fromCStruct
  :: (HasErr r, Member (Reader RenderParams) r)
  => Sem r RenderElement
fromCStruct = do
  RenderParams {..} <- ask
  genRe "ToCStruct" $ do
    tellExplicitModule (ModName "Graphics.Vulkan.CStruct")
    tellNotReexportable
    tellExport (EClass (TyConName "FromCStruct"))
    tellImport ''Ptr
    tellDoc [qi|
      -- | A class for types which can be marshalled from a C style
      -- structure.
      class FromCStruct a where
        -- | Read an @a@ and any other pointed to data from memory
        peekCStruct :: Ptr a -> IO a
    |]
