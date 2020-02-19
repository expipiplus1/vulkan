{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.ToCStruct
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                , State
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import qualified Data.Vector                   as V

import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Error
import           Render.Utils
import           Render.Element
import           Render.Type
import           Render.Scheme

toCStruct
  :: (HasErr r, Member (Reader RenderParams) r)
  => Sem r RenderElement
toCStruct = do
  RenderParams {..} <- ask
  genRe "ToCStruct" $ do
    tellExport (EClass "ToCStruct")
    tellImport ''Ptr
    tellImport ''Storable
    tellImport 'alloca
    tellDoc [qi|
      -- | A class for types which can be marshalled into a C style
      -- structure.
      class ToCStruct a where
        -- | Allocates a C type structure and all dependencies and passes
        -- it to a continuation. The space is deallocated when this
        -- continuation returns and the C type structure must not be
        -- returned out of it.
        withCStruct :: a -> (Ptr a -> IO b) -> IO b
        default withCStruct :: Storable a => a -> (Ptr a -> IO b) -> IO b
        withCStruct x f = alloca $ \p -> pokeCStruct p x (f p)

        -- | Write a C type struct into some existing memory and run a
        -- continuation. The pointed to structure is not necessarily valid
        -- outside the continuation as additional allocations may have been
        -- made.
        pokeCStruct :: Ptr a -> a -> IO b -> IO b
    |]
