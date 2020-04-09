module VMA.Bracket
  ( brackets
  ) where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           Spec.Name
import           Bracket
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Error

brackets
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Sem r (Vector (CName, CName, RenderElement))
brackets =
  let
    bs = V.fromList
      [ simpleBracket "Allocator" []
      , simpleBracket "Pool"
                      [Provided (SingleTypeName "VmaAllocator") "allocator"]
      ]
  in  fmap (\(_, c, w, r) -> (c, w, r)) <$> traverseV writePair bs

simpleBracket
  :: Text
  -- ^ The type name with no Vma prefix
  -> [Argument]
  -- ^ Extra arguments to create and destroy
  -> Bracket
  -- ^
simpleBracket tyName extraArgs = Bracket
  (SingleTypeName ("Vma" <> tyName))
  (CName $ "vmaWith" <> tyName)
  (CName $ "vmaCreate" <> tyName)
  (CName $ "vmaDestroy" <> tyName)
  (  extraArgs
  <> [ Provided (SingleTypeName $ "Vma" <> tyName <> "CreateInfo")
                "createInfo"
     ]
  )
  (extraArgs <> [Resource])
  False

