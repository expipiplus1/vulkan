module VMA.Bracket
  ( brackets
  ) where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T

import           Spec.Name
import           Bracket
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Error
import           Marshal.Command
import           Render.Utils

brackets
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector MarshaledCommand
  -> Sem r (Vector (CName, CName, RenderElement))
brackets marshaledCommands = context "brackets" $ do
  let getMarshaledCommand =
        let mcMap = Map.fromList
              [ (mcName, m)
              | m@MarshaledCommand {..} <- toList marshaledCommands
              ]
        in  note "Unable to find marshaled command" . (`Map.lookup` mcMap)
      autoBracket' :: CName -> CName -> CName -> Sem r Bracket
      autoBracket' create destroy with = do
        create'  <- getMarshaledCommand create
        destroy' <- getMarshaledCommand destroy
        autoBracket create' destroy' with
  bs <- sequenceV
    [ autoBracket' "vmaCreateAllocator" "vmaDestroyAllocator" "vmaWithAllocator"
    , autoBracket' "vmaCreatePool"      "vmaDestroyPool"      "vmaWithPool"
    , autoBracket' "vmaAllocateMemory"  "vmaFreeMemory"       "vmaWithMemory"
    , autoBracket' "vmaAllocateMemoryForBuffer" "vmaFreeMemory" "vmaWithMemoryForBuffer"
    , autoBracket' "vmaAllocateMemoryForImage" "vmaFreeMemory" "vmaWithMemoryForImage"
    , autoBracket' "vmaAllocateMemoryPages" "vmaFreeMemoryPages" "vmaWithMemoryPages"
    , autoBracket' "vmaCreateLostAllocation" "vmaFreeMemory" "vmaWithLostAllocation"
    , autoBracket' "vmaMapMemory" "vmaUnmapMemory" "vmaWithMappedMemory"
    , autoBracket' "vmaDefragmentationBegin" "vmaDefragmentationEnd" "vmaWithDefragmentation"
    , autoBracket' "vmaBeginDefragmentationPass" "vmaEndDefragmentationPass" "vmaUseDefragmentationPass"
    , autoBracket' "vmaCreateBuffer" "vmaDestroyBuffer" "vmaWithBuffer"
    , autoBracket' "vmaCreateImage"  "vmaDestroyImage"  "vmaWithImage"
    ]
  fromList <$> traverseV (renderBracket paramName) bs

paramName :: Text -> Text
paramName = unReservedWord . T.lowerCaseFirst . dropVma

dropVma :: Text -> Text
dropVma t = if "vma" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t
