{-| The unified mesh table.

Every base mesh (cube, knot, sphere) lives in one vertex SSBO, so a single pipeline
draws them all by pulling @vertex[mesh.baseVertex + id]@. Each mesh is a run of
@vec4 position + vec4 normal@ vertices. The cube and sphere are
CPU constants uploaded once; the knot is compute-generated into its slice (see
"Pipeline.Knot", which writes at 'knotBase'). The mesh table SSBO holds
@{baseVertex, vertexCount}@ per mesh, indexed by an object's @meshId@.
-}
module Scene.Meshes
  ( MeshId
  , cube
  , knot
  , sphere
  , meshCount
  , cubeBase
  , cubeVertexCount
  , knotBase
  , knotVertexCount
  , sphereBase
  , sphereVertexCount
  , totalVertexCount
  , vertexBufferSize
  , meshTableBytes
  , cpuVertexBytes
  , stageVertices
  , uploadMeshTable
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (sizeOf)
import qualified Vulkan.Core10 as Vk

import qualified Pipeline.Knot as Knot
import qualified Pipeline.Mesh as Mesh
import qualified Upload

-- | A mesh's slot in the mesh table (an object's @meshId@).
type MeshId = Word32

cube, knot, sphere :: MeshId
cube = 0
knot = 1
sphere = 2

meshCount :: Word32
meshCount = 3

-- | Bytes per vertex: @vec4 position + vec4 normal@.
vertexStride :: Vk.DeviceSize
vertexStride = 32

cubeVertexCount :: Word32
cubeVertexCount = 36

knotVertexCount :: Word32
knotVertexCount = Knot.vertexCount

-- | Cube-sphere subdivision per face edge (higher = rounder silhouette).
sphereSubdiv :: Int
sphereSubdiv = 16

sphereVertexCount :: Word32
sphereVertexCount = fromIntegral (length sphereVertices)

cubeBase :: Word32
cubeBase = 0

-- | The knot's first vertex in the unified buffer (compute-gen writes from here).
knotBase :: Word32
knotBase = cubeVertexCount

sphereBase :: Word32
sphereBase = knotBase + knotVertexCount

totalVertexCount :: Word32
totalVertexCount = cubeVertexCount + knotVertexCount + sphereVertexCount

vertexBufferSize :: Vk.DeviceSize
vertexBufferSize = fromIntegral totalVertexCount * vertexStride

-- | The mesh table stride: one @{uint baseVertex; uint vertexCount;}@.
meshEntryBytes :: Int
meshEntryBytes = sizeOf (undefined :: Mesh.MeshEntry)

meshTableBytes :: Vk.DeviceSize
meshTableBytes = fromIntegral meshCount * fromIntegral meshEntryBytes

-- | The mesh table, indexed by 'MeshId'.
meshTable :: [Mesh.MeshEntry]
meshTable =
  [ Mesh.MeshEntry cubeBase cubeVertexCount
  , Mesh.MeshEntry knotBase knotVertexCount
  , Mesh.MeshEntry sphereBase sphereVertexCount
  ]

-- | Fill the mesh table SSBO (base + count per mesh).
uploadMeshTable :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> m ()
uploadMeshTable cb buffer = Upload.slice cb buffer 0 meshTable

cubeBytes, sphereBytes :: Int
cubeBytes = fromIntegral cubeVertexCount * fromIntegral vertexStride
sphereBytes = fromIntegral sphereVertexCount * fromIntegral vertexStride

-- | Bytes of CPU-authored vertex data (cube + sphere) — size the staging buffer.
cpuVertexBytes :: Vk.DeviceSize
cpuVertexBytes = fromIntegral (cubeBytes + sphereBytes)

{- | Stage the CPU meshes (cube then sphere) into the GPU vertex buffer.

Pokes them into @staging@'s mapped @ptr@, then @cmdCopyBuffer@ (not @cmdUpdateBuffer@,
whose @dataSize@ caps at 65536) carries the bulk data to their slices; the knot slice
between them is compute-generated, so it is left untouched.
-}
stageVertices :: (MonadIO m) => Vk.CommandBuffer -> Ptr () -> Vk.Buffer -> Vk.Buffer -> m ()
stageVertices cb ptr staging dst = liftIO $ do
  pokeArray (castPtr ptr) (concatMap floats cubeVertices)
  pokeArray (castPtr (ptr `plusPtr` cubeBytes)) (concatMap floats sphereVertices)
  Vk.cmdCopyBuffer cb staging dst $
    V.fromList
      [ Vk.BufferCopy 0 (fromIntegral cubeBase * vertexStride) (fromIntegral cubeBytes)
      , Vk.BufferCopy (fromIntegral cubeBytes) (fromIntegral sphereBase * vertexStride) (fromIntegral sphereBytes)
      ]
  where
    floats ((px, py, pz), (nx, ny, nz)) = [px, py, pz, 1, nx, ny, nz, 0]

-- | Unit-cube triangle soup: 6 faces × 2 triangles, outward normal per face.
cubeVertices :: [((Float, Float, Float), (Float, Float, Float))]
cubeVertices =
  concat
    [ face [0, 1, 2, 0, 2, 3] (0, 0, -1)
    , face [5, 4, 7, 5, 7, 6] (0, 0, 1)
    , face [4, 0, 3, 4, 3, 7] (-1, 0, 0)
    , face [1, 5, 6, 1, 6, 2] (1, 0, 0)
    , face [4, 5, 1, 4, 1, 0] (0, -1, 0)
    , face [3, 2, 6, 3, 6, 7] (0, 1, 0)
    ]
  where
    face is n = [(corners !! i, n) | i <- is]
    corners =
      [ (-1, -1, -1)
      , (1, -1, -1)
      , (1, 1, -1)
      , (-1, 1, -1)
      , (-1, -1, 1)
      , (1, -1, 1)
      , (1, 1, 1)
      , (-1, 1, 1)
      ]

{- | Unit cube-sphere.

Each cube face subdivided, projected to the sphere; the normal is the (normalized)
position, so shading is smooth.
-}
sphereVertices :: [((Float, Float, Float), (Float, Float, Float))]
sphereVertices =
  [ (p, p)
  | f <- [0 .. 5 :: Int]
  , qy <- [0 .. s - 1]
  , qx <- [0 .. s - 1]
  , (ox, oy) <- [(0, 0), (1, 0), (1, 1), (0, 0), (1, 1), (0, 1)]
  , let
      u = edge (qx + ox)
      v = edge (qy + oy)
      p = normalize (cubeFace f u v)
  ]
  where
    s = sphereSubdiv
    edge i = fromIntegral i / fromIntegral s * 2 - 1
    normalize (x, y, z) = let l = sqrt (x * x + y * y + z * z) in (x / l, y / l, z / l)
    cubeFace f u v = case f of
      0 -> (1, v, -u)
      1 -> (-1, v, u)
      2 -> (u, 1, -v)
      3 -> (u, -1, v)
      4 -> (u, v, 1)
      _ -> (-u, v, -1)
