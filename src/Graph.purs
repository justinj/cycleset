module Graph where

import Data.Array (length)
import Data.Tuple
import Math (sin, cos, pi)
import qualified Data.Set as Set

type Edge = Tuple Vertex Vertex
data Vertex = Polar Number Number
            | Cartesian Number Number
-- Vertex set, Edge set, basis, center

data Graph = Graph (Set.Set Vertex) (Set.Set Edge) [Set.Set Edge]

type CartesianVertex = { x :: Number, y :: Number }

instance showVertex :: Show Vertex where
  show (Polar a b) = "(Polar " ++ (show a) ++ " " ++ (show b) ++ ")"

instance eqVertex :: Eq Vertex where
  (==) (Polar a1 d1) (Polar a2 d2) = (a1 == a2 && d1 == d2)
  (/=) (Polar a1 d1) (Polar a2 d2) = (a1 /= a2 || d1 /= d2)

instance ordVertex :: Ord Vertex where
  compare (Polar a1 d1) (Polar a2 d2) = case compare a1 a2 of
    EQ -> compare d1 d2
    other -> other

vertexXLoc :: Vertex -> Number
vertexXLoc (Cartesian x _) = x
vertexXLoc (Polar angle dist) = (sin angle) * dist

vertexYLoc :: Vertex -> Number
vertexYLoc (Cartesian _ y) = y
vertexYLoc (Polar angle dist) = (cos angle) * dist

graphVertices (Graph vertices _ _) = vertices
graphEdges (Graph _ edges _) = edges
graphBasis (Graph _ _ basis) = basis
graphDimension (Graph _ _ basis) = length basis

-- in theory we could generate a basis, seems like too much work
mkGraph :: (Set.Set Vertex) -> (Set.Set Edge) -> [Set.Set Edge] -> Graph
mkGraph = Graph
