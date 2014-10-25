module K5 where

import Data.Tuple
import Graph
import Math (pi)
import qualified Data.Set as Set

graph = Graph vertices edges basis

va = Polar (0*pi/5) 85
vb = Polar (2*pi/5) 85
vc = Polar (4*pi/5) 85
vd = Polar (6*pi/5) 85
ve = Polar (8*pi/5) 85

vertices :: Set.Set Vertex
vertices = Set.fromList [ va, vb, vc, vd, ve ]

eab = Tuple va vb
eac = Tuple va vc
ead = Tuple va vd
eae = Tuple va ve
ebc = Tuple vb vc
ebd = Tuple vb vd
ebe = Tuple vb ve
ecd = Tuple vc vd
ece = Tuple vc ve
ede = Tuple vd ve

edges :: Set.Set Edge
edges = Set.fromList [ eab, eac, ead, eae, ebc, ebd, ebe, ecd, ece, ede ]

basis :: [Set.Set Edge]
basis = Set.fromList <$> [
  [ eab, ebc, eac ],
  [ ebc, ecd, ebd ],
  [ ecd, ede, ece ],
  [ ede, eae, ead ],
  [ eae, eab, ebe ],
  [ eab, ebc, ecd, ede, eae ]
  ]

