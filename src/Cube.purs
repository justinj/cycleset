module Cube where

import Data.Tuple
import Graph
import Math (pi)
import qualified Data.Set as Set

graph = Graph vertices edges basis

center = {
  x: 100,
  y: 100
  }

va = Polar (pi/4 + 0*(pi/2)) 85
vb = Polar (pi/4 + 1*(pi/2)) 85
vc = Polar (pi/4 + 2*(pi/2)) 85
vd = Polar (pi/4 + 3*(pi/2)) 85
ve = Polar (pi/4 + 0*(pi/2)) 45
vf = Polar (pi/4 + 1*(pi/2)) 45
vg = Polar (pi/4 + 2*(pi/2)) 45
vh = Polar (pi/4 + 3*(pi/2)) 45

vertices :: Set.Set Vertex
vertices = Set.fromList [ va, vb, vc, vd, ve, vf, vg, vh ]

eab = Tuple va vb
ebc = Tuple vb vc
ecd = Tuple vc vd
ead = Tuple va vd
eef = Tuple ve vf
efg = Tuple vf vg
egh = Tuple vg vh
eeh = Tuple ve vh
eae = Tuple va ve
ebf = Tuple vb vf
ecg = Tuple vc vg
edh = Tuple vd vh

edges :: Set.Set Edge
edges = Set.fromList [ eab, ebc, ecd, ead, eef, efg, egh, eeh, eae, ebf, ecg, edh ]

basis :: [Set.Set Edge]
basis = Set.fromList <$> [
  [eab, ebf, eef, eae],
  [ebc, ecg, efg, ebf],
  [ecd, edh, egh, ecg],
  [ead, eae, eeh, edh],
  [eab, ebc, ecd, ead]
  ]
