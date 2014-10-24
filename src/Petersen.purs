module Petersen where

import Data.Tuple
import Graph
import Math (pi)
import qualified Data.Set as Set

graph = Graph vertices edges basis

center = {
  x: 100,
  y: 100
  }

va = Polar (0*pi/5) 85
vb = Polar (2*pi/5) 85
vc = Polar (4*pi/5) 85
vd = Polar (6*pi/5) 85
ve = Polar (8*pi/5) 85
vf = Polar (0*pi/5) 45
vg = Polar (2*pi/5) 45
vh = Polar (4*pi/5) 45
vi = Polar (6*pi/5) 45
vj = Polar (8*pi/5) 45

vertices :: Set.Set Vertex
vertices = Set.fromList [ va, vb, vc, vd, ve, vf, vg, vh, vi, vj ]

eab = Tuple va vb
ebc = Tuple vb vc
ecd = Tuple vc vd
ede = Tuple vd ve
eae = Tuple ve va
efh = Tuple vf vh
ehj = Tuple vh vj
egj = Tuple vj vg
egi = Tuple vg vi
efi = Tuple vi vf
eaf = Tuple va vf
ebg = Tuple vb vg
ech = Tuple vc vh
edi = Tuple vd vi
eej = Tuple ve vj

edges :: Set.Set Edge
edges = Set.fromList [ eab, ebc, ecd, ede, eae, efh, ehj, egj, egi, efi, eaf, ebg, ech, edi, eej ]

basis :: [Set.Set Edge]
basis = Set.fromList <$> [
  [eab, ebc, ecd, ede, eae],
  [eab, ebc, ech, efh, eaf],
  [ebc, ecd, edi, egi, ebg],
  [ecd, ede, eej, ehj, ech],
  [ede, eae, eaf, efi, edi],
  [eae, eab, ebg, egj, eej]
  ]

