module Main where

import Data.Tuple
import Data.Array ((!!), (..), map, elemIndex, delete, take)
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Math (sin, cos, pi, floor)
import qualified Data.Set as Set
import Data.Maybe (maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid
import Data.Foldable (foldl)
import Shuffle

data Vertex = Polar Number Number
instance showVertex :: Show Vertex where
  show (Polar a b) = "(Polar " ++ (show a) ++ " " ++ (show b) ++ ")"

instance eqVertex :: Eq Vertex where
  (==) (Polar a1 d1) (Polar a2 d2) = (a1 == a2 && d1 == d2)
  (/=) (Polar a1 d1) (Polar a2 d2) = (a1 /= a2 || d1 /= d2)

instance ordVertex :: Ord Vertex where
  compare (Polar a1 d1) (Polar a2 d2) = case compare a1 a2 of
    EQ -> compare d1 d2
    other -> other

type CartesianPoint = { x :: Number, y :: Number }

center = {
  x: 100,
  y: 100
  }

polarToCartesian :: CartesianPoint -> Vertex -> CartesianPoint
polarToCartesian center (Polar angle distance) = {
    x: center.x + (sin angle) * distance,
    y: center.y - (cos angle) * distance
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

points :: Set.Set Vertex
points = Set.fromList [ va, vb, vc, vd, ve, vf, vg, vh, vi, vj ]

type Edge = Tuple Vertex Vertex

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

displayPoint :: Vertex -> UI
displayPoint p =
  circle [
    cx (show cart.x),
    cy (show cart.y),
    r "5"
  ] [ ]
    where
      cart = polarToCartesian center p

displayEdge :: Vertex -> Vertex -> UI
displayEdge p1 p2 =
  line [
    x1 (show cart1.x),
    y1 (show cart1.y),
    x2 (show cart2.x),
    y2 (show cart2.y),
    stroke "black"
  ] [ ]
  where 
    cart1 = polarToCartesian center p1
    cart2 = polarToCartesian center p2

data Graph = Graph (Set.Set Vertex) (Set.Set Edge)

{-- displayGraph :: Graph -> [UI] --}
displayGraph (Graph v e) =
  ((displayPoint <$> Set.toList v) ++
  ((uncurry displayEdge) <$> Set.toList e))

basisElement :: Number -> (Set.Set Edge)
basisElement n = maybe Set.empty id (basis !! n)

graphFromCombination :: [Set.Set Edge] -> UI
graphFromCombination basisElements =
  svg [
    width "200",
    height "200"
    ] $ displayGraph (Graph points (foldl (<>) Set.empty basisElements))


-- Set difference
(<->) :: forall a. (Ord a) => Set.Set a -> Set.Set a -> Set.Set a
(<->) set1 set2 = foldl (flip Set.delete) set1 (Set.toList set2)

-- Symmetric difference
instance symmetricDifference :: (Ord a) => Semigroup (Set.Set a) where
  (<>) set1 set2 = (set1 <-> set2) `Set.union` (set2 <-> set1)

arrayMember :: forall a. (Eq a) => [a] -> a -> Boolean
arrayMember lst x = elemIndex x lst > -1

xorElement :: forall a. (Eq a) => a -> [a] -> [a]
xorElement x lst = case (arrayMember lst x) of
    true -> delete x lst
    false -> x:lst

updateElems elems e = do
  writeState elems

diagram cards = mkUI spec {
    getInitialState = return []
  } do 
    included <- readState
    return $ div' $ [
      graphFromCombination included,
      div' (map (\card -> div [
           className $ "component" ++ if arrayMember included card then " included" else "",
           onClick (updateElems (xorElement card included))
         ]
      [graphFromCombination [card]]) cards)
     ]

isEven :: Number -> Boolean
isEven n = (n & 1) == 0

numberToBinaryRep :: Number -> [Boolean]
numberToBinaryRep 0 = []
numberToBinaryRep n = first:(numberToBinaryRep rest)
  where first = not $ isEven n
        rest = floor (n / 2)

representatives :: [Set.Set Edge] -> [Boolean] -> Set.Set Edge
representatives _ [] = Set.empty
representatives (b:basisElements) (included:others) =
  if included then
      b <> rest
      else rest
    where rest = representatives basisElements others

allCards :: [Set.Set Edge]
allCards = map (representatives basis) (map numberToBinaryRep (1..63))

main = do
  shuffled <- shuffle allCards
  let component = div' [diagram (take 7 shuffled) {}]
  renderToBody component
