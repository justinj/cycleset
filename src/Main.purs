module Main where

import Data.Tuple
import Data.Array ((!!), (..), map, elemIndex, delete, take, (\\), length)
import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM
import Math (sin, cos, pi, floor, pow)
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

type CartesianVertex = { x :: Number, y :: Number }

center = {
  x: 100,
  y: 100
  }

polarToCartesian :: CartesianVertex -> Vertex -> CartesianVertex
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

vertices :: Set.Set Vertex
vertices = Set.fromList [ va, vb, vc, vd, ve, vf, vg, vh, vi, vj ]

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

displayVertex :: Vertex -> UI
displayVertex p =
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
  ((displayVertex <$> Set.toList v) ++
  ((uncurry displayEdge) <$> Set.toList e))

basisElement :: Number -> (Set.Set Edge)
basisElement n = maybe Set.empty id (basis !! n)

type PropList = forall s dataAttrs ariaAttrs eff props state. [DOMProps s dataAttrs ariaAttrs eff props state]

graphFromCombination :: Set.Set Vertex -> [Set.Set Edge] -> PropList -> UI
graphFromCombination vertices basisElements props =
  svg ([
    width "200",
    height "200"
    ] ++ props) $ displayGraph (Graph vertices (foldl (<>) Set.empty basisElements))


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

-- This type is scary as heck, copied from psci, annotation needs to be here so we can put the
-- restriction on elems to be Ord
updateElems :: forall a t58 t65 t69 t75. (Ord a) => [Set.Set a] ->
               t58 -> Control.Monad.Eff.Eff
                 (state :: React.ReactState (write :: React.WriteAllowed,
                                             read :: React.ReadAllowed | t75)
                                             { selected :: [Set.Set a], deck :: [Set.Set a] } | t65)
                                             { selected :: [Set.Set a], deck :: [Set.Set a] }
updateElems elems e = do
  curState <- readState
  let oldDeck = curState.deck
  writeState { selected: elems, deck: oldDeck }

checkScore :: forall t58 t65 t69 t75. t58 -> Control.Monad.Eff.Eff
                 (state :: React.ReactState (write :: React.WriteAllowed,
                                             read :: React.ReadAllowed | t75)
                                             { selected :: [Set.Set Edge], deck :: [Set.Set Edge] } | t65)
                                             { selected :: [Set.Set Edge], deck :: [Set.Set Edge] }
checkScore e = do
  curState <- readState
  let oldDeck = curState.deck
  let selected = curState.selected
  if Set.isEmpty $ foldl (<>) Set.empty selected then
    writeState { selected: [], deck: oldDeck \\ selected } else
    writeState { selected: selected, deck: oldDeck }

pressKey e = do
  curState <- readState
  writeState { selected: [], deck: curState.deck }

diagram :: forall props. Set.Set Vertex -> [Set.Set Edge] -> (props -> React.UI)
diagram vertices cards = mkUI spec {
    getInitialState = return {
      selected: [],
      deck: cards
    }
  } do 
    state <- readState
    let included = state.selected
    if length state.deck <= 7 then
      return $ div' [
          text "You win! You emptied the deck."
        ] else
      return $ div [
        className "container"
       ] [
        graphFromCombination vertices included [
          className "preview"
          ],
        div' (map (\card -> div [
             className $ "component" ++ if arrayMember included card then " included" else "",
             onClick (updateElems (xorElement card included))
           ]
        [graphFromCombination vertices [card] []]) (take 7 state.deck)),
        button [ 
          onClick checkScore
          ] [ 
          text "Score"
        ],
        div' [
          text $ ("Left in deck: " ++ (show $ (length state.deck) - 7))
          ]
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

-- power set
allCards :: [Set.Set Edge] -> [Set.Set Edge]
allCards basis = map (representatives basis) (map numberToBinaryRep (1..numResults))
  where numResults = (pow 2 (length basis)) - 1

main = do
  shuffled <- shuffle $ allCards basis
  let component = div' [diagram vertices shuffled {}]
  renderToBody component
