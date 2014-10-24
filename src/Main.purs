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
import Graph
import qualified Petersen as Petersen
import qualified Cube as Cube
import qualified K5 as K5

graphSize :: Number
graphSize = 200

center :: Number
center = graphSize / 2

displayVertex :: Vertex -> UI
displayVertex p =
  circle [
    cx (show (vertexXLoc p + center)),
    cy (show (vertexYLoc p + center)),
    r "5"
  ] [ ]

displayEdge :: Vertex -> Vertex -> UI
displayEdge p1 p2 =
  line [
    x1 (show (vertexXLoc p1 + center)),
    y1 (show (vertexYLoc p1 + center)),
    x2 (show (vertexXLoc p2 + center)),
    y2 (show (vertexYLoc p2 + center)),
    stroke "black"
  ] [ ]

displayGraph (Graph v e _) =
  ((displayVertex <$> Set.toList v) ++
  ((uncurry displayEdge) <$> Set.toList e))

type PropList = forall s dataAttrs ariaAttrs eff props state. [DOMProps s dataAttrs ariaAttrs eff props state]

graphFromCombination :: Graph -> [Set.Set Edge] -> PropList -> UI
graphFromCombination (Graph vertices _ basis) basisElements props =
  svg ([
    width (show graphSize),
    height (show graphSize)
    ] ++ props) $ displayGraph (Graph vertices (foldl (<>) Set.empty basisElements) basis)


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
                                             { selected :: [Set.Set a], deck :: [Set.Set a], graph
                                             :: Graph } | t65)
                                             { selected :: [Set.Set a], deck :: [Set.Set a], graph
                                             :: Graph }
updateElems elems e = do
  curState <- readState
  let oldDeck = curState.deck
  writeState { selected: elems, deck: oldDeck, graph: curState.graph }

checkScore :: forall t58 t65 t69 t75. t58 -> Control.Monad.Eff.Eff
                 (state :: React.ReactState (write :: React.WriteAllowed,
                                             read :: React.ReadAllowed | t75)
                                             { selected :: [Set.Set Edge], deck :: [Set.Set Edge],
                                             graph :: Graph } | t65)
                                             { selected :: [Set.Set Edge], deck :: [Set.Set Edge],
                                             graph :: Graph }

checkScore e = do
  curState <- readState
  let oldDeck = curState.deck
  let selected = curState.selected
  if Set.isEmpty $ foldl (<>) Set.empty selected then
    writeState { selected: [], deck: oldDeck \\ selected, graph: curState.graph } else
    writeState { selected: selected, deck: oldDeck, graph: curState.graph }

pressKey e = do
  curState <- readState
  writeState { selected: [], deck: curState.deck, graph: curState.graph }

changeGraphTo graph e = do
  curState <- readState
  shuffled <- shuffle $ allCards (graphBasis graph)
  writeState { selected: [], deck: shuffled, graph: graph }

diagram :: forall props. [Set.Set Edge] -> (props -> React.UI)
diagram cards = mkUI spec {
    getInitialState = return {
      selected: [],
      deck: cards,
      graph: Cube.graph
    }
  } do 
    state <- readState
    let basis = graphBasis state.graph
    let dimension = length basis
    let handSize = dimension + 1
    let included = state.selected
    if length state.deck <= handSize then
      return $ div' [
          text "You win! You emptied the deck."
        ] else
      return $ div [
        className "container"
        ] [
          div [
        className "main-game"
        ] [
        graphFromCombination state.graph included [
          className "preview"
          ],
        div' (map (\card -> div [
             className $ "component " ++ if arrayMember included card then "included" else "not-included",
             onClick (updateElems (xorElement card included))
             ] [
          graphFromCombination state.graph [card] []]) (take handSize state.deck)),
        button [ 
          onClick checkScore
          ] [ 
          text "Score"
          ],
        div' [
          text $ ("Left in deck: " ++ (show $ length state.deck - handSize))
          ]
       ],
        div' [
           graphButton Cube.graph "3-Dimensional Cube",
           graphButton Petersen.graph "Petersen Graph",
           graphButton K5.graph "Complete Graph on 5 Vertices (K5)"
          ]
       ]

graphButton graph name =
  div [
      className "button-container"
    ] [
    span [
      className "graph-button",
      onClick $ changeGraphTo graph
      ] [ text name ]
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

currentGraph = Cube.graph

main = do
  shuffled <- shuffle $ allCards (graphBasis currentGraph)
  let component = div' [diagram shuffled {}]
  renderToBody component
