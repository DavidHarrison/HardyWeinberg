module Simulation where

import Color exposing (Color, blue, green, red)
import Graphics.Collage exposing (Form, collage, filled, move, rect)
import Graphics.Element exposing (Element, flow, right, down, show)
import List
import Random as Rand
import Signal exposing ((<~), (~), constant, foldp)
import Time exposing (every, inMilliseconds, millisecond)
import Window

main : Signal Element
main = let milliseconds = inMilliseconds <~ every millisecond
           seeds = Rand.initialSeed << floor <~ milliseconds
           -- seeds' = Rand.initialSeed << floor << (\x -> x * 2) <~ milliseconds
           stream = simulate 1000 0.5 seeds
       in flow right <~ sequence
          [ chart 600 << snd <~ stream
          , uncurry label <~ stream
          ]

-- Display --------------------------------------------------------------------

chart : Float -> GenDist -> Element
chart size dist =
    let width = size / 3
        height = size
        bar : (GenDist -> Float) -> Color -> Float -> Form
        bar geno col xpos =
            let h' = height * geno dist
                hshift = (-size / 2) + (h' / 2)
                wshift = (-size / 2) + xpos + (width / 2)
            in move (wshift, hshift)  <| filled col <| rect width h'
    in collage (ceiling size) (ceiling size)
       [ bar homdom green 0
       , bar hetero blue  width
       , bar homrec red   (2 * width)
       ]

label : AlDist -> GenDist -> Element
label alDist genDist = flow down
        [ show <| "p: " ++ (toString <| dom alDist)
        , show <| "q: " ++ (toString <| rec alDist)
        , show <| "p^2: " ++ (toString <| homdom genDist)
        , show <| "2pq: " ++ (toString <| hetero genDist)
        , show <| "q^2: " ++ (toString <| homrec genDist)
        ]

-- Types ----------------------------------------------------------------------

type alias Allele = Bool
type alias Genotype = (Allele, Allele)

-- genotype distribution
type alias GenDist = (Float, Float)
-- homozygous dominant
homdom : GenDist -> Float
homdom = fst
-- homozygous recessive
homrec : GenDist -> Float
homrec = snd
-- heterozygous
hetero : GenDist -> Float
hetero dist = 1 - (homdom dist) - (homrec dist)

-- allele distribution
type alias AlDist = Float
dom : AlDist -> Float
-- dominant
dom = identity
-- recessive
rec : AlDist -> Float
rec dist = 1 - dist

type alias Avg = { running : Float, count : Int }
emptyAvg : Avg
emptyAvg = {running=0, count=0}
avg : Float -> Avg -> Avg
avg n {running,count} = {running=running + n, count=count+1}
runAvg : Avg -> Float
runAvg {running,count} = running / toFloat count

-- Simulation Logic -----------------------------------------------------------

simulate : Int -> AlDist -> Signal Rand.Seed -> Signal (AlDist,GenDist)
simulate genSize alDist seeds =
    let generate' : Rand.Seed -> Rand.Generator a -> a
        generate' seed gen = fst <| Rand.generate gen seed
        iteratee : Int -> Rand.Seed -> (AlDist, GenDist) -> (AlDist, GenDist)
        iteratee genSize seed = generate' seed << generation genSize << fst
    in foldp (iteratee genSize) (alDist, (0,0)) seeds


generation : Int -> AlDist -> Rand.Generator (AlDist, GenDist)
generation genSize alDist =
    let allele : Rand.Generator Allele
        allele = mapGen (\n -> n < alDist) <| Rand.float 0 1
        genotype : Rand.Generator Genotype
        genotype = Rand.pair allele allele
    in mapGen dists <| Rand.list genSize genotype

{-
dists : List Genotype -> (AlDist, GenDist)
dists genos =
    let gc = toFloat <| List.length genos
        alDist = List.sum <| List.map (uncurry (+) << dimap btoi btoi) genos
        hd = List.length <| List.filter (uncurry (&&)) genos
        hr = List.length <| List.filter (uncurry nor) genos
        genDist = (toFloat hd / gc, toFloat hr / gc)
    in (toFloat alDist / (2 * gc),genDist)
-}

dists : List Genotype -> (AlDist, GenDist)
dists genos =
    let iteratee : Genotype -> (Avg, (Avg, Avg)) -> (Avg, (Avg, Avg))
        iteratee (a,b) (d,(hd,hr)) =
            let d'  = (toFloat (btoi a + btoi b) / 2) `avg` d
                hd' = toFloat (btoi (a && b))         `avg` hd
                hr' = toFloat (btoi (a `nor` b))      `avg` hr
            in (d',(hd',hr'))
        init = (emptyAvg, (emptyAvg, emptyAvg))
        (d,(hd,hr)) = List.foldr iteratee init genos
    in (runAvg d, (runAvg hd, runAvg hr))

-- Utility Functions ----------------------------------------------------------

nor : Bool -> Bool -> Bool
nor a b = (not a) && (not b)

btoi : Bool -> Int
btoi n = if n then 1 else 0

dimap : (a -> c) -> (b -> d) -> (a,b) -> (c,d)
dimap f g (a,b) = (f a, g b)

mapFst : (a -> c) -> (a,b) -> (c,b)
mapFst f = dimap f identity

mapSnd : (b -> c) -> (a,b) -> (a,c)
mapSnd g = dimap identity g

sequence : List (Signal a) -> Signal (List a)
sequence = List.foldr (\a z -> (::) <~ a ~ z) (constant [])

-- from https://github.com/TheSeamau5/elm-random-extra/blob/master/src/Random/Extra.elm
-- which was having trouble installing
mapGen : (a -> b) -> Rand.Generator a -> Rand.Generator b
mapGen f generator = Rand.customGenerator <| \seed ->
    let (value, nextSeed) = Rand.generate generator seed
    in (f value, nextSeed)
