module Population (Model, init, Action(..), update, view) where

import Color exposing (blue, green, red)
import Graphics.Element exposing (Element, flow, down, show, right)
import Graphics.Input exposing (button)
import Random exposing (Generator, customGenerator, generate, initialSeed)
import Signal exposing (Address, message)
import Time exposing (Time, inMilliseconds)

import Histogram
import Evolve exposing ( Generation
                       , AlleleModel
                       , GenotypeModel
                       , Dist
                       , Fitness
                       , size
                       )
import Slider exposing (defaultSlider, slider)

-- Types ----------------------------------------------------------------------
type alias Model = { population  : Generation
                   , fitness     : GenotypeModel Fitness
                   , capacity    : Int
                   , generations : Int
                   , running     : Bool
                   }

type Action = Size Int
            | Capacity Int
            | AlleleDist (AlleleModel Dist)
            | Fitness (GenotypeModel Fitness)
            | Live
            | Running

init : Int -> Int -> AlleleModel Dist -> GenotypeModel Fitness -> Generator Model
init size cap dist fitness = customGenerator <| \seed ->
    let (population,seed') = generate (Evolve.init size dist) seed
        model = { population=population
                , fitness=fitness
                , capacity=cap
                , generations=0
                , running=False
                }
    in (model, seed')

view : Address Action -> Model -> Element
view addr model =
    let genoDist = Evolve.genotypeDist model.population
        alDist = Evolve.alleleDist genoDist
    in flow down
       [ plotGenotypes (300,300) genoDist
       , label addr model alDist genoDist
       ]

update : Action -> Model -> Generator Model
update action model = customGenerator <| \seed ->
    case action of
        Size size' ->
            let populationGen = Evolve.resize size' model.population
                (population', seed') = generate populationGen seed
                model' = { model | population <- population' }
            in (model', seed')
        Capacity cap -> ({ model | capacity <- cap }, seed)
        AlleleDist alDist ->
            let populationGen = Evolve.init (size model.population) alDist
                (population', seed') = generate populationGen seed
                model' = { model | population <- population' }
            in (model', seed')
        Fitness fitness' ->
            let model' = { model | fitness <- fitness' }
            in (model', seed)
        Live ->
            let populationGen = Evolve.live model.capacity model.fitness model.population
                (population', seed') = generate populationGen seed
                alleleDist = Evolve.alleleDist (Evolve.genotypeDist model.population)
                p = alleleDist.dom
                homogenous = not (p > 0 && p < 1)
                model' = { model | population <- population', generations <- model.generations + 1}
                model'' = if homogenous then { model' | running <- False } else model'
            in if model.running then (model'', seed') else (model, seed)
        Running ->
            let model' = { model | running <- not model.running }
            in (model', seed)

plotGenotypes : (Float, Float) -> GenotypeModel Dist -> Element
plotGenotypes dims dist = Histogram.plot dims
        [ (dist.homdom, green)
        , (dist.hetero, blue)
        , (dist.homrec, red)
        ]

label : Address Action -> Model -> AlleleModel Dist -> GenotypeModel Dist -> Element
label addr model alDist genDist = flow down
        [ button (message addr Running) "On/Off"
        , show <| "generations: " ++ (toString (model.generations))
        , flow right
                [ show <| "size: " ++ (toString (size model.population))
                , slider (\n -> message addr (Size <| floor n)) { defaultSlider | max <- toFloat model.capacity, value <- toFloat (size model.population)}
                ]
        , flow right
                [ show <| "capacity: " ++ (toString model.capacity)
                , slider (\n -> message addr (Capacity <| floor n)) { defaultSlider | max <- 10000, value <- toFloat model.capacity, length <- 1000 }
                ]
        , flow right
                [ show <| "p: " ++ (toString <| alDist.dom)
                , slider (\n -> message addr (AlleleDist { dom=n, rec=(1 - n)})) { defaultSlider | max <- 1, value <- alDist.dom, step <- 0.01 }
                ]
        , show <| "q: " ++ (toString <| alDist.rec)
        , flow right
                [ show <| "p^2: " ++ (toString <| genDist.homdom) ]
                -- , fitnessSliders addr .homdom model
                -- ]
        , flow right
                [ show <| "2pq: " ++ (toString <| genDist.hetero) ]
                -- , fitnessSliders addr .hetero model
                -- ]
        , flow right
                [ show <| "q^2: " ++ (toString <| genDist.homrec) ]
                --, fitnessSliders addr .homrec model
                --]
        ]

{-
fitnessSliders : Address Action -> ? -> Model -> Element
fitnessSliders = flow right
        [ show "survival fitness: "
        , slider (\n -> mes
        , show "reproductive fitness: average offspring: "
        , slider 
        , show "variance: "
        , slider
        ]
-}
