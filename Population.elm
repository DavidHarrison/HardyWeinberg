module Population (Model, init, Action(..), update, view) where

import Color exposing (blue, green, red)
import Graphics.Element exposing (Element, flow, down, show, right)
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
type alias Model = { population : Generation
                   , fitness    : GenotypeModel Fitness
                   , capacity   : Int
                   }

type Action = Size Int
            | Capacity Int
            | AlleleDist (AlleleModel Dist)
            | Fitness (GenotypeModel Fitness)
            | Live

init : Int -> Int -> AlleleModel Dist -> GenotypeModel Fitness -> Generator Model
init size cap dist fitness = customGenerator <| \seed ->
    let (population,seed') = generate (Evolve.init size dist) seed
        model = { population=population
                , fitness=fitness
                , capacity=cap
                }
    in (model, seed')

view : Address Action -> Model -> Element
view addr model =
    let genoDist = Evolve.genotypeDist model.population
        alDist = Evolve.alleleDist genoDist
    in flow down
       [ plotGenotypes (300,300) genoDist
       , label addr (size model.population) model.capacity alDist genoDist
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
                model' = { model | population <- population' }
            in (model', seed')

plotGenotypes : (Float, Float) -> GenotypeModel Dist -> Element
plotGenotypes dims dist = Histogram.plot dims
        [ (dist.homdom, green)
        , (dist.hetero, blue)
        , (dist.homrec, red)
        ]

label : Address Action -> Int -> Int -> AlleleModel Dist -> GenotypeModel Dist -> Element
label addr size capacity alDist genDist = flow down
        [ flow right
                [ show <| "size: " ++ (toString size)
                , slider (\n -> message addr (Size <| floor n)) { defaultSlider | max <- toFloat capacity, value <- toFloat size }
                ]
        , flow right
                [ show <| "capacity: " ++ (toString capacity)
                , slider (\n -> message addr (Capacity <| floor n)) { defaultSlider | max <- 1000, value <- toFloat capacity}
                ]
        , flow right
                [ show <| show <| "p: " ++ (toString <| alDist.dom)
                , slider (\n -> message addr (AlleleDist { dom=n, rec=(1 - n)})) { defaultSlider | max <- 1, value <- alDist.dom, step <- 0.01 }
                ]
        , show <| "q: " ++ (toString <| alDist.rec)
        , show <| "p^2: " ++ (toString <| genDist.homdom)
        , show <| "2pq: " ++ (toString <| genDist.hetero)
        , show <| "q^2: " ++ (toString <| genDist.homrec)
        ]
