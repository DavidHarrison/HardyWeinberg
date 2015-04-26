module Population (Model, init, Action(..), update, view) where

import Color exposing (blue, green, red)
import Graphics.Element exposing (Element, flow, down)
import Markdown
import Random exposing (Generator, customGenerator, generate, initialSeed)
import Signal exposing (Address)
import Time exposing (Time, inMilliseconds)

import Histogram
import Evolve exposing ( Generation
                       , AlleleModel
                       , GenotypeModel
                       , Dist
                       , Fitness
                       )

-- Types ----------------------------------------------------------------------
type alias Model = { population  : Generation
                   , size        : Int
                   , fitness     : GenotypeModel Fitness
                   }

type Action = Size Int
            | AlleleDist (AlleleModel Dist)
            | Fitness (GenotypeModel Fitness)
            | Live

init : Int -> AlleleModel Dist -> GenotypeModel Fitness -> Generator Model
init size dist fitness = customGenerator <| \seed ->
    let (population,seed') = generate (Evolve.init size dist) seed
        model = { population=population
                , size=size
                , fitness=fitness
                }
    in (model, seed')

view : Address Action -> Model -> Element
view addr model =
    let genoDist = Evolve.genotypeDist model.population
        alDist = Evolve.alleleDist genoDist
    in flow down
       [ plotGenotypes (300,300) genoDist
       , label alDist genoDist
       ]

update : Action -> Model -> Generator Model
update action model = customGenerator <| \seed ->
    case action of
        Size size' ->
            let populationGen = Evolve.resize size' model.population
                (population', seed') = generate populationGen seed
                model' = { model | population <- population', size <- size' }
            in (model', seed')
        AlleleDist alDist ->
            let populationGen = Evolve.init model.size alDist
                (population', seed') = generate populationGen seed
                model' = { model | population <- population' }
            in (model', seed')
        Fitness fitness' ->
            let model' = { model | fitness <- fitness' }
            in (model', seed)
        Live ->
            let populationGen = Evolve.live model.fitness model.population
                (population', seed') = generate populationGen seed
                model' = { model | population <- population' }
            in (model', seed')

plotGenotypes : (Float, Float) -> GenotypeModel Dist -> Element
plotGenotypes dims dist = Histogram.plot dims
        [ (dist.homdom, green)
        , (dist.hetero, blue)
        , (dist.homrec, red)
        ]

label : AlleleModel Dist -> GenotypeModel Dist -> Element
label alDist genDist = flow down
        [ Markdown.toElement <| "$p$: " ++ (toString <| alDist.dom)
        , Markdown.toElement <| "$q$: " ++ (toString <| alDist.rec)
        , Markdown.toElement <| "$p^2$: " ++ (toString <| genDist.homdom)
        , Markdown.toElement <| "$2pq$: " ++ (toString <| genDist.hetero)
        , Markdown.toElement <| "$q^2$: " ++ (toString <| genDist.homrec)
        ]
