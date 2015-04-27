module Simulation where

import Graphics.Element exposing (Element)
import Random exposing (Generator, Seed, generate, initialSeed)
import Signal exposing (Signal, Mailbox, (<~), (~), foldp, mailbox)
import Signal.Extra as Signal
import Time exposing (Time, every, inMilliseconds, millisecond, timestamp)

import Evolve
import NormalDistribution as Normal
import Population
import TupleUtil as TU

main : Signal Element
main = let time : Signal Time
           time = every (100 * millisecond)
           actionsMB : Mailbox (Population.Action)
           actionsMB = mailbox Population.Live
           actions : Signal Population.Action
           actions = Signal.merge actionsMB.signal ((always Population.Live) <~ time)
           fitness : Evolve.Fitness
           fitness = {survival=0.8, reproductive=(Normal.make 2.0 2.0)}
           fitnessModel : Evolve.GenotypeModel Evolve.Fitness
           fitnessModel = { homdom=fitness, hetero=fitness, homrec=fitness }
           size = 900
           capacity = 1000
           alleleDist = { dom=0.5, rec=0.5 }
           init : Generator Population.Model
           init = Population.init size capacity alleleDist fitnessModel
           model : Signal Population.Model
           model = genFold Population.update init actions
       in Population.view actionsMB.address <~ model

genFold : (a -> b -> Generator b) -> Generator b -> Signal a -> Signal b
genFold f init sig =
    let iteratee : (a -> b -> Generator b) -> (Time, a) -> (b, Result Seed Seed) -> (b, Result Seed Seed)
        iteratee f (t, a) (b, seedRes) = case seedRes of
            Err seed -> TU.mapSnd (always (Ok <| timeSeed t)) <| generate (f a b) seed
            Ok  seed -> TU.mapSnd Ok <| generate (f a b) seed
        init' = TU.mapSnd Err <| generate init (initialSeed 0)
    in fst <~ Signal.foldp (iteratee f) init' (timestamp sig)
    -- Unfortunately, due to unexpected merge behavior on the first element of
    -- the signal, this implementation results in a runtime error
    {-
        init' = generate init << timeSeed << fst
    in fst <~ Signal.foldp' (iteratee f) init' (timestamp sig)
    -}

timeSeed : Time -> Seed
timeSeed = initialSeed << floor << inMilliseconds
