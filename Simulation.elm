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
           time = every millisecond
           actionsMB : Mailbox (Population.Action)
           actionsMB = mailbox Population.Live
           actions : Signal Population.Action
           actions = Signal.merge actionsMB.signal ((always Population.Live) <~ time)
           fitness : Evolve.Fitness
           fitness = {survival=0.8, reproductive=(Normal.make 2.6 0.3)}
           fitnessModel : Evolve.GenotypeModel Evolve.Fitness
           fitnessModel = { homdom=fitness, hetero=fitness, homrec=fitness }
           init : Generator Population.Model
           init = Population.init 100 { dom=0.5, rec=0.5 } fitnessModel
           model : Signal Population.Model
           model = genFold Population.update init actions
       in Population.view actionsMB.address <~ model

genFold : (a -> b -> Generator b) -> Generator b -> Signal a -> Signal b
genFold f init sig =
    let iteratee : (a -> b -> Generator b) -> (Time, a) -> (b, Seed) -> (b, Seed)
        iteratee f (_, a) (b, seed) = generate (f a b) seed
        init' = generate init << timeSeed << fst
    in fst <~ Signal.foldp' (iteratee f) init' (timestamp sig)

timeSeed : Time -> Seed
timeSeed = initialSeed << floor << inMilliseconds
