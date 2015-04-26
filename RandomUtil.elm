module RandomUtil (seedGen, divideGen, chooseGen, avgIntGen) where

import Random exposing
        ( Generator
        , Seed
        , customGenerator
        , generate
        , initialSeed
        )
import Random.Extra as Random

seedGen : Generator Seed
seedGen = Random.map initialSeed <| Random.int Random.minInt Random.maxInt

divideGen : Float -> List a -> Generator (List a, List a)
divideGen bias l = customGenerator <| \seed ->
    let iteratee : a -> ((List a, List a), Seed) -> ((List a, List a), Seed)
        iteratee a ((xs, ys), seed) =
            let (n, seed') = generate (Random.float 0 1) seed
                acc' = if n < bias then (a :: xs, ys) else (xs, a :: ys)
            in (acc', seed')
    in List.foldr iteratee (([], []), seed) l

chooseGen : Float -> (a,a) -> Generator a
chooseGen bias (a,b) =
    let pick n = if n < bias then a else b
    in Random.map pick (Random.float 0 1)

boolGen : Float -> Generator Bool
boolGen bias = chooseGen bias (True, False)

avgIntGen : Float -> Float -> Generator Int
avgIntGen a b =
    let avg = (a + b / 2)
        diff = avg - (toFloat <| floor avg)
        intRound n = if n < diff then floor avg else ceiling avg
    in Random.map intRound (Random.float 0 1)
