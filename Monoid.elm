module Monoid
    ( Monoid
    , mkMonoid
    , foldMap
    , tupleMonoid
    , Avg
    , avgMonoid
    , avgPoint
    , runAvg
    ) where

type alias Monoid a = { mempty : a, mappend : a -> a -> a }
mkMonoid : a -> (a -> a -> a) -> Monoid a
mkMonoid mempty mappend = { mempty=mempty, mappend=mappend }

foldMap : Monoid b -> (a -> b) -> List a -> b
foldMap monoid f = List.foldr (monoid.mappend << f) monoid.mempty

-- Tuples ---------------------------------------------------------------------

tupleMonoid : Monoid a -> Monoid b -> Monoid (a, b)
tupleMonoid ma mb =
    let tupleMempty = (ma.mempty, mb.mempty)
        tupleMappend (a, b) (a', b') = (ma.mappend a a', mb.mappend b b')
    in mkMonoid tupleMempty tupleMappend

-- Averages -------------------------------------------------------------------

type alias Avg = { running : Float, count : Int }

avgMonoid : Monoid Avg
avgMonoid =
    let avgMempty = {running=0, count=0}
        avgMappend a b =
            let running' = a.running + b.running
                count' = a.count + b.count
            in { running=running', count=count' }
    in mkMonoid avgMempty avgMappend

avgPoint : Float -> Avg
avgPoint n = { running=n, count=1 }

runAvg : Avg -> Float
runAvg {running, count} = running / (toFloat count)
