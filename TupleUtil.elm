module TupleUtil (dimap, dup, mapFst, mapSnd) where

dup : a -> (a, a)
dup a = (a, a)

dimap : (a -> c) -> (b -> d) -> (a,b) -> (c,d)
dimap f g (a,b) = (f a, g b)

mapFst : (a -> c) -> (a,b) -> (c,b)
mapFst f = dimap f identity

mapSnd : (b -> c) -> (a,b) -> (a,c)
mapSnd g = dimap identity g
