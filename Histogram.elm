module Histogram (plot) where

import Color exposing (Color)
import Graphics.Collage exposing (Form, collage, filled, move, rect)
import Graphics.Element exposing (Element)

plot : (Float, Float) -> List (Float, Color) -> Element
plot (w,h) bins =
    let binWidth = w / (toFloat <| List.length bins)
        bar : Float -> Color -> Float -> Form
        bar relHeight col xpos =
            let binHeight = h * relHeight
                hshift = (-h / 2) + (binHeight / 2)
                wshift = (-w / 2) + xpos + (binWidth / 2)
            in move (wshift, hshift)  <| filled col <| rect binWidth binHeight
        iteratee : (Float, Color) -> Float -> (Form, Float)
        iteratee (relHeight, col) xpos =
            let xpos' = xpos + binWidth
                bin = bar relHeight col xpos
            in (bin, xpos')
    in collage (ceiling w) (ceiling h) <| accum iteratee 0 bins

accum : (a -> b -> (c, b)) -> b -> List a -> List c
accum f init =
    let iteratee a (b, cs) =
            let (c', b') = f a b
            in (b', c' :: cs)
    in List.reverse << snd << List.foldr iteratee (init, [])
