import Graphics.Element exposing (Element, show)
import Time exposing (every, second)
import Signal exposing ((<~), merge)

type MyADT = Value1 | Value2

showMyADT : MyADT -> String
showMyADT adt = case adt of
        Value1 -> "Value 1"
        Value2 -> "Value 2"

main : Signal Element
main =
    let value = Value1
        fun = showMyADT
        sig = (always value) <~ every second
    in show << fun <~ merge sig sig
