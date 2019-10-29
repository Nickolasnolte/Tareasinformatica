module Main exposing (..)

import Browser
import Round
import Html exposing (..)
import Html.Attributes as Hta
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Sva

type alias Punto =
    { x : Float, y : Float }

listaDePuntosAListaDeFloats : List Punto -> List (Float, Float)
listaDePuntosAListaDeFloats list =
    case list of
        [] -> []
        punto :: resto -> ( punto.x, punto.y ) :: listaDePuntosAListaDeFloats resto

listFloatsToPunto : List (Float, Float) -> List Punto
listFloatsToPunto list =
    case list of
        [] -> []
        (a,b) :: abs -> (Punto a b) :: listFloatsToPunto (abs)

puntoAstring : (Float, Float) -> String
puntoAstring point =
    case point of
        ( a, b ) -> String.fromFloat a ++ "," ++ String.fromFloat b

puntosAstring : List (Float, Float) -> String
puntosAstring list =
    case list of
        [] -> ""
        (x, y) :: xys -> puntoAstring (x, y) ++ " " ++ puntosAstring xys

firstElementOfList : List (Float, Float) -> (Float, Float)
firstElementOfList list =
    case list of
        [] -> (0, 0)
        x :: xs -> x

listWithFirstElement : List (Float, Float) -> List (Float, Float)
listWithFirstElement list =
    list ++ [ firstElementOfList list ]

noDuplicados : List a -> List a
noDuplicados list =
    case list of
        [] -> []

        [x] -> [x]

        x :: y :: resto ->
            if x == y then
                noDuplicados (x :: resto)
            else
                x :: noDuplicados (y :: resto)

removerPrimero : List a -> List a
removerPrimero list =
    case list of
        [] -> []
        [x] -> []
        x :: xs -> xs

pop : List a -> List a
pop list = List.reverse list |> removerPrimero |> List.reverse

round : Float -> Float 
round x = Round.round 2 x |> String.toFloat |> Maybe.withDefault 0

stringToInt : String -> Int
stringToInt str = String.toInt str |> Maybe.withDefault 0 

listaRounded : List (Float, Float) -> List (Float, Float)
listaRounded list =
    case list of
        [] -> []
        (x, y) :: xys -> (round x, round y) :: listaRounded xys

listOflistaRoundeds : List (List (Float, Float)) -> List (List (Float, Float))
listOflistaRoundeds list =
    case list of
        [] -> []
        x :: xs ->listaRounded x :: listOflistaRoundeds xs

listOfListsToPolylines : Int -> List (List (Float, Float)) -> List (Svg.Svg msg)
listOfListsToPolylines num list =
    if num == 1 then
        case list of
            [] -> []
            x :: xs -> polyline [ Sva.points (puntosAstring (listWithFirstElement x)), Sva.fill "red", Sva.stroke "black", Sva.strokeWidth "0.5" ] [] :: listOfListsToPolylines 1 xs
    else
        case list of
            [] -> []
            x :: xs -> polyline [ Sva.points (puntosAstring (listWithFirstElement x)), Sva.fill "red", Sva.stroke "black", Sva.strokeWidth "0.5" ] [] :: listOfListsToPolylines 2 xs

length = 300

p1 = Punto ((500 - length) / 2) 135 -- (100,135)
p2 = Punto ((500 - length) * 2) 135 -- (400,135)
p3 = Punto (((p2.x - p1.x) / 2) + p1.x) (p1.y + ((p2.x - p1.x) * sin (pi / 3))) -- (250,394.81)

t1 = Punto ((500 - length) / 2) 350 -- (100,350)

sumar : Punto -> Punto -> Punto
sumar a b = Punto (a.x + b.x) (a.y + b.y)

multiplicar : Float -> Punto -> Punto
multiplicar num point = Punto (num * point.x) (num * point.y)

restar : Punto -> Punto -> Punto
restar a b = multiplicar -1 b |> sumar a

hallarpuntos : Punto -> Punto -> List (Punto)
hallarpuntos punto1 punto2 =
    let
        distanciaXY = restar punto2 punto1
    in
        [
            punto1
            , sumar punto1 <| multiplicar (1 / 3) distanciaXY
            , sumar punto1 <| sumar (multiplicar (1 / 2) distanciaXY) (multiplicar (sqrt 3 / 6) <| Punto distanciaXY.y (-1 * distanciaXY.x))
            , sumar punto1 <| multiplicar (2 / 3) distanciaXY
            , punto2
        ]
            
curvaKoch : Punto -> Punto -> List (Float, Float)
curvaKoch punto1 punto2 =
    hallarpuntos punto1 punto2 |> listaDePuntosAListaDeFloats

aplicarCurvaKoch : List Punto -> List (Float, Float)
aplicarCurvaKoch list =
    case list of
        [] -> []
        [x] -> listaDePuntosAListaDeFloats [x]
        x :: y :: xys -> (curvaKoch x y) ++ aplicarCurvaKoch (y :: xys)

listaDePuntos : Punto -> Punto -> List (Float, Float)
listaDePuntos punto1 punto2 =
    (listaDePuntosAListaDeFloats [punto1]) ++ (listaDePuntosAListaDeFloats [punto2])

snowflake : Int -> List (Float, Float)
snowflake num =
    if num < 0 then []
    else if num == 0 then ((listaDePuntos p1 p2) ++ (listaDePuntos p2 p3) ++ (listaDePuntos p3 p1)) |> noDuplicados |> listaRounded
    else (snowflake (num - 1) |> listFloatsToPunto |> aplicarCurvaKoch) |> listaRounded |> pop


triangulo : Float -> Float -> Float -> List (Float, Float)
triangulo x y len = 
    case (x, y) of
        (a, b) -> [(a, b), (a + len, b), (a + len / 2, b - (len * sin (pi / 3)))]

dividirTriangulo : Float -> Float -> Float -> Int -> Int -> List (List (Float, Float))
dividirTriangulo x y len lvl max =
    if lvl == max then
        [triangulo x y len]

    else
        dividirTriangulo x y (len / 2) (lvl + 1) max
            ++ dividirTriangulo (x + len / 2) y (len / 2) (lvl + 1) max
            ++ dividirTriangulo (x + len / 4) (y - sin (pi / 3) * len / 2) (len / 2) (lvl + 1) max

sierpinski : Int -> List (List (Float, Float))
sierpinski num =
    dividirTriangulo t1.x t1.y length 0 num |> listOflistaRoundeds


type alias Model =
    { iteracionCopodeNieve : Int, puntosCopodeNieve : String, iteracionTriangulo : Int, puntosTriangulo : List (List (Float, Float)) }

type Msg
    = AumentarCopo
    | ReducirCopo
    | ReiniciarCopo
    | AumentarTriangulo
    | ReducirTriangulo
    | ReiniciarTriangulo

inicio : Model
inicio = Model 0 (puntosAstring (listWithFirstElement (snowflake 0))) 0 (sierpinski 0)

controlador : Msg -> Model -> Model
controlador msg model =
    case msg of
        AumentarCopo -> { model | iteracionCopodeNieve = model.iteracionCopodeNieve + 1, puntosCopodeNieve = puntosAstring (listWithFirstElement (snowflake (model.iteracionCopodeNieve + 1))) }

        ReducirCopo ->
            if model.iteracionCopodeNieve == 0 then { model | iteracionCopodeNieve = 0, puntosCopodeNieve = inicio.puntosCopodeNieve }
            else { model | iteracionCopodeNieve = model.iteracionCopodeNieve - 1, puntosCopodeNieve = puntosAstring (listWithFirstElement (snowflake (model.iteracionCopodeNieve - 1))) }

        ReiniciarCopo -> { model | iteracionCopodeNieve = inicio.iteracionCopodeNieve, puntosCopodeNieve = inicio.puntosCopodeNieve }

        AumentarTriangulo -> { model | iteracionTriangulo = model.iteracionTriangulo + 1, puntosTriangulo = sierpinski (model.iteracionTriangulo + 1) }

        ReducirTriangulo ->
            if model.iteracionTriangulo == 0 then { model | iteracionTriangulo = 0, puntosTriangulo = inicio.puntosTriangulo }
            else { model | iteracionTriangulo = model.iteracionTriangulo - 1, puntosTriangulo = sierpinski (model.iteracionTriangulo - 1) }

        ReiniciarTriangulo -> { model | iteracionTriangulo = 0, puntosTriangulo = inicio.puntosTriangulo }


vista : Model -> Html Msg
vista model =
    div []
            [
                div []
                [ 
                    svg [ Sva.viewBox "0 0 500 500", Sva.width "500", Sva.height "500" ]
                    [ 
                        polyline [ Sva.points model.puntosCopodeNieve, Sva.fill "red", Sva.stroke "black", Sva.strokeWidth "5" ] [] 
                    ]
                    , div [] [ Html.text ("Iteración: " ++ String.fromInt model.iteracionCopodeNieve) ]
                    , div []
                    [ 
                        button [ onClick ReducirCopo ] [ Html.text "-" ]
                        , button [ onClick AumentarCopo ] [ Html.text "+" ]
                        , button [ onClick ReiniciarCopo ] [ Html.text "Reiniciar Fractal" ]
                    ]
                ]
                , div []
                [
                    svg [ Sva.viewBox "0 0 500 500", Sva.width "500", Sva.height "500", Sva.class "svgWindow" ]
                        (listOfListsToPolylines 2 model.puntosTriangulo)
                    , div [] [ Html.text ("Iteración: " ++ String.fromInt model.iteracionTriangulo) ]
                    , div []
                    [
                        button [ onClick ReducirTriangulo, Hta.class "fractalButton" ] [ Html.text "-" ]
                        , button [ onClick AumentarTriangulo, Hta.class "fractalButton" ] [ Html.text "+" ]
                        , button [ onClick ReiniciarTriangulo, Hta.class "fractalButton" ] [ Html.text "Reiniciar Fractal" ]
                    ]
                ]
            ]

main =
    Browser.sandbox
        { init = inicio, view = vista, update = controlador }