module Shapes exposing (..)

import Html exposing (Html)
import Random
import Array
import Svg
import Svg.Attributes as SvgA
import String


type Shape
    = Poly
    | Star5
    | Image
    | Ellipse
    | Circle
    | Rectangle
    | Unknown


genShape : Random.Generator Shape
genShape =
    let
        availableShape : Array.Array Shape
        availableShape =
            Array.fromList [ Poly, Star5, Image, Ellipse, Circle, Rectangle, Unknown ]
    in
        (Random.int 0 (Array.length availableShape))
            |> Random.map
                (\o ->
                    case (Array.get o (availableShape)) of
                        Just shape ->
                            shape

                        Nothing ->
                            Unknown
                )


shapeToSvg : Shape -> Html msg
shapeToSvg shape =
    case shape of
        Poly ->
            polylineShape

        Star5 ->
            starFiveShape

        Image ->
            imageShape

        Ellipse ->
            ellipseShape

        Circle ->
            circleShape

        Rectangle ->
            rectShape

        Unknown ->
            renderSvgText "Unknown shape"


genRGB : Random.Generator ( Int, Int, Int )
genRGB =
    Random.map3 (,,) (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


genCoord : Float -> Float -> Random.Generator Float
genCoord minFLoat maxFloat =
    Random.float minFLoat maxFloat


genCoordPair : Random.Generator ( Float, Float )
genCoordPair =
    Random.pair (genCoord 0 500) (genCoord 0 500)


genTriangle : Random.Generator ( ( Float, Float ), ( Int, Int, Int ) )
genTriangle =
    Random.map2 (,) (genCoordPair) (genRGB)


triangleBase : Float -> Float -> Int -> Int -> Int -> Html msg
triangleBase xVect yVect rCol gCol bCol =
    let
        xInit =
            50

        yInit =
            80

        rgb =
            [ rCol, gCol, bCol ] |> List.map toString |> String.join ", "

        fillColor =
            String.concat [ "rgb(", rgb, ")" ]

        -- coordinates are generated using rand and xInit / yInit
        ax =
            13 + xVect

        --163
        ay =
            57 + yVect

        a =
            [ ax, ay ] |> List.map toString |> String.join (",")

        -- 287
        bx =
            7 + xVect

        -- 157
        by =
            17 + yVect

        b =
            [ bx, by ] |> List.map toString |> String.join (",")

        -- 247
        cx =
            37 + xVect

        -- 187
        cy =
            10 + yVect

        c =
            [ cx, cy ] |> List.map toString |> String.join (",")

        -- 240
        points =
            [ a, b, c ] |> String.join " "

        -- "163,287 157,247 187,240"
    in
        Svg.polygon
            [ SvgA.fill fillColor
            , SvgA.points points
            ]
            []


renderSvgText : String -> Html msg
renderSvgText m =
    Svg.text_
        [ SvgA.x "0"
        , SvgA.y "50"
        , SvgA.fontFamily "Verdana"
        , SvgA.fontSize "30px"
        , SvgA.stroke "#00ff00"
        , SvgA.fill "#0000ff"
        , SvgA.strokeWidth "1"
        , SvgA.transform "rotate(30, 20 40)"
        ]
        [ Svg.text m ]


polylineShape : Html msg
polylineShape =
    Svg.polyline
        [ SvgA.fill "none"
        , SvgA.stroke "magenta"
        , SvgA.strokeWidth "5"
        , SvgA.points "20,100 40,60 70,80 100,20"
        ]
        []


starFiveShape : Html msg
starFiveShape =
    Svg.polygon
        [ SvgA.points "50,5 20,99 95,39 5,39 80,99"
        , SvgA.fill "lime"
        , SvgA.stroke "purple"
        , SvgA.strokeWidth "5"
        , SvgA.fillRule "nonzero"
        ]
        []


imageShape : Html msg
imageShape =
    Svg.image
        [ SvgA.x "0"
        , SvgA.y "0"

        -- , SvgA.viewBox "0 0 100 100"
        , SvgA.width "100"

        --, SvgA.height "200"
        , SvgA.xlinkHref "http://www.edscuola.it/archivio/interlinea/images/annunziata02.jpg"
        ]
        []


ellipseShape : Html msg
ellipseShape =
    Svg.ellipse
        [ SvgA.cx "60"
        , SvgA.cy "60"
        , SvgA.rx "45"
        , SvgA.ry "30"
        , SvgA.stroke "green"
        , SvgA.strokeWidth "2"
        , SvgA.fill "yellow"
        ]
        []


circleShape : Html msg
circleShape =
    Svg.circle
        [ SvgA.cx "60"
        , SvgA.cy "60"
        , SvgA.r "30"
        , SvgA.stroke "blue"
        , SvgA.strokeWidth "5"
        , SvgA.fill "purple"
        ]
        []


rectShape : Html msg
rectShape =
    Svg.rect
        [ SvgA.x "10"
        , SvgA.y "10"
        , SvgA.width "100"
        , SvgA.height "100"
        , SvgA.rx "15"
        , SvgA.ry "15"
        , SvgA.stroke "green"
        , SvgA.strokeWidth "10"
        , SvgA.fill "blue"
        ]
        []
