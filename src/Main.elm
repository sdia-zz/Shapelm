module Main exposing (..)

import Random
import Array
import Maybe
import Color
import Dict
import Html exposing (Html, div, text, button, program)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlA
import Svg
import Svg.Attributes as SvgA
import Geometry


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { shapes : List Geometry.Polygon
    }


init : ( Model, Cmd Msg )
init =
    ( { shapes = [] }, Cmd.none )


type Msg
    = GetPolygon
    | NewData (List Float)


randomFloat32 : Random.Generator (List Float)
randomFloat32 =
    Random.list 32 (Random.float 0 1)


fetchData : Array.Array Float -> Float -> Int -> Float
fetchData arrayData default index =
    Maybe.withDefault default (Array.get index arrayData)


maxPolygonSize =
    6


scaleFrame =
    { width = 160
    , height = 200
    }


mainFrame =
    { width = 425
    , height = 550
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPolygon ->
            ( model, Random.generate NewData randomFloat32 )

        NewData data ->
            let
                arrayData =
                    Array.fromList data

                polygonSize =
                    (0 |> fetchData arrayData 0.0)
                        |> (*) 10
                        |> round
                        |> (+) 1
                        |> clamp 3 maxPolygonSize

                -- |> (+) 1
                polygonPoints =
                    List.map2 Geometry.Point
                        (Array.slice 1 (polygonSize + 1) arrayData |> Array.toList)
                        (Array.slice (polygonSize + 1) (2 * polygonSize + 1) arrayData |> Array.toList)

                col =
                    Color.rgba
                        ((2 * polygonSize + 2 |> fetchData arrayData 0.0) |> (*) 255 |> round)
                        ((2 * polygonSize + 3 |> fetchData arrayData 0.0) |> (*) 255 |> round)
                        ((2 * polygonSize + 4 |> fetchData arrayData 0.0) |> (*) 255 |> round)
                        ((2 * polygonSize + 5 |> fetchData arrayData 0.0))

                framePositionFinalVector =
                    Geometry.Vector
                        ((2 * polygonSize + 6 |> fetchData arrayData 0.0) |> (*) mainFrame.width)
                        ((2 * polygonSize + 7 |> fetchData arrayData 0.0) |> (*) mainFrame.height)

                polygon =
                    Geometry.Polygon polygonPoints col (Geometry.isConvex polygonPoints)
                        |> Geometry.normalizePolygon scaleFrame.width scaleFrame.height
                        |> Geometry.movePolygon framePositionFinalVector
            in
                if polygon.isConvex then
                    ( { model
                        | shapes = List.append model.shapes (List.singleton polygon)
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ HtmlA.class "mx-auto border gray m1 p1" ]
            [ renderModel model ]

        -- [ text "Hi" ]
        , Html.button
            [ HtmlA.class "btn btn-primary m1"

            -- , onClick GetShape
            , onClick GetPolygon
            ]
            [ text "add triangle" ]
        ]


pointToString : Geometry.Point -> String
pointToString point =
    [ point.x, point.y ]
        |> List.map toString
        |> String.join ","


renderPolygon : Geometry.Polygon -> Html msg
renderPolygon polygon =
    let
        renderPoints =
            polygon.points
                |> List.map pointToString
                |> String.join " "

        colorDict =
            polygon.col
                |> Color.toRgb

        colorJoin =
            [ toString colorDict.red
            , toString colorDict.green
            , toString colorDict.blue
            , toString colorDict.alpha
            ]
                |> String.join ", "

        renderColor =
            "rgba("
                ++ colorJoin
                ++ ")"
    in
        Svg.polygon
            [ SvgA.fill renderColor
            , SvgA.points renderPoints
            ]
            []


renderModel : Model -> Html msg
renderModel model =
    let
        frameWidth =
            toString mainFrame.width

        frameHeight =
            toString mainFrame.height

        shapesSVG =
            model.shapes |> List.map renderPolygon
    in
        Svg.svg
            [ SvgA.width frameWidth
            , SvgA.height frameHeight
            ]
            shapesSVG
