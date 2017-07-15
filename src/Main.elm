module Main exposing (..)

import Random
import Array
import Maybe
import Color
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
    { shapes : List Geometry.Triangle
    }


init : ( Model, Cmd Msg )
init =
    ( { shapes = [] }, Cmd.none )


type Msg
    = GetTriangle
    | NewData (List Float)


randomFloat32 : Random.Generator (List Float)
randomFloat32 =
    Random.list 32 (Random.float 0 1)


fetchData : Array.Array Float -> Float -> Int -> Float
fetchData arrayData default index =
    Maybe.withDefault default (Array.get index arrayData)


scaleFrame =
    { width = 42
    , height = 55
    }


mainFrame =
    { width = 425
    , height = 550
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTriangle ->
            ( model, Random.generate NewData randomFloat32 )

        NewData data ->
            let
                arrayData =
                    Array.fromList data

                a =
                    Geometry.Point
                        (0 |> fetchData arrayData 0.0)
                        (1 |> fetchData arrayData 0.0)

                b =
                    Geometry.Point
                        (2 |> fetchData arrayData 0.0)
                        (3 |> fetchData arrayData 0.0)

                c =
                    Geometry.Point
                        (4 |> fetchData arrayData 0.0)
                        (5 |> fetchData arrayData 0.0)

                col =
                    Color.rgb
                        ((6 |> fetchData arrayData 0.0) |> (*) 255 |> round)
                        ((7 |> fetchData arrayData 0.0) |> (*) 255 |> round)
                        ((8 |> fetchData arrayData 0.0) |> (*) 255 |> round)

                framePositionFinalVector =
                    Geometry.Vector
                        ((9 |> fetchData arrayData 0.0) |> (*) mainFrame.width)
                        ((10 |> fetchData arrayData 0.0) |> (*) mainFrame.height)

                triangle =
                    Geometry.Triangle a b c col
                        |> Geometry.normalizeTriangle scaleFrame.width scaleFrame.height
                        |> Geometry.translateTriangle framePositionFinalVector
            in
                ( { model
                    | shapes = List.append model.shapes (List.singleton triangle)
                  }
                , Cmd.none
                )


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
            , onClick GetTriangle
            ]
            [ text "add triangle" ]
        ]


renderTriangle : Geometry.Triangle -> Html msg
renderTriangle triangle =
    let
        a =
            [ triangle.a.x, triangle.a.y ]
                |> List.map toString
                |> String.join ","

        b =
            [ triangle.b.x, triangle.b.y ]
                |> List.map toString
                |> String.join ","

        c =
            [ triangle.c.x, triangle.c.y ]
                |> List.map toString
                |> String.join ","

        points =
            [ a, b, c ] |> String.join " "

        colorDict =
            Color.toRgb triangle.col

        colorString =
            [ toString colorDict.red
            , toString colorDict.green
            , toString colorDict.blue
            , toString colorDict.alpha
            ]
                |> String.join ", "

        colors =
            "rgba(" ++ colorString ++ ")"
    in
        Svg.polygon
            [ SvgA.fill colors
            , SvgA.points points
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
            model.shapes |> List.map renderTriangle
    in
        Svg.svg
            [ SvgA.width frameWidth
            , SvgA.height frameHeight
            ]
            shapesSVG
