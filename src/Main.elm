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
import Time exposing (Time, second, millisecond)
import Genetics exposing (Phenotype)


populationSize =
    10


chromosomeSize =
    -- # polygons
    10


maxPolygonSize =
    3


scaleFrame =
    { width = 32
    , height = 40
    }


mainFrame =
    { width = 100
    , height = 100
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (500 * millisecond) Tick


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { generation : List Phenotype
    , data : List (List Float)
    }


init : ( Model, Cmd Msg )
init =
    ( { generation = [], data = [] }, Cmd.none )


type Msg
    = NewData (List (List Float))
    | Tick Time


randomFloat32 : Random.Generator (List Float)
randomFloat32 =
    Random.list 32 (Random.float 0 1)


randomFloatMatrix : Random.Generator (List (List Float))
randomFloatMatrix =
    -- 512 X 32
    Random.list 512 randomFloat32


fetchData : Array.Array Float -> Float -> Int -> Float
fetchData arrayData default index =
    Maybe.withDefault default (Array.get index arrayData)


data32ToPolygon : List Float -> Geometry.Polygon
data32ToPolygon data =
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
    in
        Geometry.Polygon polygonPoints col (Geometry.isConvex polygonPoints)
            |> Geometry.normalizePolygon scaleFrame.width scaleFrame.height
            |> Geometry.movePolygon framePositionFinalVector


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model, Random.generate NewData randomFloatMatrix )

        NewData dataMat ->
            let
                phenotypeInit =
                    Phenotype
                        (dataMat
                            |> List.take chromosomeSize
                            |> List.map data32ToPolygon
                        )
                        0.0

                phenotypeFit =
                    Phenotype
                        (phenotypeInit.chromosome)
                        (Genetics.fitness phenotypeInit)

                ----------------------------------------------------------------
                randomPoly =
                    -- breeding ?
                    dataMat
                        |> List.take 1
                        |> List.map data32ToPolygon

                breeded =
                    model.generation
                        |> List.map .chromosome
                        |> List.map (List.append randomPoly)
                        |> List.map (\x -> Phenotype x 0.0)
                        |> List.map (\x -> Phenotype x.chromosome (Genetics.fitness x))

                newGeneration =
                    List.append breeded (List.singleton phenotypeFit)

                ----------------------------------------------------------------
                generation =
                    List.append model.generation newGeneration
                        |> List.sortBy (\x -> x.fitness)
                        |> List.take populationSize
            in
                ( { model
                    | generation = generation
                    , data = dataMat
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ HtmlA.class "mx-auto border gray m1 p1" ]
            [ renderModel model ]
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
            polygon.color
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

        polygonSVG =
            model.generation
                |> List.head
                |> Maybe.withDefault (Phenotype [] 0.0)
                |> .chromosome
                |> List.map renderPolygon
    in
        Svg.svg
            [ SvgA.width frameWidth
            , SvgA.height frameHeight
            ]
            polygonSVG
