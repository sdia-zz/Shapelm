module Main exposing (..)

import Random
import Html exposing (Html, div, text, button, program)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlA
import Svg
import Svg.Attributes as SvgA
import Shapes


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
    { shapes : List Shapes.Shape
    , x : Float
    , y : Float
    , r : Int
    , g : Int
    , b : Int
    }


init : ( Model, Cmd Msg )
init =
    -- ( { shapes = [] }, Cmd.none )
    ( { shapes = [], x = 0, y = 0, r = 0, g = 0, b = 0 }, Cmd.none )


type Msg
    = GetShape
    | NewShape Shapes.Shape
    | GetTriangle
    | NewTriangle ( ( Float, Float ), ( Int, Int, Int ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetShape ->
            ( model, Random.generate NewShape Shapes.genShape )

        NewShape shape ->
            let
                newShapes =
                    List.append model.shapes (List.singleton shape)
            in
                ( { model | shapes = newShapes }, Cmd.none )

        GetTriangle ->
            ( model, Random.generate NewTriangle Shapes.genTriangle )

        NewTriangle ( ( x, y ), ( r, g, b ) ) ->
            ( { model | x = x, y = y, r = r, g = g, b = b }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ HtmlA.class "mx-auto border gray m1 p1" ]
            [ renderModel model ]
        , Html.button
            [ HtmlA.class "btn btn-primary m1"

            -- , onClick GetShape
            , onClick GetTriangle
            ]
            [ text "add shape" ]
        ]


renderModel : Model -> Html msg
renderModel model =
    let
        shapesSVG =
            model.shapes |> List.map Shapes.shapeToSvg
    in
        Svg.svg
            [ SvgA.width "300"
            , SvgA.height "485"
            , SvgA.viewBox "0 0 500 500"
            ]
            -- shapesSVG
            -- [ Shapes.triangleBase ]
            [ Shapes.triangleBase model.x model.y model.r model.g model.b ]
