module Main exposing (..)

import Html exposing (Html, div, text, button, program)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlA
import Svg
import Svg.Attributes as SvgA
import Random
import Array


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
    Shape


init : ( Model, Cmd Msg )
init =
    ( Poly, Cmd.none )


type Msg
    = GetShape
    | NewShape Shape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetShape ->
            ( model, Random.generate NewShape genShape )

        NewShape shape ->
            ( shape, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ HtmlA.class "mx-auto border gray m1 p1" ]
            [ Svg.svg
                [ SvgA.width "300"
                , SvgA.height "485"
                , SvgA.viewBox "0 0 500 500"
                ]
                [ renderModel model ]
            ]
        , Html.button
            [ HtmlA.class "btn btn-primary m1"
            , onClick GetShape
            ]
            [ text "New shape" ]
        ]


renderModel : Model -> Html msg
renderModel model =
    case model of
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



-- about random generator


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



-- Some basic shapes


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
