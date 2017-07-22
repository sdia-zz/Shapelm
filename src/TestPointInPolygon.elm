module TestPointInPolygon exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA
import Random
import Svg
import Svg.Attributes as SvgA
import Main exposing (renderPolygon)
import Geometry exposing (Point, Polygon, pointInPolygon)
import Color
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { polygon : Polygon
    , point : Point
    , message : String
    }


trianglePolygon =
    Polygon
        [ Point 250 50
        , Point 10 125
        , Point 50 475
        , Point 375 375
        ]
        (Color.rgba 155 133 256 0.2)
        True


pointInit =
    Point 250 250


init : ( Model, Cmd Msg )
init =
    ( Model trianglePolygon pointInit "Starting..."
    , Cmd.none
    )


renderModel : Model -> Html msg
renderModel model =
    Svg.svg
        [ SvgA.width "500"
        , SvgA.height "500"
        ]
        [ renderPoint model.point
        , renderPolygon model.polygon
        ]


renderPoint : Point -> Html msg
renderPoint point =
    let
        cx =
            toString point.x

        cy =
            toString point.y
    in
        Svg.circle
            [ SvgA.cx cx
            , SvgA.cy cy
            , SvgA.r "5"
            , SvgA.fill "blue"
            ]
            []



-- UPDATE


type Msg
    = Update
    | NewPoint Point


randomPoint : Random.Generator Point
randomPoint =
    Random.map2 Point
        (Random.float 0 500)
        (Random.float 0 500)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            ( model, Random.generate NewPoint randomPoint )

        NewPoint point ->
            let
                test =
                    pointInPolygon model.polygon point

                message =
                    if test then
                        "In"
                    else
                        "Out"
            in
                ( { model | point = point, message = message }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ HtmlA.class "mx-auto border gray m1 p1" ]
            [ renderModel model ]
        , Html.button
            [ HtmlA.class "btn btn-primary m1"
            , onClick Update
            ]
            [ text "Update" ]
        , text model.message
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
