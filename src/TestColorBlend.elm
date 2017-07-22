module TestColorBlend exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA
import Random
import Svg
import Svg.Attributes as SvgA
import Main exposing (renderPolygon)
import Geometry exposing (Point, Polygon, pointInPolygon)
import Color
import Html.Events exposing (onClick)
import Genetics exposing (Phenotype, pixelToRGB)


-- MODEL


type alias Model =
    { phenotype : Phenotype
    , blend : Color.Color
    }


phenoInit =
    Phenotype
        []
        0.0


init : ( Model, Cmd Msg )
init =
    ( Model phenoInit Color.white
    , Cmd.none
    )


colorToSvgString : Color.Color -> String
colorToSvgString color =
    let
        colorDict =
            color
                |> Color.toRgb

        colorJoin =
            [ toString colorDict.red
            , toString colorDict.green
            , toString colorDict.blue
            , toString colorDict.alpha
            ]
                |> String.join ", "
    in
        "rgba("
            ++ colorJoin
            ++ ")"


renderModel : Model -> Html msg
renderModel model =
    let
        polygons =
            model.phenotype.chromosome
                |> List.map renderPolygon

        blend =
            Svg.circle
                [ SvgA.cx "100"
                , SvgA.cy "100"
                , SvgA.r "50"
                , SvgA.fill (colorToSvgString model.blend)
                ]
                []
    in
        Svg.svg
            [ SvgA.width "500"
            , SvgA.height "500"
            ]
            (blend :: polygons)



-- UPDATE


type Msg
    = Update
    | NewColor Color.Color


randomColor : Random.Generator Color.Color
randomColor =
    Random.map4 Color.rgba
        (Random.int 0 250)
        (Random.int 0 250)
        (Random.int 0 250)
        (Random.float 0 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            ( model, Random.generate NewColor randomColor )

        NewColor colorNew ->
            let
                polygon =
                    Polygon
                        [ (Point 50 0), (Point 0 75), (Point 75 75) ]
                        colorNew
                        True

                chromosome =
                    List.append model.phenotype.chromosome (List.singleton polygon)

                phenotype =
                    Genetics.Phenotype chromosome 0.0

                blend =
                    pixelToRGB phenotype (Point 50 50)
            in
                ( { model
                    | phenotype = phenotype
                    , blend = blend
                  }
                , Cmd.none
                )



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
