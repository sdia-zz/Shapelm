module Genetics exposing (..)

import Color
import Geometry exposing (Point, Polygon, pointInPolygon)


{--


* chromosome definition
* chromosome size
* size of population
* cost function definiton


* probability for new member of mutation rather than breeding
* size of elite
* max number of generations to run

* fraction of
  * elite parents
  * mutated child
  * breed child
  * spontaneous generation


Next generation
* mutation : Chromosome -> params -> Chromosome
* breeding : Chromosome -> Chromosome -> params -> Chromosome
--}


type alias Phenotype =
    { chromosome : List Polygon
    , fitness : Float
    }


type alias Population =
    List Phenotype


blendColor : Color.Color -> Color.Color -> Color.Color
blendColor nextCol background =
    let
        backgroundRGBa =
            Color.toRgb background

        nextColRGBa =
            Color.toRgb nextCol
    in
        --https://amindforeverprogramming.blogspot.de/2013/07/why-alpha-premultiplied-colour-blending.html
        Color.rgba
            (((toFloat nextColRGBa.red) * nextColRGBa.alpha + (1 - nextColRGBa.alpha) * (toFloat backgroundRGBa.red))
                |> round
                |> clamp 0 255
            )
            (((toFloat nextColRGBa.green) * nextColRGBa.alpha + (1 - nextColRGBa.alpha) * (toFloat backgroundRGBa.green))
                |> round
                |> clamp 0 255
            )
            (((toFloat nextColRGBa.blue) * nextColRGBa.alpha + (1 - nextColRGBa.alpha) * (toFloat backgroundRGBa.blue))
                |> round
                |> clamp 0 255
            )
            (nextColRGBa.alpha + (1 - nextColRGBa.alpha) * backgroundRGBa.alpha)



-- background


pixelToRGB : Phenotype -> Point -> Color.Color
pixelToRGB phenotype point =
    phenotype.chromosome
        |> List.filter (pointInPolygon point)
        |> List.map .color
        |> List.foldl blendColor Color.white


fitness : Phenotype -> Float
fitness phenotype =
    let
        resolution =
            List.range 0 100
                |> List.map toFloat

        pointTimeList : List Float -> Float -> List Point
        pointTimeList list x =
            list |> List.map (Point x)

        pixels =
            resolution
                |> List.map (pointTimeList resolution)
                |> List.concat

        -- for each pixel get the Color
        colorPixelsPhenotype =
            pixels
                |> List.map (pixelToRGB phenotype)

        colorPixelBase =
            pixels
                |> List.map baseImage

        distanceMatrix =
            List.map2 distanceRGBA
                colorPixelBase
                colorPixelsPhenotype
    in
        distanceMatrix
            |> List.sum


distanceRGBA : Color.Color -> Color.Color -> Float
distanceRGBA colA colB =
    let
        rgba_A =
            Color.toRgb colA

        rgba_B =
            Color.toRgb colB

        diffCoord =
            [ ((rgba_A.red - rgba_B.red) |> toFloat) / 255.0
            , ((rgba_A.green - rgba_B.green) |> toFloat) / 255.0
            , ((rgba_A.blue - rgba_B.blue) |> toFloat) / 255.0
            , rgba_A.alpha - rgba_B.alpha
            ]
    in
        diffCoord
            |> List.map (\x -> x ^ 2)
            |> List.sum
            |> sqrt


baseImage : Point -> Color.Color
baseImage point =
    let
        black =
            ((point.x <= 50) && (point.y <= 50)) || ((point.x >= 50) && (point.y >= 50))
    in
        if black then
            Color.black
        else
            Color.white



{--
breeding : Phenotype -> Phenotype -> Phenotype


mutatePhenotype : Phenotype -> Phenotype


mutatePolygon : Polygon -> Polygon


mutatePoint : Point -> Point


mutateColor : Color.Color -> Color.Color
--}
