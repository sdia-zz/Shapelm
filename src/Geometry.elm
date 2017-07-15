module Geometry exposing (..)

import Color


type alias Point =
    { x : Float
    , y : Float
    }


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Triangle =
    { a : Point
    , b : Point
    , c : Point
    , col : Color.Color
    }


scalePoint : Vector -> Point -> Point
scalePoint vector point =
    { point
        | x = point.x * vector.x
        , y = point.y * vector.y
    }


translatePoint : Vector -> Point -> Point
translatePoint vector point =
    { point
        | x = point.x + vector.x
        , y = point.y + vector.y
    }


scaleTriangle : Vector -> Triangle -> Triangle
scaleTriangle vector triangle =
    { triangle
        | a = triangle.a |> scalePoint vector
        , b = triangle.b |> scalePoint vector
        , c = triangle.c |> scalePoint vector
    }


translateTriangle : Vector -> Triangle -> Triangle
translateTriangle vector triangle =
    { triangle
        | a = triangle.a |> translatePoint vector
        , b = triangle.b |> translatePoint vector
        , c = triangle.c |> translatePoint vector
    }


normalizeTriangle : Float -> Float -> Triangle -> Triangle
normalizeTriangle scaleX scaleY triangle =
    let
        minX =
            min triangle.a.x triangle.b.x |> min triangle.c.x

        maxX =
            max triangle.a.x triangle.b.x |> max triangle.c.x

        minY =
            min triangle.a.y triangle.b.y |> min triangle.c.y

        maxY =
            max triangle.a.y triangle.b.y |> max triangle.c.y

        norm =
            sqrt ((maxX - minX) ^ 2 + (maxY - minY) ^ 2)

        translateVector =
            Vector
                (negate (maxX - minX) / 2)
                (negate (maxY - minY) / 2)

        scaleVector =
            Vector
                (scaleX / norm)
                (scaleY / norm)
    in
        triangle
            |> translateTriangle translateVector
            |> scaleTriangle scaleVector
