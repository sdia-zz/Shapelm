module Geometry exposing (..)

import Color
import Maybe
import Array


type alias Point =
    { x : Float
    , y : Float
    }


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Polygon =
    { points : List Point
    , col : Color.Color
    , isConvex : Bool
    }


type alias Edge =
    { start : Point
    , end : Point
    }


type Orientation
    = Clockwise
    | CounterClockwise
    | Null


toEdges : List Point -> List Edge
toEdges points =
    let
        numberNodes =
            List.length points

        arrayPoints =
            Array.fromList points

        firstPointArray =
            arrayPoints
                |> Array.slice 0 1

        pointsNext =
            (Array.append arrayPoints firstPointArray)
                |> Array.slice 1 (numberNodes + 1)
                |> Array.toList
    in
        List.map2 Edge
            points
            pointsNext


pointInPolygon : Polygon -> Point -> Bool
pointInPolygon polygon point =
    -- http://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/
    let
        infinity =
            10000

        pointExtended =
            Edge point (Point infinity point.y)

        isOdd n =
            (n % 2) == 1

        validateEdge edge =
            let
                p1 =
                    edge.start.x

                q1 =
                    edge.start.y

                p2 =
                    edge.end.x

                q2 =
                    edge.end.y
            in
                (not ((p1 < point.x) && (p2 < point.x)))
                    && ((q1 - point.y) * (q2 - point.y) < 0)
    in
        polygon.points
            -- filter out points on y = point.y
            |> List.filter (\o -> o.y /= point.y)
            |> toEdges
            |> List.filter validateEdge
            |> List.filter (edgeIntercept pointExtended)
            |> List.length
            |> isOdd


vectorsOrientation : Vector -> Vector -> Orientation
vectorsOrientation u v =
    let
        cp =
            crossProduct u v
    in
        if cp > 0 then
            CounterClockwise
        else if cp < 0 then
            Clockwise
        else
            Null


pointsOrientation : Point -> Point -> Point -> Orientation
pointsOrientation x y z =
    let
        u =
            toVector x y

        v =
            toVector y z
    in
        vectorsOrientation u v


edgeIntercept : Edge -> Edge -> Bool
edgeIntercept edge1 edge2 =
    -- http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
    let
        p1 =
            edge1.start

        q1 =
            edge1.end

        p2 =
            edge2.start

        q2 =
            edge2.end
    in
        -- no need to check the colinearity condition
        ((pointsOrientation p1 q1 p2)
            /= (pointsOrientation p1 q1 q2)
        )
            && ((pointsOrientation p2 q2 p1)
                    /= (pointsOrientation p2 q2 q1)
               )


comparePoint : Point -> Point -> Order
comparePoint a b =
    let
        cmp =
            if (a.x == b.x) then
                a.y - b.y
            else
                a.x - b.x
    in
        if cmp == 0 then
            EQ
        else if cmp >= 0 then
            GT
        else
            LT


toVector : Point -> Point -> Vector
toVector u v =
    Vector
        (v.x - u.x)
        (v.y - u.y)


crossProduct : Vector -> Vector -> Float
crossProduct u v =
    (u.x * v.y) - (u.y * v.x)


dotProduct : Vector -> Vector -> Float
dotProduct u v =
    (u.x * v.x) + (u.y * v.y)


scalePoint : Vector -> Point -> Point
scalePoint vector point =
    { point
        | x = point.x * vector.x
        , y = point.y * vector.y
    }


movePoint : Vector -> Point -> Point
movePoint vector point =
    { point
        | x = point.x + vector.x
        , y = point.y + vector.y
    }


scalePolygon : Vector -> Polygon -> Polygon
scalePolygon vector polygon =
    { polygon
        | points =
            polygon.points
                |> List.map (scalePoint vector)
    }


movePolygon : Vector -> Polygon -> Polygon
movePolygon vector polygon =
    { polygon
        | points =
            polygon.points
                |> List.map (movePoint vector)
    }


convexHull : List Point -> List Point
convexHull points =
    let
        pointsSorted =
            List.sortWith comparePoint points

        --@TODO remove duplicated points
        a =
            []
    in
        []


isConvex : List Point -> Bool
isConvex points =
    {--@TODO: future tests
    > points = [Point 0 0, Point 1 0, Point 1 1, Point 0.5 -0.5, Point 0 1]
    > isConvex points
      False

    > points = [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
    > isConvex points
      True
    --}
    let
        numberNodes =
            List.length points

        arrayPoints =
            Array.fromList points

        firstPointArray =
            arrayPoints
                |> Array.slice 0 1

        pointsNext =
            (Array.append arrayPoints firstPointArray)
                |> Array.slice 1 (numberNodes + 1)
                |> Array.toList

        edges =
            List.map2 toVector
                points
                pointsNext

        arrayEdges =
            Array.fromList edges

        firstArrayEdge =
            arrayEdges |> Array.slice 0 1

        edgesNext =
            (Array.append arrayEdges firstArrayEdge)
                |> Array.slice 1 (numberNodes + 1)
                |> Array.toList

        crossProducts =
            List.map2 crossProduct
                edges
                edgesNext
    in
        (List.all (\x -> (x >= 0)) crossProducts)
            || (List.all (\x -> (x <= 0)) crossProducts)


normalizePolygon : Float -> Float -> Polygon -> Polygon
normalizePolygon scaleX scaleY polygon =
    let
        minX =
            polygon.points
                |> List.map .x
                |> List.minimum
                |> Maybe.withDefault 0.0

        maxX =
            polygon.points
                |> List.map .x
                |> List.maximum
                |> Maybe.withDefault 0.0

        minY =
            polygon.points
                |> List.map .y
                |> List.minimum
                |> Maybe.withDefault 0.0

        maxY =
            polygon.points
                |> List.map .y
                |> List.maximum
                |> Maybe.withDefault 0.0

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
        polygon
            |> movePolygon translateVector
            |> scalePolygon scaleVector
