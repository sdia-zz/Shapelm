module Images exposing (..)

import Color


-- the source image is a 100x100 pixels


pixelFromBaseImage : Int -> Int -> Color.Color
pixelFromBaseImage w h =
    let
        black =
            ((w <= 50) && (h <= 50)) || ((w >= 50) && (h >= 50))
    in
        if black then
            Color.black
        else
            Color.white


colorDistance : Color.Color -> List Color.Color -> Float
colorDistance target colors_source =
    let
        r_source =
            colors_source
                |> List.map Color.toRgb
                |> List.map (\x -> (toFloat x.red) * x.alpha)
                |> List.foldl (+) 255

        g_source =
            colors_source
                |> List.map Color.toRgb
                |> List.map (\x -> (toFloat x.green) * x.alpha)
                |> List.foldl (+) 255

        b_source =
            colors_source
                |> List.map Color.toRgb
                |> List.map (\x -> (toFloat x.blue) * x.alpha)
                |> List.foldl (+) 255

        f_target =
            Color.toRgb target |> .alpha

        r_target =
            target
                |> Color.toRgb
                |> .red
                |> toFloat
                |> (*) f_target

        g_target =
            target
                |> Color.toRgb
                |> .green
                |> toFloat
                |> (*) f_target

        b_target =
            target
                |> Color.toRgb
                |> .blue
                |> toFloat
                |> (*) f_target
    in
        [ (r_source - r_target)
        , (g_source - g_target)
        , (b_source - b_target)
        ]
            |> List.map (\x -> x ^ 2)
            |> List.foldl (+) 0
            |> sqrt
