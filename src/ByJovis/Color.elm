module ByJovis.Color exposing (linearInterpolation)

import Browser

-- import Color as C
-- import Color.Interpolate as CI

import Element exposing (..)
import Element.Background as Background

import Html exposing (Html)

import Round


 -- MAIN


main = Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



 -- MODEL


type alias Model =
    { start : SRGB
    , end : SRGB
    , nSteps : Int
    }


type alias SRGB = (Float, Float, Float)


init =
    { start = (0,1,0)
    , end = (1,0,0)
    , nSteps = 1000
    }


 -- UPDATE


update model = model



 -- VIEW


cRange : Float -> Float -> Int -> List Float
cRange start end nInter =
    let
        step = (end - start) / (toFloat <| nInter + 1)
    in
        List.map (\i -> start + (toFloat i) * step) <| List.range 0 (nInter+1)


-- red : Color -> Float
-- red = .red << toRgb
-- green : Color -> Float
-- green = .green << toRgb
-- blue : Color -> Float
-- blue = .blue << toRgb


wrongInterpolation : SRGB -> SRGB -> Int -> List SRGB
wrongInterpolation (r1,g1,b1) (r2,g2,b2) n =
    List.map3
        (\r g b -> (r,g,b))
        (cRange (r1) (r2) n)
        (cRange (g1) (g2) n)
        (cRange (b1) (b2) n)


linearInterpolation : SRGB -> SRGB -> Float -> SRGB
linearInterpolation (r1,g1,b1) (r2,g2,b2) f_ =
    let
        decode cs =
            if cs <= 0.04045 then
                cs / 12.92
            else
                ((cs + 0.055)/1.055)^2.4

        encode cl =
            if cl <= 0.0031308 then
                12.92 * cl
            else
                1.055 * cl^(1/2.4) - 0.055

        f =
            if f_ < 0 then
                0
            else if f_ > 1 then
                1
            else
                f_

        interp v1 v2 =
            encode <| (decode v1) + ((decode v2) - (decode v1)) * f
    in
        (interp r1 r2, interp g1 g2, interp b1 b2)


linearInterpolationSeq : SRGB -> SRGB -> Int -> List SRGB
linearInterpolationSeq c1 c2 n =
    let
        frac : Int -> Float
        frac i = (toFloat i) / (toFloat <| n+1)
    in
        List.map
            (linearInterpolation c1 c2 << frac)
            <| List.range 0 (n+1)
            -- (\r g b -> ((encode r),(encode g),(encode b)))
            -- (cRange rl1 rl2 n) (cRange gl1 gl2 n) (cRange bl1 bl2 n)


-- builtinInterpolation : C.Color -> C.Color -> Int -> List C.Color
-- builtinInterpolation c1 c2 n =
--     List.map (\i -> CI.interpolate CI.RGB c1 c2 ((toFloat i)/(toFloat n))) <| List.range 0 (n+1)


view model =
    layout
        [ width fill 
        , height fill
        , clip
        ]
        <| column
            [ width fill
            , height fill
            ]
            [ row
                [ width fill
                , height fill
                ]
                <| List.map
                    colorView
                    (wrongInterpolation model.start model.end model.nSteps)
                , row
                [ width fill
                , height fill
                ]
                <| List.map
                    colorView
                    (linearInterpolationSeq model.start model.end model.nSteps)
                -- , row
                -- [ width fill
                -- , height fill
                -- ]
                -- <| List.map
                --     (colorView << (\{red,green,blue} -> (red,green,blue)) << C.toRgba)
                --     <| builtinInterpolation
                --         ((\(r,g,b) -> C.rgb r g b) model.start)
                --         ((\(r,g,b) -> C.rgb r g b) model.end)
                --         model.nSteps
            ]


colorView : SRGB -> Element msg
colorView (r,g,b) =
    el
        [ height fill
        , width fill
        , Background.color <| rgb r g b
        ]
        none
        -- <| text <|
        --     "(" ++ Round.round 2 r
        --     ++ "," ++ Round.round 2 g
        --     ++ "," ++ Round.round 2 b ++
        --     ")"
