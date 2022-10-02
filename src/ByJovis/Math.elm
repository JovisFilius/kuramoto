module ByJovis.Math exposing
    ( gaussian
    , mean
    , modulo
    , min
    , phaseDiff
    , phaseMean
    , phaseStd
    , std
    )

import Random


gaussian : Random.Generator Float
gaussian =
    Random.map2 boxMuller (Random.float 0 1) (Random.float 0 1)


boxMuller : Float -> Float -> Float
boxMuller u1 u2 =
    sqrt(-2 * log(u1)) * cos(2*pi*u2)


log : Float -> Float
log x =
    let
        p = 0.00001
    in
        (x^(p) - 1) / p


min : List Float -> Float
min numbers =
    case numbers of
        [] -> 0/0

        x::xs ->
            let
                rec nums curMin =
                    case nums of
                        [] -> curMin
                        y::ys ->
                            let
                                newMin = if y < curMin then y else curMin
                            in
                                rec ys newMin
            in
                rec xs x


mean : List Float -> Float
mean numbers = (List.sum numbers) / (toFloat <| List.length numbers)


std : List Float -> Float
std numbers =
    let
        mu = mean numbers
        n_ = toFloat <| (List.length numbers) - 1
    in
        sqrt <| (List.sum <| List.map (\x -> (x - mu)^2) numbers) / n_


modulo : Float -> Float -> Float
modulo a b =
    if a >= b then
        modulo (a-b) b
    else if a <= -b then
        modulo (a+b) b
    else
        a


phaseDiff : Float -> Float -> Float
phaseDiff ph1 ph2 =
    let
        naive = (ph2 - ph1)
    in
        if naive > pi then
            naive - 2*pi
        else if naive < -pi then
            naive + 2*pi
        else
            naive


phaseMean : List Float -> Float
phaseMean phases =
    let
        x = List.sum <| List.map cos phases
        y = List.sum <| List.map sin phases

        newPhase =
            if x == 0 then
                if y >= 0 then
                    pi/2
                else
                    -pi/2
            else
                atan(y/x)
    in
        if x < 0 then
            newPhase + pi
        else if newPhase < 0 then
            newPhase + 2*pi
        else newPhase

phaseStd : List Float -> Float
phaseStd numbers =
    let
        mu = phaseMean numbers
        n_ = toFloat <| (List.length numbers) - 1
        deviation x =
            if (x - mu) > pi then
                (x-mu) - 2*pi
            else if (x - mu) < -pi then
                (x-mu) + 2*pi
            else
                (x-mu)
    in
        sqrt <| (List.sum <| List.map (\x -> (phaseDiff mu x)^2) numbers) / n_
