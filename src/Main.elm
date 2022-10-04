module Main exposing (..)

import Array exposing
    ( Array
    )

import Browser

import ByJovis.Color exposing (linearInterpolation)
import ByJovis.Math exposing
    ( gaussian
    , mean
    , min
    , modulo
    , phaseDiff
    , phaseMean
    , phaseStd
    , std
    )

import Chart as C
import Chart.Attributes as CA

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

import Html exposing (Html)

import List

import Random

import Round
import String exposing
    ( fromFloat
    , fromInt
    )

import Svg exposing (Svg, svg)
import Svg.Attributes as SA

import Time exposing
    ( Posix
    , millisToPosix
    )



 -- MAIN


main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



 -- MODEL


type alias State =
    { running: Bool
    , oscillators: List Oscillator
    , coupling : Float
    , simulationSpeed : Float
    }


type alias Oscillator =
    { freq : Float
    , phases : Float
    }

type alias Kuramoto =
    { oscillators : Array Oscillator
    , coupling : Float -> Float
    }


type alias Phase = Float
type alias Freq = Float


mkColor : Float -> Float -> Float -> (Float, Float, Float)
mkColor r g b = (r,g,b)


init : () -> (State, Cmd Msg)
init _ =
    ( { running = True
        , oscillators =
            { freqs = List.repeat nO 0
            , feedbacks = List.repeat nO 0
            , phases = List.repeat nO 0
            }
        , coupling = 0.0
        , simulationSpeed = 1
    }
    , Random.generate OscillatorSeeds (seedOscillatorStates nO)
    )


seedOscillatorStates : Int -> Random.Generator (List Phase, List Freq)
seedOscillatorStates n =
    Random.pair
        (Random.list n samplePhase)
        (Random.list n sampleFreq)

samplePhase : Random.Generator Phase
samplePhase = Random.float 0 (2*pi)

sampleFreq : Random.Generator Freq
sampleFreq = Random.map (\x -> freqMean + freqStd*x) gaussian


freqMean = 10
freqStd = 2
nO = 30
dt = 1
-- dt = 10 -- milliseconds
-- nT = 1000



 -- UPDATE


type Msg
    = Tick
    | OscillatorSeeds (List Phase, List Freq)
    | SetCoupling Float
    | SetSpeed Float
    | ToggleRunning


update : Msg -> State -> (State, Cmd Msg)
update msg ({running, oscillators, coupling} as state) =
    case msg of
        ToggleRunning ->
            ( { state | running = not running }
            , Cmd.none
            )

        SetCoupling k ->
            ( { state | coupling = k }
            , Cmd.none
            )

        SetSpeed speed ->
            ( { state | simulationSpeed = speed }
            , Cmd.none
            )

        OscillatorSeeds (phases, freqs) ->
            ( { state | oscillators = 
                    { freqs = freqs
                    , feedbacks = freqs
                    , phases = phases
                    }
                }
            , Cmd.none
            )

        _ ->
            ( { state | oscillators = stepOscillators coupling oscillators }
            , Cmd.none
            )


stepOscillators : Float -> Kuramoto -> Kuramoto
stepOscillators t kuramoto =
    let
        couplingFeedbacks =
            List.map (couplingFeedback oscillators.phases (kuramoto.coupling t)) oscillators.phases

        coupledFreqs =
            List.map2
                (+)
                oscillators.freqs
                couplingFeedbacks

    in
    { oscillators
        | phases = updatePhases oscillators.phases coupledFreqs
        , feedbacks = couplingFeedbacks
    }


couplingFeedback : List Float -> Float -> Float -> Float
couplingFeedback phases k phase_i =
    let
        phaseDiffs = List.map (\phase_j -> sin(phase_j - phase_i)) phases
    in
        k * (List.sum phaseDiffs)


updatePhases : List Float -> List Float -> List Float
updatePhases phases cFreqs =
    let
        d_phase cFreq = (cFreq * dt / 1000)

        modulo_2pi phase =
            if phase > (2*pi) then
                modulo_2pi (phase - 2*pi)
            else
                phase
    in
    List.map2 (\phase cFreq -> modulo_2pi <| phase + d_phase cFreq) phases cFreqs



 -- VIEW


view : State -> Html Msg
view ({running, oscillators, coupling} as state) =
    let
        phs = oscillators.phases
        mu_phs = phaseMean phs
        lags = List.map (phaseDiff mu_phs) phs

        minX = Maybe.withDefault 0 <| List.minimum (oscillators.freqs)
        minY = Maybe.withDefault 0 <| List.minimum lags

        point x y = {x = x, y = y}
        data =
            List.map2 point
            oscillators.freqs
            lags
    in
    layout
        [ --inFront
            -- <| row
            --     [ alignTop 
            --     , alignLeft
            --     , padding 10
            --     , spacing 15
            --     ]
            --     [ debugView oscillators
            --     , controlpanelView state
            --     ]
        ]
        <| row
            [ centerY
            , spacing 40
            , width fill
            ]
            [ debugView oscillators
            , column
                [ spacing 15
                ]
                [ el
                    [ centerX
                    , centerY
                    , Background.color <| rgb 0.5 0.5 0.5
                    ]
                    <| kuramotoView oscillators
                , controlpanelView state
                ]
            , el
                [ width <| px 300
                , height <| px 300
                , centerX
                ]
                <| html <| C.chart
                    [ CA.height 300
                    , CA.width 300
                    -- , CA.range -pi pi
                    ]
                    [ C.series .x
                        [ C.scatter .y []
                        ]
                        data
                    , C.xLabels [ CA.withGrid ]
                    , C.xLabel 
                        [ CA.y minY ]
                        [ Svg.text "intrinsic frequency" ]
                    , C.yLabel
                        [ CA.x <| minX - 1 ]
                        [ Svg.text "phase lag" ]
                    , C.yLabels
                        [ CA.withGrid 
                        ]
                    ]
            ]


controlpanelView : State -> Element Msg
controlpanelView ({running, coupling} as state) =
    row
        [ alignLeft
        , paddingEach {left = 40, right = 40, top = 15, bottom = 15}
        , spacing 30
        ]
        [ column
            [ alignTop
            , width <| px 100
            , spacing 8
            ]
            [ Input.button
                [ Border.color <| rgb 0.3 0.3 0.3
                , Background.color <| rgb 1 1 1
                , Font.color <| rgb 0.2 0.2 0.2
                , Border.width <| 1
                , padding 3
                , width fill
                , Font.center
                ]
                { onPress = Just ToggleRunning
                , label = text <|
                    if running then "Pause" else "Run"
                }
            , Input.button
                [ Border.color <|
                    if running then rgb 0.6 0.6 0.6 else rgb 0.3 0.3 0.3
                , Background.color <|
                    if running then rgb 0.8 0.8 0.8 else rgb 1 1 1
                , Font.color <|
                    if running then rgb 0.5 0.5 0.5 else rgb 0.2 0.2 0.2
                , Border.width <| 1
                , padding 3
                , width fill
                , Font.center
                ]
                { onPress = if running then Nothing else Just Tick
                , label = text "Step"
                }
            ]
        , Input.slider
            [ height <| px 30
            , width <| px 150
            , behindContent <|
                el
                    [ width fill
                    , height <| px 2
                    , centerY
                    , Background.color <| rgb 0.7 0.7 0.7
                    , Border.rounded 2
                    ]
                    none
            ]
            { onChange = SetCoupling
            , label =
                Input.labelBelow [] <|
                    text <| "coupling: " ++ Round.round 2 coupling
            , min = 0
            , max = 1
            , value = coupling
            , thumb = Input.defaultThumb
            , step = Just 0.01
            }
        , Input.slider
            [ height <| px 30
            , width <| px 150
            , behindContent <|
                el
                    [ width fill
                    , height <| px 2
                    , centerY
                    , Background.color <| rgb 0.7 0.7 0.7
                    , Border.rounded 2
                    ]
                    none
            ]
            { onChange = SetSpeed
            , label =
                Input.labelBelow [] <|
                    text <| "step: " ++ Round.round 0 (dt /
                    state.simulationSpeed) ++ " ms"
            , min = 1/20
            , max = 2
            , value = state.simulationSpeed
            , thumb = Input.defaultThumb
            , step = Just 0.01
            }
        ]

debugView : Kuramoto -> Element Msg
debugView oscs =
    let
        freqs = oscs.freqs
        fdbs = oscs.feedbacks
        cFreqs = List.map2 (+) oscs.feedbacks oscs.freqs
        phs = oscs.phases
        mu_phs = phaseMean phs
        lags = List.map (phaseDiff mu_phs) phs
               
        colPadding = 10

        numCol decimals nums mu sigma =
            column
                [ padding colPadding
                -- , explain Debug.todo
                ]
                <|
                    List.map (\num -> text <| Round.round decimals num) nums
                ++
                    [ el [height <| px 7] none
                    , el
                        [ height <| px 7
                        , width fill
                        , Border.widthEach {bottom = 0, left = 0, right = 0, top = 1}
                        , Border.color <| rgb 0.2 0.2 0.2
                        ]
                        none
                    , text
                        <| Round.round decimals mu
                        ++ " ± "
                        ++ Round.round decimals sigma
                    ]
    in
        row
            [ spacing 10
            , padding 15
            , alignBottom
            , height fill
            -- , height <| px 1070
            -- , scrollbars
            , width shrink
            -- , scrollbars
            -- , explain Debug.todo
            ]
            [ column
                [ 
                ]
                [ text "frequency" 
                , numCol 1 freqs (mean freqs) (std freqs)
                ]
            , column
                []
                [ text "feedback"
                , numCol 1 fdbs (mean fdbs) (std fdbs)
                ]
            , column
                []
                [ text "effective frequency"
                , numCol 1 cFreqs (mean cFreqs) (std cFreqs)
                ]
            , column
                [ width <| px 100
                ]
                [ text "phase"
                , numCol 2 phs (phaseMean phs) (phaseStd phs)
                ]
            , column
                [ width <| px 150
                ]
                [ text "phase lag"
                , numCol 2 lags (mean lags) (phaseStd lags) 
                ]
            ]


freqColor : Float -> (Float, Float, Float)
freqColor freq =
    let
        lo = freqMean - 2*freqStd
        hi = freqMean + 2*freqStd
        f = (freq - lo) / (hi - lo)
    in
    linearInterpolation (0.25, 0.1, 1) (0, 1, 1) f


r_perc = 25


kuramotoView : Kuramoto -> Element Msg
kuramotoView oscillators =
    let
        r = 600
    in
    html <| svg
        [ SA.width <| fromFloat <| r
        , SA.height <| fromFloat <| r
        ]
        ([ Svg.rect
            [ SA.stroke "rgb(255,255,255)"
            , SA.strokeWidth <| fromFloat <| 2
            , SA.fillOpacity "0"
            , SA.color "rgb(100,100,100)"
            , SA.width "100%"
            , SA.height "100%"
            ]
            []
        ]
        ++ (List.map2
                oscillatorView
                oscillators.phases
                (List.map freqColor oscillators.freqs)
            )
        ++ [ clockArm (phaseMean oscillators.phases) (0.5,0.5,0.5)
            , oscillatorView (phaseMean oscillators.phases) (1,1,0)
            ]
        )


clockArm phase color =
    Svg.line
        [ SA.x1 "50%"
        , SA.y1 "50%"
        , SA.x2 <| (fromFloat <| 50 + r_perc * cos(radians phase)) ++ "%"
        , SA.y2 <| (fromFloat <| 50 - r_perc * sin(radians phase)) ++ "%"
        , SA.stroke <| svgColorString <| mkColor 0.7 0.7 0.7
        , SA.strokeWidth "0.5"
        ]
        []


oscillatorView :  Phase -> (Float, Float, Float) -> Svg Msg
oscillatorView phase color =
        Svg.circle
            [ SA.cx <| (fromFloat <| 50 + r_perc * cos(radians phase)) ++ "%"
            , SA.cy <| (fromFloat <| 50 - r_perc * sin(radians phase)) ++ "%"
            , SA.r <| fromFloat 5
            -- , SA.fill <| svgColorString (100/255) (200/255) (100/255) --"rgb(100,200,100)"
            , SA.fill <| svgColorString color
            ]
            []


svgColorString (r,g,b) =
    "rgb(" ++ fromFloat (255 * r) ++ "," ++ fromFloat (255 * g) ++ "," ++ fromFloat
    (255 * b) ++ ")"


 -- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions {running, simulationSpeed} =
    if running then
        Time.every 20 (\time -> Tick)
    else
        Sub.none


--function fn = mkThetaDiff(k)
--    fn = @thetaDiff;
--    function inc = thetaDiff(i, t, omega, thetas)
--        inc = omega + k(t) * sum(sin(thetas - thetas(i)));
--    end
--end

--function main(N, dt, nT, k)
--    thetas = nan(N,nT);
--    thetas(:,1) = rand(N, 1) * 2 * pi;
--    
--    timeVec = mkTimeVec(dt,nT);
--    
--    omegas = 9 + 2 * randn(N,1);
--    thetaDiff = mkThetaDiff(k);
--    
--    for iT = 2:numel(timeVec)
--        for iN = 1:N
--            thetas(iN,iT) = thetas(iN,iT-1) + dt * thetaDiff(iN, timeVec(iT-1), omegas(iN), thetas(:,iT-1));
--        end
--    end
--    
--    figure;
--    hold on
--    for iN = 1:N
--        plot(sin(thetas(iN,:)))
--    end
--    
--end
