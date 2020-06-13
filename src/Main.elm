-- 1. 電卓を作ります
-- 2. 入力を作ります
-- 3. 出力させます
-- 4. 計算させます(逆ポーランド記法)


module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, input, main_, p, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Change poland ->
            ( poland, Cmd.none )


calc : String -> Maybe Float
calc poland =
    List.head <|
        List.foldl
            (\token stack ->
                case ( stack, token ) of
                    ( x :: y :: ys, "+" ) ->
                        (y + x) :: ys

                    ( x :: y :: ys, "-" ) ->
                        (y - x) :: ys

                    ( x :: y :: ys, "*" ) ->
                        (y * x) :: ys

                    ( x :: y :: ys, "/" ) ->
                        (y / x) :: ys

                    ( xs, numString ) ->
                        case String.toFloat numString of
                            Just num ->
                                num :: xs

                            Nothing ->
                                []
            )
            []
            (String.split " " poland)


view : Model -> Html Msg
view model =
    main_ [ class "ly_cont" ]
        [ input [ value model, onInput Change, placeholder "" ] []
        , p [] [ text <| Maybe.withDefault "Enter reverse polish notation" <| Maybe.map String.fromFloat <| calc model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
