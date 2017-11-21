module Main exposing (..)

import Html exposing (Html, text, div, h2, img)
import Html.Attributes exposing (src)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra exposing (Key)


---- MODEL ----


type alias Model =
    { started : Bool
    , player_score : Int
    , ai_score : Int
    , player_position : Int
    , ai_position : Int
    , ball_position : ( Float, Float )
    , ball_x_velocity : Float
    , ball_y_velocity : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { started = False
      , player_score = 0
      , ai_score = 0
      , player_position = 0
      , ai_position = 0
      , ball_position = ( 0, 0 )
      , ball_x_velocity = 0.08
      , ball_y_velocity = 0.08
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TimeUpdate Time
    | KeyboardUpdate Key
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            applyPhysics dt model

        KeyboardUpdate keycode ->
            case keycode of
                Keyboard.Extra.CharW ->
                    ( { model | player_position = model.player_position - 5 }, Cmd.none )

                Keyboard.Extra.CharS ->
                    ( { model | player_position = model.player_position + 5 }, Cmd.none )

                Keyboard.Extra.CharO ->
                    ( { model | ai_position = model.ai_position - 5 }, Cmd.none )

                Keyboard.Extra.CharK ->
                    ( { model | ai_position = model.ai_position + 5 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        other_ ->
            ( model, Cmd.none )


applyPhysics : Time -> Model -> ( Model, Cmd Msg )
applyPhysics dt model =
    let
        ( ball_pos_x, ball_pos_y ) =
            model.ball_position

        { player_score, ai_score } =
            model

        ( ball_x_velocity, ball_position_x, new_player_score, new_ai_score ) =
            if ball_pos_x > 575 then
                ( -1 * abs model.ball_x_velocity, 574, player_score + 1, ai_score )
            else if ball_pos_x < 0 then
                ( abs model.ball_x_velocity, 0, player_score, ai_score + 1 )
            else
                ( model.ball_x_velocity, ball_pos_x, player_score, ai_score )

        ( ball_y_velocity, ball_position_y ) =
            if ball_pos_y > 375 then
                ( -1 * abs model.ball_y_velocity, 374 )
            else if ball_pos_y < 0 then
                ( abs model.ball_y_velocity, 0 )
            else
                ( model.ball_y_velocity, ball_pos_y )
    in
        ( { model
            | ball_position =
                ( ball_position_x + model.ball_x_velocity * dt
                , ball_position_y + ball_y_velocity * dt
                )
            , ball_y_velocity = ball_y_velocity
            , ball_x_velocity = ball_x_velocity
            , player_score = new_player_score
            , ai_score = new_ai_score
          }
        , Cmd.none
        )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "scores" ]
            [ h2 [ class "player-score" ] [ Html.text <| toString <| model.player_score // 2 ]
            , h2 [ class "ai-score" ] [ Html.text <| toString <| model.ai_score // 2 ]
            ]
        , div [ class "gameboard" ] [ gameBoard model ]
        ]


gameBoard : Model -> Html Msg
gameBoard model =
    Svg.svg [ width "600", height "400" ]
        [ rect
            [ width "600"
            , height "400"
            , fill "black"
            ]
            []
        , playerPaddle model.player_position
        , aiPaddle model.ai_position
        , ball model.ball_position
        ]


playerPaddle : Int -> Html Msg
playerPaddle player_position =
    rect
        [ width "40"
        , height "150"
        , fill "#fff"
        , x "50"
        , y (toString player_position)
        ]
        []


aiPaddle : Int -> Html Msg
aiPaddle ai_position =
    rect
        [ width "40"
        , height "150"
        , fill "#fff"
        , x "510"
        , y (toString ai_position)
        ]
        []


ball : ( Float, Float ) -> Html Msg
ball ( ball_position_x, ball_position_y ) =
    rect
        [ width "25"
        , height "25"
        , fill "#fff"
        , x (toString <| round ball_position_x)
        , y (toString <| round ball_position_y)
        ]
        []



-- img
-- [ src "public/logo.svg"
-- , width "50"
-- ]
-- []
---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.Extra.downs KeyboardUpdate
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
