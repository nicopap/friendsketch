module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Debug
import Lobby.Join as Join exposing (Join)


type WelcomeMsg
    = CreateGame String Settings
    | JoinGame String
    | UpdateUserName String


type Msg
    = WMsg WelcomeMsg
    | JMsg Join.Msg


type Screen
    = WelcomeScreen Welcome
    | JoinScreen Join


type alias Settings =
    { x : ()
    }


type alias Welcome =
    { userName : String
    , settings : Settings
    , settingsVisible : Bool
    }


type alias Model =
    { screen : Screen
    }


main : Program Never Model Msg
main =
    H.program
        { init = ( Model <| WelcomeScreen new, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subs
        }



-- TODO: generate random user name


new : Welcome
new =
    { userName = "default user"
    , settings = { x = () }
    , settingsVisible = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ screen } as model) =
    case screen of
        WelcomeScreen welcome ->
            (case msg of
                WMsg (CreateGame userName settings) ->
                    Debug.log
                        ("Attempt to open Create Screen, not implemented yet")
                        (model)

                WMsg (JoinGame userName) ->
                    { model | screen = JoinScreen <| Join.new "localhost:8080" userName }

                WMsg (UpdateUserName newUserName) ->
                    Model <|
                        WelcomeScreen { welcome | userName = newUserName }

                JMsg _ ->
                    Debug.log "Join message in welcome screen, impossible" model
            )
                ! []

        JoinScreen join ->
            case msg of
                JMsg jmsg ->
                    Join.update join jmsg
                        |> (\( jmodel, jmsg_ ) ->
                                ( { model | screen = JoinScreen jmodel }
                                , Cmd.map JMsg jmsg_
                                )
                           )

                WMsg _ ->
                    Debug.log "Welcome message in join screen, impossible" model ! []



-- SUBS --


subs : Model -> Sub Msg
subs { screen } =
    case screen of
        JoinScreen join ->
            Sub.map JMsg <| Join.subs join

        WelcomeScreen _ ->
            Sub.none



-- VIEW --


settingsView : Settings -> Html msg
settingsView _ =
    H.div [] [ H.text "TODO: work in progress" ]


hbutton : WelcomeMsg -> String -> Html WelcomeMsg
hbutton msg buttonLabel =
    H.button [ onClick msg ] [ H.text buttonLabel ]


view : Model -> Html Msg
view { screen } =
    case screen of
        WelcomeScreen welcome ->
            H.map WMsg <| viewWelcome welcome

        JoinScreen join ->
            H.map JMsg <| Join.view join


viewWelcome : Welcome -> Html WelcomeMsg
viewWelcome { userName, settings, settingsVisible } =
    H.div []
        [ H.h1 [] [H.text "Lobby: What to do?"]
        , hbutton (CreateGame userName settings) "Create a new game"
        , hbutton (JoinGame userName) "Join an existing game"
        , H.input
            [ HA.autofocus True
            , HA.value userName
            , onInput UpdateUserName
            ]
            [ H.text userName ]
        , if settingsVisible then
            settingsView settings
          else
            H.div [] []
        ]
