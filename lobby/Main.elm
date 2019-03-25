import String exposing (trim)
import String.UTF8 as UTF8
import Array exposing (Array)
import Browser
import Html exposing (Html, a, b, p, text, div, select, option, button, input, label, sup)
import Html.Attributes as Attr exposing (type_, classList, style, hidden, id, class, attribute)
import Html.Events exposing (onInput, onClick, onFocus)
import Json.Decode as Dec exposing (decodeString)
import Json.Encode as Enc
import Http exposing (expectString)
import Collection exposing (Collection, intoApiCollection)
import Difficulty exposing (Difficulty)
import Api
import Ports


main : Program () Model Msg
main = Browser.element
    { init = always ( init, Collection.new AvailableCollections )
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }


type alias Model =
  { entries : Entries
  , name : (String, Maybe NameError)
  , serverError : Maybe String
  , estimateTime : Maybe Float
  , showSettings : Bool
  }

type ServerError
    = NameInvalid
    | Problem String

type Msg
    = CreateRoom String Settings
    | NameInput String
    | EntryMsg EntriesMsg
    | CreateResponse { name : String, roomid : String }
    | JoinResponse { name : String, roomid : String, connid : String }
    | ServerError ServerError
    | ToggleSetting
    | AvailableCollections (Result Http.Error (Collection, Difficulty))


type NameError
    = TooLong
    | Whitespaces
    | NoInput
    | Empty
    | Invalid

type GameType
    = Classic

type ScoreScheme
    = Quadratic
    | Linear

type Choice a
    = Combo Int (Array (ChoiceEntry a))
    | Duration Int Int (Result String Int)
    | Count Int Int (Result String Int)

type alias ChoiceEntry a =
    { value : a
    , name : String
    , description : List (Html Never)
    }

type alias Entry a = ChoiceEntry (Choice a)

type WhichEntry
    = EnGameType
    | EnScoreScheme
    | EnRoundLength
    | EnSetcount

type ChoiceMsg
    = NewComboSelect Int
    | NewIntegral (Result String Int)
    | Focused

type EntriesMsg
    = EntriesMsg WhichEntry ChoiceMsg
    | CollectionMsg Collection.Msg
    | DifficultyMsg Difficulty.Msg

type alias Settings =
    { gameType: GameType
    , scoreScheme : ScoreScheme
    , roundLength : Int
    , setCount : Int
    , decks : Collection
    , difficulty : Difficulty
    }

type alias Entries =
    { gameType: Entry GameType
    , scoreScheme : Entry ScoreScheme
    , roundLength : Entry Never
    , setCount : Entry Never
    , decks : Maybe Collection
    , difficulty : Maybe Difficulty
    , focused : Maybe WhichEntry
    }

init : Model
init =
    { entries = initEntries
    , name = ("", Just NoInput)
    , estimateTime = Maybe.map estimate <| querryEstimatable initEntries
    , showSettings = False
    , serverError = Nothing
    }

querryEstimatable : Entries -> Maybe { roundLength : Int, setCount : Int }
querryEstimatable { roundLength, setCount } =
    let
        maybeLength = queryNumValue roundLength.value
        maybeCount = queryNumValue setCount.value
    in
        Maybe.map2
            (\x y -> { roundLength = x, setCount = y })
            maybeLength maybeCount

initEntries : Entries
initEntries =
    let
        comboChoice =
            Combo 0 << Array.fromList << List.map (\(v, n, d) -> ChoiceEntry v n d)

        gameType =
            { name = "Game type"
            , value = comboChoice [ (Classic, "Classic", []) ]
            , description = [ text "A single artist draws a given random word, and multiple players attempt to guess what it is. When a player guesses right, points are attributed to the artist and the guesser. (This is the only game type available)" ]
            }

        scoreScheme =
            let
                supN = sup [] [ text "N" ]
                supn = sup [] [ text "N-n" ]
                quadraticDescr =
                        [ text "For N players, first guess gets 2", supN
                        , text " points, the n-th guess gets 2", supn , text " points."
                        ]
                linearDescr =
                    [ text "First guess gets 20 points, the n-th guess gets 20/n points." ]
            in
                { name = "Score scheme"
                , value = comboChoice
                    [ (Quadratic, "Quadratic", quadraticDescr)
                    , (Linear, "Linear", linearDescr)
                    ]
                , description = [ text "Determines how points are attributed to players for guessing correctly." ]
                }

        roundLength =
            { name = "Round duration"
            , value = Duration 10 600 <| Ok 90
            , description = [ text "Determines how much time is available to the guessers to find the word being drawn." ]
            }

        setCount =
            { name = "Sets"
            , value = Count 1 10 <| Ok 2
            , description = [ text "A set is a sequence of rounds played with alternating artist and guesser roles. The number of sets indicate how many time each players gets to draw." ]
            }
    in
        { gameType = gameType
        , roundLength = roundLength
        , setCount = setCount
        , decks = Nothing
        , difficulty = Nothing
        , scoreScheme = scoreScheme
        , focused = Nothing
        }

setNumValue : Result String Int -> Entry a -> Entry a
setNumValue newValue entry =
    case entry.value of
        Duration min max _ ->
            { entry | value = Duration min max newValue }
        Count min max _ ->
            { entry | value = Count min max newValue }
        _ ->
            entry

setCombo : Int -> Entry a -> Entry a
setCombo newIndex entry =
    case entry.value of
        Combo _ array ->
            { entry | value = Combo newIndex array }
        _ ->
            entry

viewChoiceDescr : Choice a -> Maybe (Html Never)
viewChoiceDescr value =
    let
        showDescr { description, name } =
            if List.isEmpty description then
                p [] []
            else
                p [] (b [] [ text (name ++ ": ") ] ::  description)
    in
        case value of
            Duration _ _ _ -> Nothing
            Count _ _ _ -> Nothing
            Combo entry entries ->
                Array.get entry entries
                    |> Maybe.map showDescr


viewChoice : Choice a -> Html ChoiceMsg
viewChoice value =
    let
        toOption (index, { name }) =
            option [ onClick (NewComboSelect index) ] [ text name ]

        withinBounds min max boundChecked =
            if min <= boundChecked && max >= boundChecked then
                Ok boundChecked
            else
                Err (String.fromInt boundChecked)

        toNum min max toDecode =
            decodeString Dec.int toDecode
                |> Result.mapError (always toDecode)
                |> Result.andThen (withinBounds min max)
                |> NewIntegral

        toDuration min max secs =
            p []
                [ toCount min max True secs
                , a [class "seconds"] [text "(seconds)"]
                ]

        toCount min max isDuration count =
            let (isInvalid, displayCount) =
                    case count of
                        Err val -> (True, val)
                        Ok  val -> (False, String.fromInt val)
            in
                input
                    [ type_ "number"
                    , classList
                        [ ("invalid", isInvalid)
                        , ("duration", isDuration)
                        , ("count", not isDuration)
                        ]
                    , Attr.value displayCount
                    , Attr.max <| String.fromInt max
                    , Attr.min <| String.fromInt min
                    , onInput (toNum min max)
                    , onFocus Focused
                    ]
                    [ text displayCount ]
    in
        case value of
            Duration min max num -> toDuration min max num
            Count    min max num -> toCount min max False num

            Combo entry entries ->
                select
                    [ classList [("default", entry == 0)]
                    , onFocus Focused
                    ]
                    (List.map toOption <| Array.toIndexedList entries)


viewEntry : Bool -> Entry a -> Html ChoiceMsg
viewEntry descrVisible { name, value, description } =
    let
        descrStyle =
            [ style "overflow" "hidden"
            , class "descr"
            ]

        descrDiv =
            div descrStyle <| case viewChoiceDescr value of
                Just valueDescr -> [ p [] (valueDescr :: description) ]
                Nothing -> description
    in
        div []
            [ p
                [ class "title" ]
                [ label [] [ text (name ++ ":") ] , viewChoice value ]
            , Html.map never descrDiv
            ]

viewEntries : Entries -> List (Html EntriesMsg)
viewEntries { gameType, scoreScheme, roundLength, setCount, decks, difficulty, focused } =
    let
        viewMaybe tagger view_ val =
            case val of
                Just val_ -> Html.map tagger <| view_ val_
                Nothing -> div [] [ text "loading..." ]

        ((foGT, foSch), (foRlen, foSet)) =
            case focused of
                Just EnGameType -> ((True, False), (False, False))
                Just EnScoreScheme -> ((False, True), (False, False))
                Just EnRoundLength -> ((False, False), (True, False))
                Just EnSetcount -> ((False, False), (False, True))
                Nothing -> ((False, False), (False, False))

        adapt tagger isFocused =
            Html.map (EntriesMsg tagger) << viewEntry isFocused
    in
        [ adapt EnGameType foGT gameType
        , viewMaybe CollectionMsg Collection.view decks
        , viewMaybe DifficultyMsg Difficulty.view difficulty
        , adapt EnRoundLength foRlen roundLength
        , adapt EnSetcount foSet setCount
        , adapt EnScoreScheme foSch scoreScheme
        ]

updateEntries : EntriesMsg -> Entries -> Entries
updateEntries msg entries =
    case msg of
        EntriesMsg which Focused ->
            { entries | focused = Just which }
        EntriesMsg EnGameType (NewComboSelect val) ->
            { entries | gameType = setCombo val entries.gameType }
        EntriesMsg EnScoreScheme (NewComboSelect val) ->
            { entries | scoreScheme = setCombo val entries.scoreScheme }
        EntriesMsg EnRoundLength (NewIntegral val) ->
            { entries | roundLength = setNumValue val entries.roundLength }
        EntriesMsg EnSetcount (NewIntegral val) ->
            { entries | setCount = setNumValue val entries.setCount }
        DifficultyMsg msg_ ->
            let
                difficulty =
                    Maybe.map (Difficulty.update msg_) entries.difficulty
            in
                { entries | difficulty = difficulty }

        CollectionMsg msg_ ->
            let
                decks = Maybe.map (Collection.update msg_) entries.decks
            in
                { entries | decks = decks }
        _ ->
            entries


queryNumValue : Choice a -> Maybe Int
queryNumValue value =
    case value of
        Duration _ _ (Ok val) -> Just val
        Count _ _ (Ok val) -> Just val
        _ -> Nothing


queryValue : Choice a -> Maybe a
queryValue value =
    case value of
        Combo idx array -> Maybe.map .value <| Array.get idx array
        _ -> Nothing


querrySettings : Entries -> Result String Settings
querrySettings entries =
    let
        mcoll : String -> Maybe a -> List String -> (Maybe a, List String)
        mcoll name maybeA others =
            case maybeA of
                Just val -> (Just val, others)
                Nothing -> (Nothing, name :: others)

        collect
            : (Choice a -> Maybe b)
            -> Entry a -> List String
            -> (Maybe b, List String)
        collect collecter { name, value } existing =
            case collecter value of
                Nothing -> (Nothing, name :: existing)
                Just val -> (Just val, existing)

        andThen : (a -> (b, a)) -> (c, a) -> ((b, c), a)
        andThen f (xc, xa) = let (fb, fa) = f xa in ((fb, xc), fa)

        ((difficulty, (decks, (setCount, (roundLength, (scoreScheme, gameType))))), invalidFields) =
            collect queryValue entries.gameType []
                |> andThen (collect queryValue entries.scoreScheme)
                |> andThen (collect queryNumValue entries.roundLength)
                |> andThen (collect queryNumValue entries.setCount)
                |> andThen (mcoll "selected word decks" entries.decks)
                |> andThen (mcoll "difficulty" entries.difficulty)
                |> Tuple.mapSecond displayErrorList

        displayErrorList e =
            case e of
                [] -> ""
                single :: [] -> single ++ " is invalid"
                head :: tail ->
                    String.join ", " tail ++ " and " ++ head ++  " are invalid"

        maybeThen arg = Maybe.andThen (\f -> Maybe.map f arg)
    in
        Maybe.map Settings gameType
            |> maybeThen scoreScheme
            |> maybeThen roundLength
            |> maybeThen setCount
            |> maybeThen decks
            |> maybeThen difficulty
            |> Result.fromMaybe invalidFields

interpretError : Http.Error -> ServerError
interpretError err =
    case err of
        Http.BadStatus 400 -> NameInvalid
        Http.BadStatus 404 -> Problem "the room wasn't created"
        Http.BadStatus 409 -> Problem "someone is in the room you created! \u{01f47b} (this should really not happen, wow!)"
        Http.BadStatus 502 -> Problem "the server is down"
        Http.BadStatus 500 -> Problem "the server is broken"
        Http.BadStatus other -> Problem ("we got a negative response: " ++ String.fromInt other)
        Http.NetworkError -> Problem "you are not connected to the internet"
        Http.BadUrl _  -> Problem "the app is broken, and tried to send the data to the wrong place"
        Http.Timeout   -> Problem "we didn't get a response from the server"
        Http.BadBody _ -> Problem "the server sent a malformed response"


requestJoin : { name : String, roomid : String} -> Cmd Msg
requestJoin { name , roomid } =
    let
        encodeJoin =
            Enc.object
                [ ("roomid", Enc.string roomid)
                , ("username", Enc.string name)
                ]

        decodeJoin_ connid =
            { connid = connid, name = name, roomid = roomid }

        decodeJoin response =
            case response of
                Ok msg -> JoinResponse <| decodeJoin_ msg
                Err err -> ServerError <| interpretError err
    in
        Http.post
            { url = "/rooms/join"
            , body = Http.jsonBody encodeJoin
            , expect = expectString decodeJoin
            }

requestCreate : String -> Settings -> Cmd Msg
requestCreate name ({decks, difficulty} as settings) =
    let
        coll =
            intoApiCollection (decks ,difficulty)

        encodeCreate =
            Enc.object
                [ ("round_duration", Enc.int settings.roundLength)
                , ("set_count", Enc.int settings.setCount)
                , ("collection", Api.encodeCollection coll)
                , ("score_scheme", Enc.string "linear")
                ]

        decodeCreate_ roomid = { name = name, roomid = roomid }
        decodeCreate response =
            case response of
                Ok msg -> CreateResponse <| decodeCreate_ msg
                Err err -> ServerError <| interpretError err
    in
        Http.post
            { url = "/rooms/create"
            , body = Http.jsonBody encodeCreate
            , expect = expectString decodeCreate
            }

validateName : String -> (String, Maybe NameError)
validateName toValidate =
    if toValidate == "" then
        ("", Just Empty)
    else if UTF8.length toValidate >= 30 then
        (toValidate, Just TooLong)
    else if trim toValidate /= toValidate then
        (toValidate, Just Whitespaces)
    else
        (toValidate, Nothing)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model | name = validateName name } , Cmd.none )

        CreateRoom name settings ->
            ( model , requestCreate name settings )

        CreateResponse loginValues ->
            ( model , requestJoin loginValues )

        EntryMsg msg_ ->
            let
                newEntries = updateEntries msg_ model.entries
                newEstimate = Maybe.map estimate <| querryEstimatable newEntries
            in
              ( { model | entries = newEntries, estimateTime = newEstimate }
              , Cmd.none
              )

        ServerError NameInvalid ->
            let (nameVal, _) = model.name
            in  ( { model | name = (nameVal, (Just Invalid)) } , Cmd.none )

        ServerError (Problem description) ->
            ( { model | serverError = Just description } , Cmd.none )

        ToggleSetting ->
            let settingsProblem = case querrySettings model.entries of
                    Ok _ -> False
                    Err _ -> True
                shouldShow = not model.showSettings || settingsProblem
            in
                ( { model | showSettings = shouldShow } , Cmd.none )

        AvailableCollections (Ok (decks, difficulty)) ->
            let
                entries = model.entries
                newEntries =
                    { entries | decks = Just decks, difficulty = Just difficulty }
            in
                ( { model | entries = newEntries }, Cmd.none )

        AvailableCollections (Err _) ->
            ( { model | serverError = Just "Couldn't fetch available decks" }
            , Cmd.none
            )

        JoinResponse {name, connid, roomid} ->
            ( model
            , Ports.stashAndOpen ("/games/classic/",
                [ ("connid", Enc.string connid)
                , ("roomid", Enc.string roomid)
                , ("username", Enc.string name)
                , ("retries", Enc.int 0)
                ])
            )


estimate : { a | roundLength : Int, setCount : Int } -> Float
estimate { roundLength, setCount } =
    toFloat ((roundLength + 10) * setCount) / 60


viewEstimate : Maybe Float -> Html msg
viewEstimate maybeEstimate =
    let
        toMinutes : Int -> String
        toMinutes time =
            if time < 1 then
                "less than a minute"
            else if time == 1 then
                "one minute"
            else
                String.fromInt time ++ " minutes"

        anEstimate mul flavor =
            p []
                [ text "~"
                , b [] [ text <| toMinutes (ceiling mul) ]
                , text flavor
                ]

        allEstimates value =
            div []
                [ anEstimate value "per player."
                , anEstimate (value * 5) "for 5 players."
                , anEstimate (value * 10) "for 10 players."
                ]
    in
        case maybeEstimate of
            Nothing ->
                div [ id "estimate" ] [ text "Invalid setting values" ]
            Just value ->
                    div
                        [ id "estimate" ]
                        [ text "Game duration:" , allEstimates value ]

view : Model -> Html Msg
view model =
    let
        nameErrReason error =
            case error of
                NoInput -> "is required"
                TooLong -> "should be at most 30 characters"
                Whitespaces -> "should not start or end with spaces"
                Empty -> "is required"
                Invalid -> "is refused by the server."

        (nameText, nameError) = model.name

        invalidAttr msg = [ attribute "data-errmsg" msg, class "invalid" ]

        extraAttrs =
            case nameError of
                Nothing -> []
                Just NoInput -> []
                Just anyelse ->
                    invalidAttr <| "This " ++ nameErrReason anyelse

        nameView =
            div (id "name-field" :: extraAttrs)
                [ text "Display name:"
                , input [ Attr.value nameText, onInput NameInput ] []
                ]

        combineReasons name settings =
            let nameErr = "Your display name " ++ nameErrReason name
            in nameErr ++ "; " ++ settings

        validButton =
            case (model.name, querrySettings model.entries) of
                ((validName, Nothing), Ok validSettings) ->
                    [ onClick <| CreateRoom validName validSettings
                    , Attr.disabled False
                    ]
                ((_, Nothing), Err settingsFailure) ->
                    [ Attr.disabled True
                    , Attr.title settingsFailure
                    ]
                ((_, Just nameFailure), Ok _) ->
                    [ Attr.disabled True
                    , Attr.title <|
                        "Your display name " ++ nameErrReason nameFailure
                    ]
                ((_, Just nameFailure), Err settingsFailure) ->
                    [ Attr.disabled True
                    , Attr.title <|
                        combineReasons nameFailure settingsFailure
                    ]

        viewServerError =
            case model.serverError of
                Nothing -> text ""
                Just problem ->
                    div [ id "server-error" ]
                        [ p []
                            [ b [] [ text "Impossible to create the room because: " ]
                            , text (problem  ++ ".")
                            ]
                        , p [] [ text "If you think this is on our end, please get in touch" ]
                        ]
    in
        div []
            [ nameView
            , viewServerError
            , div
                [ style "display" "flex", style "justify-content" "center" ]
                [ button ( id "create" :: validButton ) [ text "Create a new room" ]
                ]
            , viewEstimate model.estimateTime
            , div
                [ id "toggle" ]
                [ button [ onClick ToggleSetting ] [ text "View settings" ] ]
            , Html.map EntryMsg <| div
                [ style "max-height" (if model.showSettings then "2000px" else "1px")
                , id "settings"
                ]
                (viewEntries model.entries)
            ]
