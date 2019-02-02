module Pintclone.Room
    exposing
        ( newInLobby
        , joinBreak
        , joinRound
        , joins
        , leaves
        , becomeMaster
        , setArtist
        , syncScores
        , isMaster
        , view
        , alone
        , canStart
        , amArtist
        , myName
        , Room
        )

{-| Manages state changes related to the user list.

Handles who is the master in the lobby part of the game. And who is the
artist in the game part of the game.

-}

import Tuple exposing (first, second, mapFirst)
import Maybe exposing (withDefault)
import Dict exposing (Dict, fromList, get, remove, insert, empty, isEmpty, singleton, update, toList)
import Html as H exposing (Html, text, ul, li, a)
import Html.Attributes exposing (id, classList)
import Api


type alias Room =
    { me : (Api.Name, Score)
    , state : State
    }

type alias Score = List Api.RoundScore
type alias Scores = Dict String Score

type State
    = LobbyWith Scores
    | MasterOf Scores
    | PlayingWith String Scores
    | ArtistWith Scores
    | BreakWith Scores
    | Alone


{-| New room with `Scores` not containing myName -}
new : Scores -> Api.Name -> Score -> (Scores -> State) -> Room
new scores myName myScore notAlone =
    let state = if isEmpty scores then Alone else notAlone scores
    in  Room (myName, myScore) state

{-| New room from `Api.Scoreboard` (assumes `myName in Scoreboard`) -}
newFromList : Api.Scoreboard -> Api.Name -> (Scores -> State) -> Room
newFromList scoreboard myName =
    let
        myName_ = Api.showName myName
        scores = fromList (List.map (mapFirst Api.showName) scoreboard)
        myScore = withDefault [] <| get myName_ scores
        scoresWithoutMe = remove myName_ scores
    in
        new scoresWithoutMe myName myScore


{-| newInLobby, returns an initialized Room

    Room.newInLobby user userList

Is the room state during the "lobby" period of the game where the current
user is `user` and the other users are `userList`. The empty list case is
handled properly.

In order to initialize the room during a "round", see `joinRound`.
-}
newInLobby : Api.Name -> Api.Name -> Api.Scoreboard -> Room
newInLobby master myName scoreboard =
    let notAlone =
            if master == myName then MasterOf else LobbyWith
    in
        newFromList scoreboard myName notAlone


{-| Create a room for player joining while the scores are shown
-}
joinBreak : Api.Name -> Api.Scoreboard -> Room
joinBreak me scoreboard = newFromList scoreboard me BreakWith


{-| Create a room in "round" state.
-}
joinRound : Api.Name -> Api.Name -> Api.Scoreboard -> Room
joinRound artistName myName scoreboard =
    let
        artistName_ = Api.showName artistName
        notAlone =
            if artistName == myName then ArtistWith else PlayingWith artistName_
    in
        newFromList scoreboard myName notAlone


{-| The given room where an opponent left.
-}
leaves : Api.Name -> Room -> Room
leaves leaving_ { me, state } =
    let
        leaving = Api.showName leaving_
        (myName, myScore) = me
        pop scores = new (remove leaving scores) myName myScore
    in
        case state of
            Alone -> { me = me, state = Alone }
            BreakWith  others -> pop others BreakWith
            LobbyWith  others -> pop others LobbyWith
            MasterOf   others -> pop others MasterOf
            ArtistWith others -> pop others ArtistWith
            PlayingWith artistName others ->
                if artistName == leaving then
                    pop others BreakWith
                else
                    pop others (PlayingWith artistName)



{-| The given room where someone new joined.
TODO: `repeat roundCount Api.Absent`
-}
joins : Api.Name -> Room -> Room
joins joining_ { me, state } =
    let
        joining = Api.showName joining_
        (myName, myScore) = me
        push scores = new (insert joining [] scores) myName myScore
    in
        case state of
            Alone -> push empty MasterOf
            BreakWith  others -> push others BreakWith
            LobbyWith  others -> push others LobbyWith
            MasterOf   others -> push others MasterOf
            ArtistWith others -> push others ArtistWith
            PlayingWith a others -> push others (PlayingWith a)


{-| Turn the user into the game master (that means he has the role of
starting the game).
-}
becomeMaster : Room -> Room
becomeMaster room =
    let
        masterState state =
            case state of
                LobbyWith others -> MasterOf others
                anyelse -> anyelse
    in
        { room | state = masterState room.state }


setArtist : Api.Name -> Room -> Room
setArtist newArtist_ { me, state } =
    let
        newArtist = Api.showName newArtist_
        withArtist others =
            if newArtist == Api.showName (first me) then
                { me = me, state = ArtistWith others }
            else
                { me = me, state = PlayingWith newArtist others }
    in
        case state of
            Alone -> { me = me, state = Alone }
            BreakWith  others -> withArtist others
            LobbyWith  others -> withArtist others
            ArtistWith others -> withArtist others
            MasterOf   others -> withArtist others
            PlayingWith _ others -> withArtist others


{-| Whether the player is master of the room or not.
-}
isMaster : Room -> Bool
isMaster { state } =
    case state of
        Alone      -> True
        MasterOf _ -> True
        anyelse    -> False


{-| Whether the player is the artist
-}
amArtist : Room -> Bool
amArtist room =
    case room.state of
        ArtistWith _ -> True
        anyelse      -> False

{-| Whether the player is left alone in the game
-}
alone : Room -> Bool
alone room =
    case room.state of
        Alone   -> True
        anyelse -> False


{-| Whether the room leader can start the game
-}
canStart : Room -> Bool
canStart room =
    case room.state of
        MasterOf ps -> Dict.size ps > 1
        anyelse -> False


{-| Update scores to make sure they are as the server sees them
-}
syncScores : List (Api.Name, Api.RoundScore) -> Room -> Room
syncScores roundScores_ { me, state } =
    let
        (myName_, myScore) = me
        myName = Api.showName myName_
        roundScores =
            List.map (mapFirst Api.showName) roundScores_
                |> List.filter ((/=) myName << first)

        maybeAddScore : Api.RoundScore -> Maybe Score -> Maybe Score
        maybeAddScore roundScore = Just << (::) roundScore << withDefault []

        updatePlayer : (String, Api.RoundScore) -> Scores -> Scores
        updatePlayer (name, roundScore) =
            update name (maybeAddScore roundScore)

        updateOthers scores = List.foldl updatePlayer scores roundScores

        myNewScore =
            List.filter ((==) myName_ << first) roundScores_
                |> List.head
                |> Maybe.map (\(_,newScore) -> newScore :: second me)
                |> withDefault []

        updateAll others = new (updateOthers others) myName_ myNewScore
    in
        case state of
            Alone -> { me = me, state = Alone }
            BreakWith  others -> updateAll others BreakWith
            LobbyWith  others -> updateAll others LobbyWith
            ArtistWith others -> updateAll others ArtistWith
            MasterOf   others -> updateAll others MasterOf
            PlayingWith a others -> updateAll others (PlayingWith a)


myName : Room -> Api.Name
myName { me } = first me

showScore : Score -> String
showScore =
    let
        addScore score acc =
            case score of
                Api.WasArtist  s -> s + acc
                Api.HasGuessed s -> s + acc
                Api.HasFailed -> acc
                Api.WasAbsent -> acc
    in
        toString << List.foldl addScore 0


view : Room -> Html msg
view { me, state } =
    let
        showMe = mapFirst Api.showName me
        userRow : Maybe String -> ( String, Score ) -> Html msg
        userRow artistName ( name, score ) =
            li
                [ classList
                    [ ( "userentry", True )
                    , ( "userentry-me", name == first showMe )
                    , ( "userentry-artist", Just name == artistName )
                    ]
                ]
                [ a [] [ text name ]
                , a [] [ text <| showScore score ]
                ]

        render maybeartist others =
            others
                |> List.sortBy first
                |> List.map (userRow maybeartist)
                |> ul [ id "userlist" ]
    in
        case state of
            Alone -> render Nothing [ showMe ]
            BreakWith  others -> render Nothing (showMe :: toList others)
            LobbyWith  others -> render Nothing (showMe :: toList others)
            MasterOf   others -> render Nothing (showMe :: toList others)
            ArtistWith others -> render (Just (first showMe)) (showMe :: toList others)
            PlayingWith artistName others ->
                render (Just artistName) (showMe :: toList others)

