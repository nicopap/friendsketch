module Game.Room
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
        , viewRoundTally
        , viewPreviousTally
        , correct
        , guessed
        , Room
        )

{-| Manages state changes related to the user list.

Handles who is the master in the lobby part of the game. And who is the
artist in the game part of the game.

-}

import Tuple exposing (first, second, mapFirst, mapSecond)
import Maybe exposing (withDefault)
import Array
import Dict exposing (Dict, fromList, get, remove, insert, empty, isEmpty, singleton, update, toList)

import Html as H exposing (Html, text, ul, li, a, div, p)
import Html.Attributes exposing (id, classList, class, style)

import Api


type alias Room =
    { me : (Api.Name, Player)
    , state : State
    }

type alias Player =
    { succeeded : Bool
    , score : Score
    }

type alias Players = Dict String Player

type alias Scores = Dict String Score
type alias Score = List Api.RoundScore

type State
    = LobbyWith Players
    | MasterOf Players
    | PlayingWith String Players
    | ArtistWith Players
    | BreakWith Players
    | Alone


{-| New room with `Scores` not containing myName -}
new : Scores -> Api.Name -> List Api.RoundScore -> (Players -> State) -> Room
new scores myName myScore notAlone =
    let state =
            if isEmpty scores
            then Alone
            else notAlone <| Dict.map (always (Player False)) scores
    in
        Room (myName, Player False myScore) state

{-| New room from `Api.Scoreboard` (assumes `myName in Scoreboard`) -}
newFromList : Api.Scoreboard -> Api.Name -> (Players -> State) -> Room
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
        pop scores notAlone =
            let removed = remove leaving scores
            in  if isEmpty removed then
                    { me = me, state = Alone }
                else
                    Room me <| notAlone <| removed
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
-}
joins : Api.Name -> Room -> Room
joins joining_ { me, state } =
    let
        joining = Api.showName joining_
        push scores notAlone =
            Room me <| notAlone <| insert joining (Player False []) scores
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
        (myName_, mePlay) = me
        myName = Api.showName myName_
        roundScores =
            List.map (mapFirst Api.showName) roundScores_
                |> List.filter ((/=) myName << first)

        maybeAddScore : Api.RoundScore -> Maybe Player -> Maybe Player
        maybeAddScore newScore =
            let
                addScore old = Player False (newScore :: old.score)
            in
                Just << addScore << withDefault (Player False [])

        updatePlayer : (String, Api.RoundScore) -> Players -> Players
        updatePlayer (name, roundScore) =
            update name (maybeAddScore roundScore)

        updateOthers scores = List.foldl updatePlayer scores roundScores

        myNewScore =
            List.filter ((==) myName_ << first) roundScores_
                |> List.head
                |> Maybe.map (\(_,s) -> Player False (s :: mePlay.score))
                |> withDefault (Player False [])

        updateAll others notAlone =
            Room (myName_, myNewScore)
                <| notAlone (updateOthers others)
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

correct : Room -> Room
correct { me, state } =
    { me = mapSecond (\x -> { x | succeeded = True }) me, state = state }

{-| Set a player as having guessed the word correctly.
!!!! This doesn't work for the current player (me) !!!!
Use `correct` instead.
-}
guessed : Api.Name -> Room -> Room
guessed guesser { state, me } =
    let
        setSuccess = Maybe.map (\player -> { player | succeeded = True })
        withGuess s notA =
            Room me <| notA <| update (Api.showName guesser) setSuccess s
    in case state of
            Alone -> { me = me, state = Alone }
            BreakWith  others -> withGuess others BreakWith
            LobbyWith  others -> withGuess others LobbyWith
            ArtistWith others -> withGuess others ArtistWith
            MasterOf   others -> withGuess others MasterOf
            PlayingWith a others -> withGuess others (PlayingWith a)

{-| Displays the score for a player. If first argument is true, will also
display the differences in points from last round and offset the element to
reorder the list according to the given offsets.
-}
playerTally : Bool -> Int -> String -> Player -> Html msg
playerTally showDiff offset name { score } =
    let
        scoreDiff = case score of
            h :: _ ->
                if showDiff
                then roundAsInt h
                else 0
            []    -> 0

        styleOffset = style [("top", toString (offset * 30) ++ "px")]

        totalScore = toString <| scoreAsInt score
        viewScore =
            if scoreDiff /= 0 then
                [ a [ class "tally-value" ] [ text totalScore ]
                , a [ class "tally-diff" ]  [ text <| toString scoreDiff ]
                ]
            else
                [ a [ class "tally-value" ] [ text totalScore ] ]
    in
        div [ styleOffset, class "tally" ]
            [ a [ class "tally-name" ] [text (name ++ " :")]
            , p [ class "tally-score" ] viewScore
            ]


{-|Given a list of things that can be compared with a given criteria,
get the offsets for each element when the list is ordered by the criteria:

     -- list after ordering: [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]
     offsetBy identity [ 'c', 'a', 'b', 'd', 'e', 'g', 'f' ]
         --> [ 2, -1, -1, 0, 0, 1, -1 ]

In our example, 'c' went from index 0 -> 2 (offset = +2) and 'a' 1 -> 0 (= -1),
etc.
-}
offsetBy : (a -> comparable) -> List a -> List Int
offsetBy sortCriteria ordered =
    let
        newOrder =
            List.indexedMap (,) ordered
                |> List.sortBy (sortCriteria << second)
                |> List.indexedMap (\ni (oi,_) -> (ni, oi))

        setRankChange (current, previous) =
            Array.set previous (current - previous)

        initArray =
            Array.repeat (List.length newOrder) 0
    in
        List.foldl setRankChange initArray newOrder |> Array.toList


{-| Sorted list of tally for this round, with differences. -}
viewRoundTally : Room -> Html msg
viewRoundTally room =
    let
        previousRankings =
            roomAsList room
                |> List.sortBy (\(_, {score}) ->
                    -(scoreAsInt (List.drop 1 score))
                )

        offsets =
            offsetBy (\(_, {score}) -> -(scoreAsInt score)) previousRankings

        toTallies offset (name, score) =
            playerTally True offset name score
    in
        div [ id "tally-round-board" ]
            (List.map2 toTallies offsets previousRankings)


{-| Sorted list of tally for the previous round -}
viewPreviousTally : Room -> Html msg
viewPreviousTally room =
    let
        roomList =
            roomAsList room
                |> List.map (\(x, p) -> (x, { p | score = List.drop 1 p.score}))
                |> List.sortBy (\(_, {score}) -> -(scoreAsInt score))
    in
        div [ id "tally-round-board" ]
            (List.map (uncurry (playerTally False 0)) roomList)


{-| List of association name <-> Player (including `me`)
-}
roomAsList : Room -> List (String, Player)
roomAsList { me, state } =
    let
        fullList ps = (mapFirst Api.showName me :: toList ps)
    in
        case state of
            Alone -> [ mapFirst Api.showName me ]
            BreakWith  ps -> fullList ps
            LobbyWith  ps -> fullList ps
            MasterOf   ps -> fullList ps
            ArtistWith ps -> fullList ps
            PlayingWith _ ps -> fullList ps


roundAsInt : Api.RoundScore -> Int
roundAsInt roundScore =
    case roundScore of
        Api.WasArtist  s -> s
        Api.HasGuessed s -> s
        Api.HasFailed -> 0
        Api.WasAbsent -> 0

scoreAsInt : Score -> Int
scoreAsInt =
    List.foldl (+) 0 << List.map roundAsInt


view : Room -> Html msg
view { me, state } =
    let
        showMe = mapFirst Api.showName me
        userRow : Maybe String -> ( String, Player ) -> Html msg
        userRow artistName ( name, { score, succeeded } ) =
            li
                [ classList
                    [ ( "user", True )
                    , ( "me", name == first showMe )
                    , ( "artist", Just name == artistName )
                    , ( "guessed", succeeded )
                    ]
                ]
                [ a [] [ text name ]
                , a [] [ text <| toString <| scoreAsInt score ]
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

