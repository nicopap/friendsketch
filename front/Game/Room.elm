module Game.Room
    exposing
        ( newInLobby
        , newGame
        , joinBreak
        , joinRound
        , joinScores
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
        , viewBoard
        , guessed
        , playerCount
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

import Html as H exposing (Html, text, ul, li, a, div, p, h2, td, th, tr)
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
    | BreakWith Players
    | EndWith Players
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

    Room.newInLobby master user userList

Is the room state during the "lobby" period of the game where the current
user is `user` and the other users are `userList`. The empty list case is
handled properly.

In order to initialize the room during a "round", see `joinRound`.
-}
newInLobby : Api.Name -> Api.Name -> Api.Scoreboard -> Room
newInLobby master myName scoreboard =
    let notAlone = if master == myName then MasterOf else LobbyWith
    in  newFromList scoreboard myName notAlone

{-| Resets the scores of all players, as if the game just started
-}
newGame : Room -> Room
newGame { me, state } =
    let
        resetPlayer = Dict.map (\_ _ -> Player False [])
    in
        Room (mapSecond (always (Player False [])) me)
            <| mapState resetPlayer state

{-| Create a room for player joining while the scores are shown
-}
joinBreak : Api.Name -> Api.Scoreboard -> Room
joinBreak me scoreboard = newFromList scoreboard me BreakWith

{-| Join the game during the final score display
-}
joinScores : Api.Name -> Api.Scoreboard -> Room
joinScores me scoreboard = newFromList scoreboard me EndWith

{-| Create a room in "round" state.
-}
joinRound : Api.Name -> Api.Name -> Api.Scoreboard -> Room
joinRound artistName myName scoreboard =
    newFromList scoreboard myName (PlayingWith <| Api.showName artistName)

forPlayers : (Players -> a) -> State -> a
forPlayers f = andThenState (always f) (always f)

andThenState :
   ((Players -> State) -> Players -> a)
   -> (String -> Players -> a)
   -> State -> a
andThenState bind mapArtist state =
    case state of
        Alone -> bind MasterOf Dict.empty
        EndWith s -> bind EndWith s
        BreakWith  s -> bind BreakWith s
        LobbyWith  s -> bind LobbyWith s
        MasterOf   s -> bind MasterOf s
        PlayingWith a s -> mapArtist a s

mapState : (Players -> Players) -> State -> State
mapState trans =
    let
        mapper toState players =
            let newPlayers = trans players
            in  if Dict.isEmpty newPlayers then Alone else toState newPlayers
    in
        andThenState mapper (mapper << PlayingWith)


{-| The given room where an opponent left.
-}
leaves : Api.Name -> Room -> Room
leaves leaving_ { me, state } =
    let
        leaving = Api.showName leaving_

        popState toState s =
            let removed = remove leaving s
            in  if Dict.isEmpty removed then Alone else toState removed

        popArtist artist =
            if artist == leaving then
                popState BreakWith
            else
                popState (PlayingWith artist)
    in
        Room me <| andThenState popState popArtist state


{-| The given room where someone new joined.
-}
joins : Api.Name -> Room -> Room
joins joining { me, state } =
    let
        push = insert (Api.showName joining) (Player False [])
    in
        Room me <| mapState push state


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
setArtist newArtist { me, state } =
    let
        withArtist others =
            if Dict.isEmpty others
            then Alone
            else PlayingWith (Api.showName newArtist) others
    in
        Room me <| forPlayers withArtist state


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
amArtist { me, state } =
    case state of
        PlayingWith artistName _ -> artistName == Api.showName (first me)
        _ -> False

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
        _ -> False


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
    in
        Room (myName_, myNewScore) <| mapState updateOthers state


myName : Room -> Api.Name
myName { me } = first me

{-| Set a player as having guessed the word correctly.
-}
guessed : Api.Name -> Room -> Room
guessed guesser { state, me } =
    if guesser == first me then
        { me = mapSecond (\x -> { x | succeeded = True }) me, state = state }
    else
        let
            setSuccess = Maybe.map (\player -> { player | succeeded = True })
            withGuess = update (Api.showName guesser) setSuccess
        in
            Room me <| mapState withGuess state

{-| Displays the score for a player. If first argument is true, will also
display the differences in points from last round and offset the element to
reorder the list according to the given offsets.
-}
playerTally : Bool -> Int -> String -> Player -> Html msg
playerTally showDiff offset name { score } =
    let
        scoreDiff = case (score, showDiff) of
            (h::_, True) -> roundAsInt h
            _ -> 0

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
                    negate (scoreAsInt (List.drop 1 score))
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
                |> List.sortBy (\(_, {score}) -> negate (scoreAsInt score))
    in
        div [ id "tally-round-board" ]
            (List.map (uncurry (playerTally False 0)) roomList)


{-| List of association name <-> Player (including `me`)
-}
roomAsList : Room -> List (String, Player)
roomAsList { me, state } =
    forPlayers ((::) (mapFirst Api.showName me) << toList) state


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

playerCount : Room -> Int
playerCount = (+) 1 << forPlayers Dict.size << .state


view : Room -> Html msg
view { me, state } =
    let
        userRow : Maybe String -> ( String, Player ) -> Html msg
        userRow artistName ( name, { score, succeeded } ) =
            li
                [ classList
                    [ ( "user", True )
                    , ( "me", name == Api.showName (first me) )
                    , ( "artist", Just name == artistName )
                    , ( "guessed", succeeded )
                    ]
                ]
                [ a [] [ text name ]
                , a [] [ text <| toString <| scoreAsInt score ]
                ]

        render : Maybe String -> Players -> Html msg
        render maybeartist others =
            others
                |> toList
                |> (::) (mapFirst Api.showName me)
                |> List.sortBy first
                |> List.map (userRow maybeartist)
                |> ul [ id "userlist" ]
    in
        andThenState (always <| render Nothing) (render << Just) state


endBoard : List { name : String, total : Int, score : Score } -> Html msg
endBoard scores =
    let
        sortedScores = List.sortBy (negate << .total) scores

        displayWinners w =
            case w of
                [] -> "No winners?? (please get in touch, this is a bug)"
                [ single ] -> single ++ " wins!"
                h::t -> String.join ", " t ++ " and " ++ h ++ " win!"

        bestScore =
            List.head sortedScores
                |> Maybe.map .total
                |> withDefault 0

        winners =
            List.filter ((==) bestScore << .total) sortedScores
                |> List.map .name
                |> displayWinners

        toCell : Api.RoundScore -> Html msg
        toCell score =
            case score of
                Api.WasArtist s -> td [ class "artist" ] [ text (toString s) ]
                Api.HasGuessed s -> td [] [ text (toString s) ]
                Api.HasFailed -> td [] [ text "0" ]
                Api.WasAbsent -> td [] [ text "–" ]

        toRow { name, total, score } =
            tr []
                (  th [ class "name" ] [ text name ]
                :: th [ class "total" ] [ text (toString total) ]
                :: td [ class "colon" ] [ text ":" ]
                :: List.map toCell score
                )
    in
        div [ id "finalscores" ]
            [ h2 [] [ text winners ]
            , H.table [] [ H.tbody [] (List.map toRow sortedScores) ]
            ]


viewBoard : Room -> Html msg
viewBoard room =
    let
        adaptEntry (name, { score }) =
            { name = name
            , total = scoreAsInt score
            , score = score
            }
    in
        roomAsList room
            |> List.map adaptEntry
            |> endBoard
