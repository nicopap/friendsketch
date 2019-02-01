module Pintclone.Room
    exposing
        ( newInLobby
        , joinBreak
        , joinRound
        , joins
        , leaves
        , becomeMaster
        , setArtist
        , isMaster
        , view
        , alone
        , Room
        )

{-| Manages state changes related to the user list.

Handles who is the master in the lobby part of the game. And who is the
artist in the game part of the game.

-}

import Html as H exposing (Html, text, ul, li, a)
import Html.Attributes exposing (id, classList)
import List.Nonempty exposing (Nonempty(Nonempty), (:::), filter, fromElement, cons, toList, sortWith)
import Api


type alias Room =
    { me : Api.Name
    , state : State
    }


type State
    = LobbyWith (Nonempty Api.Name)
    | MasterOf (Nonempty Api.Name)
    | PlayingWith (Nonempty Api.Name) -- head is the artist
    | ArtistWith (Nonempty Api.Name)
    | BreakWith (Nonempty Api.Name)
    | Alone


{-| newInLobby, returns an initialized Room

    Room.newInLobby user userList

Is the room state during the "lobby" period of the game where the current
user is `user` and the other users are `userList`. The empty list case is
handled properly.

In order to initialize the room during a "round", see `joinRound`.
-}
newInLobby : Api.Name -> Api.Name -> List Api.Name -> Room
newInLobby master me others =
    case List.filter ((/=) me) others of
        []   -> Room me Alone
        h::t ->
            if master == me then
                Room me <| MasterOf <| Nonempty h t
            else
                Room me <| LobbyWith <| Nonempty h t


{-| Create a room for player joining while the scores are shown
-}
joinBreak : Api.Name -> List Api.Name -> Room
joinBreak me others =
    case List.filter (\n -> n /= me) others of
        []   -> Room me Alone
        h::t -> Room me <| BreakWith <| Nonempty h t


{-| Create a room in "round" state.
-}
joinRound : Api.Name -> List Api.Name -> Api.Name -> Room
joinRound me others_ artist =
    let
        others = List.filter (\n -> n /= me && n /= artist) others_
    in
        if artist == me then
            case others of
                []   -> Room me Alone
                h::t -> Room me <| ArtistWith <| Nonempty h t
        else
            Room me <| PlayingWith <| Nonempty artist others


{-| Remove a name from the opponents list, operation on the state.
-}
popState : Api.Name -> State -> State
popState leaving state =
    let
        pop state_ others =
            case others of
                Nonempty head [] ->
                    if head == leaving then
                        Alone
                    else
                        state_ <| fromElement head

                Nonempty head ((subHead :: subTail) as tail) ->
                    state_ <|
                        if head == leaving then
                            Nonempty subHead subTail
                        else if subHead == leaving then
                            Nonempty head subTail
                        else
                            Nonempty head (List.filter ((/=) leaving) tail)
    in
        case state of
            Alone -> Alone
            BreakWith  others -> pop BreakWith others
            LobbyWith  others -> pop LobbyWith others
            MasterOf   others -> pop MasterOf others
            ArtistWith others -> pop ArtistWith others
            PlayingWith (Nonempty artist others) ->
                if artist == leaving then
                    case others of
                        []   -> Alone
                        h::t -> pop BreakWith <| Nonempty h t
                else
                    pop PlayingWith <| Nonempty artist others



{-| Add an opponent to the list of users, operation on state.
-}
pushState : Api.Name -> State -> State
pushState new state =
    case state of
        Alone -> MasterOf <| fromElement new
        BreakWith others -> BreakWith <| cons new others
        LobbyWith others -> LobbyWith <| cons new others
        MasterOf  others -> MasterOf  <| cons new others
        ArtistWith others -> ArtistWith <| cons new others
        PlayingWith (Nonempty artist others) ->
            PlayingWith <| Nonempty artist (new :: others)


{-| Promote user as master from given state.

This does nothing if the `state` is not in the Lobby state.

-}
masterState : State -> State
masterState state =
    case state of
        LobbyWith others -> MasterOf others
        anyelse -> anyelse


{-| The given room where an opponent left.
-}
leaves : Api.Name -> Room -> Room
leaves leaving room =
    { room | state = popState leaving room.state }


{-| Turn the user into the game master (that means he has the role of
starting the game).
-}
becomeMaster : Room -> Room
becomeMaster room =
    { room | state = masterState room.state }


setArtist : Api.Name -> Room -> Room
setArtist newArtist { me, state } =
    let
        withArtist others =
            let a = newArtist
                artistFirst elem next =
                    if elem == a then LT else if next == a then GT else EQ

                othersArtistFirst =
                    sortWith artistFirst others
            in
                if newArtist == me then
                    { me = me, state = ArtistWith others }
                else
                    { me = me, state = PlayingWith othersArtistFirst }
    in
        case state of
            BreakWith   others -> withArtist others
            PlayingWith others -> withArtist others
            LobbyWith   others -> withArtist others
            ArtistWith  others -> withArtist others
            MasterOf others -> withArtist others
            Alone -> { me = me, state = Alone }



{-| Whether the player is master of the room or not.
-}
isMaster : Room -> Bool
isMaster { state } =
    case state of
        Alone      -> True
        MasterOf _ -> True
        anyelse    -> False


{-| Whether the room contains only the current player
-}
alone : Room -> Bool
alone room =
    case room.state of
        Alone   -> True
        anyelse -> False

{-| The given room where someone new joined.
-}
joins : Api.Name -> Room -> Room
joins joining room =
    if joining == room.me then
        room
    else
        { room | state = pushState joining room.state }


view : Room -> Html msg
view { me, state } =
    let
        userRow artistName name =
            li
                [ classList
                    [ ( "userentry", True )
                    , ( "userentry-me", name == me )
                    , ( "userentry-artist", Just name == artistName )
                    ]
                ]
                [ a [] [ text <| Api.showName name ]
                , a [] [ text "" ]
                ]

        render maybeartist others =
            toList others
                |> List.sortBy Api.showName
                |> List.map (userRow maybeartist)
                |> ul [ id "userlist" ]
    in
        case state of
            Alone -> render Nothing (fromElement me)
            BreakWith others  -> render Nothing (me ::: others)
            LobbyWith others  -> render Nothing (me ::: others)
            MasterOf others   -> render Nothing (me ::: others)
            ArtistWith others -> render (Just me) (me ::: others)
            PlayingWith (Nonempty artist others) ->
                render (Just artist) (Nonempty artist (me :: others))

