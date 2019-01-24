module Pintclone.Room
    exposing
        ( newLobby
        , newRound
        , joins
        , leaves
        , becomeMaster
        , isMaster
        , view
        , Room
        , Artist(Me, Another)
        , MasterStatus(Master, Peasant)
        )

{-| Manages state changes related to the user list.

Handles who is the master in the lobby part of the game. And who is the
artist in the game part of the game.

-}

import Html as H exposing (Html, div, text, ul, li, label)
import Html.Attributes exposing (id, class, align, classList)
import List.Nonempty exposing (Nonempty(..), filter, uniq, fromElement, cons, toList, (:::))
import List exposing (map, sortBy)
import API


type alias Room =
    { me : API.Name
    , state : State
    }


type Artist
    = Me
    | Another API.Name


type State
    = NormalWith (Nonempty API.Name)
    | MasterOf (Nonempty API.Name)
    | PlayingWith (Nonempty API.Name) -- head is the artist
    | ArtistWith (Nonempty API.Name)
    | Alone


{-| If the current user is master or not, used as flag for the
`newLobby` function.
-}
type MasterStatus
    = Master
    | Peasant


{-| newLobby, returns an initialized Room

    Room.newLobby user userList

Is the room state during the "lobby" period of the game where the current
user is `user` and the other users are `userList`. The empty list case is
handled properly.

In order to initialize the room during a "round", see `newRound`.

TODO: think about what happens when me \in opponentsList.

-}
newLobby : MasterStatus -> API.Name -> List API.Name -> Room
newLobby status me opponentsList_ =
    let
        opponentsList =
            List.filter ((/=) me) opponentsList_
    in
        case opponentsList of
            [] ->
                Room me Alone

            h :: t ->
                case status of
                    Master ->
                        Room me <| MasterOf <| uniq <| Nonempty h t

                    Peasant ->
                        Room me <| NormalWith <| uniq <| Nonempty h t


{-| Create a room in "round" state.
TODO: think about what happens when me \in opponentsList.
-}
newRound : API.Name -> List API.Name -> Artist -> Room
newRound me opponentsList_ artist =
    let
        opponentsList =
            List.filter ((/=) me) opponentsList_
    in
        case artist of
            Me ->
                case opponentsList of
                    [] ->
                        Room me Alone

                    h :: t ->
                        Room me <| ArtistWith <| uniq <| Nonempty h t

            Another name ->
                Room me <| PlayingWith <| uniq <| Nonempty name opponentsList


{-| Remove a name from the opponents list, operation on the state.

TODO: think what happens when the artist leaves.
TODO: signal an issue if we try to `popState Alone` (so we can `Sync`)

-}
popState : API.Name -> State -> State
popState leaving state =
    let
        removeFromState state_ opponents =
            case opponents of
                Nonempty head [] ->
                    if head == leaving then
                        Alone
                    else
                        state_ <| fromElement head

                Nonempty head (tail :: []) ->
                    if head == leaving then
                        state_ <| fromElement tail
                    else if tail == leaving then
                        state_ <| fromElement head
                    else
                        state_ opponents

                Nonempty head (head2 :: tail) ->
                    state_ <| filter ((/=) leaving) head opponents
    in
        case state of
            Alone ->
                Alone

            NormalWith opponents ->
                removeFromState NormalWith opponents

            MasterOf opponents ->
                removeFromState MasterOf opponents

            -- We need to handle what happens if the leaver is the artist
            PlayingWith (Nonempty artist opponents) ->
                if artist == leaving then
                    case opponents of
                        [] ->
                            Alone

                        h :: t ->
                            removeFromState NormalWith <| Nonempty h t
                else
                    removeFromState PlayingWith <| Nonempty artist opponents

            ArtistWith opponents ->
                removeFromState ArtistWith opponents


{-| Add an opponent to the list of users, operation on state.
-}
pushState : API.Name -> State -> State
pushState joining state =
    case state of
        Alone ->
            MasterOf <| fromElement joining

        NormalWith opponents ->
            NormalWith <| uniq <| cons joining opponents

        MasterOf opponents ->
            MasterOf <| uniq <| cons joining opponents

        PlayingWith (Nonempty head tail) ->
            PlayingWith <| uniq <| Nonempty head (joining :: tail)

        ArtistWith opponents ->
            ArtistWith <| uniq <| cons joining opponents


{-| Promote user as master from given state.

This does nothing if the `state` is in a "Round" state.

-}
masterState : State -> State
masterState state =
    case state of
        NormalWith opponents ->
            MasterOf opponents

        anyelse ->
            anyelse


{-| The given room where an opponent left.
TODO: think about what to do if the opponent doesn't exist.
-}
leaves : API.Name -> Room -> Room
leaves leaving room =
    { room | state = popState leaving room.state }


{-| Turn the user into the game master (that means he has the role of
starting the game).
-}
becomeMaster : Room -> Room
becomeMaster room =
    { room | state = masterState room.state }


{-| Whether the player is master of the room or not.
-}
isMaster : Room -> Bool
isMaster { state } =
    case state of
        Alone ->
            True

        MasterOf _ ->
            True

        _ ->
            False


{-| The given room where someone new joined.
-}
joins : API.Name -> Room -> Room
joins joining ({ me } as room) =
    if joining == me then
        room
    else
        { room | state = pushState joining room.state }


view : Room -> Html msg
view ({ me, state } as room) =
    let
        userRow artistName name =
            li
                [ classList
                    [ ( "userentry", True )
                    , ( "userentry-me", name == me )
                    , ( "userentry-artist", Just name == artistName )
                    ]
                ]
                [ label [ class "username", align "left" ]
                    [ text <| API.showName name ]
                , label [ class "userscore", align "right" ]
                    [ text "100" ]
                ]

        nameList maybeartist opponents =
            toList opponents
                |> sortBy API.showName
                |> map (userRow maybeartist)
                |> ul [ id "userlist", class "top-layout" ]
    in
        case state of
            Alone ->
                nameList Nothing <| fromElement me

            NormalWith opponents ->
                nameList Nothing (me ::: opponents)

            MasterOf opponents ->
                nameList Nothing (me ::: opponents)

            PlayingWith (Nonempty artist opponents) ->
                nameList (Just artist) (artist ::: Nonempty me opponents)

            ArtistWith opponents ->
                nameList (Just me) (me ::: opponents)
