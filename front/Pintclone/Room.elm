module Pintclone.Room
    exposing
        ( newInLobby
        , joinRound
        , joins
        , leaves
        , becomeMaster
        , setArtist
        , isMaster
        , view
        , alone
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
import List.Nonempty exposing (Nonempty(..), filter, uniq, fromElement, cons, toList, (:::), sortWith)
import List exposing (map, sortBy)
import Api


type alias Room =
    { me : Api.Name
    , state : State
    }


type Artist
    = Me
    | Another Api.Name


type State
    = NormalWith (Nonempty Api.Name)
    | MasterOf (Nonempty Api.Name)
    | PlayingWith (Nonempty Api.Name) -- head is the artist
    | ArtistWith (Nonempty Api.Name)
    | Alone


{-| If the current user is master or not, used as flag for the
`newInLobby` function.
-}
type MasterStatus
    = Master
    | Peasant


{-| newInLobby, returns an initialized Room

    Room.newLInobby user userList

Is the room state during the "lobby" period of the game where the current
user is `user` and the other users are `userList`. The empty list case is
handled properly.

In order to initialize the room during a "round", see `joinRound`.
-}
newInLobby : MasterStatus -> Api.Name -> List Api.Name -> Room
newInLobby status me opponentsList_ =
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
-}
joinRound : Api.Name -> List Api.Name -> Artist -> Room
joinRound me opponentsList_ artist =
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
-}
popState : Api.Name -> State -> State
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
pushState : Api.Name -> State -> State
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
        updateState others =
            if newArtist == me then
                { me = me, state = ArtistWith others }
            else
                let
                    artistFirst first second =
                        if first == newArtist then
                            LT
                        else if second == newArtist then
                            GT
                        else
                            EQ

                    newState =
                        PlayingWith <| sortWith artistFirst others
                in
                    { me = me, state = newState }
    in
        case state of
            PlayingWith others -> updateState others
            NormalWith others  -> updateState others
            ArtistWith others  -> updateState others
            MasterOf others    -> updateState others
            anyelse -> { me = me, state = anyelse }



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


{-| Whether the room contains only the current player
-}
alone : Room -> Bool
alone room =
    case room.state of
        Alone -> True
        _ -> False

{-| The given room where someone new joined.
-}
joins : Api.Name -> Room -> Room
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
                    [ text <| Api.showName name ]
                , label [ class "userscore", align "right" ]
                    [ text "100" ]
                ]

        nameList maybeartist opponents =
            toList opponents
                |> sortBy Api.showName
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
