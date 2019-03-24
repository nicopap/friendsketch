module Collection exposing (Collection, view, update, Msg, new, intoApiCollection)

import Json.Decode exposing (map)
import Html as H exposing (Html, div, label, text, br, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, title, classList)
import Dict exposing (Dict)
import Http
import Difficulty exposing (Difficulty(..))
import Array exposing (Array)
import Api exposing (Topic)


type alias Deck =
    { topic : Topic
    , wordCount : Int
    , hard : Bool
    , normal : Bool
    , easy : Bool
    }

type Collection
    = Collection (Dict Int (Bool, Deck))


type Msg
    = ToggleDeck Int

describeDeck : Deck -> String
describeDeck { topic, wordCount, hard, normal, easy } =
    let
        count = String.fromInt wordCount ++ " words"
        difficulty =
            case (easy, normal, hard) of
                (True, True, True) -> "of all difficulties"
                (False, False, False) -> "OOPS"
                (True, False, False) -> "only easy ones"
                (False, True, False) -> "only normal ones"
                (False, False, True) -> "only hard ones"
                (False, True, True) -> "no easy ones"
                (True, False, True) -> "no normal ones"
                (True, True, False) -> "no hard ones"
        description = topic.name
    in
        count ++ ",\n" ++ difficulty ++ ".\n\n" ++ description


viewDeck : (Int, (Bool, Deck)) -> Html Msg
viewDeck (id, (isSelected, deck)) =
    let
        deckClass =
            classList [ ( "deck", True ) , ( "selected", isSelected ) ]
    in
        label
            [ deckClass, title (describeDeck deck), onClick (ToggleDeck id) ]
            [ text deck.topic.name ]

view : Collection -> Html Msg
view (Collection decks) =
    let
        decksView =
            Dict.toList decks
                |> List.map viewDeck
                |> div [ class "collection" ]

        totalWordCount =
            Dict.values decks
                |> List.filter Tuple.first
                |> List.foldl ((+) << .wordCount << Tuple.second) 0
    in
        div []
            [ p [ class "title" ] [ label [] [ text "Categories:" ] ]
            , decksView
            , div [ class "descr" ]
                [ p [] [ text ("words: " ++ String.fromInt totalWordCount) ]
                , text "Select the word categories you want to include in your game."
                ]
            ]



update : Msg -> Collection -> Collection
update msg (Collection decks) =
    let
        maybeToggle = Maybe.map (\(x,y) -> (not x, y))
    in
        case msg of
            ToggleDeck id -> Collection (Dict.update id maybeToggle decks)

-- Build dictionary TopicId -> Deck
fromApiDecks : List Api.Deck -> (Collection, Difficulty)
fromApiDecks decks =
    let
        collectionWith : Api.Deck -> Maybe (Bool, Deck) -> Maybe (Bool, Deck)
        collectionWith { difficulty, topic, wordCount } maybeUpdate =
            let
                defaultDeck =
                    { topic = topic
                    , wordCount = wordCount
                    , hard = False
                    , normal = False
                    , easy = False
                    }

                modify =
                    Tuple.mapSecond <| case difficulty of
                        Api.Easy   -> (\d -> { d | easy = True})
                        Api.Normal -> (\d -> { d | normal = True})
                        Api.Hard   -> (\d -> { d | hard = True})
            in
                Maybe.withDefault (True, defaultDeck) maybeUpdate
                        |> modify
                        |> Just

        addDeck deck acc =
            Dict.update deck.topic.id (collectionWith deck) acc

        collection =
            List.foldl addDeck Dict.empty decks
    in
        ( Collection collection, Difficulty 0.03 0.33 0.13 )


intoApiDeck : Deck -> List Api.Deck
intoApiDeck { topic, easy, normal, hard, wordCount } =
    let
        makeDeck difficulty =
            { difficulty = difficulty
            , topic = topic
            , wordCount = wordCount
            }
    in
        case (easy, normal, hard) of
            (True, True, True) -> [ makeDeck Api.Easy , makeDeck Api.Normal , makeDeck Api.Hard ]
            (True, True, False) -> [ makeDeck Api.Easy , makeDeck Api.Normal ]
            (True, False, True) -> [ makeDeck Api.Easy , makeDeck Api.Hard ]
            (False, True, True) -> [ makeDeck Api.Normal , makeDeck Api.Hard ]
            (True, False, False) -> [ makeDeck Api.Easy ]
            (False, True, False) -> [ makeDeck Api.Normal ]
            (False, False, True) -> [ makeDeck Api.Hard ]
            (False, False, False) -> []


intoApiCollection : (Collection, Difficulty) -> Api.Collection
intoApiCollection (Collection collection, Difficulty e n h) =
    let
        decks =
            Dict.values collection
                |> List.map Tuple.second
                |> List.concatMap intoApiDeck
    in
        { decks = decks , distrs = (e,n,h) }

-- (Res Er a -> msg) -> D a -> Exp msg
-- (a -> C) | ((a -> C) -> Res Er a -> Res Er C) | (Res Er a -> Res Er C)
-- (Res Er C -> gsm)
-- (b -> c) -> (a -> b) -> (a -> c)
new : (Result Http.Error (Collection, Difficulty) -> msg) -> Cmd msg
new adaptor =
    Http.get
        { url = "/decks/en"
        , expect = Http.expectJson adaptor (map fromApiDecks Api.decodeDecks)
        }
