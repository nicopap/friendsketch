module Pintclone.Guess exposing (Guess, Msg(..), view, update, new)

import API
import String exposing (toList, fromChar)
import Array exposing (Array)
import Html.Attributes exposing (id, class)
import Html exposing (Html, div, a, text)


type Guess
    = Word
        { word : Word
        , timeout : Int
        }


type Word
    = ToGuess (Array (Maybe Char))
    | Completed String
    | Artist String
    | Spectator


type Msg
    = RevealLetter Int Char
    | SetTimeout Int
    | RevealAll String


new : Maybe API.Guess -> Int -> Guess
new maybeArtist timeout =
    let
        word =
            case maybeArtist of
                Just (API.ForArtist wordToGuess) ->
                    Artist wordToGuess

                Just (API.GuessOfLength wordLength) ->
                    ToGuess (Array.repeat wordLength Nothing)

                Nothing ->
                    Spectator

    in
        Word { word = word, timeout = timeout }


reveal : Int -> Char -> Word -> Word
reveal index char word =
    case word of
        ToGuess charArray ->
            ToGuess <| Array.set index (Just char) charArray

        anythingElse ->
            anythingElse


update : Msg -> Guess -> Guess
update msg (Word guess) =
    case msg of
        RevealLetter index char ->
            Word { guess | word = reveal index char guess.word }

        RevealAll completeWord ->
            Word { guess | word = Completed completeWord }

        SetTimeout timeout ->
            Word { guess | timeout = timeout }


view : Guess -> Html msg
view (Word { word, timeout }) =
    let
        toGuessLetter maybeChar =
            case maybeChar of
                Just char ->
                    a [ class "guess-letter" ] [ text (fromChar char) ]

                Nothing ->
                    a [ class "guess-hidden" ] [ text "_" ]

        viewTimeout =
            a [ id "timer" ] [ text <| toString timeout ]

        viewWord =
            case word of
                Completed completed ->
                    toList completed
                        |> String.fromList
                        |> (\word -> [ a [ class "complete" ] [ text word ] ])

                ToGuess charArray ->
                    Array.map (toGuessLetter) charArray
                        |> Array.toList

                Artist artist ->
                    toList artist
                        |> String.fromList
                        |> (\word -> [ a [ class "complete" ] [ text word ] ])

                Spectator ->
                    [ text "Specating, you'll start next round" ]

    in
        div [ id "guess-bar" ] [ viewTimeout, div [id "guess-word"] viewWord ]