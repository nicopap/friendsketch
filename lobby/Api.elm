module Api exposing (Deck, Topic, Difficulty(..), Collection, decodeDecks, encodeCollection)

import Json.Decode as D exposing (Decoder, oneOf, andThen)
import Json.Encode as E exposing (Value)

type alias Collection =
    { decks : List Deck
    , distrs : (Float, Float, Float)
    }

type Difficulty
    = Easy
    | Normal
    | Hard


type alias Topic =
    { id : Int
    , name : String
    }


-- TODO: separate type for encoding and decoding, eluding wordCount on
-- encoding.
type alias Deck =
    { difficulty : Difficulty
    , topic : Topic
    , wordCount : Int
    }

{-| A convenience function to build empty sum type alternatives.
Note:

    -- isAlt field constr = D.field field D.succeed constr

    (isAlt "action" Action) == (D.field "action" (D.succeed Action))

-}
isAlt : String -> x -> Decoder x
isAlt field constr =
    D.string
        |> D.andThen (\stringName ->
            if stringName == field then
                D.succeed constr
            else
                D.fail ("expected a field with name \""++ field ++ "\""
                    ++ " instead got: \"" ++ stringName ++ "\"")
        )



encodeDifficulty : Difficulty -> Value
encodeDifficulty difficulty =
    case difficulty of
        Easy -> E.string "easy"
        Normal -> E.string "normal"
        Hard -> E.string "hard"

encodeDeck : Deck -> Value
encodeDeck deck =
    E.object
        [ ("difficulty", encodeDifficulty deck.difficulty)
        , ("topic", E.int deck.topic.id)
        ]


encodeCollection : Collection -> Value
encodeCollection { decks, distrs } =
    let (easy, normal, hard) = distrs
    in
        E.object
            [ ("decks", E.list encodeDeck decks)
            , ("distrs", E.list E.float [ easy, normal, hard ])
            ]

decodeTopic : Decoder Topic
decodeTopic =
    D.map2 Topic
        (D.field "id" D.int)
        (D.field "name" D.string)


decodeDifficulty : Decoder Difficulty
decodeDifficulty =
    oneOf
        [ isAlt "easy" Easy
        , isAlt "normal" Normal
        , isAlt "hard" Hard
        ]

decodeDeck : Decoder Deck
decodeDeck =
    D.map3 Deck
        (D.field "difficulty" decodeDifficulty)
        (D.field "topic" decodeTopic)
        (D.field "word_count" D.int)

decodeDecks : Decoder (List Deck)
decodeDecks = D.list decodeDeck

