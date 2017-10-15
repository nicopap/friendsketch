module TypeDecoders
    exposing
        ( (<+|), (|+|), (|+>)
        , (<*|), (|*|), (:*), (:^)
        , (:-), (:=)
        , sumType
        )

{-| #TypeDecoders

A mini module that defines an embed DSL for parsing json into types.

I wanted a terser and more autogeneration-friendly way to create elm json
decoders. This module provides operators to help define type decoders as
a natural extension of type declarations.

If you are looking for a more explicit API, you should look into
[elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest)
or the standard library.

The basic idea behind decoding sum types is to use the constructor as
the unique field of an object, and take its content as the constructor
arguments.

Example:

    import Json.Decode as J
    import TypeDecodersMocks exposing (..)

    type alias User = {name : String, id : Int }
    type alias Date = {month: Int, day: Int}
    type Sum = Top User | Bottom String | Left User Date | Right

    userDecoder : J.Decoder User
    userDecoder =
        User
            <*| "name" :* J.string
            |*| "id" :* J.int

    dateDecoder : J.Decoder Date
    dateDecoder =
        Date
            <*| "month" :* J.int
            |*| "day" :* J.int

    sumDecoder : J.Decoder Sum
    sumDecoder =
        sumType
            <+| "bottom" := Bottom <*| J.string
            |+| "top" := Top <*| userDecoder
            |+| "left" := Left <*| userDecoder |*| dateDecoder
            |+> "right" :- Right

    J.decodeString sumDecoder """{"bottom": "Charlemagne"}"""
    --> Ok (Bottom "Charlemagne")

    J.decodeString sumDecoder """{"right": null}"""
    --> Ok Right

    J.decodeString sumDecoder """{"top" : {"name": "Philip", "id": 2}}"""
    --> Ok (Top {name = "Philip", id = 2})

    J.decodeString sumDecoder """{"left" : {"id": 3, "name": "Henry", "month": 5, "day": 30}}"""
    --> Ok (Left { name = "Henry", id = 3 } { month = 5, day = 30 })



## Build product type decoders

The `<*|` and `|*|` operators are delimiters between different type
decoders to combine into one type decoder.

The `:*` and `:^` operators let you specify where to get the object to
decode.

With the following types:

    type alias User_ =
        { name : String
        , id : Int
        , birthDay : Day
        }

    type Day = Day Int Int

You can build a decoder the following way:

    decodeUser_ : J.Decoder User_
    decodeUser_ =
        User_
            <*| "name" :* J.string
            |*| "id" :* J.int
            |*| "birthDay" :* decodeDay

    decodeDay : J.Decoder Day
    decodeDay =
        Day <*| 0 :^ J.int |*| 1 :^ J.int

    J.decodeString decodeUser_ """{ "name" : "Charles", "id" : 1, "birthDay" : [24, 9] }"""
    --> Ok ({ name = "Charles", id = 1, birthDay = (Day 24 9) })

You can also use product type decoders within sum type decoders to decode
constructors with one or more arguments.

@docs (<*|), (|*|), (:*), (:^)



## Sum types

I wrote this library because of the verbosity of sum type decoders in
normal elm.

With the assumption that the data constructor will be the name of an
object that contains what the constructor needs as argument (or a `null`
value), it is very simple to construct a decoder for a sum type.

Consider the following data type and JSON equivalent:

    type ChoiceOfFour
        = Top_ User_
        | Bottom_ String
        | Left_ User_ Day
        | Right_

    -- Json:
    topjson : String
    topjson = """{"top": {"name" : "Louis", "id" : 13, "birthDay" : [14, 5] }}"""
    bottomjson : String
    bottomjson = """{"bottom": "Clovis"}"""
    leftjson : String
    leftjson = """{"left" : [{"name" : "Dagobert", "id" : 1, "birthDay" : [18, 9] },[19,1]]}"""
    rightjson : String
    rightjson = """{"right": null}"""

    -- Decoder:
    decodeChoiceOfFour : J.Decoder ChoiceOfFour
    decodeChoiceOfFour =
        sumType
            <+| "top" := Top_ <*| decodeUser_
            |+| "bottom" := Bottom_ <*| J.string
            |+| "left" := Left_ <*| 0 :^ decodeUser_ |*| 1 :^ decodeDay
            |+> "right" :- Right_

    -- Decoding:
    J.decodeString decodeChoiceOfFour topjson
    --> Ok (Top_ { name = "Louis", id = 13, birthDay = (Day 14 5) })

    J.decodeString decodeChoiceOfFour bottomjson
    --> Ok (Bottom_ "Clovis")

    J.decodeString decodeChoiceOfFour leftjson
    --> Ok (Left_ ({ name = "Dagobert", id = 1, birthDay = (Day 18 9)}) (Day 19 1))

    J.decodeString decodeChoiceOfFour rightjson
    --> Ok Right_

You'll notice the sum type decoders and product type decoders are
composable seamlessly. You'll notice that each sum type constructor
decoder is in fact a praticular case of a product type decoder (for
example: `Top <*| decodeUser_` is the constructor for `Top` with argument
`decodeUser_`). Compare this to the previous example where we defined
`decodeDay`.

@docs sumType, (<+|), (|+|), (|+>), (:-), (:=)

-}

import Json.Decode as J


{-| Give a list of decoders, make it a sum type decoder.

this function is a synonym for `Json.Decode.oneOf`

the `|+|` and `|+>` operators are alias for list concatenation of
decoders, and `<+|` *feeds* the resulting list into this function.
For sum types, we are in fact *just* constructing a list of possible
object fields and picking the existing one. There is no magic really.
-}
sumType : List (J.Decoder a) -> J.Decoder a
sumType = J.oneOf


{-| This is a convenience operator to build empty sum type alternatives.
Note:

    (:-) field constr = field := J.succeed constr

    ("action" :- Action) == ("action" := J.succeed Action)

-}
(:-) : String -> x -> J.Decoder x
(:-) field constr = field := J.succeed constr


{-| What to do with a given field in a sum type decoder declaration.

In a sum type decoder, use `:=` to tell how to build the type with the
given constructor. Combine it with product type decoders for constructors
that have one or more arguments.

Note:

    ("text" := decoder) == (J.field "text" decoder)

It is a synonym for `Json.Decode.field`.

The `:=` operator is identical to `:*`, with a different priority. It is
here so you can build sum type decoders seamlessly.
-}
(:=) : String -> J.Decoder x -> J.Decoder x
(:=) = J.field


{-| Access an object by field in a product type decoder declaration.

It is a synonym for `Json.Decode.field`.
-}
(:*) : String -> J.Decoder o -> J.Decoder o
(:*) = J.field


{-| Last term in a sum type expression.

It concatenates the last two arguments and creates a list.
-}
(|+>) : J.Decoder x -> J.Decoder x -> List (J.Decoder x)
(|+>) d1 d2 = [d1, d2]


{-| Middle terms in sum type expressions.

It's just the concatenation operator `(::)` with a different priority.
-}
(|+|) : J.Decoder x -> List (J.Decoder x) -> List (J.Decoder x)
(|+|) = (::)


{-| First term in sum type expressions.

It is an alias for `<|`, with different priority. It is here mostly to
have a consistent pipeline.
-}
(<+|) : (a -> b) -> a -> b
(<+|) = (<|)


{-| Access an object by index in a product type expression.

It is a synonym for `Json.Decode.index`.
-}
(:^) : Int -> J.Decoder o -> J.Decoder o
(:^) = J.index


{-| First term in product type expressions.

The decoding facilities for product types are inspired by
[elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest),
-}
(<*|) : (a -> b) -> J.Decoder a -> J.Decoder b
(<*|) f da = J.succeed f |*| da


{-| Middle term in product type expressions.

This is equivalent to `Json.Decode.map2 (<|)`.
-}
(|*|) : J.Decoder (a -> b) -> J.Decoder a -> J.Decoder b
(|*|) = J.map2 (<|)


infixl 5 :*
infixl 5 :^
infixl 4 <*|
infixl 4 |*|

infixr 3 :-
infixr 3 :=

infixr 2 <+|
infixr 2 |+|
infixr 2 |+>

