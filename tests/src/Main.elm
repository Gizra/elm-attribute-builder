port module Main exposing (..)

import Expect
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.AttributeBuilder exposing (..)
import Json.Decode exposing (Value)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner.Node exposing (run)


port emit : ( String, Value ) -> Cmd msg


main =
    run emit all


all : Test
all =
    describe "AttributeBuilder tests"
        [ test "A plain attributeBuilder should produce no attributes" <|
            \_ ->
                attributeBuilder
                    |> toAttributes
                    |> List.length
                    |> Expect.equal 0
        ]
