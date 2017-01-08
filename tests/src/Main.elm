port module Main exposing (..)

import Html.AttributeBuilder
import Json.Decode exposing (Value)
import Test exposing (..)
import Test.Runner.Node exposing (run)


port emit : ( String, Value ) -> Cmd msg


main =
    run emit all


all : Test
all =
    describe "All tests"
        []
