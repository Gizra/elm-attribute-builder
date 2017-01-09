port module Main exposing (..)

import Expect
import Html exposing (Html, Attribute)
import Html.AttributeBuilder exposing (..)
import Json.Decode exposing (Value)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Test.Runner.Node exposing (TestProgram, run)


port emit : ( String, Value ) -> Cmd msg


main : TestProgram
main =
    run emit all


vid : List (Html msg) -> List (Attribute msg) -> Html msg
vid =
    flip Html.div


testAddClass : Test
testAddClass =
    describe "addClass"
        [ test "add single class" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "add second class" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> addClass "second-class"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ classes
                            [ "classy"
                            , "second-class"
                            ]
                        ]
        , test "add class twice" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> addClass "classy"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        ]


testAddClassList : Test
testAddClassList =
    describe "addClassList"
        [ test "add single class" <|
            \_ ->
                attributeBuilder
                    |> addClassList [ ( "classy", True ) ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "add two classes" <|
            \_ ->
                attributeBuilder
                    |> addClassList
                        [ ( "classy", True )
                        , ( "second-class", True )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ classes
                            [ "classy"
                            , "second-class"
                            ]
                        ]
        , test "add class twice" <|
            \_ ->
                attributeBuilder
                    |> addClassList
                        [ ( "classy", True )
                        , ( "classy", True )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "add some true some false" <|
            \_ ->
                attributeBuilder
                    |> addClassList
                        [ ( "classy", True )
                        , ( "second-class", False )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "false doesn't remove" <|
            \_ ->
                attributeBuilder
                    |> addClass "second-class"
                    |> addClassList
                        [ ( "classy", True )
                        , ( "second-class", False )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ classes
                            [ "classy"
                            , "second-class"
                            ]
                        ]
        ]


testApplyClassList : Test
testApplyClassList =
    describe "applyClassList"
        [ test "add single class" <|
            \_ ->
                attributeBuilder
                    |> applyClassList [ ( "classy", True ) ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "add two classes" <|
            \_ ->
                attributeBuilder
                    |> applyClassList
                        [ ( "classy", True )
                        , ( "second-class", True )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ classes
                            [ "classy"
                            , "second-class"
                            ]
                        ]
        , test "add class twice" <|
            \_ ->
                attributeBuilder
                    |> applyClassList
                        [ ( "classy", True )
                        , ( "classy", True )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "add some true some false" <|
            \_ ->
                attributeBuilder
                    |> applyClassList
                        [ ( "classy", True )
                        , ( "second-class", False )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "false actually removes" <|
            \_ ->
                attributeBuilder
                    |> addClass "second-class"
                    |> applyClassList
                        [ ( "classy", True )
                        , ( "second-class", False )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        ]


testPlainAttributeBuilder : Test
testPlainAttributeBuilder =
    test "A plain attributeBuilder should produce no attributes" <|
        \_ ->
            attributeBuilder
                |> toAttributes
                |> List.length
                |> Expect.equal 0


testRemoveClass : Test
testRemoveClass =
    describe "removeClass"
        [ test "remove class that was added" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> removeClass "classy"
                    |> toAttributes
                    |> List.length
                    |> Expect.equal 0
        , test "remove one of two classes" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> addClass "to-remove"
                    |> removeClass "to-remove"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        , test "remove class that wasn't added" <|
            \_ ->
                attributeBuilder
                    |> addClass "classy"
                    |> removeClass "not-there"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ className "classy" ]
        ]


all : Test
all =
    describe "AttributeBuilder tests"
        [ testAddClass
        , testAddClassList
        , testApplyClassList
        , testPlainAttributeBuilder
        , testRemoveClass
        ]
