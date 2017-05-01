port module Main exposing (..)

import Expect exposing (Expectation)
import Html exposing (Html, Attribute)
import Html.Attributes
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


hasOne : List Selector -> Query.Single -> Expectation
hasOne selectors =
    Query.findAll selectors >> Query.count (Expect.equal 1)


hasNone : List Selector -> Query.Single -> Expectation
hasNone selectors =
    Query.findAll selectors >> Query.count (Expect.equal 0)


{-| This tests various assertions about the behaviour of lists of
    attributes if you don't use AttributeBuilder.
-}
testAlternatives : Test
testAlternatives =
    describe "Alternatives to AttributeBuilder"
        [ test "adding class twice uses both classes" <|
            -- The behaviour here changed between Elm 0.17 and Elm 0.18.
            -- In Elm 0.17, the last class "won", but in Elm 0.18 both
            -- classes get used.
            \_ ->
                [ Html.Attributes.class "first-class"
                , Html.Attributes.class "second-class"
                ]
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "first-class", "second-class" ] ]
        , test "adding different styles twice uses both" <|
            \_ ->
                [ Html.Attributes.style [ ( "width", "300px" ) ]
                , Html.Attributes.style [ ( "height", "400px" ) ]
                ]
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "width" "300px"
                        , containsStyle "height" "400px"
                        ]
        , test "adding same style twice uses last value" <|
            \_ ->
                [ Html.Attributes.style [ ( "width", "300px" ) ]
                , Html.Attributes.style [ ( "width", "400px" ) ]
                ]
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "width" "400px"
                        , doesNotContainStyle "width" "300px"
                        ]
        , test "adding same attribute twice uses last value" <|
            \_ ->
                [ Html.Attributes.href "index1.html"
                , Html.Attributes.href "index2.html"
                ]
                    |> vid []
                    |> (\wrap -> Html.div [] [ wrap ])
                    |> Query.fromHtml
                    |> Expect.all
                        [ hasOne [ attribute "href" "index2.html" ]
                        , hasNone [ attribute "href" "index1.html" ]
                        ]
        ]


testAddAttribute : Test
testAddAttribute =
    describe "addAttribute"
        [ test "add single attribute" <|
            \_ ->
                attributeBuilder
                    |> addAttribute (Html.Attributes.src "index.html")
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ attribute "src" "index.html" ]
        , test "add second attribute" <|
            \_ ->
                attributeBuilder
                    |> addAttribute (Html.Attributes.src "index.html")
                    |> addAttribute (Html.Attributes.href "page.html")
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ attribute "src" "index.html"
                        , attribute "href" "page.html"
                        ]
        ]


testAddAttributes : Test
testAddAttributes =
    describe "addAttributes"
        [ test "add single attribute" <|
            \_ ->
                attributeBuilder
                    |> addAttributes
                        [ Html.Attributes.src "index.html" ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ attribute "src" "index.html" ]
        , test "add second attribute" <|
            \_ ->
                attributeBuilder
                    |> addAttributes
                        [ Html.Attributes.src "index.html"
                        , Html.Attributes.href "page.html"
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Query.has
                        [ attribute "src" "index.html"
                        , attribute "href" "page.html"
                        ]
        ]


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


{-| Check if the `toString` representation of the HTML contains
the specified style. This doesn't seem to be supported by `elm-html-test`
itself.
-}
containsStyle : String -> String -> Query.Single -> Expectation
containsStyle styleName styleValue html =
    let
        -- This is a bit hack-ish, but the best we can do without support
        -- from elm-html-test
        expectedText =
            "(" ++ toString styleName ++ "," ++ toString styleValue ++ ")"
    in
        if (String.contains expectedText (toString html)) then
            Expect.pass
        else
            Expect.fail <|
                "Did not find style '"
                    ++ styleName
                    ++ "', '"
                    ++ styleValue
                    ++ "'"


doesNotContainStyle : String -> String -> Query.Single -> Expectation
doesNotContainStyle styleName styleValue html =
    let
        unexpectedText =
            "(" ++ toString styleName ++ "," ++ toString styleValue ++ ")"
    in
        if (String.contains unexpectedText (toString html)) then
            Expect.fail <|
                "Found unexpected style '"
                    ++ styleName
                    ++ "', '"
                    ++ styleValue
                    ++ "'"
        else
            Expect.pass


testAddStyle : Test
testAddStyle =
    describe "addStyle"
        [ test "add single style" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> containsStyle "position" "absolute"
        , test "add second style" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> addStyle "z-index" "3000"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "position" "absolute"
                        , containsStyle "z-index" "3000"
                        ]
        , test "add style twice replaces" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> addStyle "position" "relative"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "position" "relative"
                        , doesNotContainStyle "position" "absolute"
                        ]
        ]


testAddStyles : Test
testAddStyles =
    describe "addStyles"
        [ test "add single style" <|
            \_ ->
                attributeBuilder
                    |> addStyles [ ( "position", "absolute" ) ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> containsStyle "position" "absolute"
        , test "add two styles" <|
            \_ ->
                attributeBuilder
                    |> addStyles
                        [ ( "position", "absolute" )
                        , ( "z-index", "100" )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "position" "absolute"
                        , containsStyle "z-index" "100"
                        ]
        , test "add style twice" <|
            \_ ->
                attributeBuilder
                    |> addStyles
                        [ ( "position", "absolute" )
                        , ( "position", "relative" )
                        ]
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "position" "relative"
                        , doesNotContainStyle "position" "absolute"
                        ]
        ]


testRemoveStyle : Test
testRemoveStyle =
    describe "removeStyle"
        [ test "remove style that was added" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> removeStyle "position"
                    |> toAttributes
                    |> List.length
                    |> Expect.equal 0
        , test "remove one of two styles" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> addStyle "z-index" "10000"
                    |> removeStyle "z-index"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> Expect.all
                        [ containsStyle "position" "absolute"
                        , doesNotContainStyle "z-index" "10000"
                        ]
        , test "remove style that wasn't added" <|
            \_ ->
                attributeBuilder
                    |> addStyle "position" "absolute"
                    |> removeStyle "not-there"
                    |> toAttributes
                    |> vid []
                    |> Query.fromHtml
                    |> containsStyle "position" "absolute"
        ]


all : Test
all =
    describe "AttributeBuilder tests"
        [ testAlternatives
        , testPlainAttributeBuilder
        , testAddAttribute
        , testAddAttributes
        , testAddClass
        , testRemoveClass
        , testApplyClassList
        , testAddClassList
        , testAddStyle
        , testAddStyles
        , testRemoveStyle
        ]
