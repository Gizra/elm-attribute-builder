module Html.AttributeBuilder exposing
    ( AttributeBuilder, attributeBuilder, union, toAttributes
    , addAttribute, addAttributes
    , addClass, removeClass, applyClassList, addClassList
    , addStyle, addStyles, removeStyle
    )

{-| Elm's [Html] module constructs HTML nodes by asking you to provide,
among other things, a list of HTML attributes. So, you might typically
see something like:

[Html]: http://package.elm-lang.org/packages/elm-lang/html/latest

    div
        [ class "ui button primary"
        , onClick DoLogin
        ]
        [ text "Login" ]

Sometimes it is convenient to build up the list of attributes through
a series of computations. One can, of course, simply use functions
that return lists of attributes and combine them together ... perhaps
something like this:

    div
        (List.concat
            [ computation1 model
            , computation2 user
            , [ class "another-class"
              , style "position" "absolute"
              ]
            ]
        )
        [ text "Login" ]

And, in Elm 0.18, Elm does some things you might expect with this:

  - If you provide multiple "class" attributes, they get combined appropriately.
    (This wasn't the case in Elm 0.17, which was actually the original motivation
    for this package).

  - If you provide multiple "style" attributes, they are all used.

However, sometimes you might want to build attributes up using types that
give you some more fine-tuned control. That is the purpose of this package.
It allows you to something like this:

    import Html.AttributeBuilder as AB

    AB.attributeBuilder
        |> AB.union (computation1 model)
        |> AB.union (computation2 user)
        |> AB.addClass "another-class"
        |> AB.removeClass "unwanted-class"
        |> AB.addStyle "position" "absolute"
        |> AB.toAttributes

What you get at the end of such a pipeline is a `List Html.Attribute`,
constructed in a way that you might enjoy.

@docs AttributeBuilder, attributeBuilder, union, toAttributes
@docs addAttribute, addAttributes
@docs addClass, removeClass, applyClassList, addClassList
@docs addStyle, addStyles, removeStyle

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)
import String


{-| An opaque type which you can use to gradually build up attributes for
an `Html` element.
-}
type AttributeBuilder msg
    = AttributeBuilder
        { attributes : List (Attribute msg)
        , classes : Set String
        , styles : Dict String String
        }


{-| An empty `AttributeBuilder`, with no attributes yet. You will often
want to start with this.

    attributeBuilder
        |> addClass "a-class"
        |> addStyle "background-color" "red"
        |> toAttributes

-}
attributeBuilder : AttributeBuilder msg
attributeBuilder =
    AttributeBuilder
        { attributes = []
        , classes = Set.empty
        , styles = Dict.empty
        }


{-| Combine two `AttributeBuilder` values. If the names of styles
collide, the values in the first `AttributeBuilder` will be preferred.
-}
union : AttributeBuilder msg -> AttributeBuilder msg -> AttributeBuilder msg
union (AttributeBuilder left) (AttributeBuilder right) =
    AttributeBuilder
        { attributes = left.attributes ++ right.attributes
        , classes = Set.union left.classes right.classes
        , styles = Dict.union left.styles right.styles
        }


{-| Convert an `AttributeBuilder` to a list of attributes. This
will typically be your final step, when you have collected all
the attributes you need and want to supply the attributes to
the `Html` module.
-}
toAttributes : AttributeBuilder msg -> List (Attribute msg)
toAttributes (AttributeBuilder builder) =
    let
        classAttribute =
            if Set.isEmpty builder.classes then
                []

            else
                [ builder.classes
                    |> Set.toList
                    |> String.join " "
                    |> class
                ]

        styleAttribute =
            if Dict.isEmpty builder.styles then
                []

            else
                builder.styles
                    |> Dict.map style
                    |> Dict.values
    in
    classAttribute ++ styleAttribute ++ builder.attributes


{-| Add an attribute to an attribute builder.

This is particularly meant for event handlers (e.g. `on "click" ...`), for which
we don't provide any special support.

Where possible, you should prefer the more specific functions for classes and
styles, since they combine classes and styles in a more predictable manner.

    attributeBuilder
        |> addClass "login"
        |> addAttribute (onClick Login)
        |> toAttributes

-}
addAttribute : Attribute msg -> AttributeBuilder msg -> AttributeBuilder msg
addAttribute attribute (AttributeBuilder builder) =
    AttributeBuilder
        { builder | attributes = attribute :: builder.attributes }


{-| Add a list of attributes to an attribute builder.

This is particularly meant for event handlers (e.g. `on "click" ...`), for which
we don't provide any special support.

Where possible, you should prefer the more specific functions for classes and
styles, since they combine classes and styles in a more predictable manner.

-}
addAttributes : List (Attribute msg) -> AttributeBuilder msg -> AttributeBuilder msg
addAttributes attributes (AttributeBuilder builder) =
    AttributeBuilder
        { builder | attributes = attributes ++ builder.attributes }


{-| Add a class to the `AttributeBuilder`.
-}
addClass : String -> AttributeBuilder msg -> AttributeBuilder msg
addClass class (AttributeBuilder builder) =
    AttributeBuilder
        { builder | classes = Set.insert class builder.classes }


{-| Remove a class from the `AttributeBuilder`.
-}
removeClass : String -> AttributeBuilder msg -> AttributeBuilder msg
removeClass class (AttributeBuilder builder) =
    AttributeBuilder
        { builder | classes = Set.remove class builder.classes }


{-| Like `Html.Attributes.classList`, but also removes the classes in the
`List` which are associated with a `False`, if already present.
-}
applyClassList : List ( String, Bool ) -> AttributeBuilder msg -> AttributeBuilder msg
applyClassList classes (AttributeBuilder builder) =
    let
        newClasses =
            List.foldl
                (\( className, include ) accum ->
                    if include then
                        Set.insert className accum

                    else
                        Set.remove className accum
                )
                builder.classes
                classes
    in
    AttributeBuilder
        { builder | classes = newClasses }


{-| Like `applyClassList`, but only adds classes, never removes.
-}
addClassList : List ( String, Bool ) -> AttributeBuilder msg -> AttributeBuilder msg
addClassList classes (AttributeBuilder builder) =
    let
        newClasses =
            List.foldl
                (\( className, include ) accum ->
                    if include then
                        Set.insert className accum

                    else
                        accum
                )
                builder.classes
                classes
    in
    AttributeBuilder
        { builder | classes = newClasses }


{-| Add a value for a style, replacing any previous value for the style with that name.
The first parameter is the name of the style, and the second is its value.

    attributeBuilder
        |> addClass "some-class"
        |> addStyle "background-color" "red"
        |> toAttributes

-}
addStyle : String -> String -> AttributeBuilder msg -> AttributeBuilder msg
addStyle name value (AttributeBuilder builder) =
    AttributeBuilder
        { builder | styles = Dict.insert name value builder.styles }


{-| Add several styles at once, analogous to `Html.Attribute.style`.
-}
addStyles : List ( String, String ) -> AttributeBuilder msg -> AttributeBuilder msg
addStyles styleList (AttributeBuilder builder) =
    let
        newStyles =
            List.foldl
                (\( name, value ) accum -> Dict.insert name value accum)
                builder.styles
                styleList
    in
    AttributeBuilder
        { builder | styles = newStyles }


{-| Remove a style from an `AttributeBuilder`.
-}
removeStyle : String -> AttributeBuilder msg -> AttributeBuilder msg
removeStyle name (AttributeBuilder builder) =
    AttributeBuilder
        { builder | styles = Dict.remove name builder.styles }
