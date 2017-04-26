module Html.AttributeBuilder
    exposing
        ( AttributeBuilder
        , attributeBuilder
        , union
        , toAttributes
        , addAttribute
        , addAttributes
        , addClass
        , removeClass
        , applyClassList
        , addClassList
        , addStyle
        , addStyles
        , removeStyle
        )

{-| Bulding up lists of `Html.Attribute` values in a modular way
can be a bit tricky, because once you create an attribute with, say, `class`,
`classList` or `style`, you can't introspect on the `Attribute` later, for
instance to add another class.

So, this provides a type that you can use to gradually build up some
attributes.

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


{-| An empty `AttributeBuilder`, with no attributes yet.
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
                [ builder.styles
                    |> Dict.toList
                    |> style
                ]
    in
        classAttribute ++ styleAttribute ++ builder.attributes


{-| Add an attribute to an attribute builder.

Note that you should use the more specific functions for classes and styles, since
they combine classes and styles in a predictable manner.
-}
addAttribute : Attribute msg -> AttributeBuilder msg -> AttributeBuilder msg
addAttribute attribute (AttributeBuilder builder) =
    AttributeBuilder
        { builder | attributes = attribute :: builder.attributes }


{-| Add a list of attributes to an attribute builder.

For clsses and styles, you should use the more specific functions, since they
combine classes and styles more predictably.
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


{-| Like `applyClassList`, but only adds, never removes.
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


{-| Add a value for a style, replacing any previous value with that key.
-}
addStyle : String -> String -> AttributeBuilder msg -> AttributeBuilder msg
addStyle name value (AttributeBuilder builder) =
    AttributeBuilder
        { builder | styles = Dict.insert name value builder.styles }


{-| Add several styles at once.
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
