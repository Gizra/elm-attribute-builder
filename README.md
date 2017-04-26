[![Build Status](https://travis-ci.org/Gizra/elm-attribute-builder.svg?branch=master)](https://travis-ci.org/Gizra/elm-attribute-builder)

# elm-attribute-builder

Bulding up lists of `Html.Attribute` values in a modular way
can be a bit tricky, because once you create an attribute with, say, `class`,
`classList` or `style`, you can't introspect on the `Attribute` later, for
instance to add another class.

So, this provides a type that you can use to gradually build up some
attributes.

## API

For the detailed API, see the
[Elm package site](http://package.elm-lang.org/packages/Gizra/elm-attribute-builder/latest),
or the links to the right, if you're already there.

## Installation

Try `elm-package install Gizra/elm-attribute-builder`

## Development

Try something like:

    git clone https://github.com/Gizra/elm-attribute-builder
    cd elm-attribute-builder
    npm install
    npm test
