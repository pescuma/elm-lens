# elm-lens  [![Build Status](https://travis-ci.org/pescuma/elm-lens.svg?branch=master)](https://travis-ci.org/pescuma/elm-lens)

Unconventional lens for eml.

```elm
> import Lens exposing (Lens)
> import Lens.Operators exposing (..)
> import Lens.Extensions.Dict as Dict
> import Lens.Extensions.Maybe as Maybe
> import Lens.Extensions.Record as Record
>
> import Dict exposing (Dict)
>
>
> val = Just <| Dict.fromList [("k", { name = "Name" })]
Just (Dict.fromList [("k",{ name = "Name" })])
    : Maybe.Maybe (Dict.Dict String { name : String })
>
> lens = Maybe.justLens ^?>> Dict.valueLens "k" ^?>> Record.nameLens
Lens { target = FixedFields, getIfPossible = <function>, get = <function>, setIfPossible = <function>, removeIfPossible = <function> }
    : Lens.Lens
        Lens.ValueMaybeExists
        (Maybe.Maybe (Dict.Dict String { whole | name : part }))
        part
>
> Lens.getIfPossible lens val
Just "Name" : Maybe.Maybe String
>
> Lens.setIfPossible lens "New name" val
Just (Dict.fromList [("k",{ name = "New name" })])
    : Maybe.Maybe (Dict.Dict String { name : String })
```

