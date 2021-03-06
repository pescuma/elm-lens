module Lens.Extensions.Tuple3 exposing (..)

import Lens exposing (Lens)


firstLens : Lens Lens.ValueAlwaysExists ( a, b, c ) a
firstLens = Lens.fixedFieldsLens (\( a, b, c ) -> a) (\a ( _, b, c ) -> ( a, b, c ))


secondLens : Lens Lens.ValueAlwaysExists ( a, b, c ) b
secondLens = Lens.fixedFieldsLens (\( a, b, c ) -> b) (\b ( a, _, c ) -> ( a, b, c ))


thirdLens : Lens Lens.ValueAlwaysExists ( a, b, c ) c
thirdLens = Lens.fixedFieldsLens (\( a, b, c ) -> c) (\c ( a, b, _ ) -> ( a, b, c ))
