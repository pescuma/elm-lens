module Lens.Extensions.Tuple2 exposing (..)

import Lens exposing (Lens, ValueAlwaysExists, ValueMaybeExists)


firstLens : Lens ValueAlwaysExists ( a, b ) a
firstLens = Lens.fixedFieldsLens (\( a, b ) -> a) (\a ( _, b ) -> ( a, b ))


secondLens : Lens ValueAlwaysExists ( a, b ) b
secondLens = Lens.fixedFieldsLens (\( a, b ) -> b) (\b ( a, _ ) -> ( a, b ))
