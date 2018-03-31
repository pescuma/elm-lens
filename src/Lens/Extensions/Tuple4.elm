module Lens.Extensions.Tuple4 exposing (..)

import Lens exposing (Lens)


firstLens : Lens Lens.ValueAlwaysExists ( a, b, c, d ) a
firstLens = Lens.fixedFieldsLens (\( a, b, c, d ) -> a) (\a ( _, b, c, d ) -> ( a, b, c, d ))


secondLens : Lens Lens.ValueAlwaysExists ( a, b, c, d ) b
secondLens = Lens.fixedFieldsLens (\( a, b, c, d ) -> b) (\b ( a, _, c, d ) -> ( a, b, c, d ))


thirdLens : Lens Lens.ValueAlwaysExists ( a, b, c, d ) c
thirdLens = Lens.fixedFieldsLens (\( a, b, c, d ) -> c) (\c ( a, b, _, d ) -> ( a, b, c, d ))


fourthLens : Lens Lens.ValueAlwaysExists ( a, b, c, d ) d
fourthLens = Lens.fixedFieldsLens (\( a, b, c, d ) -> d) (\d ( a, b, c, _ ) -> ( a, b, c, d ))
