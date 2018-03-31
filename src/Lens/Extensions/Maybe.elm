module Lens.Extensions.Maybe exposing (..)

import Lens exposing (Lens)


justLens : Lens Lens.ValueMaybeExists (Maybe value) value
justLens = Lens.sumTypeLens2 identity Just Nothing
