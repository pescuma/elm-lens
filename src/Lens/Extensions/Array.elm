module Lens.Extensions.Array exposing (..)

import Array exposing (Array)
import Lens exposing (Lens)


elementLens : Int -> Lens Lens.ValueMaybeExists (Array value) value
elementLens index = Lens.flexibleFieldsLens (Array.get index) (Array.set index) (\whole -> whole)


headLens : Lens Lens.ValueMaybeExists (Array value) value
headLens = elementLens 0
