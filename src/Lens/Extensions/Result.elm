module Lens.Extensions.Result exposing (..)

import Lens exposing (Lens)


okLens : Lens Lens.ValueMaybeExists (Result error value) value
okLens = Lens.sumTypeLens Result.toMaybe Ok


errLens : Lens Lens.ValueMaybeExists (Result error value) error
errLens =
    let
        getIfPossible result =
            case result of
                Err error -> Just error
                Ok _ -> Nothing
    in
        Lens.sumTypeLens getIfPossible Err
