module Lens.Extensions.Dict exposing (..)

import Dict exposing (Dict)
import Lens exposing (Lens, ValueAlwaysExists, ValueMaybeExists)


valueLens : comparableKey -> Lens ValueMaybeExists (Dict comparableKey value) value
valueLens key = Lens.flexibleFieldsLens (Dict.get key) (Dict.insert key) (Dict.remove key)
