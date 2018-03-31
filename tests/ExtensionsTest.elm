module ExtensionsTest exposing (..)

import Test exposing (..)
import Expect
import Lens exposing (Lens)
import Lens.Extensions.Tuple2 as Tuple2
import Lens.Extensions.Tuple3 as Tuple3
import Lens.Extensions.Tuple4 as Tuple4
import Dict exposing (Dict)
import Lens.Extensions.Dict as Dict
import Array exposing (Array)
import Lens.Extensions.Array as Array
import Color exposing (Color)
import Lens.Extensions.Color as Color
import Lens.Extensions.Maybe as Maybe
import Lens.Extensions.Result as Result
import Lens.Extensions.Record as Record
import TestUtils exposing (..)


tuple2_firstLens : Test
tuple2_firstLens =
    describe "Tuple2.firstLens" <| testAlwaysExists Tuple2.firstLens ( 1, 100 ) 1 2 ( 2, 100 ) ( 1, 100 )


tuple2_secondLens : Test
tuple2_secondLens =
    describe "Tuple2.secondLens" <| testAlwaysExists Tuple2.secondLens ( 100, 1 ) 1 2 ( 100, 2 ) ( 100, 1 )


tuple3_thirdLens : Test
tuple3_thirdLens =
    describe "Tuple3.thirdLens" <| testAlwaysExists Tuple3.thirdLens ( 100, 100, 1 ) 1 2 ( 100, 100, 2 ) ( 100, 100, 1 )


tuple4_fourthLens : Test
tuple4_fourthLens =
    describe "Tuple4.fourthLens" <| testAlwaysExists Tuple4.fourthLens ( 1000, 100, 100, 1 ) 1 2 ( 1000, 100, 100, 2 ) ( 1000, 100, 100, 1 )


dict_valueLens_exists : Test
dict_valueLens_exists =
    let
        pre = Dict.fromList [ ( "exists", 1 ) ]
        pos = Dict.fromList [ ( "exists", 2 ) ]
    in
        describe "Dict.valueLens exists" <| testMaybeExists (Dict.valueLens "exists") pre (Just 1) 2 pos pos Dict.empty


dict_valueLens_notExists : Test
dict_valueLens_notExists =
    let
        pre = Dict.fromList [ ( "exists", 1 ) ]
        pos = Dict.fromList [ ( "exists", 1 ), ( "not exists", 2 ) ]
    in
        describe "Dict.valueLens not exists" <| testMaybeExists (Dict.valueLens "not exists") pre Nothing 2 pos pre pre


array_elementLens_exists : Test
array_elementLens_exists =
    let
        pre = Array.fromList [ 1, 10 ]
        pos = Array.fromList [ 2, 10 ]
    in
        describe "Array.elementLens exists" <| testMaybeExists (Array.elementLens 0) pre (Just 1) 2 pos pos pre


array_elementLens_notExists : Test
array_elementLens_notExists =
    let
        pre = Array.fromList [ 1, 10 ]
    in
        describe "Array.elementLens not exists" <| testMaybeExists (Array.elementLens 2) pre Nothing 2 pre pre pre


color_redLens : Test
color_redLens =
    let
        c = Color.rgba 1 10 100 0.1
    in
        describe "Color.redLens" <| testAlwaysExists Color.redLens c 1 2 (Color.rgba 2 10 100 0.1) c


color_rgbaTupleLens : Test
color_rgbaTupleLens =
    let
        pre = Color.rgba 1 10 100 0.1
        pos = Color.rgba 2 13 104 0.5
    in
        describe "Color.rgbaTupleLens" <| testAlwaysExists Color.rgbaTupleLens pre ( 1, 10, 100, 0.1 ) ( 2, 13, 104, 0.5 ) pos pre


maybe_justLens_exists : Test
maybe_justLens_exists =
    describe "Maybe.justLens exists" <| testMaybeExists Maybe.justLens (Just 1) (Just 1) 2 (Just 2) (Just 2) Nothing


maybe_justLens_notExists : Test
maybe_justLens_notExists =
    describe "Maybe.justLens not exists" <| testMaybeExists Maybe.justLens Nothing Nothing 2 (Just 2) Nothing Nothing


result_okLens_exists : Test
result_okLens_exists =
    describe "Result.okLens exists" <| testMaybeExists Result.okLens (Ok 1) (Just 1) 2 (Ok 2) (Ok 2) (Ok 1)


result_errLens_notExists : Test
result_errLens_notExists =
    describe "Result.okLens not exists" <| testMaybeExists Result.okLens (Err 1) Nothing 2 (Ok 2) (Err 1) (Err 1)


result_errLens_exists : Test
result_errLens_exists =
    describe "Result.errLens exists" <| testMaybeExists Result.errLens (Err 1) (Just 1) 2 (Err 2) (Err 2) (Err 1)


result_okLens_notExists : Test
result_okLens_notExists =
    describe "Result.errLens not exists" <| testMaybeExists Result.errLens (Ok 1) Nothing 2 (Err 2) (Ok 1) (Ok 1)


record_idLens : Test
record_idLens =
    let
        pre = { id = 1, x = "a" }
        pos = { id = 2, x = "a" }
    in
        describe "Record.idLens" <| testAlwaysExists Record.idLens pre 1 2 pos pre


record_nameLens : Test
record_nameLens =
    let
        pre = { name = 1, x = "a" }
        pos = { name = 2, x = "a" }
    in
        describe "Record.nameLens" <| testAlwaysExists Record.nameLens pre 1 2 pos pre
