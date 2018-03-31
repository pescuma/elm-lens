module OperatorsTest exposing (..)

import Test exposing (..)
import Expect
import Lens exposing (Lens)
import Lens.Operators exposing (..)
import Lens.Extensions.Tuple2 as Tuple2
import Lens.Extensions.Record as Record
import Lens.Extensions.Maybe as Maybe
import Lens.Extensions.Color as Color
import Color exposing (Color)
import TestUtils exposing (..)


operators_getset : Test
operators_getset =
    describe "operators_getset"
        [ test "^." <| \_ -> ( 1, 10 ) ^. Tuple2.secondLens |> Expect.equal 10
        , test "^?." <| \_ -> ( 1, 10 ) ^?. Tuple2.secondLens |> Expect.equal (Just 10)
        , test "^.=" <| \_ -> (( 1, 10 ) ^.= Tuple2.secondLens <| 20) |> Expect.equal ( 1, 20 )
        , test "^?.=" <| \_ -> (( 1, 10 ) ^?.= Tuple2.secondLens <| 20) |> Expect.equal ( 1, 20 )
        ]


alwaysExists : Test
alwaysExists =
    let
        lens = Tuple2.secondLens ^>> Record.colorLens ^>> Color.redLens
        pre = ( 1, { x = 1000, color = Color.rgb 1 10 100 } )
        pos = ( 1, { x = 1000, color = Color.rgb 2 10 100 } )
    in
        describe "^>>" <| testAlwaysExists lens pre 1 2 pos pre


maybeExists_exists : Test
maybeExists_exists =
    let
        lens = Tuple2.secondLens ^?>> Maybe.justLens ^?>> Color.redLens
        pre = ( 1, Just (Color.rgb 1 10 100) )
        pos = ( 1, Just (Color.rgb 2 10 100) )
    in
        describe "^?>>" <| testMaybeExists lens pre (Just 1) 2 pos pos pre


allTogetherNow : Test
allTogetherNow =
    test "allTogetherNow" <|
        \_ ->
            ( 1, { x = 1000, color = Color.rgb 1 10 100 } )
                ^. (Tuple2.secondLens ^>> Record.colorLens ^>> Color.redLens)
                |> Expect.equal 1
