module CompositionTest exposing (..)

import Test exposing (..)
import Expect
import Lens exposing (Lens)
import Lens.Extensions.Tuple2 as Tuple2
import Lens.Extensions.Record as Record
import Lens.Extensions.Maybe as Maybe
import Lens.Extensions.Color as Color
import Color exposing (Color)
import TestUtils exposing (..)


identity : Test
identity =
    describe "composition with identity"
        [ describe "after - AlwaysExists" <| testAlwaysExists (Lens.composeAlwaysExists Tuple2.firstLens Lens.identity) ( 1, 10 ) 1 2 ( 2, 10 ) ( 1, 10 )
        , describe "before - AlwaysExists" <| testAlwaysExists (Lens.composeAlwaysExists Lens.identity Tuple2.firstLens) ( 1, 10 ) 1 2 ( 2, 10 ) ( 1, 10 )
        , describe "after - AlwaysExists composed as MaybeExists" <| testMaybeExists (Lens.composeMaybeExists Tuple2.firstLens Lens.identity) ( 1, 10 ) (Just 1) 2 ( 2, 10 ) ( 2, 10 ) ( 1, 10 )
        , describe "before - AlwaysExists composed as MaybeExists" <| testMaybeExists (Lens.composeMaybeExists Lens.identity Tuple2.firstLens) ( 1, 10 ) (Just 1) 2 ( 2, 10 ) ( 2, 10 ) ( 1, 10 )
        , describe "after - MaybeExists" <| testMaybeExists (Lens.composeMaybeExists Maybe.justLens Lens.identity) (Just 1) (Just 1) 2 (Just 2) (Just 2) Nothing
        , describe "before - MaybeExists" <| testMaybeExists (Lens.composeMaybeExists Lens.identity Maybe.justLens) (Just 1) (Just 1) 2 (Just 2) (Just 2) Nothing
        ]


alwaysExists : Test
alwaysExists =
    let
        lens = Lens.composeAlwaysExists Tuple2.secondLens (Lens.composeAlwaysExists Record.colorLens Color.redLens)
        pre = ( 1, { x = 1000, color = Color.rgb 1 10 100 } )
        pos = ( 1, { x = 1000, color = Color.rgb 2 10 100 } )
    in
        describe "composeAlwaysExists" <| testAlwaysExists lens pre 1 2 pos pre


maybeExists_exists : Test
maybeExists_exists =
    let
        lens = Lens.composeMaybeExists Tuple2.secondLens (Lens.composeMaybeExists Maybe.justLens Color.redLens)
        pre = ( 1, Just (Color.rgb 1 10 100) )
        pos = ( 1, Just (Color.rgb 2 10 100) )
    in
        describe "composeMaybeExists exists" <| testMaybeExists lens pre (Just 1) 2 pos pos pre


maybeExists_notExists : Test
maybeExists_notExists =
    let
        lens = Lens.composeMaybeExists Tuple2.secondLens (Lens.composeMaybeExists Maybe.justLens Color.redLens)
        pre = ( 1000, Nothing )
    in
        describe "composeMaybeExists notExists" <| testMaybeExists lens pre Nothing 2 pre pre pre


maybeExists_wholeLast_exists : Test
maybeExists_wholeLast_exists =
    let
        lens = Lens.composeMaybeExists Tuple2.secondLens (Lens.composeMaybeExists Maybe.justLens Color.rgbaTupleLens)
        pre = ( 1000, Just (Color.rgba 1 10 100 0.1) )
        pos = ( 1000, Just (Color.rgba 2 20 200 0.2) )
    in
        describe "composeMaybeExists wholeLast_exists" <| testMaybeExists lens pre (Just ( 1, 10, 100, 0.1 )) ( 2, 20, 200, 0.2 ) pos pos ( 1000, Nothing )


maybeExists_wholeLast_notExists : Test
maybeExists_wholeLast_notExists =
    let
        lens = Lens.composeMaybeExists Tuple2.secondLens (Lens.composeMaybeExists Maybe.justLens Color.rgbaTupleLens)
        pre = ( 1000, Nothing )
    in
        describe "composeMaybeExists wholeLast_notExists" <| testMaybeExists lens pre Nothing ( 2, 20, 200, 0.2 ) pre pre pre
