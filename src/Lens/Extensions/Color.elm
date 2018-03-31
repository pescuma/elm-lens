module Lens.Extensions.Color exposing (..)

import Color exposing (Color)
import Lens exposing (Lens)


rgbaLens : Lens Lens.ValueAlwaysExists Color { red : Int, green : Int, blue : Int, alpha : Float }
rgbaLens = Lens.converterLens Color.toRgb (\c -> Color.rgba c.red c.green c.blue c.alpha)


rgbaTupleLens : Lens Lens.ValueAlwaysExists Color ( Int, Int, Int, Float )
rgbaTupleLens =
    let
        get color =
            let
                c = Color.toRgb color
            in
                ( c.red, c.green, c.blue, c.alpha )
    in
        Lens.converterLens get (\( r, g, b, a ) -> Color.rgba r g b a)


redLens : Lens Lens.ValueAlwaysExists Color Int
redLens =
    let
        get color =
            let
                c = Color.toRgb color
            in
                c.red

        set r color =
            let
                c = Color.toRgb color
            in
                Color.rgba r c.green c.blue c.alpha
    in
        Lens.fixedFieldsLens get set


greenLens : Lens Lens.ValueAlwaysExists Color Int
greenLens =
    let
        get color =
            let
                c = Color.toRgb color
            in
                c.green

        set g color =
            let
                c = Color.toRgb color
            in
                Color.rgba c.red g c.blue c.alpha
    in
        Lens.fixedFieldsLens get set


blueLens : Lens Lens.ValueAlwaysExists Color Int
blueLens =
    let
        get color =
            let
                c = Color.toRgb color
            in
                c.blue

        set b color =
            let
                c = Color.toRgb color
            in
                Color.rgba c.red c.green b c.alpha
    in
        Lens.fixedFieldsLens get set


alphaLens : Lens Lens.ValueAlwaysExists Color Float
alphaLens =
    let
        get color =
            let
                c = Color.toRgb color
            in
                c.alpha

        set a color =
            let
                c = Color.toRgb color
            in
                Color.rgba c.red c.green c.blue a
    in
        Lens.fixedFieldsLens get set
