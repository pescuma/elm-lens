module Lens.Operators exposing (..)

import Lens exposing (Lens)


(^.) : whole -> Lens Lens.ValueAlwaysExists whole part -> part
(^.) = flip Lens.get

(^?.) : whole -> Lens hasValue whole part -> Maybe part
(^?.) = flip Lens.getMaybe

(^.=) : whole -> Lens Lens.ValueAlwaysExists whole part -> part -> whole
(^.=) whole lens part = Lens.set lens part whole

(^?.=) : whole -> Lens hasValue whole part -> part -> whole
(^?.=) whole lens part = Lens.set lens part whole

(^>>) : Lens Lens.ValueAlwaysExists a b -> Lens Lens.ValueAlwaysExists b c -> Lens Lens.ValueAlwaysExists a c
(^>>) = Lens.composeAlwaysExists

(^?>>) : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens Lens.ValueMaybeExists a c
(^?>>) = Lens.composeMaybeExists
