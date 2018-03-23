module Lens.Operators exposing (..)

import Lens exposing (Lens, ValueAlwaysExists, ValueMaybeExists)


(^.) : whole -> Lens ValueAlwaysExists whole part -> part
(^.) = flip Lens.get

(^?.) : whole -> Lens behaviour whole part -> Maybe part
(^?.) = flip Lens.getIfPossible

(^.=) : whole -> Lens ValueAlwaysExists whole part -> part -> whole
(^.=) whole lens part = Lens.set lens part whole

(^?.=) : whole -> Lens behaviour whole part -> part -> whole
(^?.=) whole lens part = Lens.setIfPossible lens part whole

(^>>) : Lens ValueAlwaysExists a b -> Lens ValueAlwaysExists b c -> Lens ValueAlwaysExists a c
(^>>) = Lens.composeAlwaysExists

(^?>>) : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens ValueMaybeExists a c
(^?>>) = Lens.composeMaybeExists
