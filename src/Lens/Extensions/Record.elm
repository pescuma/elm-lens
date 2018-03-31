module Lens.Extensions.Record exposing (..)

import Lens exposing (Lens)


idLens : Lens Lens.ValueAlwaysExists { whole | id : part } part
idLens = Lens.fixedFieldsLens .id (\part whole -> { whole | id = part })


nameLens : Lens Lens.ValueAlwaysExists { whole | name : part } part
nameLens = Lens.fixedFieldsLens .name (\part whole -> { whole | name = part })


colorLens : Lens Lens.ValueAlwaysExists { whole | color : part } part
colorLens = Lens.fixedFieldsLens .color (\part whole -> { whole | color = part })
