module Lens.Extensions.Record exposing (..)

import Lens exposing (Lens, ValueAlwaysExists, ValueMaybeExists)


idLens : Lens ValueAlwaysExists { whole | id : part } part
idLens = Lens.fixedFieldsLens .id (\part whole -> { whole | id = part })


nameLens : Lens ValueAlwaysExists { whole | name : part } part
nameLens = Lens.fixedFieldsLens .name (\part whole -> { whole | name = part })


colorLens : Lens ValueAlwaysExists { whole | color : part } part
colorLens = Lens.fixedFieldsLens .color (\part whole -> { whole | color = part })
