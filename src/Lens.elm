module Lens exposing
        (
          -- Types
          Lens
        , ValueAlwaysExists
        , ValueMaybeExists

          -- API
        , getIfPossible
        , get
        , setIfPossible
        , set
        , updateIfPossible
        , update
        , removeIfPossible

          -- Constructors
        , wholeValueLens
        , sumTypeLens
        , sumTypeLens2
        , fixedFieldsLens
        , flexibleFieldsLens

          -- Composition
        , identity
        , composeAlwaysExists
        , composeMaybeExists
        )


-- Types


type Lens behaviour whole part = Lens (LensData whole part)


type alias LensData whole part =
    { target : Target
    , getIfPossible : whole -> Maybe part
    , get : whole -> part
    , setIfPossible : part -> whole -> whole
    , removeIfPossible : whole -> whole
    }


type Target
    = WholeValue
    | SumType
    | FixedFields
    | FlexibleFields


type ValueAlwaysExists = ValueAlwaysExistTag Never
type ValueMaybeExists = ValueMaybeExistsTag Never



-- API


getIfPossible : Lens behaviour whole part -> whole -> Maybe part
getIfPossible (Lens lens) whole = lens.getIfPossible whole


get : Lens ValueAlwaysExists whole part -> whole -> part
get (Lens lens) whole = lens.get whole


setIfPossible : Lens behaviour whole part -> part -> whole -> whole
setIfPossible (Lens lens) part whole = lens.setIfPossible part whole


set : Lens ValueAlwaysExists whole part -> part -> whole -> whole
set = setIfPossible


updateIfPossible : Lens behaviour whole part -> (part -> part) -> whole -> whole
updateIfPossible (Lens lens) f whole =
    lens.getIfPossible whole
        |> Maybe.map (\part -> lens.setIfPossible (f part) whole)
        |> Maybe.withDefault whole


update : Lens ValueAlwaysExists whole part -> (part -> part) -> whole -> whole
update = updateIfPossible


removeIfPossible : Lens behaviour whole part -> whole -> whole
removeIfPossible (Lens lens) whole = lens.removeIfPossible whole


-- Constructors


wholeValueLens :
    (whole -> part)
    -> (part -> whole)
    -> Lens ValueAlwaysExists whole part
wholeValueLens get create =
    Lens
        { target = WholeValue
        , getIfPossible = Just << get
        , get = get
        , setIfPossible = \part whole -> create part
        , removeIfPossible = \whole -> whole
        }


sumTypeLens :
    (whole -> Maybe part)
    -> (part -> whole)
    -> Lens ValueMaybeExists whole part
sumTypeLens getIfPossible create =
    Lens
        { target = SumType
        , getIfPossible = getIfPossible
        , get = \whole -> Debug.crash "Famous last words: whis will never be called!"
        , setIfPossible = \part whole -> create part
        , removeIfPossible = \whole -> whole
        }


sumTypeLens2 :
    (whole -> Maybe part)
    -> (part -> whole)
    -> whole
    -> Lens ValueMaybeExists whole part
sumTypeLens2 getIfPossible create removedValue =
    Lens
        { target = SumType
        , getIfPossible = getIfPossible
        , get = \whole -> Debug.crash "Famous last words: whis will never be called!"
        , setIfPossible = \part whole -> create part
        , removeIfPossible = \whole -> removedValue
        }


fixedFieldsLens :
    (whole -> part)
    -> (part -> whole -> whole)
    -> Lens ValueAlwaysExists whole part
fixedFieldsLens get set =
    Lens
        { target = FixedFields
        , getIfPossible = Just << get
        , get = get
        , setIfPossible = \part whole -> set part whole
        , removeIfPossible = \whole -> whole
        }


flexibleFieldsLens :
    (whole -> Maybe part)
    -> (part -> whole -> whole)
    -> (whole -> whole)
    -> Lens ValueMaybeExists whole part
flexibleFieldsLens getIfPossible setIfPossible removeIfPossible =
    Lens
        { target = FlexibleFields
        , getIfPossible = getIfPossible
        , get = \whole -> Debug.crash "Famous last words: whis will never be called!"
        , setIfPossible = setIfPossible
        , removeIfPossible = removeIfPossible
        }



-- Composition


identity : Lens ValueAlwaysExists a a
identity = wholeValueLens Basics.identity Basics.identity


composeAlwaysExists : Lens ValueAlwaysExists a b -> Lens ValueAlwaysExists b c -> Lens ValueAlwaysExists a c
composeAlwaysExists = composeLosingType

composeMaybeExists : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens ValueMaybeExists a c
composeMaybeExists = composeLosingType

composeLosingType : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens behaviour3 a c
composeLosingType (Lens a2b) (Lens b2c) =
    let
        target =
            case ( a2b.target, b2c.target ) of
                ( v, WholeValue ) -> v
                ( _, v ) -> v

        setIfPossible c a =
            a2b.getIfPossible a
                |> Maybe.map (\b -> b2c.setIfPossible c b)
                |> Maybe.map (\b -> a2b.setIfPossible b a)
                |> Maybe.withDefault a

        removeIfPossible a =
            case b2c.target of
                WholeValue -> a2b.removeIfPossible a
                FixedFields -> a
                _ ->
                    a2b.getIfPossible a
                        |> Maybe.map (\b -> b2c.removeIfPossible b)
                        |> Maybe.map (\b -> a2b.setIfPossible b a)
                        |> Maybe.withDefault a
    in
        Lens
            { target = target
            , getIfPossible = a2b.getIfPossible >> Maybe.andThen b2c.getIfPossible
            , get = a2b.get >> b2c.get
            , setIfPossible = setIfPossible
            , removeIfPossible = removeIfPossible
            }
