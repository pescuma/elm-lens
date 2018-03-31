module Lens exposing
        (
          -- Types
          Lens
        , ValueAlwaysExists
        , ValueMaybeExists

          -- API
        , get
        , getMaybe
        , set
        , update
        , remove

          -- Constructors
        , converterLens
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


type Lens hasValue whole part = Lens (LensData whole part)


type alias LensData whole part =
    { target : Kind
    , get : whole -> part
    , getIfPossible : whole -> Maybe part
    , setIfPossible : part -> whole -> whole
    , removeIfPossible : whole -> whole
    }


type Kind
    = Converter
    | SumType
    | FixedFields
    | FlexibleFields

type ValueAlwaysExists = ValueAlwaysExistTag Never
type ValueMaybeExists = ValueMaybeExistsTag Never



-- API


get : Lens ValueAlwaysExists whole part -> whole -> part
get (Lens lens) whole = lens.get whole

getMaybe : Lens hasValue whole part -> whole -> Maybe part
getMaybe (Lens lens) whole = lens.getIfPossible whole

set : Lens hasValue whole part -> part -> whole -> whole
set (Lens lens) part whole = lens.setIfPossible part whole

update : Lens hasValue whole part -> (part -> part) -> whole -> whole
update (Lens lens) f whole =
    lens.getIfPossible whole
        |> Maybe.map (\part -> lens.setIfPossible (f part) whole)
        |> Maybe.withDefault whole

remove : Lens hasValue whole part -> whole -> whole
remove (Lens lens) whole = lens.removeIfPossible whole

setOrRemove : Lens hasValue whole part -> Maybe part -> whole -> whole
setOrRemove (Lens lens) maybePart whole =
    case maybePart of
        Just part -> lens.setIfPossible part whole
        Nothing -> lens.removeIfPossible whole


-- Constructors


converterLens :
    (whole -> part)
    -> (part -> whole)
    -> Lens ValueAlwaysExists whole part
converterLens get reverseGet =
    Lens
        { target = Converter
        , get = get
        , getIfPossible = Just << get
        , setIfPossible = \part whole -> reverseGet part
        , removeIfPossible = \whole -> whole
        }


sumTypeLens :
    (whole -> Maybe part)
    -> (part -> whole)
    -> Lens ValueMaybeExists whole part
sumTypeLens getIfPossible create =
    Lens
        { target = SumType
        , get = neverWillBeCalled
        , getIfPossible = getIfPossible
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
        , get = neverWillBeCalled
        , getIfPossible = getIfPossible
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
        , get = get
        , getIfPossible = Just << get
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
        , get = neverWillBeCalled
        , getIfPossible = getIfPossible
        , setIfPossible = setIfPossible
        , removeIfPossible = removeIfPossible
        }


neverWillBeCalled  : a -> b
neverWillBeCalled a = Debug.crash "Famous last words: this will never be called!"


-- Composition


identity : Lens ValueAlwaysExists a a
identity = converterLens Basics.identity Basics.identity


composeAlwaysExists : Lens ValueAlwaysExists a b -> Lens ValueAlwaysExists b c -> Lens ValueAlwaysExists a c
composeAlwaysExists = composeLosingType


composeMaybeExists : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens ValueMaybeExists a c
composeMaybeExists = composeLosingType


composeLosingType : Lens behaviour1 a b -> Lens behaviour2 b c -> Lens behaviour3 a c
composeLosingType (Lens a2b) (Lens b2c) =
    let
        target =
            case ( a2b.target, b2c.target ) of
                ( v, Converter ) -> v
                ( _, v ) -> v

        setIfPossible c a =
            a2b.getIfPossible a
                |> Maybe.map (\b -> b2c.setIfPossible c b)
                |> Maybe.map (\b -> a2b.setIfPossible b a)
                |> Maybe.withDefault a

        removeIfPossible a =
            case b2c.target of
                Converter -> a2b.removeIfPossible a
                FixedFields -> a
                _ ->
                    a2b.getIfPossible a
                        |> Maybe.map (\b -> b2c.removeIfPossible b)
                        |> Maybe.map (\b -> a2b.setIfPossible b a)
                        |> Maybe.withDefault a
    in
        Lens
            { target = target
            , get = a2b.get >> b2c.get
            , getIfPossible = a2b.getIfPossible >> Maybe.andThen b2c.getIfPossible
            , setIfPossible = setIfPossible
            , removeIfPossible = removeIfPossible
            }

