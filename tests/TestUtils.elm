module TestUtils exposing (..)

import Test exposing (..)
import Expect
import Lens exposing (Lens, ValueAlwaysExists, ValueMaybeExists)


testAlwaysExists : Lens ValueAlwaysExists whole part -> whole -> part -> part -> whole -> whole -> List Test
testAlwaysExists lens whole currentPart newPart wholeAfterSetOrUpdate wholeAfterRemove =
    List.append (testMaybeExists lens whole (Just currentPart) newPart wholeAfterSetOrUpdate wholeAfterSetOrUpdate wholeAfterRemove)
        [ test "get" <|
            \_ -> Expect.equal currentPart <| Lens.get lens whole
        , test "set" <|
            \_ -> Expect.equal wholeAfterSetOrUpdate <| Lens.set lens newPart whole
        , test "update" <|
            \_ -> Expect.equal wholeAfterSetOrUpdate <| Lens.update lens (always newPart) whole
        , test "You get back what you set" <|
            \_ -> Expect.equal newPart <| Lens.get lens (Lens.set lens newPart whole)
        , test "Setting back what you get results produces a value equal to the original whole" <|
            \_ -> Expect.equal whole <| Lens.set lens (Lens.get lens whole) whole
        , test "The effect of a set doesn’t depend on earlier set" <|
            \_ -> Expect.equal whole <| Lens.set lens currentPart (Lens.set lens newPart whole)
        ]


testMaybeExists : Lens behaviour whole part -> whole -> Maybe part -> part -> whole -> whole -> whole -> List Test
testMaybeExists lens whole currentPart newPart wholeAfterSet wholeAfterUpdate wholeAfterRemove =
    [ test "getIfPossible" <|
        \_ -> Expect.equal currentPart <| Lens.getIfPossible lens whole
    , test "setIfPossible" <|
        \_ -> Expect.equal wholeAfterSet <| Lens.setIfPossible lens newPart whole
    , test "updateIfPossible" <|
        \_ -> Expect.equal wholeAfterUpdate <| Lens.updateIfPossible lens (always newPart) whole
    , test "removeIfPossible" <|
        \_ ->
            Expect.equal wholeAfterRemove <| Lens.removeIfPossible lens whole
    , test "If a set replaces an element, a later get will return the new element (wrapped in a Just)" <|
        \_ ->
            case currentPart of
                Nothing ->
                    -- Can change or not the value
                    case Lens.getIfPossible lens (Lens.setIfPossible lens newPart whole) of
                        Nothing -> Expect.pass
                        Just v -> Expect.equal newPart v
                _ ->
                    Expect.equal (Just newPart) <| Lens.getIfPossible lens (Lens.setIfPossible lens newPart whole)
    , test "If the lens gets a value, then sets it back, the result is equal to the original whole: " <|
        \_ ->
            case Lens.getIfPossible lens whole of
                Nothing -> Expect.pass
                Just v -> Expect.equal whole <| Lens.setIfPossible lens v whole
    , test "The effect of a setIfPossible doesn’t depend on earlier setIfPossible" <|
        \_ ->
            case currentPart of
                Nothing -> Expect.pass
                Just v -> Expect.equal whole <| Lens.setIfPossible lens v (Lens.setIfPossible lens newPart whole)
    ]
