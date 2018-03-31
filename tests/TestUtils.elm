module TestUtils exposing (..)

import Test exposing (..)
import Expect
import Lens exposing (Lens)


testAlwaysExists : Lens Lens.ValueAlwaysExists whole part -> whole -> part -> part -> whole -> whole -> List Test
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


testMaybeExists : Lens hasValue whole part -> whole -> Maybe part -> part -> whole -> whole -> whole -> List Test
testMaybeExists lens whole currentPart newPart wholeAfterSet wholeAfterUpdate wholeAfterRemove =
    [ test "maybe - getIfPossible" <|
        \_ -> Expect.equal currentPart <| Lens.getMaybe lens whole
    , test "maybe - set" <|
        \_ -> Expect.equal wholeAfterSet <| Lens.set lens newPart whole
    , test "maybe - update" <|
        \_ -> Expect.equal wholeAfterUpdate <| Lens.update lens (always newPart) whole
    , test "maybe - remove" <|
        \_ ->
            Expect.equal wholeAfterRemove <| Lens.remove lens whole
    , test "maybe - If a set replaces an element, a later get will return the new element (wrapped in a Just)" <|
        \_ ->
            case currentPart of
                Nothing ->
                    -- Can change or not the value
                    case Lens.getMaybe lens (Lens.set lens newPart whole) of
                        Nothing -> Expect.pass
                        Just v -> Expect.equal newPart v
                _ ->
                    Expect.equal (Just newPart) <| Lens.getMaybe lens (Lens.set lens newPart whole)
    , test "maybe - If the lens gets a value, then sets it back, the result is equal to the original whole: " <|
        \_ ->
            case Lens.getMaybe lens whole of
                Nothing -> Expect.pass
                Just v -> Expect.equal whole <| Lens.set lens v whole
    , test "maybe - The effect of a set doesn’t depend on earlier set" <|
        \_ ->
            case currentPart of
                Nothing -> Expect.pass
                Just v -> Expect.equal whole <| Lens.set lens v (Lens.set lens newPart whole)
    ]
