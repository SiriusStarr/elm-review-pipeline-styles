module ReviewPipelineStyles.Predicates exposing
    ( and, or, doNot
    , spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInputStep, haveAnUnnecessaryInputStep, haveAnInputStepOf, separateATestFromItsLambda, haveInternalComments
    , haveAParent, haveAParentNotSeparatedBy, haveMoreNestedParentsThan, aLetBlock, aLambdaFunction, aFlowControlStructure, aDataStructure
    , predicate, predicateWithLookupTable
    , getSteps, getParents, getNode, getInternalComments
    , isRightPizza, isLeftPizza, isRightComposition, isLeftComposition, isParentheticalApplication
    , Predicate, Operator, Pipeline, NestedWithin, ApplicationPipeline, CompositionPipeline
    )

{-| This module contains various `Predicate`s that can be used to filter
pipelines.


## Combining Predicates

@docs and, or, doNot


## Predicates

@docs spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInputStep, haveAnUnnecessaryInputStep, haveAnInputStepOf, separateATestFromItsLambda, haveInternalComments


## Nesting Predicates

@docs haveAParent, haveAParentNotSeparatedBy, haveMoreNestedParentsThan, aLetBlock, aLambdaFunction, aFlowControlStructure, aDataStructure


## Custom Predicates

If you need predicates beyond what is provided above, you can create them
manually by writing a function of type `Pipeline -> Bool` or
`ModuleNameLookupTable -> Pipeline -> Bool` and using one of the functions
below.

Use the functions in
[Getting Information About Pipelines](#getting-information-about-pipelines) to
build your custom predicate.

@docs predicate, predicateWithLookupTable


## Getting Information About Pipelines

Note that some of the types returned by these functions are from
[`stil4m/elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.7/)
if you need to work with them directly.

@docs getSteps, getParents, getNode, getInternalComments


## Query Pipeline Types

@docs isRightPizza, isLeftPizza, isRightComposition, isLeftComposition, isParentheticalApplication


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs Predicate, Operator, Pipeline, NestedWithin, ApplicationPipeline, CompositionPipeline

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Internal.Types as Types exposing (ApplicationPipeline, NestedWithin(..), Operator(..), Pipeline, Predicate(..))
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable, moduleNameFor)


{-| A predicate for filtering pipelines, or a logical combination of them.
-}
type alias Predicate pipelineType =
    Types.Predicate pipelineType


{-| The operator type of a pipeline, e.g. `|>` or `<<`.
-}
type alias Operator pipelineType =
    Types.Operator pipelineType


{-| A detected pipeline. You only need be concerned with this type if you are
writing a manual predicate.
-}
type alias Pipeline =
    Types.Pipeline


{-| The degree to which a parent or child is removed from a pipeline.
-}
type alias NestedWithin =
    Types.NestedWithin


{-| Pipelines that are function application.
-}
type alias ApplicationPipeline =
    Types.ApplicationPipeline


{-| Pipelines that are function composition.
-}
type alias CompositionPipeline =
    Types.CompositionPipeline


{-| Create a `Predicate` that matches pipelines that match both of two
predicates.
-}
and : Predicate anyType -> Predicate anyType -> Predicate anyType
and (Predicate p1) (Predicate p2) =
    predicateWithLookupTable <| \l p -> p1 l p && p2 l p


{-| Create a `Predicate` that matches pipelines that match either or both of two
predicates.
-}
or : Predicate anyType -> Predicate anyType -> Predicate anyType
or (Predicate p1) (Predicate p2) =
    predicateWithLookupTable <| \l p -> p1 l p || p2 l p


{-| Negate a `Predicate`.
-}
doNot : Predicate anyType -> Predicate anyType
doNot (Predicate pred) =
    predicateWithLookupTable <| \l p -> not <| pred l p


{-| Checks whether or not a pipeline spans multiple lines of code.
-}
spanMultipleLines : Predicate anyType
spanMultipleLines =
    predicate <|
        \{ node } ->
            let
                range : Range
                range =
                    Node.range node
            in
            range.end.row > range.start.row


{-| Checks whether the length of a pipeline is longer than a specified number.
Note that the length of a pipeline is the number of operators in it, e.g.

    foo
        |> bar
        |> baz

has length **2** for the purposes of this predicate.

-}
haveMoreStepsThan : Int -> Predicate anyType
haveMoreStepsThan i =
    predicate <| \{ steps } -> List.length steps > i + 1


{-| Checks whether the length of a pipeline is less than a specified number.
Note that the length of a pipeline is the number of operators in it, e.g.

    foo
        |> bar
        |> baz

has length **2** for the purposes of this predicate.

-}
haveFewerStepsThan : Int -> Predicate anyType
haveFewerStepsThan i =
    predicate <| \{ steps } -> List.length steps < i + 1


{-| Checks whether the pipeline is nested to any degree within another pipeline.
Note that this is quite a strict requirement and you probably want to use one of
the other nesting predicates instead.
-}
haveAParent : Predicate anyType
haveAParent =
    predicate <|
        \{ parents } ->
            not <| List.isEmpty parents


{-| Checks whether the pipeline is nested to a greater degree than specified
within other pipelines. For example, `haveMoreNestedParentsThan 1` will forbid

    a =
        foo
            |> (bar <| (a |> b |> c))
            |> baz

-}
haveMoreNestedParentsThan : Int -> Predicate anyType
haveMoreNestedParentsThan n =
    predicate <| \{ parents } -> List.length parents > n


{-| Checks whether the immediate parent of a pipeline (if one exists) is not
separated by one of a list of acceptable abstractions.
-}
haveAParentNotSeparatedBy : List (NestedWithin -> Bool) -> Predicate anyType
haveAParentNotSeparatedBy ls =
    predicate <|
        \{ parents } ->
            List.head parents
                |> MaybeX.unwrap True (\( _, n ) -> List.any (\f -> f n) ls)
                |> not


{-| Either within a `let` declaration or in the `let` expression of a `let`
block from the surrounding pipeline.
-}
aLetBlock : NestedWithin -> Bool
aLetBlock (NestedWithin r) =
    r.aLetBlock


{-| Within a lambda function in the surrounding pipeline.
-}
aLambdaFunction : NestedWithin -> Bool
aLambdaFunction (NestedWithin r) =
    r.aLambdaFunction


{-| Within a `case` expression of `if...then` expression in the surrounding
pipeline.
-}
aFlowControlStructure : NestedWithin -> Bool
aFlowControlStructure (NestedWithin r) =
    r.aFlowControlStructure


{-| Within a tuple, list, or record in the surrounding pipeline.
-}
aDataStructure : NestedWithin -> Bool
aDataStructure (NestedWithin r) =
    r.aDataStructure


{-| Determine whether the pipeline has a simple input or not. This is somewhat
subjective, of course, so use [`haveAnInputStepOf`](#haveAnInputStepOf) if you
want to customize its behavior. A pipeline is considered to have a simple input
if it [has an unnecessary input](#haveAnUnnecessaryInputStep), if its input is
**40 characters or less**, is only a **single line**, and is one of the
following:

    -- Unit
    ()
        |> foo

    -- Name
    a
        |> foo

    -- Prefix operator
    (+)
        |> foo

    -- Int literal
    1
        |> foo

    -- Hex literal
    0x0F
        |> foo

    -- Float literal
    1.5
        |> foo

    -- String literal
    "bar"
        |> foo

    -- Char literal
    'c'
        |> foo

    -- Record access function
    .field
        |> foo

or is one of the following where all subexpressions are simple:

    -- Tuple
    ( a, "b" )
        |> foo

    -- Record
    { a = "value" }
        |> foo

    -- List
    []
        |> foo

    -- Record access
    a.field
        |> foo

    -- Negation
    elmFormatWontLetThisBeAnExample
        |> foo

    -- Parentheses
    elmFormatWontLetThisBeAnExample
        |> foo

-}
haveASimpleInputStep : Predicate ApplicationPipeline
haveASimpleInputStep =
    let
        go : Node Expression -> Bool
        go e =
            let
                range : Range
                range =
                    Node.range e

                singleLine : Bool
                singleLine =
                    range.end.row == range.start.row

                short : Bool
                short =
                    range.end.column - range.start.column <= 40
            in
            singleLine
                && short
                && (case Node.value e of
                        UnitExpr ->
                            True

                        FunctionOrValue _ _ ->
                            True

                        PrefixOperator _ ->
                            True

                        Operator _ ->
                            True

                        Integer _ ->
                            True

                        Hex _ ->
                            True

                        Floatable _ ->
                            True

                        Literal _ ->
                            True

                        CharLiteral _ ->
                            True

                        RecordAccessFunction _ ->
                            True

                        TupledExpression es ->
                            List.all go es

                        RecordExpr rs ->
                            List.all (go << Tuple.second << Node.value) rs

                        ListExpr es ->
                            List.all go es

                        Negation e_ ->
                            go e_

                        ParenthesizedExpression e_ ->
                            go e_

                        RecordAccess e_ _ ->
                            go e_

                        RecordUpdateExpression _ _ ->
                            False

                        Application _ ->
                            False

                        OperatorApplication _ _ _ _ ->
                            False

                        IfBlock _ _ _ ->
                            False

                        LetExpression _ ->
                            False

                        CaseExpression _ ->
                            False

                        LambdaExpression _ ->
                            False

                        GLSLExpression _ ->
                            False
                   )

        (Predicate unnecessaryInput) =
            haveAnUnnecessaryInputStep
    in
    predicateWithLookupTable <|
        \l ({ steps } as p) ->
            unnecessaryInput l p
                && (List.head steps
                        |> Maybe.map (go << .node)
                        |> Maybe.withDefault False
                   )


{-| Determine whether the pipeline has an input step that simply isn't
necessary, e.g. `foo |> bar |> baz`, which may be written as `bar foo |> baz`.

This is not perfectly exhaustive as it does not consider operator precedence and
the like but will suffice for finding most simple cases.

Note that this will potentially flag quite complex inputs, so you might want to
use [`haveASimpleInputStep`](#haveASimpleInputStep) instead, since that only
detects visually simple inputs.

-}
haveAnUnnecessaryInputStep : Predicate ApplicationPipeline
haveAnUnnecessaryInputStep =
    predicate <|
        \{ steps } ->
            case steps of
                s1 :: s2 :: _ ->
                    let
                        firstNoApplication : Bool
                        firstNoApplication =
                            case Node.value s1.node of
                                Application _ ->
                                    False

                                OperatorApplication _ _ _ _ ->
                                    False

                                IfBlock _ _ _ ->
                                    False

                                LetExpression _ ->
                                    False

                                CaseExpression _ ->
                                    False

                                LambdaExpression _ ->
                                    False

                                GLSLExpression _ ->
                                    False

                                _ ->
                                    -- These are all simple values that can be fed right into a pipeline
                                    -- UnitExpr
                                    -- FunctionOrValue
                                    -- PrefixOperator
                                    -- Operator
                                    -- Integer
                                    -- Hex
                                    -- Floatable
                                    -- Literal
                                    -- CharLiteral
                                    -- RecordAccessFunction
                                    -- TupledExpression
                                    -- RecordExpr
                                    -- ListExpr
                                    -- Negation
                                    -- ParenthesizedExpression
                                    -- RecordAccess
                                    -- RecordUpdateExpression
                                    True

                        secondCanBeApplied : Bool
                        secondCanBeApplied =
                            case Node.value s2.node of
                                -- Definitely!
                                Application _ ->
                                    True

                                FunctionOrValue _ _ ->
                                    True

                                PrefixOperator _ ->
                                    True

                                -- Makes sense, but could maybe be written better
                                RecordAccessFunction _ ->
                                    True

                                -- Sure, if it compiled before
                                ParenthesizedExpression _ ->
                                    True

                                RecordAccess _ _ ->
                                    True

                                _ ->
                                    -- * Only if it was in parentheses (which it isn't)
                                    -- OperatorApplication
                                    -- IfBlock
                                    -- LetExpression
                                    -- CaseExpression
                                    -- LambdaExpression
                                    -- * Doesn't seem right to me
                                    -- UnitExpr
                                    -- Operator
                                    -- Integer
                                    -- Hex
                                    -- Floatable
                                    -- GLSLExpression
                                    -- Literal
                                    -- CharLiteral
                                    -- TupledExpression
                                    -- RecordExpr
                                    -- ListExpr
                                    -- Negation
                                    -- RecordUpdateExpression
                                    False
                    in
                    firstNoApplication && secondCanBeApplied

                _ ->
                    False


{-| Like [`haveASimpleInputStep`](#haveASimpleInputStep) but with a user-providable
function to check if an expression is simple.
-}
haveAnInputStepOf : (Node Expression -> Bool) -> Predicate anyType
haveAnInputStepOf pred =
    predicate <|
        \{ steps } ->
            List.head steps
                |> Maybe.map (pred << .node)
                |> Maybe.withDefault False


{-| Checks if a left "pizza" (`<|`) operator is used in the "canonical" fashion
in a test suite, to separate the lambda containing the test from the `test`.
All of the following will "pass" this predicate, and all other `<|`'s will not:

    import Test exposing (..)

    suite =
        describe "tests"
            [ test "foo" <|
                \() ->
                    a
            , fuzz fooFuzz "fuzz" <|
                \foo ->
                    a
            , fuzz2 fooFuzz barFuzz "fuzz2" <|
                \foo bar ->
                    a
            , fuzz3 fooFuzz barFuzz bazFuzz "fuzz3" <|
                \foo bar baz ->
                    a
            , fuzzWith { runs = 117 } fooFuzz "fuzzWith" <|
                \foo ->
                    a
            ]

-}
separateATestFromItsLambda : Predicate ApplicationPipeline
separateATestFromItsLambda =
    predicateWithLookupTable <|
        \lookupTable { operator, steps } ->
            case ( operator, List.map (Node.value << .node) steps ) of
                ( LeftPizza, [ LambdaExpression _, Application es ] ) ->
                    List.head es
                        |> Maybe.map
                            (\h ->
                                case ( Node.value h, moduleNameFor lookupTable h ) of
                                    ( FunctionOrValue _ n, Just [ "Test" ] ) ->
                                        List.member n [ "test", "fuzz", "fuzz2", "fuzz3", "fuzzWith" ]

                                    _ ->
                                        False
                            )
                        |> Maybe.withDefault False

                _ ->
                    -- Any other case isn't the "test" usage
                    False


{-| Checks whether any comments are located within the pipeline, e.g.

    a =
        foo
            -- Comment
            |> bar
            |> baz

Comments _around_ the pipeline are ignored, e.g.


    a =
        -- Ignored
        foo
            |> bar
            |> baz

    -- Ignored

-}
haveInternalComments : Predicate anyType
haveInternalComments =
    predicate <| \{ internalComments } -> not <| List.isEmpty internalComments


{-| Given a function of type `Pipeline -> Bool`, create a `Predicate` from it.
This is only useful if you want to write custom predicates. Note that this will
allow you to create predicates that match any type of pipeline, so be careful in
how you use it.

If you think a generally useful predicate is missing, please open an issue or PR
on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
predicate : (Pipeline -> Bool) -> Predicate anyType
predicate p =
    Predicate <| always p


{-| Given a function of type `ModuleNameLookupTable -> Pipeline -> Bool`, create
a `Predicate` from it. This is only useful if you want to write custom
predicates and are going to need the full module name for expressions in the
pipeline. Note that this will allow you to create predicates that match any type
of pipeline, so be careful in how you use it.

If you think a generally useful predicate is missing, please open an issue or PR
on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
predicateWithLookupTable : (ModuleNameLookupTable -> Pipeline -> Bool) -> Predicate anyType
predicateWithLookupTable =
    Predicate


{-| Get the "steps" in the pipeline. Note that this is in "logical" order, e.g.
`a 1 >> b >> c`, `c << b << a 1`, `a 1 |> b |> c`, `c <| b <| a 1`, and
`c (b (a 1))` will all have the same steps of `[a 1, b, c]`.
-}
getSteps : Pipeline -> List (Node Expression)
getSteps =
    List.map .node << .steps


{-| Get the "parent" pipelines of the pipeline, i.e. a hierarchy of pipelines
that this pipeline is nested within (start of the list being the most immediate
parent). Note that you can use the functions in
[Nesting Predicates](#nesting-predicates) to interrogate the `NestedWithin`
type and in [Query Pipeline Types](#query-pipeline-types) for the
`Operator` type.
-}
getParents : Pipeline -> List ( Operator (), NestedWithin )
getParents =
    .parents


{-| Get the outermost `Node` of a pipeline; you probably don't need to work with
this directly.
-}
getNode : Pipeline -> Node Expression
getNode =
    .node


{-| Get a list of all comments that are inside of a pipeline.
-}
getInternalComments : Pipeline -> List (Node String)
getInternalComments =
    .internalComments


{-| Check if an `Operator` is the right "pizza" operator (right function
application), i.e. `|>`. An example of this pipeline is below:

    foo
        |> bar
        |> baz

-}
isRightPizza : Operator () -> Bool
isRightPizza =
    (==) RightPizza


{-| Check if an `Operator` is the left "pizza" operator (left function
application), i.e. `<|`. An example of this pipeline is below:

    foo <| bar <| baz

-}
isLeftPizza : Operator () -> Bool
isLeftPizza =
    (==) LeftPizza


{-| Check if an `Operator` is the right composition operator, i.e. `>>`. An
example of this pipeline is below:

    foo
        >> bar
        >> baz

-}
isRightComposition : Operator () -> Bool
isRightComposition =
    (==) RightComposition


{-| Check if an `Operator` is the left composition operator, i.e. `<<`. An
example of this pipeline is below:

    foo << bar << baz

-}
isLeftComposition : Operator () -> Bool
isLeftComposition =
    (==) LeftComposition


{-| Check if an `Operator` is simply parenthetical application (not an
operator), i.e. successive function calls using parentheses, e.g.

    foo (bar (baz (i (j k))))

-}
isParentheticalApplication : Operator () -> Bool
isParentheticalApplication =
    (==) ParentheticalApplication
