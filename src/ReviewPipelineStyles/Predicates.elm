module ReviewPipelineStyles.Predicates exposing
    ( and, or, doNot
    , spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInputStep, haveAnUnnecessaryInputStep, separateATestFromItsLambda, haveInternalComments
    , haveAnInputStepThatIs, haveASecondStepThatIs, haveAnyNonInputStepThatIs, haveNonInputStepsThatAreAll, haveAnyStepThatIs, haveStepsThatAreAll
    , aSemanticallyInfixFunction, aConfusingNonCommutativePrefixOperator, aConfusingNonCommutativeFunction, aSimpleStep, onASingleLine, onMultipleLines, stepPredicate, stepPredicateWithLookupTable
    , haveAParent, haveAParentNotSeparatedBy, haveMoreNestedParentsThan, aLetBlock, aLambdaFunction, aFlowControlStructure, aDataStructure
    , predicate, predicateWithLookupTable
    , getSteps, getParents, getNode, getInternalComments
    , isRightPizza, isLeftPizza, isRightComposition, isLeftComposition, isParentheticalApplication
    , Predicate, StepPredicate, Operator, Pipeline, NestedWithin, ApplicationPipeline, CompositionPipeline
    , haveAnInputStepOf
    )

{-| This module contains various `Predicate`s that can be used to filter
pipelines.


## Combining Predicates

Predicates can be combined and negated using `and`, `or`, and `doNot`.

@docs and, or, doNot


## Predicates

@docs spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInputStep, haveAnUnnecessaryInputStep, separateATestFromItsLambda, haveInternalComments


## Step Predicates

These predicates allow one to filter based on a specific step of a pipeline.

@docs haveAnInputStepThatIs, haveASecondStepThatIs, haveAnyNonInputStepThatIs, haveNonInputStepsThatAreAll, haveAnyStepThatIs, haveStepsThatAreAll

@docs aSemanticallyInfixFunction, aConfusingNonCommutativePrefixOperator, aConfusingNonCommutativeFunction, aSimpleStep, onASingleLine, onMultipleLines, stepPredicate, stepPredicateWithLookupTable


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

@docs Predicate, StepPredicate, Operator, Pipeline, NestedWithin, ApplicationPipeline, CompositionPipeline


### Deprecated

These functions have been deprecated and are included only to avoid a breaking
change.

@docs haveAnInputStepOf

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Internal.Types as Types
    exposing
        ( ApplicationPipeline
        , NestedWithin(..)
        , Operator(..)
        , Pipeline
        , Predicate(..)
        , StepPredicate(..)
        )
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable, moduleNameFor)
import Set exposing (Set)


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


{-| A predicate for a single step of a pipeline.
-}
type alias StepPredicate =
    Types.StepPredicate


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
subjective, of course. A pipeline is considered to have a simple input if it
[has an unnecessary input](#haveAnUnnecessaryInputStep), if its second step is
not [a semantically-infix function](#aSemanticallyInfixFunction) (like `logBase`
), and if its input step is [simple](#aSimpleStep).

This predicate is constructed as follows:

    haveASimpleInputStep =
        haveAnUnnecessaryInputStep
            |> and (doNot <| haveASecondStepThatIs aSemanticallyInfixFunction)
            |> and (haveAnInputStepThatIs aSimpleStep)

-}
haveASimpleInputStep : Predicate ApplicationPipeline
haveASimpleInputStep =
    haveAnUnnecessaryInputStep
        |> and (doNot <| haveASecondStepThatIs aSemanticallyInfixFunction)
        |> and (haveAnInputStepThatIs aSimpleStep)


{-| Determine whether the pipeline has an input step that simply isn't
necessary, e.g. `foo |> bar |> baz`, which may be written as `bar foo |> baz`.

This is not perfectly exhaustive as it does not consider operator precedence and
the like but will suffice for finding most simple cases.

Note that this will potentially flag quite complex inputs, so you might want to
use [`haveASimpleInputStep`](#haveASimpleInputStep) instead, since that only
detects visually simple inputs.

This predicate is constructed as follows:

    haveAnUnnecessaryInputStep =
        let
            ableToBeAnArgument =
                stepPredicate <|
                    \node ->
                        case Node.value node of
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
                                True

            ableToTakeAnArgument =
                stepPredicate <|
                    \node ->
                        case Node.value node of
                            Application _ ->
                                True

                            FunctionOrValue _ _ ->
                                True

                            PrefixOperator _ ->
                                True

                            RecordAccessFunction _ ->
                                True

                            ParenthesizedExpression _ ->
                                True

                            RecordAccess _ _ ->
                                True

                            _ ->
                                False
        in
        haveAnInputStepThatIs ableToBeAnArgument
            |> and (haveASecondStepThatIs ableToTakeAnArgument)

-}
haveAnUnnecessaryInputStep : Predicate ApplicationPipeline
haveAnUnnecessaryInputStep =
    let
        ableToBeAnArgument : StepPredicate
        ableToBeAnArgument =
            stepPredicate <|
                \node ->
                    case Node.value node of
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

        ableToTakeAnArgument : StepPredicate
        ableToTakeAnArgument =
            stepPredicate <|
                \node ->
                    case Node.value node of
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
    haveAnInputStepThatIs ableToBeAnArgument
        |> and (haveASecondStepThatIs ableToTakeAnArgument)


{-| @deprecated **Use [`haveAnInputStepThatIs`](#haveAnInputStepThatIs)
instead.**
-}
haveAnInputStepOf : (Node Expression -> Bool) -> Predicate anyType
haveAnInputStepOf =
    haveAnInputStepThatIs << stepPredicate


{-| Given a predicate for a single step, check if the first step in a pipeline
matches said predicate.
-}
haveAnInputStepThatIs : StepPredicate -> Predicate anyType
haveAnInputStepThatIs (StepPredicate pred) =
    predicateWithLookupTable <|
        \lookupTable { steps } ->
            List.head steps
                |> Maybe.map (pred lookupTable << .node)
                |> Maybe.withDefault False


{-| Given a predicate for a single step, check if the second step in a pipeline
matches said predicate.
-}
haveASecondStepThatIs : StepPredicate -> Predicate anyType
haveASecondStepThatIs (StepPredicate pred) =
    predicateWithLookupTable <|
        \l { steps } ->
            case steps of
                _ :: s2 :: _ ->
                    pred l s2.node

                _ ->
                    False


{-| Given a predicate for a single step, check if any step in a pipeline matches
said predicate.
-}
haveAnyStepThatIs : StepPredicate -> Predicate anyType
haveAnyStepThatIs (StepPredicate pred) =
    predicateWithLookupTable <|
        \lookupTable { steps } ->
            List.any (pred lookupTable << .node) steps


{-| Given a predicate for a single step, check if all step in a pipeline match
said predicate.
-}
haveStepsThatAreAll : StepPredicate -> Predicate anyType
haveStepsThatAreAll (StepPredicate pred) =
    predicateWithLookupTable <|
        \lookupTable { steps } ->
            List.all (pred lookupTable << .node) steps


{-| Given a predicate for a single step, check if any step in a pipeline except
for the first matches said predicate.
-}
haveAnyNonInputStepThatIs : StepPredicate -> Predicate anyType
haveAnyNonInputStepThatIs (StepPredicate pred) =
    predicateWithLookupTable <|
        \lookupTable { steps } ->
            List.tail steps
                |> Maybe.map (List.any (pred lookupTable << .node))
                |> Maybe.withDefault False


{-| Given a predicate for a single step, check if all steps in a pipeline except
for the first match said predicate.
-}
haveNonInputStepsThatAreAll : StepPredicate -> Predicate anyType
haveNonInputStepsThatAreAll (StepPredicate pred) =
    predicateWithLookupTable <|
        \lookupTable { steps } ->
            List.tail steps
                |> Maybe.map (List.all (pred lookupTable << .node))
                |> Maybe.withDefault False


{-| Checks if a step consists of a single line of code.
-}
onASingleLine : StepPredicate
onASingleLine =
    stepPredicate <|
        \node ->
            let
                r : Range
                r =
                    Node.range node
            in
            r.start.row == r.end.row


{-| Checks if a step consists of more than one line of code.
-}
onMultipleLines : StepPredicate
onMultipleLines =
    stepPredicate <|
        \node ->
            let
                r : Range
                r =
                    Node.range node
            in
            r.end.row > r.start.row


{-| A "semantically-infix" function is a function that is intended (by name) to
be read in an infix fashion as part of a pipeline. For example, `Maybe.andThen`
or `remainderBy` or `Maybe.Extra.orElse`:

    List.head |> Maybe.andThen String.toFloat

    10 |> remainderBy 2

In practice, it checks if a step begins with a function that begins with the
word "or" or "and" or ends in "By", or is on the following whitelist of
functions:

  - `logBase`
  - `atMost`
  - `atLeast`

They are then required to have exactly one argument applied to them, as with no
arguments, they are not yet infix (and with 2+ they are no longer functions).
For example, the following are not infix:

    -- This is "backwards" from how it reads
    2 |> remainderBy |> 10

    -- This doesn't even compile
    foo |> remainderBy 2 10

It also rules out confusing, non-commutative functions, even if they match the
above.

If you have suggestions for additions to this list, please open an issue or PR
on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
aSemanticallyInfixFunction : StepPredicate
aSemanticallyInfixFunction =
    let
        whitelist : Set String
        whitelist =
            -- Functions that do not match "or...", "and...", or "...By"
            Set.fromList
                [ "logBase"
                , "atMost"
                , "atLeast"
                ]
    in
    stepPredicateWithLookupTable <|
        \l node ->
            getFirstFunction l node
                |> Maybe.map
                    (\{ name, numAppliedArgs } ->
                        let
                            len : Int
                            len =
                                String.length name

                            beginsWithWord : String -> Bool
                            beginsWithWord w =
                                let
                                    wordLen : Int
                                    wordLen =
                                        String.length w
                                in
                                (len > wordLen)
                                    && String.startsWith w name
                                    && (String.all Char.isUpper <| String.slice wordLen (wordLen + 1) name)
                        in
                        (numAppliedArgs == 1)
                            && (List.any beginsWithWord [ "and", "or" ]
                                    || (String.endsWith "By" name && len > 2)
                                    || Set.member name whitelist
                               )
                    )
                |> Maybe.withDefault False


{-| Return the first function in an expression, along with the correct module
name. Note that this returns an impossible module of `[ "Record Access" ]` for
record access functions.
-}
getFirstFunction : ModuleNameLookupTable -> Node Expression -> Maybe { moduleName : ModuleName, name : String, numAppliedArgs : Int }
getFirstFunction lookupTable node =
    case Node.value node of
        ParenthesizedExpression e ->
            getFirstFunction lookupTable e

        RecordAccessFunction s ->
            -- This is a weird case, so assign a module name that is not possible.
            Just { moduleName = [ "Record Access" ], name = s, numAppliedArgs = 0 }

        PrefixOperator s ->
            moduleNameFor lookupTable node
                |> Maybe.map (\mn -> { moduleName = mn, name = s, numAppliedArgs = 0 })

        FunctionOrValue _ s ->
            moduleNameFor lookupTable node
                |> Maybe.map (\mn -> { moduleName = mn, name = s, numAppliedArgs = 0 })

        Application es ->
            List.head es
                |> Maybe.andThen
                    (getFirstFunction lookupTable)
                |> Maybe.map (\r -> { r | numAppliedArgs = List.length es - 1 })

        -- OperatorApplication
        -- Negation
        -- UnitExpr
        -- GLSLExpression
        -- RecordUpdateExpression
        -- RecordAccess
        -- LetExpression LetBlock
        -- ListExpr (List (Node Expression))
        -- RecordExpr
        -- LambdaExpression
        -- CaseExpression
        -- Operator
        -- Integer
        -- Hex
        -- Floatable
        -- TupledExpression
        -- Literal
        -- CharLiteral
        -- IfBlock
        _ ->
            Nothing


{-| This checks if a step is a commonly-confused, non-commutative function by
checking if it is [`aConfusingNonCommutativePrefixOperator`](#aConfusingNonCommutativePrefixOperator)
or on the following blacklist of such functions:

  - `Basics.compare`

  - `Array.append`

  - `Dict.diff`

  - `List.append`

  - `Set.diff`

  - `String.append`

  - `Basics.Extra.safeDivide`

  - `Basics.Extra.safeIntegerDivide`

  - `IntDict.diff`

  - `Maybe.Extra.or`

  - `Result.Extra.or`

If you have suggestions for additions to this list, please open an issue or PR
on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
aConfusingNonCommutativeFunction : StepPredicate
aConfusingNonCommutativeFunction =
    let
        blacklist : Set ( ModuleName, String )
        blacklist =
            Set.fromList
                [ ( [ "Basics" ], "compare" )
                , ( [ "Array" ], "append" )
                , ( [ "Dict" ], "diff" )
                , ( [ "List" ], "append" )
                , ( [ "Set" ], "diff" )
                , ( [ "String" ], "append" )
                , ( [ "Basics", "Extra" ], "safeDivide" )
                , ( [ "Basics", "Extra" ], "safeIntegerDivide" )
                , ( [ "IntDict" ], "diff" )
                , ( [ "Maybe", "Extra" ], "or" )
                , ( [ "Result", "Extra" ], "or" )
                ]

        (StepPredicate opPredicate) =
            aConfusingNonCommutativePrefixOperator
    in
    stepPredicateWithLookupTable <|
        \lookupTable node ->
            opPredicate lookupTable node
                || (getFirstFunction lookupTable node
                        |> Maybe.map (\{ moduleName, name } -> Set.member ( moduleName, name ) blacklist)
                        |> Maybe.withDefault False
                   )


{-| This checks if a step is a commonly-confused, non-commutative prefix
operator by checking if it is on the following blacklist of such functions:

  - `Basics.(-)`

  - `Basics.(/)`

  - `Basics.(//)`

  - `Basics.(^)`

  - `Basics.(<)`

  - `Basics.(>)`

  - `Basics.(<=)`

  - `Basics.(>=)`

  - `Basics.(++)`

  - `Basics.(>>)`

  - `Basics.(<<)`

  - `Parser.(|.)`

  - `Parser.(|=)`

  - `Parser.Advanced.(|.)`

  - `Parser.Advanced.(|=)`

  - `Url.Parser.(</>)`

  - `Url.Parser.(<?>)`

-}
aConfusingNonCommutativePrefixOperator : StepPredicate
aConfusingNonCommutativePrefixOperator =
    let
        blacklist : Set ( ModuleName, String )
        blacklist =
            Set.fromList
                [ ( [ "Basics" ], "-" )
                , ( [ "Basics" ], "/" )
                , ( [ "Basics" ], "//" )
                , ( [ "Basics" ], "^" )
                , ( [ "Basics" ], "<" )
                , ( [ "Basics" ], ">" )
                , ( [ "Basics" ], "<=" )
                , ( [ "Basics" ], ">=" )
                , ( [ "Basics" ], "++" )
                , ( [ "Basics" ], ">>" )
                , ( [ "Basics" ], "<<" )
                , ( [ "Parser" ], "|." )
                , ( [ "Parser" ], "|=" )
                , ( [ "Parser", "Advanced" ], "|." )
                , ( [ "Parser", "Advanced" ], "|=" )
                , ( [ "Url", "Parser" ], "</>" )
                , ( [ "Url", "Parser" ], "<?>" )
                ]
    in
    stepPredicateWithLookupTable <|
        \lookupTable node ->
            getFirstFunction lookupTable node
                |> Maybe.map (\{ moduleName, name } -> Set.member ( moduleName, name ) blacklist)
                |> Maybe.withDefault False


{-| Checks if a step is "visually" simple. This is of course extremely
subjective; if you require different behavior, you can use
[`stepPredicate`](#stepPredicate) to fully customize it. A step is considered
"simple" if it is **40 characters or less**, is only a **single line**, and is
one of the following:

    -- Unit
    ()

    -- Name
    a

    -- Prefix operator
    (+)

    -- Int literal
    1

    -- Hex literal
    0x0F

    -- Float literal
    1.5

    -- String literal
    "bar"

    -- Char literal
    'c'

    -- Record access function
    .field

or is one of the following where all subexpressions are simple:

    -- Application
    foo bar baz

    -- Tuple
    ( a, "b" )

    -- Record
    { a = "value" }

    -- List
    []

    -- Record access
    a.field

    -- Negation
    elmFormatWontLetThisBeAnExample

    -- Parentheses
    elmFormatWontLetThisBeAnExample

-}
aSimpleStep : StepPredicate
aSimpleStep =
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

                        Application es ->
                            List.all go es

                        Negation e_ ->
                            go e_

                        ParenthesizedExpression e_ ->
                            go e_

                        RecordAccess e_ _ ->
                            go e_

                        RecordUpdateExpression _ _ ->
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
    in
    stepPredicate go


{-| Given a function of type `Node Expression -> Bool`, create a `StepPredicate`
from it. This is only useful if you want to write custom predicates for steps.

If you think a generally useful step predicate is missing, please open an issue
or PR on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
stepPredicate : (Node Expression -> Bool) -> StepPredicate
stepPredicate p =
    StepPredicate <| always p


{-| Given a function of type `ModuleNameLookupTable -> Node Expression -> Bool`,
create a `StepPredicate` from it. This is only useful if you want to write
custom predicates for steps and need full module names.

If you think a generally useful step predicate is missing, please open an issue
or PR on Github: <https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
stepPredicateWithLookupTable : (ModuleNameLookupTable -> Node Expression -> Bool) -> StepPredicate
stepPredicateWithLookupTable =
    StepPredicate


{-| Checks if an operator (typically left pizza (`<|`)) is used in the
"canonical" fashion in a test suite, to separate the lambda containing the test
from the `test`. All of the following will pass this predicate, and all other
uses will not:

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

Here is how to construct this predicate, for an example of how to build complex
predicates:

    separateATestFromItsLambda =
        let
            aLambdaExpression : StepPredicate
            aLambdaExpression =
                stepPredicate <|
                    \node ->
                        case Node.value node of
                            LambdaExpression _ ->
                                True

                            _ ->
                                False

            aTestFunction : StepPredicate
            aTestFunction =
                stepPredicateWithLookupTable <|
                    \lookupTable node ->
                        case Node.value node of
                            Application (h :: _) ->
                                case ( Node.value h, moduleNameFor lookupTable h ) of
                                    ( FunctionOrValue _ n, Just [ "Test" ] ) ->
                                        List.member n
                                            [ "test"
                                            , "fuzz"
                                            , "fuzz2"
                                            , "fuzz3"
                                            , "fuzzWith"
                                            ]

                                    _ ->
                                        False

                            _ ->
                                False
        in
        haveFewerStepsThan 3
            |> and (haveAnInputStepThatIs aLambdaExpression)
            |> and (haveASecondStepThatIs aTestFunction)

-}
separateATestFromItsLambda : Predicate ApplicationPipeline
separateATestFromItsLambda =
    let
        aLambdaExpression : StepPredicate
        aLambdaExpression =
            stepPredicate <|
                \node ->
                    case Node.value node of
                        LambdaExpression _ ->
                            True

                        _ ->
                            False

        aTestFunction : StepPredicate
        aTestFunction =
            stepPredicateWithLookupTable <|
                \lookupTable node ->
                    case Node.value node of
                        Application (h :: _) ->
                            case ( Node.value h, moduleNameFor lookupTable h ) of
                                ( FunctionOrValue _ n, Just [ "Test" ] ) ->
                                    List.member n
                                        [ "test"
                                        , "fuzz"
                                        , "fuzz2"
                                        , "fuzz3"
                                        , "fuzzWith"
                                        ]

                                _ ->
                                    False

                        _ ->
                            False
    in
    haveFewerStepsThan 3
        |> and (haveAnInputStepThatIs aLambdaExpression)
        |> and (haveASecondStepThatIs aTestFunction)


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
