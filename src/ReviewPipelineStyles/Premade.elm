module ReviewPipelineStyles.Premade exposing (noMultilineLeftPizza, noMultilineLeftComposition, noSingleLineRightPizza, noSingleLineRightComposition, noPipelinesWithSimpleInputs, noRepeatedParentheticalApplication, noPipelinesWithConfusingNonCommutativeFunctions, noSemanticallyInfixFunctionsInLeftPipelines)

{-|


## Premade Rules

This package module some commonly useful rules, as well as how to construct
them, both so that one might use them as is but also get a sense of how to
construct one's own `PipelineRule`s.

@docs noMultilineLeftPizza, noMultilineLeftComposition, noSingleLineRightPizza, noSingleLineRightComposition, noPipelinesWithSimpleInputs, noRepeatedParentheticalApplication, noPipelinesWithConfusingNonCommutativeFunctions, noSemanticallyInfixFunctionsInLeftPipelines

-}

import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , andCallThem
        , andTryToFixThemBy
        , exceptThoseThat
        , forbid
        , leftCompositionPipelines
        , leftPizzaPipelines
        , parentheticalApplicationPipelines
        , rightCompositionPipelines
        , rightPizzaPipelines
        , that
        )
import ReviewPipelineStyles.Fixes
    exposing
        ( convertingToRightComposition
        , convertingToRightPizza
        , eliminatingInputStep
        , makingMultiline
        , makingSingleLine
        )
import ReviewPipelineStyles.Predicates
    exposing
        ( aConfusingNonCommutativeFunction
        , aConfusingNonCommutativePrefixOperator
        , aDataStructure
        , aFlowControlStructure
        , aLambdaFunction
        , aLetBlock
        , aSemanticallyInfixFunction
        , and
        , doNot
        , haveAParentNotSeparatedBy
        , haveASimpleInputStep
        , haveAnUnnecessaryInputStep
        , haveAnyNonInputStepThatIs
        , haveAnyStepThatIs
        , haveFewerStepsThan
        , haveMoreStepsThan
        , separateATestFromItsLambda
        , spanMultipleLines
        )


{-| These `PipelineRule`s forbid "left pizza" (`<|`) pipelines that span
multiple lines, except for those that are used in the common/"canonical" case in
tests, separating a test from its lambda function. Multiple operator pipelines
will be converted to "right pizza" (`|>`) pipelines, while single operator ones
will (try) to be fixed by placing them on a single line.

For example:

    foo <|
        bar <|
            baz

    a <|
        b c

will be converted to

    baz
        |> bar
        |> foo

    a <| b c

Configuration:

    noMultilineLeftPizza =
        [ forbid leftPizzaPipelines
            |> that
                (spanMultipleLines
                    |> and (haveMoreStepsThan 1)
                )
            |> andTryToFixThemBy convertingToRightPizza
            |> andCallThem "multiline <| pipeline with several steps"
        , forbid leftPizzaPipelines
            |> that
                (spanMultipleLines
                    |> and (haveFewerStepsThan 2)
                )
            |> exceptThoseThat separateATestFromItsLambda
            |> andTryToFixThemBy makingSingleLine
            |> andCallThem "multiline <| pipeline with one step"
        ]

-}
noMultilineLeftPizza : List (PipelineRule ())
noMultilineLeftPizza =
    [ forbid leftPizzaPipelines
        |> that
            (spanMultipleLines
                |> and (haveMoreStepsThan 1)
            )
        |> andTryToFixThemBy convertingToRightPizza
        |> andCallThem "multiline <| pipeline with several steps"
    , forbid leftPizzaPipelines
        |> that
            (spanMultipleLines
                |> and (haveFewerStepsThan 2)
            )
        |> exceptThoseThat separateATestFromItsLambda
        |> andTryToFixThemBy makingSingleLine
        |> andCallThem "multiline <| pipeline with one step"
    ]


{-| These `PipelineRule`s forbid left composition (`<<`) pipelines that span
multiple lines. Multiple operator pipelines will be converted to right
composition (`>>`) pipelines, while single operator ones will (try) to be fixed
by placing them on a single line.

For example:

    foo
        << bar
        << baz

    a
        << b

will be converted to

    baz
        >> bar
        >> foo

    a << b

Configuration:

    noMultilineLeftComposition =
        [ forbid leftCompositionPipelines
            |> that
                (spanMultipleLines
                    |> and (haveMoreStepsThan 1)
                )
            |> andTryToFixThemBy convertingToRightComposition
            |> andCallThem "multiline << pipeline with several steps"
        , forbid leftCompositionPipelines
            |> that
                (spanMultipleLines
                    |> and (haveFewerStepsThan 2)
                )
            |> andTryToFixThemBy makingSingleLine
            |> andCallThem "multiline << pipeline with one step"
        ]

-}
noMultilineLeftComposition : List (PipelineRule ())
noMultilineLeftComposition =
    [ forbid leftCompositionPipelines
        |> that
            (spanMultipleLines
                |> and (haveMoreStepsThan 1)
            )
        |> andTryToFixThemBy convertingToRightComposition
        |> andCallThem "multiline << pipeline with several steps"
    , forbid leftCompositionPipelines
        |> that
            (spanMultipleLines
                |> and (haveFewerStepsThan 2)
            )
        |> andTryToFixThemBy makingSingleLine
        |> andCallThem "multiline << pipeline with one step"
    ]


{-| These `PipelineRule`s forbid "right pizza" (`|>`) pipelines that are
entirely on a single line and try to fix them by making them multiline.

For example:

    foo |> bar |> baz

will be converted to:

    foo
        |> bar
        |> baz

Configuration:

    noSingleLineRightPizza =
        [ forbid rightPizzaPipelines
            |> that (doNot spanMultipleLines)
            |> andTryToFixThemBy makingMultiline
            |> andCallThem "single line |> pipeline"
        ]

-}
noSingleLineRightPizza : List (PipelineRule ())
noSingleLineRightPizza =
    [ forbid rightPizzaPipelines
        |> that (doNot spanMultipleLines)
        |> andTryToFixThemBy makingMultiline
        |> andCallThem "single line |> pipeline"
    ]


{-| These `PipelineRule`s forbid right composition (`>>`) pipelines that are
entirely on a single line and try to fix them by making them multiline.

For example:

    foo >> bar >> baz

will be converted to:

    foo
        >> bar
        >> baz

Configuration:

    noSingleLineRightComposition =
        [ forbid rightCompositionPipelines
            |> that (doNot spanMultipleLines)
            |> andTryToFixThemBy makingMultiline
            |> andCallThem "single line >> pipeline"
        ]

-}
noSingleLineRightComposition : List (PipelineRule ())
noSingleLineRightComposition =
    [ forbid rightCompositionPipelines
        |> that (doNot spanMultipleLines)
        |> andTryToFixThemBy makingMultiline
        |> andCallThem "single line >> pipeline"
    ]


{-| These `PipelineRule`s forbid "right pizza" (`|>`) and "left pizza" (`<|`)
pipelines that have "simple" (unnecessary) inputs and try to fix them by
eliminating the input step.

For example:

    foo |> bar |> baz

    foo <| bar <| baz

will be converted to:

    bar foo |> baz

    foo <| bar baz

Note that all unnecessary left pipeline inputs will be removed (since those
operators do not even add clarity), whereas only visually-simple right pipeline
inputs are removed

Configuration:

    noPipelinesWithSimpleInputs =
        [ forbid rightPizzaPipelines
            |> that haveASimpleInputStep
            |> andTryToFixThemBy eliminatingInputStep
            |> andCallThem "|> pipeline with simple input"
        , forbid leftPizzaPipelines
            |> that haveAnUnnecessaryInputStep
            |> andTryToFixThemBy eliminatingInputStep
            |> andCallThem "<| pipeline with simple input"
        ]

-}
noPipelinesWithSimpleInputs : List (PipelineRule ())
noPipelinesWithSimpleInputs =
    [ forbid rightPizzaPipelines
        |> that haveASimpleInputStep
        |> andTryToFixThemBy eliminatingInputStep
        |> andCallThem "|> pipeline with simple input"
    , forbid leftPizzaPipelines
        |> that haveAnUnnecessaryInputStep
        |> andTryToFixThemBy eliminatingInputStep
        |> andCallThem "<| pipeline with simple input"
    ]


{-| These `PipelineRule`s forbid any pipeline that uses a non-commutative
function with commonly confused argument order. It cannot provide fixes.

For example, the following are flagged by this rule:

    startOfList |> (++) endOfList |> whoops

    keepDict |> Dict.diff subtractDict |> whoops

    1 |> (-) 2 |> whoops

    startOfList |> List.append endOfList |> whoops

The following however are not flagged:

    1 |> (+) 2 |> commutativeFunction

    dict1 |> Dict.union |> dict2 |> commutativeFunction

    foo |> bar |> baz

Note that left pipelines and parenthetical application pipelines are only
flagged with confusing prefix operators, not functions like `compare`, since the
arguments are in the correct order in those cases.

Configuration:

    noPipelinesWithConfusingNonCommutativeFunctions =
        [ forbid rightPizzaPipelines
            |> that (haveAnyStepThatIs aConfusingNonCommutativeFunction)
            |> andCallThem "|> pipeline with confusing non-commutative function"
        , forbid rightCompositionPipelines
            |> that (haveAnyStepThatIs aConfusingNonCommutativeFunction)
            |> andCallThem ">> pipeline with confusing non-commutative function"
        , forbid leftPizzaPipelines
            |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
            |> andCallThem "<| pipeline with confusing non-commutative prefix operator"
        , forbid leftCompositionPipelines
            |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
            |> andCallThem "<< pipeline with confusing non-commutative prefix operator"
        , forbid parentheticalApplicationPipelines
            |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
            |> andCallThem "parenthetical application pipeline with confusing non-commutative prefix operator"
        ]

-}
noPipelinesWithConfusingNonCommutativeFunctions : List (PipelineRule ())
noPipelinesWithConfusingNonCommutativeFunctions =
    [ forbid rightPizzaPipelines
        |> that (haveAnyStepThatIs aConfusingNonCommutativeFunction)
        |> andCallThem "|> pipeline with confusing non-commutative function"
    , forbid rightCompositionPipelines
        |> that (haveAnyStepThatIs aConfusingNonCommutativeFunction)
        |> andCallThem ">> pipeline with confusing non-commutative function"
    , forbid leftPizzaPipelines
        |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
        |> andCallThem "<| pipeline with confusing non-commutative prefix operator"
    , forbid leftCompositionPipelines
        |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
        |> andCallThem "<< pipeline with confusing non-commutative prefix operator"
    , forbid parentheticalApplicationPipelines
        |> that (haveAnyStepThatIs aConfusingNonCommutativePrefixOperator)
        |> andCallThem "parenthetical application pipeline with confusing non-commutative prefix operator"
    ]


{-| These `PipelineRule`s forbid any left pipelines that use a function named in
a "semantically-infix" fashion, i.e. whose function name is intended to be read
with an argument on each side. It fixes them by converting them to the
equivalent right pipeline.

For example:

    Maybe.andThen String.toFloat << List.head

    remainderBy 2 <| 1 + 3

    logBase 2 (10 + 2)

will be converted to:

    List.head >> Maybe.andThen String.toFloat

    1 + 3 |> remainderBy 2

    10 + 2 |> logBase 2

Configuration:

    noSemanticallyInfixFunctionsInLeftPipelines =
        [ forbid leftPizzaPipelines
            |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
            |> andTryToFixThemBy convertingToRightPizza
            |> andCallThem "<| pipeline with a semantically-infix function"
        , forbid leftCompositionPipelines
            |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
            |> andTryToFixThemBy convertingToRightComposition
            |> andCallThem "<< pipeline with a semantically-infix function"
        , forbid parentheticalApplicationPipelines
            |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
            |> andTryToFixThemBy convertingToRightPizza
            |> andCallThem "parenthetical application pipeline with a semantically-infix function"
        ]

-}
noSemanticallyInfixFunctionsInLeftPipelines : List (PipelineRule ())
noSemanticallyInfixFunctionsInLeftPipelines =
    [ forbid leftPizzaPipelines
        |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
        |> andTryToFixThemBy convertingToRightPizza
        |> andCallThem "<| pipeline with a semantically-infix function"
    , forbid leftCompositionPipelines
        |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
        |> andTryToFixThemBy convertingToRightComposition
        |> andCallThem "<< pipeline with a semantically-infix function"
    , forbid parentheticalApplicationPipelines
        |> that (haveAnyNonInputStepThatIs aSemanticallyInfixFunction)
        |> andTryToFixThemBy convertingToRightPizza
        |> andCallThem "parenthetical application pipeline with a semantically-infix function"
    ]


{-| These `PipelineRule`s forbid parenthetical application with more than a
single step and try to fix it by converting it to "right pizza" (`|>`) pipeline.

For example:

    foo (bar (baz i))

will be converted to:

    baz i
        |> bar
        |> foo

It excludes ones that are nested immediately within a pipeline already, as is
often the case with e.g. nested `map`s. For example:

    foo
        |> Maybe.map (Result.map (yi << er))
        |> bar

will not be flagged, as it cannot be converted to a "right pizza" (`|>`)
pipeline or written in a way that is particularly "nicer."

Configuration:

    noRepeatedParentheticalApplication =
        forbid parentheticalApplicationPipelines
            |> that
                (haveMoreStepsThan 1
                    |> and
                        (doNot
                            (haveAParentNotSeparatedBy
                                [ aLetBlock
                                , aLambdaFunction
                                , aFlowControlStructure
                                , aDataStructure
                                ]
                            )
                        )
                )
            |> andTryToFixThemBy convertingToRightPizza
            |> andCallThem "parenthetical application with several steps"

-}
noRepeatedParentheticalApplication : List (PipelineRule ())
noRepeatedParentheticalApplication =
    [ forbid parentheticalApplicationPipelines
        |> that
            (haveMoreStepsThan 1
                |> and
                    (doNot
                        (haveAParentNotSeparatedBy
                            [ aLetBlock
                            , aLambdaFunction
                            , aFlowControlStructure
                            , aDataStructure
                            ]
                        )
                    )
            )
        |> andTryToFixThemBy convertingToRightPizza
        |> andCallThem "parenthetical application with several steps"
    ]
