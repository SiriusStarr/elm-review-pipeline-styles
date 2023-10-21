module ReviewPipelineStyles.Fixes exposing
    ( eliminatingInputStep, makingMultiline, makingSingleLine, convertingToRightPizza, convertingToLeftPizza, convertingToParentheticalApplication, convertingToRightComposition, convertingToLeftComposition
    , fix, fixWithLookupTable
    , PipelineFix
    )

{-| This module contains various `PipelineFix`s that can be used to fix failing
pipelines. Pre-made rules already contain fixes were possible, so if you're only
using those, you shouldn't need this module.

Fixes may be specified with
[`ReviewPipelineStyles.andTryToFixThemBy`](ReviewPipelineStyles#andTryToFixThemBy)
as follows. If, for example, one made the following rule to detect undesired
multi-step `<|` pipelines:

    forbid leftPizzaPipelines
        |> that (haveMoreStepsThan 1)
        |> andCallThem "<| pipeline with several steps"

then automatic fixes could be used to convert such pipelines to `|>` pipelines as follows:

    forbid leftPizzaPipelines
        |> that (haveMoreStepsThan 1)
        -- Adding fixes vvvv
        |> andTryToFixThemBy convertingToRightPizza
        -- Adding fixes ^^^^
        |> andCallThem "<| pipeline with several steps"

Note that all fixes will only run if it is **possible** to fix the pipeline that
way, i.e. that the fix will not generate invalid code. Look through
[Fixes](#fixes) for pre-made fixes or write your own using [Custom
Fixes](#custom-fixes).


## Fixes

@docs eliminatingInputStep, makingMultiline, makingSingleLine, convertingToRightPizza, convertingToLeftPizza, convertingToParentheticalApplication, convertingToRightComposition, convertingToLeftComposition


## Custom Fixes

If you need fixes beyond what is provided above, you can create them manually
by writing a function of type
`(Range -> String) -> Pipeline -> Maybe (List Fix)` or
`ModuleNameLookupTable -> (Range -> String) -> Pipeline -> Maybe (List Fix)` and
using one of the functions below.

Use the functions in
[Getting Information About Pipelines](ReviewPipelineStyles-Predicates#getting-information-about-pipelines)
to build your custom fix.

@docs fix, fixWithLookupTable


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs PipelineFix

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Internal.Types as Types exposing (NestedWithin(..), Operator(..), Predicate(..))
import Maybe.Extra as MaybeX
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import ReviewPipelineStyles.Predicates
    exposing
        ( ApplicationPipeline
        , CompositionPipeline
        , Pipeline
        , doNot
        , haveAnUnnecessaryInputStep
        , haveInternalComments
        , haveStepsThatAreAll
        , onASingleLine
        , spanMultipleLines
        )


{-| A means of fixing a pipeline, to (presumably) bring it stylistically inline
with what is desired.
-}
type alias PipelineFix pipelineType =
    Types.PipelineFix pipelineType


{-| A fix that eliminates the input step of a pipeline by applying it directly
to the next step, e.g.

    a =
        foo |> bar |> baz

becomes

    a =
        bar foo |> baz

Note that this fix will only be applied if the input step actually _can_ be
combined in this way. It is thus recommended that you combine it with
[`ReviewPipelineStyles.Predicates.haveASimpleInputStep`](ReviewPipelineStyles-Predicates#haveASimpleInputStep)
or the like.

-}
eliminatingInputStep : PipelineFix ApplicationPipeline
eliminatingInputStep =
    fixWithLookupTable <|
        \lookupTable extractSource ({ steps } as pipeline) ->
            let
                matchesPredicate : Predicate ApplicationPipeline -> Bool
                matchesPredicate (Predicate p) =
                    p lookupTable pipeline
            in
            -- Make certain it is safe to remove first step
            if matchesPredicate haveAnUnnecessaryInputStep then
                case steps of
                    s1 :: s2 :: _ ->
                        let
                            ( r1, r2 ) =
                                ( Node.range s1.node, Node.range s2.node )

                            ( source1, source2 ) =
                                ( extractSource r1, extractSource r2 )
                        in
                        Just [ Fix.replaceRangeBy s2.totalRangeAtThisStep <| source2 ++ " " ++ source1 ]

                    _ ->
                        Nothing

            else
                Nothing


{-| Force a pipeline to span a new line with each step. This will not run on
pipelines that already span multiple lines. It is thus recommended that you
combine it with a negated
[`ReviewPipelineStyles.Predicates.spanMultipleLines`](ReviewPipelineStyles-Predicates#spanMultipleLines)
or the like.

This fix (as with most) relies on `elm-format` to clean up the resulting code.

-}
makingMultiline : PipelineFix pipelineType
makingMultiline =
    fixWithLookupTable <|
        \lookupTable _ ({ steps } as pipeline) ->
            let
                matchesPredicate : Predicate ApplicationPipeline -> Bool
                matchesPredicate (Predicate p) =
                    p lookupTable pipeline
            in
            if matchesPredicate spanMultipleLines then
                Nothing

            else
                let
                    break : String
                    break =
                        List.map (\{ node } -> (Node.range node).start.column) steps
                            |> List.minimum
                            |> Maybe.withDefault 1
                            |> (\i -> "\n" ++ String.repeat (i - 1) " ")
                in
                List.map
                    (\{ node } -> Fix.insertAt (Node.range node).end break)
                    steps
                    |> Just


{-| Force a pipeline onto a single line. This can only run on a very limited
set of pipelines, due to the possibility of generating invalid code.
Specifically, for this fix to run, all steps of a pipeline must consist of
expressions on a single line and no comments may exist in the pipeline (as they
would get clobbered by the fix). It will, of course, also not run on a pipeline
that is already on a single line, so it is recommended that you combine it with
[`ReviewPipelineStyles.Predicates.spanMultipleLines`](ReviewPipelineStyles-Predicates#spanMultipleLines)
or the like.
-}
makingSingleLine : PipelineFix pipelineType
makingSingleLine =
    fixWithLookupTable <|
        \l extractSource ({ operator } as pipeline) ->
            let
                matchesPredicate : Predicate ApplicationPipeline -> Bool
                matchesPredicate (Predicate p) =
                    p l pipeline
            in
            if List.all matchesPredicate [ spanMultipleLines, haveStepsThatAreAll onASingleLine, doNot haveInternalComments ] then
                Just
                    [ writeAs extractSource operator pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Convert an application pipeline to right function application (`|>`). This
requires there to be no internal comments (as they would be clobbered), to not
already be a right function application pipeline, and to not be an immediate
nested pipeline (as operator precedence rules preclude this).
-}
convertingToRightPizza : PipelineFix ApplicationPipeline
convertingToRightPizza =
    fix <|
        \extractSource ({ operator, internalComments, parents } as pipeline) ->
            if operator /= RightPizza && List.isEmpty internalComments && isNotImmediateChild parents then
                Just
                    [ writeAs extractSource RightPizza pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Convert an application pipeline to left function application (`<|`). This
requires there to be no internal comments (as they would be clobbered), to not
already be a left function application pipeline, and to not be an immediate
nested pipeline (as operator precedence rules preclude this).
-}
convertingToLeftPizza : PipelineFix ApplicationPipeline
convertingToLeftPizza =
    fix <|
        \extractSource ({ operator, internalComments, parents } as pipeline) ->
            if operator /= LeftPizza && List.isEmpty internalComments && isNotImmediateChild parents then
                Just
                    [ writeAs extractSource LeftPizza pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Convert an application pipeline to parenthetical function application. This
requires there to be no internal comments (as they would be clobbered) and to
not already be a parenthetical function application pipeline.
-}
convertingToParentheticalApplication : PipelineFix ApplicationPipeline
convertingToParentheticalApplication =
    fix <|
        \extractSource ({ operator, internalComments } as pipeline) ->
            if operator /= ParentheticalApplication && List.isEmpty internalComments then
                Just
                    [ writeAs extractSource ParentheticalApplication pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Convert a composition pipeline to right function composition (`>>`). This
requires there to be no internal comments (as they would be clobbered), to not
already be a right function composition pipeline, and to not be an immediate
nested pipeline (as operator precedence rules preclude this).
-}
convertingToRightComposition : PipelineFix CompositionPipeline
convertingToRightComposition =
    fix <|
        \extractSource ({ operator, internalComments, parents } as pipeline) ->
            if operator /= RightComposition && List.isEmpty internalComments && isNotImmediateChild parents then
                Just
                    [ writeAs extractSource RightComposition pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Convert a composition pipeline to left function composition (`<<`). This
requires there to be no internal comments (as they would be clobbered), to not
already be a left function composition pipeline, and to not be an immediate
nested pipeline (as operator precedence rules preclude this).
-}
convertingToLeftComposition : PipelineFix CompositionPipeline
convertingToLeftComposition =
    fix <|
        \extractSource ({ operator, internalComments, parents } as pipeline) ->
            if operator /= LeftComposition && List.isEmpty internalComments && isNotImmediateChild parents then
                Just
                    [ writeAs extractSource LeftComposition pipeline
                        |> Fix.replaceRangeBy (Node.range pipeline.node)
                    ]

            else
                Nothing


{-| Converting pipeline types should check that the pipeline is not immediately
nested within another pipeline, or else it can mangle the code.
-}
isNotImmediateChild : List ( Operator (), NestedWithin ) -> Bool
isNotImmediateChild parents =
    case parents of
        [] ->
            True

        ( _, NestedWithin { aLambdaFunction, aFlowControlStructure, aDataStructure, aLetBlock } ) :: _ ->
            aLambdaFunction || aFlowControlStructure || aDataStructure || aLetBlock


{-| Rewrite a pipeline as another operator type. Note that case must be taken
as this does not check that e.g. application is replaced by composition. The
resulting pipeline will not have line breaks between operators (but of course
may still be multi-line, if the expressions are).
-}
writeAs : (Range -> String) -> Operator () -> Pipeline -> String
writeAs extractSource op { steps, immediateParent } =
    let
        wrapInParens : String -> String
        wrapInParens s =
            if MaybeX.unwrap False (needsParentheses << Node.value) immediateParent then
                "(" ++ s ++ ")"

            else
                s

        ( concatOp, orderSteps, finalize ) =
            case op of
                RightPizza ->
                    ( " |> ", identity, wrapInParens )

                LeftPizza ->
                    ( " <| ", List.reverse, wrapInParens )

                RightComposition ->
                    ( " >> ", identity, wrapInParens )

                LeftComposition ->
                    ( " << ", List.reverse, wrapInParens )

                ParentheticalApplication ->
                    ( " ("
                    , List.reverse
                    , \s -> wrapInParens s ++ String.repeat (List.length steps - 1) ")"
                    )

        needsParentheses : Expression -> Bool
        needsParentheses p =
            case p of
                -- These are nicely set-off already
                TupledExpression _ ->
                    False

                RecordUpdateExpression _ _ ->
                    False

                ListExpr _ ->
                    False

                RecordExpr _ ->
                    False

                LambdaExpression _ ->
                    False

                IfBlock _ _ _ ->
                    False

                LetExpression _ ->
                    False

                CaseExpression _ ->
                    False

                -- Already has them
                ParenthesizedExpression _ ->
                    False

                -- Definitely need them
                Application _ ->
                    True

                -- Safer to wrap it than to depend on precedence
                OperatorApplication _ _ _ _ ->
                    True

                -- Probably not possible, but best not to risk it
                Negation _ ->
                    True

                -- These...shouldn't be possible
                UnitExpr ->
                    False

                Integer _ ->
                    False

                Hex _ ->
                    False

                Literal _ ->
                    False

                CharLiteral _ ->
                    False

                GLSLExpression _ ->
                    False

                Floatable _ ->
                    False

                FunctionOrValue _ _ ->
                    False

                RecordAccess _ _ ->
                    False

                RecordAccessFunction _ ->
                    False

                PrefixOperator _ ->
                    False

                Operator _ ->
                    -- Impossible
                    False
    in
    List.map (\{ node } -> makeStepAsString extractSource node) steps
        |> orderSteps
        |> String.join concatOp
        |> finalize


{-| Given a source extractor, write an expression as a string, wrapping it in
parentheses if necessary.
-}
makeStepAsString : (Range -> String) -> Node Expression -> String
makeStepAsString extractSource node =
    let
        needsParentheses : Bool
        needsParentheses =
            case Node.value node of
                -- Literals
                UnitExpr ->
                    False

                Integer _ ->
                    False

                Hex _ ->
                    False

                RecordExpr _ ->
                    False

                Literal _ ->
                    False

                CharLiteral _ ->
                    False

                GLSLExpression _ ->
                    False

                RecordUpdateExpression _ _ ->
                    False

                ListExpr _ ->
                    False

                Floatable _ ->
                    False

                TupledExpression _ ->
                    False

                -- Just values
                FunctionOrValue _ _ ->
                    False

                RecordAccess _ _ ->
                    False

                RecordAccessFunction _ ->
                    False

                PrefixOperator _ ->
                    False

                Operator _ ->
                    -- Impossible
                    False

                Negation _ ->
                    -- Anything that could be negated is fine
                    False

                Application _ ->
                    -- Higher precedence than any pipeline
                    False

                ParenthesizedExpression _ ->
                    -- Already has them
                    False

                -- Best to wrap them to prevent problems
                LambdaExpression _ ->
                    True

                OperatorApplication _ _ _ _ ->
                    -- Safer to wrap it than to depend on precedence
                    True

                IfBlock _ _ _ ->
                    True

                CaseExpression _ ->
                    True

                LetExpression _ ->
                    True
    in
    Node.range node
        |> extractSource
        |> (\s ->
                if needsParentheses then
                    "(" ++ s ++ ")"

                else
                    s
           )


{-| Create a `PipelineFix` from a function that takes a source code extractor
and a `Pipeline` and maybe returns a list of fixes. Needless to say, this is
dangerous, as it is possible to generate invalid code if you are not careful.

If you think a generally useful fix is missing, please open an issue or PR on
Github:
<https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
fix : ((Range -> String) -> Pipeline -> Maybe (List Fix)) -> PipelineFix pipelineType
fix =
    Types.PipelineFix << always


{-| Create a `PipelineFix` from a function that takes a `ModuleNameLookupTable`,
source code extractor, and a `Pipeline` and maybe returns a list of fixes.
Needless to say, this is dangerous, as it is possible to generate invalid code
if you are not careful.

If you think a generally useful fix is missing, please open an issue or PR on
Github:
<https://github.com/SiriusStarr/elm-review-pipeline-styles/issues>

-}
fixWithLookupTable : (ModuleNameLookupTable -> (Range -> String) -> Pipeline -> Maybe (List Fix)) -> PipelineFix pipelineType
fixWithLookupTable =
    Types.PipelineFix
