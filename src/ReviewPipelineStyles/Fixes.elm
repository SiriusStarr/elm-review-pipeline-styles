module ReviewPipelineStyles.Fixes exposing
    ( eliminatingInputStep, makingMultiline, makingSingleLine
    , PipelineFix
    )

{-| This module contains various `PipelineFix`s that can be used to fix failing
pipelines.

Note that all fixes will only run if it is **possible** to fix the pipeline that
way, i.e. that the fix will not generate invalid code.


## Fixes

@docs eliminatingInputStep, makingMultiline, makingSingleLine


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs PipelineFix

-}

import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Internal.Types as Types exposing (Operator(..), Pipeline, Predicate(..))
import Review.Fix as Fix
import ReviewPipelineStyles.Predicates exposing (ApplicationPipeline, haveAnUnnecessaryInputStep, spanMultipleLines)


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
    Types.PipelineFix <|
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
    Types.PipelineFix <|
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
    Types.PipelineFix <|
        \lookupTable extractSource ({ steps, operator, internalComments } as pipeline) ->
            let
                matchesPredicate : Predicate ApplicationPipeline -> Bool
                matchesPredicate (Predicate p) =
                    p lookupTable pipeline
            in
            if matchesPredicate spanMultipleLines then
                let
                    allExpressionsSingleLine : Bool
                    allExpressionsSingleLine =
                        List.all
                            (\{ node } ->
                                let
                                    r : Range
                                    r =
                                        Node.range node
                                in
                                r.start.row == r.end.row
                            )
                            steps
                in
                if List.isEmpty internalComments && allExpressionsSingleLine then
                    Just
                        [ writeAs extractSource operator pipeline
                            |> Fix.replaceRangeBy (Node.range pipeline.node)
                        ]

                else
                    Nothing

            else
                Nothing


{-| Rewrite a pipeline as another operator type. Note that case must be taken
as this does not check that e.g. application is replaced by composition. The
resulting pipeline will not have line breaks between operators (but of course
may still be multi-line, if the expressions are).
-}
writeAs : (Range -> String) -> Operator () -> Pipeline -> String
writeAs extractSource op { steps } =
    let
        ( concatOp, orderSteps, finalize ) =
            case op of
                RightPizza ->
                    ( " |> ", identity, identity )

                LeftPizza ->
                    ( " <| ", List.reverse, identity )

                RightComposition ->
                    ( " >> ", identity, identity )

                LeftComposition ->
                    ( " << ", List.reverse, identity )

                ParentheticalApplication ->
                    ( " (", List.reverse, \s -> s ++ String.repeat (List.length steps - 1) ")" )
    in
    List.map (\{ node } -> extractSource <| Node.range node) steps
        |> orderSteps
        |> String.join concatOp
        |> finalize
