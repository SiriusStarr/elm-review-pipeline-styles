module ReviewPipelineStyles.Fixes exposing
    ( eliminatingInputStep
    , PipelineFix
    )

{-| This module contains various `PipelineFix`s that can be used to fix failing
pipelines.

Note that all fixes will only run if it is **possible** to fix the pipeline that
way, i.e. that the fix will not generate invalid code.


## Fixes

@docs eliminatingInputStep


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs PipelineFix

-}

import Elm.Syntax.Node as Node
import Internal.Types as Types exposing (Predicate(..))
import Review.Fix as Fix
import ReviewPipelineStyles.Predicates exposing (ApplicationPipeline, haveAnUnnecessaryInputStep)


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
    Types.PipelineFix
        (\lookupTable extractSource ({ steps } as pipeline) ->
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
        )
