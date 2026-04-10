module ReviewPipelineStylesDeprecatedTest exposing (allDeprecated)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Test
import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , andCallThem
        , forbid
        , leftCompositionPipelines
        , leftPizzaPipelines
        , parentheticalApplicationPipelines
        , rightCompositionPipelines
        , rightPizzaPipelines
        , rule
        , that
        )
import ReviewPipelineStyles.Predicates
    exposing
        ( haveAnInputStepOf
        )
import Test exposing (Test, describe, test)


allDeprecated : Test
allDeprecated =
    describe "ReviewPipelineStyles deprecated"
        [ haveAnInputStepOfTests ]


haveAnInputStepOfTests : Test
haveAnInputStepOfTests =
    test "haveAnInputStepOf works for all pipelines" <|
        \() ->
            let
                specificIntLiteral : Node Expression -> Bool
                specificIntLiteral n =
                    Node.value n == Expression.Integer 117
            in
            """module A exposing (..)

a1 =
    foo <| bar <| 117
a2 =
    foo <| bar2 <| 117 <| baz
b1 =
    foo << bar << 117
b2 =
    foo << bar2 << 117 << baz
c1 =
    117 >> bar >> baz
c2 =
    foo >> 117 >> bar2 >> baz
d1 =
    foo (bar (baz (i (117))))
d2 =
    foo (bar2 (baz (i (117 (j k)))))
e1 =
    117 |> bar |> baz
e2 =
    foo |> 117 |> bar2 |> baz
"""
                |> Review.Test.run
                    (rule
                        [ forbid leftPizzaPipelines
                            |> that (haveAnInputStepOf specificIntLiteral)
                            |> fail
                        , forbid rightPizzaPipelines
                            |> that (haveAnInputStepOf specificIntLiteral)
                            |> fail
                        , forbid leftCompositionPipelines
                            |> that (haveAnInputStepOf specificIntLiteral)
                            |> fail
                        , forbid rightCompositionPipelines
                            |> that (haveAnInputStepOf specificIntLiteral)
                            |> fail
                        , forbid parentheticalApplicationPipelines
                            |> that (haveAnInputStepOf specificIntLiteral)
                            |> fail
                        ]
                    )
                |> Review.Test.expectErrors
                    [ expectFail """117 |> bar |> baz"""
                    , expectFail """foo <| bar <| 117"""
                    , expectFail """foo << bar << 117"""
                    , expectFail """117 >> bar >> baz"""
                    , expectFail """foo (bar (baz (i (117))))"""
                    ]


fail : PipelineRule anyType -> PipelineRule ()
fail =
    andCallThem "failing pipeline"


expectFail : String -> Review.Test.ExpectedError
expectFail under =
    Review.Test.error
        { message = "Forbidden pipeline style: failing pipeline"
        , details =
            [ "This pipeline is a: failing pipeline"
            , "It is stylistically-invalid by one of the rules specified in your elm-review config."
            , "If you're still unsure why you're seeing it, you should use ReviewPipelineStyles.andReportCustomError to provide a more descriptive error message."
            ]
        , under = under
        }
