module ReviewPipelineStylesTest exposing (all)

import Review.Test
import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , byReportingError
        , forbid
        , haveFewerStepsThan
        , haveMoreStepsThan
        , leftCompositionPipelines
        , leftPizzaPipelines
        , parentheticalApplicationPipelines
        , rightCompositionPipelines
        , rightPizzaPipelines
        , rule
        , spanMultipleLines
        , that
        )
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ReviewPipelineStyles"
        [ operatorSpecificityTests
        , recoveryTests
        , defaultErrorTest
        , ruleHierarchyTests
        , spanMultipleLinesTests
        , lengthTests
        ]


operatorSpecificityTests : Test
operatorSpecificityTests =
    let
        allPipelines : String
        allPipelines =
            """module A exposing (..)

a =
    foo <| bar <| baz

b =
    foo << bar << baz

c =
    foo >> bar >> baz

d =
    foo (bar (baz (i (j k))))

e =
    foo |> bar |> baz

f =
    a |. b |. c

g =
    a |= b |= c
"""
    in
    describe "operator specificity"
        [ test "right pizza does not match other pipelines" <|
            \() ->
                allPipelines
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz""" ]
        , test "left pizza does not match other pipelines" <|
            \() ->
                allPipelines
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo <| bar <| baz""" ]
        , test "right composition does not match other pipelines" <|
            \() ->
                allPipelines
                    |> Review.Test.run
                        (rule
                            [ forbid rightCompositionPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo >> bar >> baz""" ]
        , test "left composition does not match other pipelines" <|
            \() ->
                allPipelines
                    |> Review.Test.run
                        (rule
                            [ forbid leftCompositionPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo << bar << baz""" ]
        , test "parenthetical application does not match other pipelines" <|
            \() ->
                allPipelines
                    |> Review.Test.run
                        (rule
                            [ forbid parentheticalApplicationPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo (bar (baz (i (j k))))""" ]
        ]


recoveryTests : Test
recoveryTests =
    describe "recovers from checking things that aren't pipelines"
        [ test "operator application" <|
            \() ->
                """module A exposing (..)

a =
    10 + (5 - (foo |> bar |> baz))
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "foo |> bar |> baz" ]
        , test "application" <|
            \() ->
                """module A exposing (..)

a =
    foo baz (i |> j |> k) baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "i |> j |> k" ]
        ]


defaultErrorTest : Test
defaultErrorTest =
    test "right pizza does not match other pipelines" <|
        \() ->
            """module A exposing (..)
a = foo |> bar
"""
                |> Review.Test.run
                    (rule [ forbid rightPizzaPipelines ])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Forbidden pipeline style"
                        , details =
                            [ "This pipeline is stylistically-invalid by one of the rules specified in your elm-review config."
                            , "This is the default error message, so if you're unsure why you're seeing it, you should really use ReviewPipelineStyles.byReportingError to provide a more descriptive one!"
                            ]
                        , under = """foo |> bar"""
                        }
                    ]


ruleHierarchyTests : Test
ruleHierarchyTests =
    describe "rule hierarchy"
        [ test "first rule is the one that is reported" <|
            \() ->
                """module A exposing (..)
a = foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> byReportingError "Rule A" [ "Rule A" ]
                            , forbid rightPizzaPipelines
                                |> byReportingError "Rule B" [ "Rule B" ]
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Rule A"
                            , details =
                                [ "Rule A"
                                ]
                            , under = """foo |> bar"""
                            }
                        ]
        , test "first rule is the one that is reported 2" <|
            \() ->
                """module A exposing (..)
a = foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> byReportingError "Rule B" [ "Rule B" ]
                            , forbid rightPizzaPipelines
                                |> byReportingError "Rule A" [ "Rule A" ]
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Rule B"
                            , details =
                                [ "Rule B"
                                ]
                            , under = """foo |> bar"""
                            }
                        ]
        ]


spanMultipleLinesTests : Test
spanMultipleLinesTests =
    describe "spanMultipleLines"
        [ test "single line are single line" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz |> i |> j |> k
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that spanMultipleLines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectNoErrors
        , test "multi line are multi line" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
    |> i |> j |> k
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that spanMultipleLines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz
    |> i |> j |> k""" ]
        ]


lengthTests : Test
lengthTests =
    describe "length"
        [ test "haveMoreStepsThan" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
    |> i |> j |> k
b =
    foo |> bar |> baz |> i |> j
c =
    foo |> bar |> baz |> i
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (haveMoreStepsThan 3)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz
    |> i |> j |> k""", expectFail """foo |> bar |> baz |> i |> j""" ]
        , test "haveFewerStepsThan" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
    |> i
    |> j |> k
b =
    foo |> bar |> baz
    |> i |> j
c =
    foo |> bar |> baz |> i
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (haveFewerStepsThan 5)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz |> i""", expectFail """foo |> bar |> baz
    |> i |> j""" ]
        ]


fail : PipelineRule { r | hasNoError : () } -> PipelineRule { r | hasError : () }
fail =
    byReportingError "Fail" [ "Fail" ]


expectFail : String -> Review.Test.ExpectedError
expectFail under =
    Review.Test.error { message = "Fail", details = [ "Fail" ], under = under }
