module ReviewPipelineStylesTest exposing (all)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Test
import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , and
        , byReportingError
        , doNot
        , exceptThoseThat
        , forbid
        , haveASimpleInput
        , haveAnInputOf
        , haveFewerStepsThan
        , haveMoreStepsThan
        , leftCompositionPipelines
        , leftPizzaPipelines
        , or
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
        , defaultErrorTests
        , ruleHierarchyTests
        , predicateCombinationTests
        , spanMultipleLinesTests
        , lengthTests
        , haveASimpleInputTests
        , haveAnInputOfTests
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


defaultErrorTests : Test
defaultErrorTests =
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


predicateCombinationTests : Test
predicateCombinationTests =
    describe "predicate combination"
        [ test "blacklist works" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (haveMoreStepsThan 1)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "foo |> bar |> baz" ]
        , test "whitelist works" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> exceptThoseThat (haveFewerStepsThan 2)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "foo |> bar |> baz" ]
        , test "doNot works" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
    |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (doNot spanMultipleLines)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "foo |> bar |> baz" ]
        , test "and works" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
    |> baz
c = foo
    |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that
                                    (haveMoreStepsThan 1
                                        |> and spanMultipleLines
                                    )
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar
    |> baz""" ]
        , test "or works" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
    |> baz
c = foo
    |> bar
d = foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that
                                    (haveMoreStepsThan 1
                                        |> or spanMultipleLines
                                    )
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar
    |> baz"""
                        , expectFail """foo |> bar |> baz"""
                        , expectFail """foo
    |> bar"""
                        ]
        , test "that works as or" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
    |> baz
c = foo
    |> bar
d = foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (haveMoreStepsThan 1)
                                |> that spanMultipleLines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar
    |> baz"""
                        , expectFail """foo |> bar |> baz"""
                        , expectFail """foo
    |> bar"""
                        ]
        , test "exceptThoseThat works as or" <|
            \() ->
                """module A exposing (..)

a =
    foo |> bar |> baz
b =
    foo |> bar
    |> baz
c = foo
    |> bar
d = a |> b
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> exceptThoseThat (haveMoreStepsThan 1)
                                |> exceptThoseThat spanMultipleLines
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a |> b""" ]
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


haveASimpleInputTests : Test
haveASimpleInputTests =
    describe "haveASimpleInput"
        [ test "more than 40 chars is not simple" <|
            \() ->
                """module A exposing (..)

a = "1234567890abcdefghijklmnopqrstuvwxyz12" |> foo |> bar |> baz

b = "1234567890abcdefghijklmnopqrstuvwxyz123" |> foo1 |> bar1 |> baz1
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (doNot haveASimpleInput)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """"1234567890abcdefghijklmnopqrstuvwxyz123" |> foo1 |> bar1 |> baz1""" ]
        , test "multiline is not simple" <|
            \() ->
                """module A exposing (..)

a = "abc def"
    |> foo
    |> bar
    |> baz

b = \"\"\"abc
def\"\"\"
    |> foo1
    |> bar1
    |> baz1
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (doNot haveASimpleInput)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """\"\"\"abc
def\"\"\"
    |> foo1
    |> bar1
    |> baz1""" ]
        , test "some things are simple and some are not" <|
            \() ->
                """module A exposing (..)

a = () |> unit
b = func |> name
c = (+) |> prefixOperator
d = 117 |> intLiteral
e = 0x0F |> hexLiteral
f = 1.3 |> floatLiteral
g = "abc" |> stringLiteral
h = '字' |> charLiteral
i = .field |> recordAccessFunction
j = (t1, t2) |> simpleTuple
k = (t1, foo bar) |> notSimpleTuple
l = {field = "simple"} |> simpleRecord
m = {field = foo bar} |> notSimpleRecord
n = [ "a" ] |> simpleList
o = [ foo bar ] |> notSimpleList
p = name.field |> simpleRecordAccess
q = (foo bar).field |> notSimpleRecordAccess
r = -int |> simpleNegation
s = -(foo bar) |> notSimpleNegation
t = (a) |> simpleParentheses
u = (foo bar) |> notSimpleParentheses
v = {rec | update = 0} |> recordUpdateNeverSimple
w = foo bar |> applicationNeverSimple
x = 1 + 2 |> operatorApplicationNeverSimple
y = (if True then 0 else 1) |> ifBlockNeverSimple
z = (let foo = bar in baz) |> letBlockNeverSimple
aa = (case foo of _ -> 0) |> caseNeverSimple
ab = (\\i -> i + 1) |> lambdaNeverSimple
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that haveASimpleInput
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """() |> unit"""
                        , expectFail """func |> name"""
                        , expectFail """(+) |> prefixOperator"""
                        , expectFail """117 |> intLiteral"""
                        , expectFail """0x0F |> hexLiteral"""
                        , expectFail """1.3 |> floatLiteral"""
                        , expectFail """"abc" |> stringLiteral"""
                        , expectFail """'字' |> charLiteral"""
                        , expectFail """.field |> recordAccessFunction"""
                        , expectFail """(t1, t2) |> simpleTuple"""
                        , expectFail """{field = "simple"} |> simpleRecord"""
                        , expectFail """[ "a" ] |> simpleList"""
                        , expectFail """name.field |> simpleRecordAccess"""
                        , expectFail """-int |> simpleNegation"""
                        , expectFail """(a) |> simpleParentheses"""
                        ]
        ]


haveAnInputOfTests : Test
haveAnInputOfTests =
    test "haveAnInputOf works for all pipelines" <|
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
                            |> that (haveAnInputOf specificIntLiteral)
                            |> fail
                        , forbid rightPizzaPipelines
                            |> that (haveAnInputOf specificIntLiteral)
                            |> fail
                        , forbid leftCompositionPipelines
                            |> that (haveAnInputOf specificIntLiteral)
                            |> fail
                        , forbid rightCompositionPipelines
                            |> that (haveAnInputOf specificIntLiteral)
                            |> fail
                        , forbid parentheticalApplicationPipelines
                            |> that (haveAnInputOf specificIntLiteral)
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


fail : PipelineRule r -> PipelineRule r
fail =
    byReportingError "Fail" [ "Fail" ]


expectFail : String -> Review.Test.ExpectedError
expectFail under =
    Review.Test.error { message = "Fail", details = [ "Fail" ], under = under }
