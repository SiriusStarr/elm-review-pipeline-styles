module ReviewPipelineStylesTest exposing (all)

import Dependencies.ElmExplorationsTest
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Expect
import Review.ModuleNameLookupTable exposing (moduleNameFor)
import Review.Project exposing (addDependency)
import Review.Test
import Review.Test.Dependencies exposing (projectWithElmCore)
import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , andCallThem
        , andReportCustomError
        , andTryToFixThemBy
        , exceptThoseThat
        , forbid
        , leftCompositionPipelines
        , leftPizzaPipelines
        , parentheticalApplicationPipelines
        , rightCompositionPipelines
        , rightPizzaPipelines
        , rule
        , that
        )
import ReviewPipelineStyles.Fixes exposing (eliminatingInputStep, makingMultiline)
import ReviewPipelineStyles.Predicates
    exposing
        ( aDataStructure
        , aFlowControlStructure
        , aLambdaFunction
        , aLetBlock
        , and
        , doNot
        , getSteps
        , haveAParent
        , haveAParentNotSeparatedBy
        , haveASimpleInputStep
        , haveAnInputStepOf
        , haveAnUnnecessaryInputStep
        , haveFewerStepsThan
        , haveInternalComments
        , haveMoreNestedParentsThan
        , haveMoreStepsThan
        , or
        , predicate
        , predicateWithLookupTable
        , separateATestFromItsLambda
        , spanMultipleLines
        )
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ReviewPipelineStyles"
        [ operatorSpecificityTests
        , recoveryTests
        , ruleHierarchyTests
        , predicateCombinationTests
        , spanMultipleLinesTests
        , lengthTests
        , haveASimpleInputStepTests
        , haveAnInputStepOfTests
        , subpipelineTests
        , nestingTests
        , testUsageTests
        , customPredicateTests
        , haveInternalCommentsTests
        , fixTests
        ]


fixTests : Test
fixTests =
    describe "fixes"
        [ eliminatingInputStepTests
        , makingMultilineTests
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
                                |> andReportCustomError "Rule A" [ "Rule A" ]
                            , forbid rightPizzaPipelines
                                |> andReportCustomError "Rule B" [ "Rule B" ]
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Rule A"
                            , details =
                                [ "Rule A" ]
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
                                |> andReportCustomError "Rule B" [ "Rule B" ]
                            , forbid rightPizzaPipelines
                                |> andReportCustomError "Rule A" [ "Rule A" ]
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Rule B"
                            , details =
                                [ "Rule B" ]
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


haveASimpleInputStepTests : Test
haveASimpleInputStepTests =
    describe "haveASimpleInputStep"
        [ test "more than 40 chars is not simple" <|
            \() ->
                """module A exposing (..)

a = "1234567890abcdefghijklmnopqrstuvwxyz12" |> foo |> bar |> baz

b = "1234567890abcdefghijklmnopqrstuvwxyz123" |> foo1 |> bar1 |> baz1
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that (doNot haveASimpleInputStep)
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
                                |> that (doNot haveASimpleInputStep)
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
                                |> that haveASimpleInputStep
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


subpipelineTests : Test
subpipelineTests =
    describe "subpipeline tests"
        [ test "gets nested pipelines" <|
            \() ->
                """module A exposing (..)

a = foo |> bar (a <| (b (i (j k))) <| c) |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> fail
                            , forbid rightPizzaPipelines
                                |> fail
                            , forbid parentheticalApplicationPipelines
                                |> that (haveMoreStepsThan 1)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar (a <| (b (i (j k))) <| c) |> baz"""
                        , expectFail """a <| (b (i (j k))) <| c"""
                        , expectFail """b (i (j k))"""
                        ]
        ]


nestingTests : Test
nestingTests =
    describe "nesting predicates"
        [ test "haveAParent" <|
            \() ->
                """module A exposing (..)

a = foo |> bar (a <| (b (i (j k))) <| c) |> baz
b = x <| y <| z
c = x (y (z t))
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that haveAParent
                                |> fail
                            , forbid parentheticalApplicationPipelines
                                |> that
                                    (haveAParent
                                        |> and (haveMoreStepsThan 1)
                                    )
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| (b (i (j k))) <| c"""
                        , expectFail """b (i (j k))"""
                        ]
        , test "haveMoreNestedParentsThan" <|
            \() ->
                """module A exposing (..)

a = foo |> bar (a <| (b (i (j k))) <| c) |> baz
b = x <| y <| z
c = x (y (z t))
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveMoreNestedParentsThan 2)
                                |> fail
                            , forbid parentheticalApplicationPipelines
                                |> that (haveMoreNestedParentsThan 1)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """b (i (j k))""" ]
        , test "haveAParentNotSeparatedBy nothing" <|
            \() ->
                """module A exposing (..)

a = foo |> bar |> (a <| b <| c) |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        , test "haveAParentNotSeparatedBy a let block" <|
            \() ->
                """module A exposing (..)

a =
    foo
        |> (a <| b <| c)
        |> (let
                x =
                    5
            in
            i <| j <| k
           )
        |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [ aLetBlock ])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        , test "haveAParentNotSeparatedBy a lambda function" <|
            \() ->
                """module A exposing (..)

a =
    foo
        |> (a <| b <| c)
        |> (\\x -> i <| j <| k x )
        |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [ aLambdaFunction ])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        , test "haveAParentNotSeparatedBy flow control" <|
            \() ->
                """module A exposing (..)

a =
    foo
        |> (a <| b <| c)
        |> (if x then
                i <| j <| k

            else
                y
           )
        |> (case x of
                True ->
                    i <| j <| k

                False ->
                    y
           )
        |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [ aFlowControlStructure ])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        , test "haveAParentNotSeparatedBy a data structure" <|
            \() ->
                """module A exposing (..)

a =
    foo
        |> (a <| b <| c)
        |> f (i <| j <| k, y)
        |> x [y, i <| j <| k]
        |> b {field = i <| j <| k}
        |> b {r | field = i <| j <| k}
        |> {field = i <| j <| k, f = f}.f
        |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [ aDataStructure ])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        , test "haveAParentNotSeparatedBy can deal with multiple degrees of nesting" <|
            \() ->
                """module A exposing (..)

a =
    foo
        |> (a <| b <| c)
        |> (\\f -> ( i <| j <| k, y ))
        |> (if x then
                [ y, i <| j <| k ]

            else
                [ y ]
           )
        |> (let
                x =
                    { field = i <| j <| k }
            in
            x
           )
        |> baz
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> that (haveAParentNotSeparatedBy [ aDataStructure ])
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """a <| b <| c""" ]
        ]


testUsageTests : Test
testUsageTests =
    describe "separateATestFromItsLambda"
        [ test "imported unqualified" <|
            \() ->
                """module A exposing (..)
import Test exposing (..)

suite =
    describe "tests"
        [ test "foo" <|
            \\() ->
                Expect.equal 0 (foo <| bar <| baz)
        , fuzz fooFuzz "fuzz" <|
            \\foo ->
                Expect.equal 0 foo
        , fuzz2 fooFuzz barFuzz "fuzz2" <|
            \\foo bar ->
                Expect.equal foo bar
        , fuzz3 fooFuzz barFuzz bazFuzz "fuzz3" <|
            \\foo bar baz ->
                Expect.equal foo ( bar, baz )
        , fuzzWith { runs = 117 } fooFuzz "fuzzWith" <|
            \\foo ->
                Expect.equal 0 foo
        ]
"""
                    |> Review.Test.runWithProjectData
                        (projectWithElmCore
                            |> addDependency Dependencies.ElmExplorationsTest.dependency
                        )
                        (rule
                            [ forbid leftPizzaPipelines
                                |> exceptThoseThat separateATestFromItsLambda
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail "foo <| bar <| baz" ]
        , test "qualified" <|
            \() ->
                """module A exposing (..)
import Test

suite =
    describe "tests"
        [ Test.test "foo" <|
            \\() ->
                Expect.equal 0 1
        , fuzz fooFuzz "fuzz" <|
            (\\foo ->
                Expect.equal 0 foo)
        , Test.fuzz2 fooFuzz barFuzz "fuzz2" <|
            \\foo bar ->
                Expect.equal foo bar
        , fuzz3 fooFuzz barFuzz bazFuzz "fuzz3" <|
            (\\foo bar baz ->
                Expect.equal foo ( bar, baz ))
        , fuzzWith { runs = 117 } fooFuzz "fuzzWith" <|
            (\\foo ->
                Expect.equal 0 foo)
        ]
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> exceptThoseThat separateATestFromItsLambda
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """fuzz fooFuzz "fuzz" <|
            (\\foo ->
                Expect.equal 0 foo)"""
                        , expectFail """fuzz3 fooFuzz barFuzz bazFuzz "fuzz3" <|
            (\\foo bar baz ->
                Expect.equal foo ( bar, baz ))"""
                        , expectFail """fuzzWith { runs = 117 } fooFuzz "fuzzWith" <|
            (\\foo ->
                Expect.equal 0 foo)"""
                        ]
        , test "not actually the test cases" <|
            \() ->
                """module A exposing (..)

suite =
    describe "tests"
        [ test "foo" <|
            \\() ->
                Expect.equal 0 (foo <| bar <| baz)
        , fuzz fooFuzz "fuzz" <|
            \\foo ->
                Expect.equal 0 foo
        , fuzz2 fooFuzz barFuzz "fuzz2" <|
            \\foo bar ->
                Expect.equal foo bar
        , fuzz3 fooFuzz barFuzz bazFuzz "fuzz3" <|
            \\foo bar baz ->
                Expect.equal foo ( bar, baz )
        , fuzzWith { runs = 117 } fooFuzz "fuzzWith" <|
            \\foo ->
                Expect.equal 0 foo
        ]
"""
                    |> Review.Test.run
                        (rule
                            [ forbid leftPizzaPipelines
                                |> exceptThoseThat (doNot separateATestFromItsLambda)
                                |> fail
                            ]
                        )
                    |> Review.Test.expectNoErrors
        ]


customPredicateTests : Test
customPredicateTests =
    describe "custom predicates"
        [ test "predicate" <|
            \() ->
                """module A exposing (..)

a = foo |> bar |> baz
b = foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that
                                    (predicate
                                        (\p ->
                                            getSteps p
                                                |> List.length
                                                |> (==) 3
                                        )
                                    )
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz""" ]
        , test "predicateWithLookupTable" <|
            \() ->
                """module A exposing (..)

a = foo |> bar |> baz
b = A.foo |> bar
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that
                                    (predicateWithLookupTable
                                        (\table p ->
                                            getSteps p
                                                |> List.head
                                                |> Maybe.andThen (moduleNameFor table)
                                                |> Maybe.map ((==) [])
                                                |> Maybe.withDefault False
                                        )
                                    )
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo |> bar |> baz""" ]
        ]


haveInternalCommentsTests : Test
haveInternalCommentsTests =
    describe "haveInternalComments"
        [ test "ignores comments around, flags comments in" <|
            \() ->
                """module A exposing (..)
a =
    foo
        -- Comment
        |> bar
        |> baz

b =
    -- Ignored
    foo
        |> bar
        |> baz

-- Ignored
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> that haveInternalComments
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo
        -- Comment
        |> bar
        |> baz""" ]
        ]


eliminatingInputStepTests : Test
eliminatingInputStepTests =
    describe "eliminatingInputStep"
        [ test "eliminates simple input right" <|
            \() ->
                let
                    cases : List String
                    cases =
                        [ """() |> unit |> bar"""
                        , """func |> name |> bar"""
                        , """(+) |> prefixOperator |> bar"""
                        , """117 |> intLiteral |> bar"""
                        , """0x0F |> hexLiteral |> bar"""
                        , """1.3 |> floatLiteral |> bar"""
                        , """"abc" |> stringLiteral |> bar"""
                        , """'字' |> charLiteral |> bar"""
                        , """.field |> recordAccessFunction |> bar"""
                        , """(t1, t2) |> simpleTuple |> bar"""
                        , """(t1, foo bar) |> notSimpleTuple |> bar"""
                        , """{field = "simple"} |> simpleRecord |> bar"""
                        , """{field = foo bar} |> notSimpleRecord |> bar"""
                        , """[ "a" ] |> simpleList |> bar"""
                        , """[ foo bar ] |> notSimpleList |> bar"""
                        , """name.field |> simpleRecordAccess |> bar"""
                        , """(foo bar).field |> notSimpleRecordAccess |> bar"""
                        , """-int |> simpleNegation |> bar"""
                        , """-(foo bar) |> notSimpleNegation |> bar"""
                        , """(a) |> simpleParentheses |> bar"""
                        , """(foo bar) |> notSimpleParentheses |> bar"""
                        , """{rec | update = 0} |> recordUpdateNeverSimple |> bar"""
                        , """(if True then 0 else 1) |> ifBlockNeverSimple |> bar"""
                        , """(let foo = bar in baz) |> letBlockNeverSimple |> bar"""
                        , """(case foo of _ -> 0) |> caseNeverSimple |> bar"""
                        , """(\\i -> i + 1) |> lambdaNeverSimple |> bar"""
                        ]

                    fixes : List String
                    fixes =
                        [ """unit () |> bar"""
                        , """name func |> bar"""
                        , """prefixOperator (+) |> bar"""
                        , """intLiteral 117 |> bar"""
                        , """hexLiteral 0x0F |> bar"""
                        , """floatLiteral 1.3 |> bar"""
                        , """stringLiteral "abc" |> bar"""
                        , """charLiteral '字' |> bar"""
                        , """recordAccessFunction .field |> bar"""
                        , """simpleTuple (t1, t2) |> bar"""
                        , """notSimpleTuple (t1, foo bar) |> bar"""
                        , """simpleRecord {field = "simple"} |> bar"""
                        , """notSimpleRecord {field = foo bar} |> bar"""
                        , """simpleList [ "a" ] |> bar"""
                        , """notSimpleList [ foo bar ] |> bar"""
                        , """simpleRecordAccess name.field |> bar"""
                        , """notSimpleRecordAccess (foo bar).field |> bar"""
                        , """simpleNegation -int |> bar"""
                        , """notSimpleNegation -(foo bar) |> bar"""
                        , """simpleParentheses (a) |> bar"""
                        , """notSimpleParentheses (foo bar) |> bar"""
                        , """recordUpdateNeverSimple {rec | update = 0} |> bar"""
                        , """ifBlockNeverSimple (if True then 0 else 1) |> bar"""
                        , """letBlockNeverSimple (let foo = bar in baz) |> bar"""
                        , """caseNeverSimple (case foo of _ -> 0) |> bar"""
                        , """lambdaNeverSimple (\\i -> i + 1) |> bar"""
                        ]
                in
                List.map2
                    (\c f ->
                        let
                            header : String
                            header =
                                "module A exposing (..)\na = "
                        in
                        \() ->
                            (header ++ c)
                                |> Review.Test.run
                                    (rule
                                        [ forbid rightPizzaPipelines
                                            |> that haveAnUnnecessaryInputStep
                                            |> andTryToFixThemBy eliminatingInputStep
                                            |> fail
                                        ]
                                    )
                                |> Review.Test.expectErrors
                                    [ expectFail c
                                        |> Review.Test.whenFixed (header ++ f)
                                    ]
                    )
                    cases
                    fixes
                    |> Expect.all
                    |> (|>) ()
        , test "eliminates simple input left and parenthetical" <|
            \() ->
                let
                    cases : List String
                    cases =
                        [ """bar <| unit <| ()"""
                        , """bar <| name <| func"""
                        , """bar <|
                            name
                               <|  [ 1,
                               2
    ,3]"""
                        , """baz (bar (foo))"""
                        , """baz (bar ( foo    )
                            )"""
                        ]

                    fixes : List String
                    fixes =
                        [ """bar <| unit ()"""
                        , """bar <| name func"""
                        , """bar <|
                            name [ 1,
                               2
    ,3]"""
                        , """baz (bar foo)"""
                        , """baz (bar foo
                            )"""
                        ]
                in
                List.map2
                    (\c f ->
                        let
                            header : String
                            header =
                                "module A exposing (..)\na = "
                        in
                        \() ->
                            (header ++ c)
                                |> Review.Test.run
                                    (rule
                                        [ forbid leftPizzaPipelines
                                            |> that haveAnUnnecessaryInputStep
                                            |> andTryToFixThemBy eliminatingInputStep
                                            |> fail
                                        , forbid parentheticalApplicationPipelines
                                            |> that haveAnUnnecessaryInputStep
                                            |> andTryToFixThemBy eliminatingInputStep
                                            |> fail
                                        ]
                                    )
                                |> Review.Test.expectErrors
                                    [ expectFail c
                                        |> Review.Test.whenFixed (header ++ f)
                                    ]
                    )
                    cases
                    fixes
                    |> Expect.all
                    |> (|>) ()
        , test "unfixable cases" <|
            \() ->
                """module A exposing (..)

a = foo bar |> applicationNeverSimple
b = 1 + 2 |> operatorApplicationNeverSimple
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> andTryToFixThemBy eliminatingInputStep
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo bar |> applicationNeverSimple"""
                        , expectFail """1 + 2 |> operatorApplicationNeverSimple"""
                        ]
        ]


makingMultilineTests : Test
makingMultilineTests =
    describe "makingMultiline"
        [ test "makes single-line pipelines multi-line" <|
            \() ->
                let
                    cases : List String
                    cases =
                        [ """foo |> bar |> baz"""
                        , """func >> func2 >> func 3"""
                        , """a <| (b b2) <| c"""
                        , """a << b"""
                        , """paren (thetical (application pipeline))"""
                        ]

                    fixes : List String
                    fixes =
                        [ """foo
     |> bar
     |> baz
    """
                        , """func
     >> func2
     >> func 3
    """
                        , """a
     <| (b b2)
     <| c
    """
                        , """a
     << b
    """
                        , """paren
     (thetical
     (application pipeline
    ))"""
                        ]
                in
                List.map2
                    (\c f ->
                        let
                            header : String
                            header =
                                "module A exposing (..)\na = "
                        in
                        \() ->
                            (header ++ c)
                                |> Review.Test.run
                                    (rule
                                        [ forbid rightPizzaPipelines
                                            |> that (doNot spanMultipleLines)
                                            |> andTryToFixThemBy makingMultiline
                                            |> fail
                                        , forbid leftPizzaPipelines
                                            |> that (doNot spanMultipleLines)
                                            |> andTryToFixThemBy makingMultiline
                                            |> fail
                                        , forbid rightCompositionPipelines
                                            |> that (doNot spanMultipleLines)
                                            |> andTryToFixThemBy makingMultiline
                                            |> fail
                                        , forbid leftCompositionPipelines
                                            |> that (doNot spanMultipleLines)
                                            |> andTryToFixThemBy makingMultiline
                                            |> fail
                                        , forbid parentheticalApplicationPipelines
                                            |> that (doNot spanMultipleLines)
                                            |> andTryToFixThemBy makingMultiline
                                            |> fail
                                        ]
                                    )
                                |> Review.Test.expectErrors
                                    [ expectFail c
                                        |> Review.Test.whenFixed (header ++ f)
                                    ]
                    )
                    cases
                    fixes
                    |> Expect.all
                    |> (|>) ()
        , test "unfixable cases" <|
            \() ->
                """module A exposing (..)

a = foo bar
   |> applicationNeverSimple |> c
   |> d
b = 1 + 2 |>
  operatorApplicationNeverSimple |> b
"""
                    |> Review.Test.run
                        (rule
                            [ forbid rightPizzaPipelines
                                |> andTryToFixThemBy makingMultiline
                                |> fail
                            ]
                        )
                    |> Review.Test.expectErrors
                        [ expectFail """foo bar
   |> applicationNeverSimple |> c
   |> d"""
                        , expectFail """1 + 2 |>
  operatorApplicationNeverSimple |> b"""
                        ]
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
