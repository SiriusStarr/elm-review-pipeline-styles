module ReviewPipelineStyles exposing
    ( rule
    , PipelineRule, forbid, that, exceptThoseThat
    , andCallThem, andReportCustomError
    , andTryToFixThemBy
    , rightPizzaPipelines, leftPizzaPipelines, rightCompositionPipelines, leftCompositionPipelines, parentheticalApplicationPipelines
    )

{-|


## Review Rule

@docs rule


## Premade Rules

Check out the [`ReviewPipelineStyles.Premade`](ReviewPipelineStyles-Premade)
module for some ready-to-go, common `PipelineRule`s as well as how to construct
them.


## Config

@docs PipelineRule, forbid, that, exceptThoseThat


## Failures

@docs andCallThem, andReportCustomError


## Fixes

@docs andTryToFixThemBy


## Pipeline Types

@docs rightPizzaPipelines, leftPizzaPipelines, rightCompositionPipelines, leftCompositionPipelines, parentheticalApplicationPipelines

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Internal.Types as Types exposing (NestedWithin(..), Pipeline)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import ReviewPipelineStyles.Fixes exposing (PipelineFix)
import ReviewPipelineStyles.Predicates exposing (ApplicationPipeline, CompositionPipeline, Operator, Predicate, or)


{-| Reports pipelines that are not valid by user-defined rules. For example,
the usage of `<|` or the usage of excessively-long `|>` pipelines.

    config =
        [ ReviewPipelineStyles.rule
            [ forbid leftPizzaPipelines
                |> andCallThem "forbidden <| pipeline"
            , forbid rightPizzaPipelines
                |> that (haveMoreStepsThan 10)
                |> andCallThem "overly long |> pipeline"
            ]
        ]

This rule works with the following pipeline types:

  - `|>` -- Right "pizza" pipelines, i.e. right function application, so-called
    because the operator resembles a slice of pizza 🍕.
  - `<|` -- Left "pizza" pipelines, i.e. left function application, so-called
    because the operator resembles a slice of pizza 🍕.
  - `>>` -- Right composition pipelines.
  - `<<` -- Left composition pipelines.
  - `foo (bar (baz (i (j k))))` -- Parenthetical application pipelines.


## Fail

By the above config:

    a =
        Just <| foo bar

    b =
        foo
            |> bar
            |> baz
            |> a
            |> b
            |> c
            |> d
            |> e
            |> f
            |> g
            |> h
            |> i
            |> j
            |> k


## Success

By the above config:

    a =
        foo bar
            |> Just

    b =
        foo
            |> bar
            |> baz
            |> a
            |> b
            |> c
            |> d


## When (not) to enable this rule

This rule is useful when you have strong opinions about how functions should be
composed/applied and/or want to enforce consistent code style in a project.

This rule is not useful if you don't care what sorts of pipelines are used in a
project.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-pipeline-styles/example --rules ReviewPipelineStyles
```

-}
rule : List (PipelineRule ()) -> Rule
rule rules =
    Rule.newModuleRuleSchemaUsingContextCreator "ReviewPipelineStyles" initialContext
        |> Rule.withCommentsVisitor (\cs context -> ( [], { context | comments = cs } ))
        |> Rule.withDeclarationEnterVisitor (\d context -> ( declarationVisitor (List.map (ruleToFilter context) rules) context d, context ))
        |> Rule.fromModuleRuleSchema


{-| Create the initial context for the rule.
-}
initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable extractSource () -> { lookupTable = lookupTable, extractSource = extractSource, comments = [] })
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor


{-| Context for the declaration visitor.
-}
type alias Context =
    { lookupTable : ModuleNameLookupTable
    , extractSource : Range -> String
    , comments : List (Node String)
    }


{-| Configuration of this rule is in the form of a list of `PipelineRule`s. It
should be noted that these are hierarchical, i.e. only the first matching error
will be generated in the event that a pipeline would generate multiple errors.

To create a new `PipelineRule`, use [`forbid`](#forbid), then a pipeline type,
then the desired predicates and error. If no predicates are provided, the rule
matches **all** pipelines of that type. For example, to entirely forbid `<|` in
your project, you could use:

    forbid leftPizzaPipelines
        |> andCallThem "forbidden <| pipeline"

Or, to forbid only `|>` pipelines that are extremely long, you could use:

    forbid rightPizzaPipelines
        |> that (haveMoreStepsThan 10)
        |> andCallThem "overly long |> pipeline"

-}
type PipelineRule pipelineType
    = PipelineRule
        { forbidden : Maybe (Predicate pipelineType)
        , except : Maybe (Predicate pipelineType)
        , operator : Operator pipelineType
        , error : Maybe PipelineError
        , fix : Maybe (PipelineFix pipelineType)
        }


{-| Specify the type of error to output for a failed pipeline.
-}
type PipelineError
    = Fail { message : String, details : List String }


{-| The right "pizza" operator is right function application, i.e. `|>`. An
example of this pipeline is below:

    foo
        |> bar
        |> baz

-}
rightPizzaPipelines : Operator ApplicationPipeline
rightPizzaPipelines =
    Types.RightPizza


{-| The left "pizza" operator is left function application, i.e. `<|`. An
example of this pipeline is below:

    foo <| bar <| baz

-}
leftPizzaPipelines : Operator ApplicationPipeline
leftPizzaPipelines =
    Types.LeftPizza


{-| The right composition operator is right function composition, i.e. `>>`. An
example of this pipeline is below:

    foo
        >> bar
        >> baz

-}
rightCompositionPipelines : Operator CompositionPipeline
rightCompositionPipelines =
    Types.RightComposition


{-| The left composition operator is left function composition, i.e. `<<`. An
example of this pipeline is below:

    foo << bar << baz

-}
leftCompositionPipelines : Operator CompositionPipeline
leftCompositionPipelines =
    Types.LeftComposition


{-| Parenthetical application is actually the absence of a pipeline, but rather
successive function calls using parentheses, e.g.

    foo (bar (baz (i (j k))))

-}
parentheticalApplicationPipelines : Operator ApplicationPipeline
parentheticalApplicationPipelines =
    Types.ParentheticalApplication


{-| Forbid certain pipelines.
-}
forbid : Operator pipelineType -> PipelineRule pipelineType
forbid o =
    PipelineRule
        { forbidden = Nothing
        , except = Nothing
        , operator = o
        , error = Nothing
        , fix = Nothing
        }


{-| Convert a `PipelineRule` of a specific type into a generic rule.
-}
finalizeRule : PipelineRule pipelineType -> PipelineRule ()
finalizeRule (PipelineRule { forbidden, except, operator, error, fix }) =
    let
        fixPredicateType : Predicate pipelineType -> Predicate ()
        fixPredicateType (Types.Predicate p) =
            Types.Predicate p

        fixOperatorType : Operator pipelineType -> Operator ()
        fixOperatorType o =
            case o of
                Types.RightPizza ->
                    Types.RightPizza

                Types.LeftPizza ->
                    Types.LeftPizza

                Types.RightComposition ->
                    Types.RightComposition

                Types.LeftComposition ->
                    Types.LeftComposition

                Types.ParentheticalApplication ->
                    Types.ParentheticalApplication

        fixFixType : PipelineFix pipelineType -> PipelineFix ()
        fixFixType (Types.PipelineFix f) =
            Types.PipelineFix f
    in
    PipelineRule
        { forbidden = Maybe.map fixPredicateType forbidden
        , except = Maybe.map fixPredicateType except
        , operator = fixOperatorType operator
        , error = error
        , fix = Maybe.map fixFixType fix
        }


{-| Provide a descriptive name for this type of failing pipeline. This will
appear in the `elm-review` error generated and should give you a sense of what's
wrong and how to fix it. Either this or
[`andReportCustomError`](#andReportCustomError) must be the last thing in your
rule.
-}
andCallThem : String -> PipelineRule anyType -> PipelineRule ()
andCallThem description pRule =
    let
        (PipelineRule r) =
            finalizeRule pRule
    in
    PipelineRule { r | error = Just <| defaultError description }


{-| Provide a fully custom error message for failing pipelines, with both
message and details. Either this or [`andCallThem`](#andCallThem) must be the
last thing in your rule.
-}
andReportCustomError : String -> List String -> PipelineRule anyType -> PipelineRule ()
andReportCustomError message details pRule =
    let
        (PipelineRule r) =
            finalizeRule pRule
    in
    PipelineRule { r | error = Just <| Fail { message = message, details = details } }


{-| Add fixes to a `PipelineRule`.
-}
andTryToFixThemBy : PipelineFix pipelineType -> PipelineRule pipelineType -> PipelineRule pipelineType
andTryToFixThemBy fix (PipelineRule r) =
    PipelineRule { r | fix = Just fix }


{-| Exclude (whitelist) pipelines that match a predicate from being forbidden.

    forbid rightPizzaPipelines
        |> that spanMultipleLines
        |> exceptThoseThat (haveMoreStepsThan 5)

Note that if `exceptThoseThat` is used multiple times, it is equivalent to using
[`or`](ReviewPipelineStyles-Predicates#or). For example, the following two rules are equivalent:

    forbid leftPizzaPipelines
        |> exceptThoseThat
            (doNot spanMultipleLines
                |> or (haveFewerStepsThan 2)
            )

    forbid rightPizzaPipelines
        |> exceptThoseThat (doNot spanMultipleLines)
        |> exceptThoseThat (haveFewerStepsThan 2)

-}
exceptThoseThat : Predicate pipelineType -> PipelineRule pipelineType -> PipelineRule pipelineType
exceptThoseThat p (PipelineRule r) =
    case r.except of
        Nothing ->
            PipelineRule { r | except = Just p }

        Just p_ ->
            PipelineRule { r | except = Just <| or p p_ }


{-| Limit (blacklist) forbidden pipelines to those that match a specific
predicate.

    forbid rightPizzaPipelines
        |> that spanMultipleLines

Note that if `that` is used multiple times, it is equivalent to using
[`or`](ReviewPipelineStyles-Predicates#or). For example, the following two rules are equivalent:

    forbid rightPizzaPipelines
        |> that
            (spanMultipleLines
                |> or (haveMoreStepsThan 5)
            )

    forbid rightPizzaPipelines
        |> that spanMultipleLines
        |> that (haveMoreStepsThan 5)

-}
that : Predicate pipelineType -> PipelineRule pipelineType -> PipelineRule pipelineType
that p (PipelineRule r) =
    case r.forbidden of
        Nothing ->
            PipelineRule { r | forbidden = Just p }

        Just p_ ->
            PipelineRule { r | forbidden = Just <| or p p_ }


{-| Convert a single `PipelineRule`, as passed to the configuration, into a
`Filter` that is actually useful for generating errors.
-}
ruleToFilter : Context -> PipelineRule () -> Filter
ruleToFilter ({ lookupTable } as context) (PipelineRule { forbidden, except, operator, error, fix }) pipeline =
    let
        matchesPredicate : Predicate () -> Bool
        matchesPredicate (Types.Predicate p) =
            p lookupTable pipeline
    in
    if
        (operator == pipeline.operator)
            && MaybeX.unwrap True matchesPredicate forbidden
            && not (MaybeX.unwrap False matchesPredicate except)
    then
        Maybe.withDefault (Fail { message = "Invalid ReviewPipelineStyles config!", details = [ "This should be impossible; please open a Github issue with your elm-review config!" ] }) error
            |> makeError context pipeline fix
            |> Just

    else
        Nothing


{-| Convenience alias for the configuration.
-}
type alias Filter =
    Pipeline -> Maybe (List (Error {}))


{-| Visit function TLDs and pass their expression to `expressionVisitor`.
-}
declarationVisitor : List Filter -> Context -> Node Declaration -> List (Error {})
declarationVisitor filters context d =
    case Node.value d of
        Declaration.FunctionDeclaration { declaration } ->
            Node.value declaration
                |> .expression
                |> descendToPipelines context []
                |> List.filterMap (MaybeX.oneOf filters)
                |> List.concat

        _ ->
            -- No pipelines in any of:
            -- AliasDeclaration TypeAlias
            -- CustomTypeDeclaration Type
            -- PortDeclaration Signature
            -- InfixDeclaration Infix
            -- Destructuring (Node Pattern) (Node Expression)
            []


{-| Given a list of parent pipelines and an expression node, check for child
pipelines within that node, descending as necessary.
-}
descendToPipelines : Context -> List ( Operator (), NestedWithin ) -> Node Expression -> List Pipeline
descendToPipelines context parents node =
    let
        go : Node Expression -> List Pipeline
        go =
            descendToPipelines context parents

        goIn : (NestedWithin -> NestedWithin) -> Node Expression -> List Pipeline
        goIn f =
            case List.head parents of
                Just ( op, n ) ->
                    descendToPipelines context (( op, f n ) :: List.drop 1 parents)

                _ ->
                    go

        flowControl : NestedWithin -> NestedWithin
        flowControl (NestedWithin r) =
            NestedWithin { r | aFlowControlStructure = True }

        dataStructure : NestedWithin -> NestedWithin
        dataStructure (NestedWithin r) =
            NestedWithin { r | aDataStructure = True }

        letBlock : NestedWithin -> NestedWithin
        letBlock (NestedWithin r) =
            NestedWithin { r | aLetBlock = True }

        lambdaFunction : NestedWithin -> NestedWithin
        lambdaFunction (NestedWithin r) =
            NestedWithin { r | aLambdaFunction = True }
    in
    case Node.value node of
        OperatorApplication op dir left right ->
            getPipeline context parents node op dir left right
                |> Maybe.withDefault (go left ++ go right)

        Application es ->
            -- Application might be the start of a parenthetical application pipeline
            getParentheticalPipeline context parents node
                |> Maybe.withDefault (List.concatMap go es)

        -- Descend into subexpression until we encounter a pipeline
        ParenthesizedExpression e ->
            go e

        Negation e ->
            go e

        CaseExpression { expression, cases } ->
            goIn flowControl expression ++ List.concatMap (goIn flowControl << Tuple.second) cases

        IfBlock predE thenE elseE ->
            List.concatMap (goIn flowControl) [ predE, thenE, elseE ]

        TupledExpression es ->
            List.concatMap (goIn dataStructure) es

        LetExpression { declarations, expression } ->
            let
                goLetDecl : Node LetDeclaration -> List Pipeline
                goLetDecl d =
                    case Node.value d of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression
                                |> goIn letBlock

                        LetDestructuring _ e ->
                            goIn letBlock e
            in
            goIn letBlock expression
                ++ List.concatMap goLetDecl declarations

        LambdaExpression { expression } ->
            goIn lambdaFunction expression

        RecordExpr rs ->
            List.concatMap (goIn dataStructure << Tuple.second << Node.value) rs

        ListExpr es ->
            List.concatMap (goIn dataStructure) es

        RecordAccess e _ ->
            go e

        RecordUpdateExpression _ rs ->
            List.concatMap (goIn dataStructure << Tuple.second << Node.value) rs

        _ ->
            -- Cannot descend into
            -- FunctionOrValue ModuleName String
            -- Operator String
            -- PrefixOperator String
            -- UnitExpr
            -- Integer Int
            -- Hex Int
            -- Floatable Float
            -- Literal String
            -- CharLiteral Char
            -- RecordAccessFunction String
            -- GLSLExpression String
            []


{-| Given a list of parent pipelines, get a pipeline from an operator
application or fail if it's not a pipeline.
-}
getPipeline : Context -> List ( Operator (), NestedWithin ) -> Node Expression -> String -> InfixDirection -> Node Expression -> Node Expression -> Maybe (List Pipeline)
getPipeline ({ comments } as context) parents node op dir left right =
    let
        go : Node Expression -> ( { node : Node Expression, totalRangeAtThisStep : Range }, List { node : Node Expression, totalRangeAtThisStep : Range } )
        go e =
            case Node.value e of
                OperatorApplication op_ dir_ left_ right_ ->
                    case ( op == op_ && dir == dir_, dir_ ) of
                        ( True, Left ) ->
                            go left_
                                |> Tuple.mapSecond ((::) { node = right_, totalRangeAtThisStep = Node.range e })

                        ( True, Right ) ->
                            go right_
                                |> Tuple.mapSecond ((::) { node = left_, totalRangeAtThisStep = Node.range e })

                        _ ->
                            -- Pipeline ended
                            ( { node = e, totalRangeAtThisStep = Node.range e }, [] )

                _ ->
                    -- Pipeline ended
                    ( { node = e, totalRangeAtThisStep = Node.range e }, [] )

        makePipeline : Operator () -> List { node : Node Expression, totalRangeAtThisStep : Range } -> List Pipeline
        makePipeline operator steps =
            List.concatMap
                (.node
                    >> descendToPipelines context
                        (( operator
                         , NestedWithin
                            { aLambdaFunction = False
                            , aFlowControlStructure = False
                            , aDataStructure = False
                            , aLetBlock = False
                            }
                         )
                            :: parents
                        )
                )
                steps
                |> (::)
                    { operator = operator
                    , steps = steps
                    , node = node
                    , parents = parents
                    , internalComments = getCommentsIn (Node.range node) comments
                    }
    in
    case ( op, dir ) of
        ( "|>", Left ) ->
            go left
                |> (\( input, steps ) ->
                        input
                            :: List.reverse ({ node = right, totalRangeAtThisStep = Node.range node } :: steps)
                            |> makePipeline Types.RightPizza
                   )
                |> Just

        ( "<|", Right ) ->
            go right
                |> (\( input, steps ) ->
                        input
                            :: List.reverse ({ node = left, totalRangeAtThisStep = Node.range node } :: steps)
                            |> makePipeline Types.LeftPizza
                   )
                |> Just

        ( ">>", Right ) ->
            go right
                |> (\( input, steps ) ->
                        input
                            :: List.reverse ({ node = left, totalRangeAtThisStep = Node.range node } :: steps)
                            |> List.reverse
                            |> makePipeline Types.RightComposition
                   )
                |> Just

        ( "<<", Left ) ->
            go left
                |> (\( input, steps ) ->
                        input
                            :: List.reverse ({ node = right, totalRangeAtThisStep = Node.range node } :: steps)
                            |> List.reverse
                            |> makePipeline Types.LeftComposition
                   )
                |> Just

        _ ->
            Nothing


{-| Given a list of parent pipelines, get a parenthetical application pipeline
from an `Application` node or fail.
-}
getParentheticalPipeline : Context -> List ( Operator (), NestedWithin ) -> Node Expression -> Maybe (List Pipeline)
getParentheticalPipeline ({ comments } as context) parents node =
    let
        go : Node Expression -> ( { node : Node Expression, totalRangeAtThisStep : Range }, List { node : Node Expression, totalRangeAtThisStep : Range } )
        go e =
            case Node.value e of
                Application es ->
                    case Maybe.map (Tuple.mapFirst Node.value) <| ListX.unconsLast es of
                        Just ( ParenthesizedExpression e_, es_ ) ->
                            makeAppNode es_
                                |> Maybe.map (\step -> Tuple.mapSecond ((::) { node = step, totalRangeAtThisStep = Node.range e }) <| go e_)
                                -- Pipeline ended
                                |> Maybe.withDefault ( { node = e, totalRangeAtThisStep = Node.range e }, [] )

                        _ ->
                            -- Pipeline ended
                            ( { node = e, totalRangeAtThisStep = Node.range e }, [] )

                _ ->
                    -- Pipeline ended
                    ( { node = e, totalRangeAtThisStep = Node.range e }, [] )

        makeAppNode : List (Node Expression) -> Maybe (Node Expression)
        makeAppNode es =
            -- Have to convert into a node less the last element of the application, since that's what the "step" of the pipeline is
            case es of
                [] ->
                    Nothing

                [ e ] ->
                    Just e

                _ ->
                    Just <| Node (Range.combine <| List.map Node.range es) (Application es)
    in
    case go node of
        ( _, [] ) ->
            -- No steps, so not a pipeline
            Nothing

        ( input, steps ) ->
            (input :: List.reverse steps)
                |> (\allSteps ->
                        List.concatMap
                            (.node
                                >> descendToPipelines context
                                    (( Types.ParentheticalApplication
                                     , NestedWithin
                                        { aDataStructure = False
                                        , aFlowControlStructure = False
                                        , aLetBlock = False
                                        , aLambdaFunction = False
                                        }
                                     )
                                        :: parents
                                    )
                            )
                            allSteps
                            |> (::)
                                { operator = Types.ParentheticalApplication
                                , steps = allSteps
                                , node = node
                                , parents = parents
                                , internalComments = getCommentsIn (Node.range node) comments
                                }
                   )
                |> Just


{-| Given a range and a list of comments, find all comments that are within that
range.
-}
getCommentsIn : Range -> List (Node String) -> List (Node String)
getCommentsIn range =
    List.filter
        (\c ->
            let
                r : Range
                r =
                    Node.range c
            in
            ((r.start.row > range.start.row)
                || (r.start.row == range.start.row && r.start.column >= range.start.column)
            )
                && ((r.end.row < range.end.row)
                        || (r.end.row == range.end.row && r.end.column <= range.end.column)
                   )
        )


{-| Given a `Pipeline` and a `PipelineError`, create an actual `elm-review`
error.
-}
makeError : Context -> Pipeline -> Maybe (PipelineFix ()) -> PipelineError -> List (Error {})
makeError { extractSource, lookupTable } ({ node } as p) fix (Fail err) =
    Maybe.map (\(Types.PipelineFix f) -> f) fix
        |> Maybe.andThen (\f -> f lookupTable extractSource p)
        |> MaybeX.unwrap Rule.error (\f e r -> Rule.errorWithFix e r f)
        |> (\makeErr -> makeErr err (Node.range node))
        |> List.singleton


{-| The error that is reported for invalid pipelines if none is provided.
-}
defaultError : String -> PipelineError
defaultError description =
    Fail
        { message = "Forbidden pipeline style: " ++ description
        , details =
            [ "This pipeline is a: " ++ description
            , "It is stylistically-invalid by one of the rules specified in your elm-review config."
            , "If you're still unsure why you're seeing it, you should use ReviewPipelineStyles.andReportCustomError to provide a more descriptive error message."
            ]
        }
