module ReviewPipelineStyles exposing
    ( rule
    , PipelineRule, forbid, that, exceptThoseThat
    , byReportingError
    , rightPizzaPipelines, leftPizzaPipelines, rightCompositionPipelines, leftCompositionPipelines, parentheticalApplicationPipelines
    , and, or, doNot
    , spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInput, haveAnInputOf
    , Pipeline
    , Predicate, Operator
    )

{-|

@docs rule


# Config

@docs PipelineRule, forbid, that, exceptThoseThat


## Failures

@docs byReportingError


## Pipeline Types

@docs rightPizzaPipelines, leftPizzaPipelines, rightCompositionPipelines, leftCompositionPipelines, parentheticalApplicationPipelines


## Creating Predicates

@docs and, or, doNot


## Predicates

@docs spanMultipleLines, haveMoreStepsThan, haveFewerStepsThan, haveASimpleInput, haveAnInputOf


## Manual Predicates

If you need predicates beyond what is provided above, you can create them
manually by simply writing a function of type `Pipeline -> Bool`.

@docs Pipeline


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs Predicate, Operator

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.Rule as Rule exposing (Error, Rule)
import String exposing (right)


{-| Reports pipelines that are not valid by user-defined rules. For example,
the usage of `<|` or the usage of excessively-long `|>` pipelines.

    config =
        [ ReviewPipelineStyles.rule
            [ forbid leftPizzaPipelines
                |> byReportingError "Forbidden <| pipeline!" [ "Left application pipelines are forbidden in this project, so please remove it." ]
            , forbid rightPizzaPipelines
                |> that (haveMoreStepsThan 10)
                |> byReportingError "Overly long |> pipeline!" [ "Right application pipelines may only be a maximum of 11 steps long in this project, so please remove it." ]
            ]
        ]

This rule works with the following pipeline types:

  - `|>`
  - `<|`
  - `>>`
  - `<<`
  - `foo (bar (baz (i (j k))))`


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
rule : List (PipelineRule r) -> Rule
rule rules =
    Rule.newModuleRuleSchema "ReviewPipelineStyles" ()
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor <| List.map ruleToFilter rules)
        |> Rule.fromModuleRuleSchema


{-| Configuration of this rule is in the form of a list of `PipelineRule`s. It
should be noted that these are hierarchical, i.e. only the first matching error
will be generated in the event that a pipeline would generate multiple errors.

To create a new `PipelineRule`, use [`forbid`](#forbid), then a pipeline type,
then the desired predicates and error. If no predicates are provided, the rule
matches **all** pipelines of that type. For example, to entirely forbid `<|` in
your project, you could use:

    forbid leftPizzaPipelines
        |> byReportingError "Forbidden <| pipeline!" [ "Left application pipelines are forbidden in this project, so please remove it." ]

Or, to forbid only `|>` pipelines that are extremely long, you could use:

    forbid rightPizzaPipelines
        |> that (haveMoreStepsThan 10)
        |> byReportingError "Overly long |> pipeline!" [ "Right application pipelines may only be a maximum of 11 steps long in this project, so please remove it." ]

-}
type PipelineRule a
    = PipelineRule
        { forbidden : Maybe Predicate
        , except : Maybe Predicate
        , operator : Operator
        , error : Maybe PipelineError
        }


{-| A detected pipeline. You only need be concerned with this type if you are
writing a manual predicate. Note that the types contained within this are from
[`stil4m/elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.7/)
if you need to work with them directly.

  - `operator`: The operator that was detected
  - `steps`: The steps of the pipeline. Note that this is in "logical" order,
    e.g. `a >> b >> c`, `c << b << a`, `a |> b |> c`, `c <| b <| a`, and
    `c (b a)` will all have the same steps of `[a, b, c]`.
  - `node`: The outermost `Node` of the pipeline; you probably don't need to
    work with this directly.

-}
type alias Pipeline =
    { operator : Operator
    , steps : List (Node Expression)
    , node : Node Expression
    }


{-| A predicate for filtering pipelines, or a logical combination of them.
-}
type Predicate
    = Predicate (Pipeline -> Bool)


{-| The operator type of a pipeline.

  - `RightPizza` -- `|>`
  - `LeftPizza` -- `<|`
  - `RightComposition` -- `>>`
  - `LeftComposition` -- `<<`
  - `ParentheticalApplication` -- `foo (bar (baz (i (j k))))`

-}
type Operator
    = RightPizza
    | LeftPizza
    | RightComposition
    | LeftComposition
    | ParentheticalApplication


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
rightPizzaPipelines : Operator
rightPizzaPipelines =
    RightPizza


{-| The left "pizza" operator is left function application, i.e. `<|`. An
example of this pipeline is below:

    foo <| bar <| baz

-}
leftPizzaPipelines : Operator
leftPizzaPipelines =
    LeftPizza


{-| The right composition operator is right function composition, i.e. `>>`. An
example of this pipeline is below:

    foo
        >> bar
        >> baz

-}
rightCompositionPipelines : Operator
rightCompositionPipelines =
    RightComposition


{-| The left composition operator is left function composition, i.e. `<<`. An
example of this pipeline is below:

    foo << bar << baz

-}
leftCompositionPipelines : Operator
leftCompositionPipelines =
    LeftComposition


{-| Parenthetical application is actually the absence of a pipeline, but rather
successive function calls using parentheses, e.g.

    foo (bar (baz (i (j k))))

-}
parentheticalApplicationPipelines : Operator
parentheticalApplicationPipelines =
    ParentheticalApplication


{-| Forbid certain pipelines.
-}
forbid : Operator -> PipelineRule { hasNoLimit : (), hasNoException : (), hasNoError : () }
forbid o =
    PipelineRule
        { forbidden = Nothing
        , except = Nothing
        , operator = o
        , error = Nothing
        }


{-| Provide a message and details to forbid pipelines by reporting an error
without a fix. If you don't pass any error to a `PipelineRule`, you will get a
generic error message that isn't very helpful, so you should always specify an
error!
-}
byReportingError : String -> List String -> PipelineRule r -> PipelineRule r
byReportingError message details (PipelineRule r) =
    PipelineRule { r | error = Just <| Fail { message = message, details = details } }


{-| Exclude (whitelist) pipelines that match a predicate from being forbidden.

    forbid rightPizzaPipelines
        |> that spanMultipleLines
        |> exceptThoseThat (haveMoreStepsThan 5)

Note that if `exceptThoseThat` is used multiple times, it is equivalent to using
[`or`](#or). For example, the following two rules are equivalent:

    forbid leftPizzaPipelines
        |> exceptThoseThat
            (doNot spanMultipleLines
                |> or (haveFewerStepsThan 2)
            )

    forbid rightPizzaPipelines
        |> exceptThoseThat (doNot spanMultipleLines)
        |> exceptThoseThat (haveFewerStepsThan 2)

-}
exceptThoseThat : Predicate -> PipelineRule r -> PipelineRule r
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
[`or`](#or). For example, the following two rules are equivalent:

    forbid rightPizzaPipelines
        |> that
            (spanMultipleLines
                |> or (haveMoreStepsThan 5)
            )

    forbid rightPizzaPipelines
        |> that spanMultipleLines
        |> that (haveMoreStepsThan 5)

-}
that : Predicate -> PipelineRule r -> PipelineRule r
that p (PipelineRule r) =
    case r.forbidden of
        Nothing ->
            PipelineRule { r | forbidden = Just p }

        Just p_ ->
            PipelineRule { r | forbidden = Just <| or p p_ }


{-| Checks whether or not a pipeline spans multiple lines of code.
-}
spanMultipleLines : Predicate
spanMultipleLines =
    Predicate <|
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
haveMoreStepsThan : Int -> Predicate
haveMoreStepsThan i =
    Predicate <| \{ steps } -> List.length steps > i + 1


{-| Checks whether the length of a pipeline is less than a specified number.
Note that the length of a pipeline is the number of operators in it, e.g.

    foo
        |> bar
        |> baz

has length **2** for the purposes of this predicate.

-}
haveFewerStepsThan : Int -> Predicate
haveFewerStepsThan i =
    Predicate <| \{ steps } -> List.length steps < i + 1


{-| Create a `Predicate` that matches pipelines that match both of two
predicates.
-}
and : Predicate -> Predicate -> Predicate
and (Predicate p1) (Predicate p2) =
    Predicate <| \p -> p1 p && p2 p


{-| Create a `Predicate` that matches pipelines that match either or both of two
predicates.
-}
or : Predicate -> Predicate -> Predicate
or (Predicate p1) (Predicate p2) =
    Predicate <| \p -> p1 p || p2 p


{-| Negate a `Predicate`.
-}
doNot : Predicate -> Predicate
doNot (Predicate p) =
    Predicate <| (not << p)


{-| Convert a single `PipelineRule`, as passed to the configuration, into a
`Filter` that is actually useful for generating errors.
-}
ruleToFilter : PipelineRule r -> Filter
ruleToFilter (PipelineRule { forbidden, except, operator, error }) pipeline =
    let
        matchesPredicate : Predicate -> Bool
        matchesPredicate (Predicate p) =
            p pipeline
    in
    if
        (operator == pipeline.operator)
            && MaybeX.unwrap True matchesPredicate forbidden
            && not (MaybeX.unwrap False matchesPredicate except)
    then
        Just <| MaybeX.unwrap (makeError pipeline defaultError) (makeError pipeline) error

    else
        Nothing


{-| Convenience alias for the configuration.
-}
type alias Filter =
    Pipeline -> Maybe (List (Error {}))


{-| Determine whether the pipeline has a simple input or not. This is somewhat
subjective, of course, so use [`haveAnInputOf`](#haveAnInputOf) if you want to customize its
behavior. A pipeline is considered to have a simple input if its input is **40
characters or less**, is only a **single line**, and also:

Is one of the following:

    -- Unit
    ()
        |> foo

    -- Name
    a
        |> foo

    -- Prefix operator
    (+)
        |> foo

    -- Int literal
    1
        |> foo

    -- Hex literal
    0x0F
        |> foo

    -- Float literal
    1.5
        |> foo

    -- String literal
    "bar"
        |> foo

    -- Char literal
    'c'
        |> foo

    -- Record access function
    .field
        |> foo

or is one of the following where all subexpressions are simple:

    -- Tuple
    ( a, "b" )
        |> foo

    -- Record
    { a = "value" }
        |> foo

    -- List
    []
        |> foo

    -- Record access
    a.field
        |> foo

    -- Negation
    elmFormatWontLetThisBeAnExample
        |> foo

    -- Parentheses
    elmFormatWontLetThisBeAnExample
        |> foo

-}
haveASimpleInput : Predicate
haveASimpleInput =
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

                        Negation e_ ->
                            go e_

                        ParenthesizedExpression e_ ->
                            go e_

                        RecordAccess e_ _ ->
                            go e_

                        RecordUpdateExpression _ _ ->
                            False

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
                   )
    in
    Predicate <|
        \{ steps } ->
            List.head steps
                |> Maybe.map go
                |> Maybe.withDefault False


{-| Like [`haveASimpleInput`](#haveASimpleInput) but with a user-providable
function to check if an expression is simple.
-}
haveAnInputOf : (Node Expression -> Bool) -> Predicate
haveAnInputOf pred =
    Predicate <|
        \{ steps } ->
            List.head steps
                |> Maybe.map pred
                |> Maybe.withDefault False


{-| Visit function TLDs and pass their expression to `expressionVisitor`.
-}
declarationVisitor : List Filter -> Node Declaration -> List (Error {})
declarationVisitor filters d =
    case Node.value d of
        Declaration.FunctionDeclaration { declaration } ->
            Node.value declaration
                |> .expression
                |> expressionVisitor filters

        _ ->
            -- No pipelines in any of:
            -- AliasDeclaration TypeAlias
            -- CustomTypeDeclaration Type
            -- PortDeclaration Signature
            -- InfixDeclaration Infix
            -- Destructuring (Node Pattern) (Node Expression)
            []


{-| Visit the TLD of an expression and descend until a pipeline is found, at
which point check it for errors.
-}
expressionVisitor : List Filter -> Node Expression -> List (Error {})
expressionVisitor filters node =
    let
        go : Node Expression -> List (Error {})
        go =
            expressionVisitor filters
    in
    case Node.value node of
        OperatorApplication op dir left right ->
            case getPipeline node op dir left right of
                Just p ->
                    MaybeX.oneOf filters p
                        |> Maybe.withDefault []

                Nothing ->
                    -- If it's not the start of a pipeline, just descend
                    go left ++ go right

        Application es ->
            -- Application might be the start of a parenthetical application pipeline
            case getParentheticalPipeline node of
                Just p ->
                    MaybeX.oneOf filters p
                        |> Maybe.withDefault []

                Nothing ->
                    -- If it's not the start of a pipeline, just descend
                    List.concatMap go es

        -- Descend into subexpression until we encounter a pipeline
        ParenthesizedExpression e ->
            go e

        Negation e ->
            go e

        CaseExpression { expression, cases } ->
            go expression ++ List.concatMap (go << Tuple.second) cases

        IfBlock predE thenE elseE ->
            List.concatMap go [ predE, thenE, elseE ]

        TupledExpression es ->
            List.concatMap go es

        LetExpression { declarations, expression } ->
            let
                goLetDecl : Node LetDeclaration -> List (Error {})
                goLetDecl d =
                    case Node.value d of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression
                                |> go

                        LetDestructuring _ e ->
                            go e
            in
            go expression
                ++ List.concatMap goLetDecl declarations

        LambdaExpression { expression } ->
            go expression

        RecordExpr rs ->
            List.concatMap (go << Tuple.second << Node.value) rs

        ListExpr es ->
            List.concatMap go es

        RecordAccess e _ ->
            go e

        RecordUpdateExpression _ rs ->
            List.concatMap (go << Tuple.second << Node.value) rs

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


{-| Get a pipeline from an operator application or fail if it's not a pipeline.
-}
getPipeline : Node Expression -> String -> InfixDirection -> Node Expression -> Node Expression -> Maybe Pipeline
getPipeline node op dir left right =
    let
        go : Node Expression -> ( Node Expression, List (Node Expression) )
        go e =
            case Node.value e of
                OperatorApplication op_ dir_ left_ right_ ->
                    case ( op == op_ && dir == dir_, dir_ ) of
                        ( True, Left ) ->
                            go left_
                                |> Tuple.mapSecond ((::) right_)

                        ( True, Right ) ->
                            go right_
                                |> Tuple.mapSecond ((::) left_)

                        _ ->
                            -- Pipeline ended
                            ( e, [] )

                _ ->
                    -- Pipeline ended
                    ( e, [] )

        makePipeline : Operator -> List (Node Expression) -> Pipeline
        makePipeline operator steps =
            { operator = operator, steps = steps, node = node }
    in
    case ( op, dir ) of
        ( "|>", Left ) ->
            go left
                |> (\( input, steps ) ->
                        input
                            :: List.reverse (right :: steps)
                            |> makePipeline RightPizza
                   )
                |> Just

        ( "<|", Right ) ->
            go right
                |> (\( input, steps ) ->
                        input
                            :: List.reverse (left :: steps)
                            |> makePipeline LeftPizza
                   )
                |> Just

        ( ">>", Right ) ->
            go right
                |> (\( input, steps ) ->
                        input
                            :: List.reverse (left :: steps)
                            |> List.reverse
                            |> makePipeline RightComposition
                   )
                |> Just

        ( "<<", Left ) ->
            go left
                |> (\( input, steps ) ->
                        input
                            :: List.reverse (right :: steps)
                            |> List.reverse
                            |> makePipeline LeftComposition
                   )
                |> Just

        _ ->
            Nothing


getParentheticalPipeline : Node Expression -> Maybe Pipeline
getParentheticalPipeline node =
    let
        go : Node Expression -> ( Node Expression, List (Node Expression) )
        go e =
            case Node.value e of
                Application es ->
                    case Maybe.map (Tuple.mapFirst Node.value) <| ListX.unconsLast es of
                        Just ( ParenthesizedExpression e_, es_ ) ->
                            makeAppNode es_
                                |> Maybe.map (\step -> Tuple.mapSecond ((::) step) <| go e_)
                                -- Pipeline ended
                                |> Maybe.withDefault ( e, [] )

                        _ ->
                            -- Pipeline ended
                            ( e, [] )

                _ ->
                    -- Pipeline ended
                    ( e, [] )

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
                |> (\allSteps -> { operator = ParentheticalApplication, steps = allSteps, node = node })
                |> Just


{-| Given a `Pipeline` and a `PipelineError`, create an actual `elm-review`
error.
-}
makeError : Pipeline -> PipelineError -> List (Error {})
makeError { node } (Fail err) =
    [ Rule.error err (Node.range node) ]


{-| The error that is reported for invalid pipelines if none is provided.
-}
defaultError : PipelineError
defaultError =
    Fail
        { message = "Forbidden pipeline style"
        , details =
            [ "This pipeline is stylistically-invalid by one of the rules specified in your elm-review config."
            , "This is the default error message, so if you're unsure why you're seeing it, you should really use ReviewPipelineStyles.byReportingError to provide a more descriptive one!"
            ]
        }
