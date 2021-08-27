module Internal.Types exposing (NestedWithin(..), Operator(..), Pipeline, Predicate(..))

{-| Internal types, not to have their details exposed.
-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


{-| A predicate for filtering pipelines, or a logical combination of them.
-}
type Predicate
    = Predicate (ModuleNameLookupTable -> Pipeline -> Bool)


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
  - `parents`: A hierarchy of pipelines that this pipeline is nested within
    (start of the list being the immediate parent).

-}
type alias Pipeline =
    { operator : Operator
    , steps : List (Node Expression)
    , node : Node Expression
    , parents : List ( Operator, NestedWithin )
    }


{-| The degree to which a parent or child is removed from a pipeline.

  - `ALambdaFunction` -- The pipeline is within a lambda function within the other.
  - `AFlowControlStructure` -- The pipeline is within an `if` or `case` block within
    the other.
  - `ADataStructure` -- The pipeline is within a tuple, list, or record
    within the other (note that this includes record updates).
  - `ALetBlock` -- The pipeline is within a `let` block within the other.

-}
type NestedWithin
    = NestedWithin
        { aLambdaFunction : Bool
        , aFlowControlStructure : Bool
        , aDataStructure : Bool
        , aLetBlock : Bool
        }


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
