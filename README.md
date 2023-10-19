# elm-review-pipeline-styles

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to forbid pipelines for code-style reasons.

## Provided rules

* [🔧 `ReviewPipelineStyles`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles/) - Reports pipelines that are not valid by user-defined rules.

For example, the usage of `<|` or the usage of excessively-long `|>` pipelines.

This rule works with the following pipeline types:

* `|>` -- Right "pizza" pipelines, i.e. right function application, so-called
  because the operator resembles a slice of pizza 🍕.
* `<|` -- Left "pizza" pipelines, i.e. left function application, so-called
  because the operator resembles a slice of pizza 🍕.
* `>>` -- Right composition pipelines.
* `<<` -- Left composition pipelines.
* `foo (bar (baz (i (j k))))` -- Parenthetical application pipelines.

If you don't know where to start, be sure to check out
[`ReviewPipelineStyles.Premade`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/)
for some ready-made rules, as well as examples of how to construct them.

## Configuration

```elm
import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Premade
    exposing
        ( noMultilineLeftPizza
        , noPipelinesWithConfusingNonCommutativeFunctions
        , noPipelinesWithSimpleInputs
        , noRepeatedParentheticalApplication
        , noSemanticallyInfixFunctionsInLeftPipelines
        , noSingleLineRightPizza
        )


config : List Rule
config =
    [ ReviewPipelineStyles.rule <|
        List.concat
            [ noMultilineLeftPizza
            , noSingleLineRightPizza
            , noPipelinesWithSimpleInputs
            , noRepeatedParentheticalApplication
            , noPipelinesWithConfusingNonCommutativeFunctions
            , noSemanticallyInfixFunctionsInLeftPipelines
            ]
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-pipeline-styles/example
```

## Changelog

* `1.3.7`
  * 🐛 No longer report invalid parenthetical pipelines.  Previously:

    ```elm
    foo
        (a (b c))
        (d (e f))
    ```

    would treat

    ```elm
    foo
        (a (b c))
    ```

    as a pipeline, instead of the correct `a (b c)` (since only the terminal
    argument can be pipelined).
  * ⚡️ Improve performance by skipping some unnecessary recursion.  (`Maybe.withDefault`'s lack of laziness strikes again.)
  * Bump `elm-review` to v2.13.1 and `elm-syntax` to v7.3.2.
* `1.3.6` -- Bump `elm-review` to v2.12.2 for upstream bugfix and improve
  documentation for adding fixes to rules.
* `1.3.5` -- Bump `elm-review` to v2.12.1 for upstream bugfix.
* `1.3.4` -- Bump `elm-review` to v2.11.1 and mark rule as providing fixes, if
  it does.
* `1.3.3`
  * 🐛 `aConfusingNonCommutativeFunction` no longer flags fully-saturated
    functions, since they are not actually confusing.  For example:

    ```elm
    Set.diff a b -- Passes
        |> Set.toList
    ```

    is now allowed, while

    ```elm
    b
        |> Set.diff a -- Fails
        |> Set.toList
    ```

    is not.  This was not the behavior previously, when both would have been
    flagged as confusing.

    Non-commutative operator behavior is unchanged, i.e. they are still not
    allowed in prefix form even when fully-saturated, as they remain confusing.
    For example:

    ```elm
    xs ++ ys -- Passes
        |> List.map foo
    ```

    is still allowed, while

    ```elm
    (++) xs ys -- Fails
        |> List.map foo
    ```

    is still not.
* `1.3.2` -- Improve handling of nested pipelines.
  * 🐛 Do not allow fixes that would lead to compilation errors by converting
    a parenthetical pipeline that is immediately nested within another pipeline
    to another type.
  * 🚸 `noRepeatedParentheticalApplication` premade rule will not flag
    parenthetical application that is immediately nested within another
    pipeline, as it cannot be readily rewritten as another type.  This often
    occurs with nested `map`s, for example:

    ```elm
    foo
        |> Maybe.map (Result.map (yi << er))
        |> bar
    ```

* `1.3.1` -- 🐛 Fix `aSemanticallyInfixFunction`.  Previously, it did not check
  how many args were applied, leading to it flagging things as infix that
  weren't yet infix, because no args had been applied.  Now it requires exactly
  one arg to be applied.
* `1.3.0`
  * 🚸 Improved [`noPipelinesWithSimpleInputs`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/#noPipelinesWithSimpleInputs).
    Now flags *all* unnecessary *left* inputs (since the operator does not
    improve clarity in any way), but only flags simple (non-semantic) right
    inputs.
  * 🚸 Improved [`noPipelinesWithConfusingNonCommutativeFunctions`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/#noPipelinesWithConfusingNonCommutativeFunctions).
    Now only flags *all* non-commutative functions in *right* pipelines, whereas
    only confusing *prefix operators* are flagged in *left*/parenthetical
    pipelines, as the other functions are in the correct argument order.
  * 🚩 Added new [`StepPredicate`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Predicates/#step-predicates):
    [`aConfusingNonCommutativePrefixOperator`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Predicates/#aConfusingNonCommutativePrefixOperator)
    This only flags non-commutative prefix operators rather than also flagging
    e.g. `compare`.
* `1.2.0`
  * ✨ Added new [`noMultilineLeftComposition`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/#noMultilineLeftComposition)
  and [`noSingleLineRightComposition`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/#noSingleLineRightComposition)
  premade rules.
  * 📝 Fix minor doc issues.
* `1.1.0`
  * Added premade rules for ease of use!  Find them in the
    [`ReviewPipelineStyles.Premade`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Premade/)
    module.  The documentation for these rules includes the source for how to construct them as a sort of tutorial to learn to create your own.
  * Added [`StepPredicate`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.7/ReviewPipelineStyles-Predicates/#step-predicates)s.  Easily inspect
    individual steps of pipelines!
  * Updated `elm-syntax` to avoid mangling fixes involving some lambdas.
  * Minor documentation fixes/clarifications.
* `1.0.0` -- Initial release
