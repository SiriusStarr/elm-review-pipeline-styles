# elm-review-pipeline-styles

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to forbid pipelines for code-style reasons.

## Provided rules

* [🔧 `ReviewPipelineStyles`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles/) - Reports pipelines that are not valid by user-defined rules.

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
[`ReviewPipelineStyles.Premade`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/)
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

## Changlelog

* `1.3.0`
  * 🚸 Improved [`noPipelinesWithSimpleInputs`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/#noPipelinesWithSimpleInputs).
    Now flags *all* unnecessary *left* inputs (since the operator does not
    improve clarity in any way), but only flags simple (non-semantic) right
    inputs.
  * 🚸 Improved [`noPipelinesWithConfusingNonCommutativeFunctions`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/#noPipelinesWithConfusingNonCommutativeFunctions).
    Now only flags *all* non-commutative functions in *right* pipelines, whereas
    only confusing *prefix operators* are flagged in *left*/parenthetical
    pipelines, as the other functions are in the correct argument order.
  * 🚩 Added new [`StepPredicate`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Predicates/#step-predicates):
    [`aConfusingNonCommutativePrefixOperator`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Predicates/#aConfusingNonCommutativePrefixOperator)
    This only flags non-commutative prefix operators rather than also flagging
    e.g. `compare`.
* `1.2.0`
  * ✨ Added new [`noMultilineLeftComposition`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/#noMultilineLeftComposition)
  and [`noSingleLineRightComposition`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/#noSingleLineRightComposition)
  premade rules.
  * 📝 Fix minor doc issues.
* `1.1.0`
  * Added premade rules for ease of use!  Find them in the
    [`ReviewPipelineStyles.Premade`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Premade/)
    module.  The documentation for these rules includes the source for how to construct them as a sort of tutorial to learn to create your own.
  * Added [`StepPredicate`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.3.0/ReviewPipelineStyles-Predicates/#step-predicates)s.  Easily inspect
    individual steps of pipelines!
  * Updated `elm-syntax` to avoid mangling fixes involving some lambdas.
  * Minor documentation fixes/clarifications.
* `1.0.0` -- Initial release
