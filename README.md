# elm-review-pipeline-styles

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to forbid pipelines for code-style reasons.

## Provided rules

* [🔧 `ReviewPipelineStyles`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.0.0/ReviewPipelineStyles) - Reports pipelines that are not valid by user-defined rules.

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
[`ReviewPipelineStyles.Premade`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.0.0/ReviewPipelineStyles-Premade)
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
