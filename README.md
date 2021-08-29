# elm-review-pipeline-styles

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/)
rules to forbid pipelines for code-style reasons.

## Provided rules

* [`ReviewPipelineStyles`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.0.0/ReviewPipelineStyles) - Reports pipelines that are not valid by user-defined rules.

For example, the usage of `<|` or the usage of excessively-long `|>` pipelines.

This rule works with the following pipeline types:

* `|>`
* `<|`
* `>>`
* `<<`
* `foo (bar (baz (i (j k))))`

## Configuration

```elm
module ReviewConfig exposing (config)

import ReviewPipelineStyles
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ReviewPipelineStyles.rule
        [ forbid leftPizzaPipelines
            |> andCallThem "forbidden <| pipeline"
        , forbid rightPizzaPipelines
            |> that (haveMoreStepsThan 10)
            |> andCallThem "overly long |> pipeline"
        ]
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-pipeline-styles/example
```
