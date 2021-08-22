# elm-review-pipeline-styles

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`ReviewPipelineStyles`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-pipeline-styles/1.0.0/ReviewPipelineStyles) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import ReviewPipelineStyles
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ReviewPipelineStyles.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-pipeline-styles/example
```
