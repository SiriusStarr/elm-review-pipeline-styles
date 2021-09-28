module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import ReviewPipelineStyles exposing (leftPizzaPipelines,exceptThoseThat, forbid, andReportCustomError)
import ReviewPipelineStyles.Predicates exposing (separateATestFromItsLambda)


config : List Rule
config =
    [ ReviewPipelineStyles.rule
        [  forbid leftPizzaPipelines
            |> exceptThoseThat separateATestFromItsLambda
            |> andReportCustomError "No left pizza!" [ "Left pizza <| pipelines have been forbidden, except in the \"canonical\" test usage." ]
        ]
    ]
