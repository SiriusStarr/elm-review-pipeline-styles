module ReviewPipelineStyles.Fixes exposing (PipelineFix)

{-| This module contains various `PipelineFix`s that can be used to fix failing
pipelines.

Note that all fixes will only run if it is **possible** to fix the pipeline that
way, i.e. that the fix will not generate invalid code.


### Types

These are exposed only for the sake of type annotations; you shouldn't need to
work with them directly.

@docs PipelineFix

-}

import Internal.Types as Types


{-| A means of fixing a pipeline, to (presumably) bring it stylistically inline
with what is desired.
-}
type alias PipelineFix pipelineType =
    Types.PipelineFix pipelineType
