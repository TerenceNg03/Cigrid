module Diagnose (reportErr) where

import Data.Text (Text)
import Error.Diagnose (Marker (This), Position, Report (Err))

reportErr :: Position -> Text -> Text -> Report Text
reportErr p msg hint =
    Err
        Nothing
        msg
        [(p, This hint)]
        []
