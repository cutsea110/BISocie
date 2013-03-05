module BISocie.Helpers.Form where

import Import
import BISocie.Helpers.Util

fs :: RenderMessage m msg => msg -> FieldSettings m
fs msg = FieldSettings { fsLabel = SomeMessage msg
                       , fsTooltip = Nothing
                       , fsId = Nothing
                       , fsName = Nothing
                       , fsAttrs = []
                       }

fs' :: RenderMessage m msg => msg -> [(Text, Text)] -> FieldSettings m
fs' msg attrs = FieldSettings { fsLabel = SomeMessage msg
                              , fsTooltip = Nothing
                              , fsId = Nothing
                              , fsName = Nothing
                              , fsAttrs = attrs
                              }
