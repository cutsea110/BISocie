module BISocie.Helpers.Form where

import Import
import BISocie.Helpers.Util

fs :: RenderMessage m msg => msg -> FieldSettings m
fs m = FieldSettings { fsLabel = SomeMessage m
                     , fsTooltip = Nothing
                     , fsId = Nothing
                     , fsName = Nothing
                     , fsAttrs = []
                     }

fs' :: RenderMessage m msg => msg -> [(Text, Text)] -> FieldSettings m
fs' m attrs = FieldSettings { fsLabel = SomeMessage m
                            , fsTooltip = Nothing
                            , fsId = Nothing
                            , fsName = Nothing
                            , fsAttrs = attrs
                            }

projectForm :: Maybe (Text, Textarea, Textarea) -> Form (Text, Textarea, Textarea)
projectForm mv = renderBootstrap2 $ (,,)
                 <$> areq textField (fs MsgProjectName) (fst3 <$> mv)
                 <*> areq textareaField (fs MsgDescription) (snd3 <$> mv)
                 <*> areq textareaField (fs MsgStatus) (thd3 <$> mv)
