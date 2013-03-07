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

projectForm :: Maybe (Text, Textarea, Textarea) -> Form (Text, Textarea, Textarea)
projectForm mv = renderBootstrap $ (,,)
                 <$> areq textField (fs MsgProjectName) (fst3 <$> mv)
                 <*> areq textareaField (fs MsgDescription) (snd3 <$> mv)
                 <*> areq textareaField (fs MsgStatus) (thd3 <$> mv)
