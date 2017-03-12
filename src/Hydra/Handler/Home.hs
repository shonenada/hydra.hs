module Hydra.Handler.Home where

import Hydra.Import

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hail Hydra!|]
