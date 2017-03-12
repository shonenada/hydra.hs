module Hydra.TestImport
    ( module Hydra.TestImport
    , module X
    ) where

import Hydra.Application           (makeFoundation, makeLogWare)
#if MIN_VERSION_classy_prelude(1, 0, 0)
import ClassyPrelude         as X hiding (Handler)
#else
import ClassyPrelude         as X
#endif
import Hydra.Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Test            as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)
