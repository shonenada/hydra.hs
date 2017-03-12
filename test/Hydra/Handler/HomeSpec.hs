module Hydra.Handler.HomeSpec (spec) where

import Hydra.TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
          get HomeR
          statusIs 200
          htmlAnyContain "" "Hydra"
