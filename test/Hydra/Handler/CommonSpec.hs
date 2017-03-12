module Hydra.Handler.CommonSpec (spec) where

import Hydra.TestImport

spec :: Spec
spec = withApp $ do
    describe "robots.txt" $ do
        it "gives a 200" $ do
            get RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            get RobotsR
            bodyContains "User-agent: *"
    describe "favicon.ico" $ do
        it "gives a 200" $ do
            get FaviconR
            statusIs 200
