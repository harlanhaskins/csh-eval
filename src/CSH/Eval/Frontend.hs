{-|
Module      : CSH.Eval.Frontend
Description : The top level routing of calls to the web viewer
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Defines the web application layer of Evals
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module CSH.Eval.Frontend (evalFrontend) where

import CSH.Eval.Routes (evalAPI)
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod

-- | datatype for Yesod
data EvalFrontend = EvalFrontend

-- Route definition for the Evaluations Database Website
-- This is where you add new HTML pages
-- The EvalAPI is defined as a subsite of this website, under the /api route.
mkYesod "EvalFrontend" [parseRoutes|
/                           HomeR GET
/evals/membership/overview  EvalsMembershipOverviewR GET
/api                        EvalSubsiteR WaiSubsite getEvalAPI
|]

-- | The basic layout for every CSH Eval page
evalLayout :: Widget -> Handler Html
evalLayout widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget $(luciusFile "frontend/static/csh-bootstrap/release/members.min.css")
    withUrlRenderer $(hamletFile "frontend/templates/base.hamlet")

instance Yesod EvalFrontend where
    defaultLayout = evalLayout

-- | Defines the WAI Application for the eval Yesod app
evalFrontend :: IO Application
evalFrontend = toWaiApp EvalFrontend

-- | A Yesod subsite for the evalAPI defined using servant in "CSH.Eval.Routes"
getEvalAPI :: EvalFrontend -> WaiSubsite
getEvalAPI _ = WaiSubsite evalAPI

-- | An example Html handler for the frontend. 
getHomeR :: Handler Html
getHomeR = evalLayout $(whamletFile "frontend/index.hamlet")

getEvalsMembershipOverviewR :: Handler Html
getEvalsMembershipOverviewR = defaultLayout $(whamletFile "frontend/evals/membership/overview.hamlet")
