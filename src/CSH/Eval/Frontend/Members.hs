{-
Module      : CSH.Eval.Frontend.Members
Description : The route handler for the members page
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

DOCUMENT THIS!
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Members (
    getMembersR
  , getMemberR
  ) where

import qualified Data.ByteString.Char8 as B
import CSH.Eval.Cacheable.Fetch
import CSH.Eval.Model
import CSH.Eval.Frontend.Data
import qualified Data.Text as T
import System.Log.Logger
import Yesod hiding (Active)
import Network.HTTP.Types
import Data.Either.Combinators

hasActiveMembership :: [Membership] -> Bool
hasActiveMembership [] = False
hasActiveMembership ((Membership Active _ Nothing _):_) = True
hasActiveMembership (_:xs) = hasActiveMembership xs

-- | The handler for the members listing page
getMembersR :: Handler Html
getMembersR = do
    cache <- getCache <$> getYesod
    eitherMembers <- execCacheable cache getMembers
    case eitherMembers of
        (Left err) -> sendResponseStatus internalServerError500 ("Could not Load members " ++ (show err))
        (Right memberList) -> do
            memberships <- mapM (\m -> execCacheable cache (getMemberMemberships (memberID m))) memberList
            let members = filter (hasActiveMembership . fromRight [] . snd) $ zip memberList memberships
            defaultLayout $(whamletFile "frontend/templates/members/index.hamlet")

widgetFor :: Bool -> Widget
widgetFor True = [whamlet|
    <span .glyphicon .glyphicon-ok>
|]
widgetFor False = [whamlet|
    <span .glyphicon .glyphicon-remove>
|]

-- | The handler for a single member's page
getMemberR :: String -> Handler Html
getMemberR username = do
           y <- getYesod
           let cache = getCache y
           let logger = getFrontendLogger y
           eitherUsr <- execCacheable cache (getMemberUsername (T.pack username))
           let attendance = [("Evals", "Committee", "10/13/2015"), ("Financial", "Committee", "10/13/2015")]
           case eitherUsr of
            (Left _) -> sendResponseStatus internalServerError500 ("Could not find " ++ username)
            (Right usr) -> defaultLayout $(whamletFile "frontend/templates/index.hamlet")

widgetEval :: Evaluation -> Widget
widgetEval eval = do
    y <- getYesod
    $(whamletFile "frontend/templates/member/widgets/evaluation.hamlet")
