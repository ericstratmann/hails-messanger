{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Messanger.Server where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Data.IterIO.Http.Support hiding (Action)
import Hails.App
import Hails.Database
import Hails.Database.MongoDB hiding (reverse)
import Hails.Database.MongoDB.Structured
import Messanger.Views
import Messanger.Policy
--import LIO.TCB

server :: AppReqHandler
server = runAction $ runActionRoute $ mconcat [
    routeRestController "messages" MessagesController
  ]

data MessagesController = MessagesController

instance RestController t (DCLabeled L8.ByteString) DC MessagesController where
  restNew _ = do
    currentUser <- getHailsUser
    respondHtml $ newMessage currentUser
  
  restCreate _ = do
    policy <- liftLIO messangerDB
    req  <- getHttpReq
    body <- getBody
    ldoc <- liftLIO $ labeledDocI req body
--    liftLIO $ do
--      doc <- unlabel ldoc
--      ioTCB $ Prelude.putStrLn $ show doc
    liftLIO $ withDB policy $ do
      insert "messages" ldoc
    redirectTo "/messages"
  
  restShow _ pid = do
    policy <- liftLIO messangerDB
    message <- liftLIO $ findBy policy "messages" "_id" (read $ L8.unpack pid :: ObjectId)
    case message of
      Just message -> respondHtml $ showMessage message
      Nothing -> respond404

  restIndex _ = do
   policy <- liftLIO messangerDB
   messages <- liftLIO $ findAll policy $ select [] "messages"
   respondHtml $ indexMessages messages



-- Hm, thought this function was supposed to be in the source version of hails. Copied from blog tutorial - Eric
-- | Find all records that satisfy the query and can be read subject
-- to the current clearance.
findAll :: (DCRecord a, DatabasePolicy p) => p -> Query DCLabel -> DC ([a])
findAll policy query = do
  eRes <- withDB policy $ do
    find query >>= cursorToRecords []
  case eRes of
    Right res -> return res
    Left _ -> return []
  where cursorToRecords arr cur = do
          nc <- next cur
          case nc of
            Just ldoc -> do
              post <- liftLIO $ do
                clearance <- getClearance
                if labelOf ldoc `canflowto` clearance then
                  unlabel ldoc >>= return . fromDocument
                  else return Nothing
              let resarr = maybe arr (:arr) post
              cursorToRecords resarr cur
            Nothing -> return $ reverse arr
