{-# LANGUAGE OverloadedStrings #-}
module Messanger.Models ( Message(..), DCRecord(..), Username) where

import Prelude hiding (lookup)

import Data.Maybe
import Hails.Database.MongoDB
import Hails.Database.MongoDB.Structured
import LIO.Data.Time

type Username = String

data Message = Message
  { messageId         :: Maybe ObjectId
  , messageBody       :: String
  , messageFrom       :: Username
  , messageTo         :: Username
  , messageTime       :: Int
  } deriving Show

instance DCRecord Message where
  fromDocument doc = do
    let pid = lookup "_id" doc
    body <- lookup "body" doc
    from <- lookup "from" doc
    to <- lookup "to" doc
    mTime <- lookup "time" doc
    time <- maybeRead mTime
    return Message { messageId = pid
                   , messageBody = body
                   , messageFrom = from
                   , messageTo = to
                   , messageTime = time }

  toDocument message = 
    let mpid = messageId message
        pidField = if isJust mpid
                     then [ "_id" =: fromJust mpid ]
                     else []
    in  pidField ++
        [ "body" =: messageBody message
        , "from" =: messageFrom message
        , "to" =: messageTo message
        , "time" =: messageTime message ]

  collectionName _ = "messages"


-- Util function
maybeRead :: (Monad m, Read a) => String -> m a
maybeRead s = let x = fmap fst . listToMaybe . reads $ s
              in maybe (fail "Cannot read") return x
