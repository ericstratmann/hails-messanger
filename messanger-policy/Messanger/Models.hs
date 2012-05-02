{-# LANGUAGE OverloadedStrings #-}
module Messanger.Models ( User(..), Message(..), DCRecord(..), Username) where

import Prelude hiding (lookup)

import Data.Maybe
import Hails.Database.MongoDB
import Hails.Database.MongoDB.Structured

type Username = String

data User = User
  { userId :: Username
  , userBlacklist :: [Username]
  } deriving Show

instance DCRecord User where
  fromDocument doc = do
    pid <- lookup "_id" doc
    blacklist <- lookup "blacklist" doc
    return User { userId = pid
                , userBlacklist = blacklist }

  toDocument user = 
        [ "_id" =: userId user
        , "blacklist" =: userBlacklist user ]

  collectionName _ = "users"

data Message = Message
  { messageId         :: Maybe ObjectId
  , messageBody       :: String
  , messageFrom       :: Username
  , messageTo         :: Username
  , messageTime       :: String
  } deriving Show

instance DCRecord Message where
  fromDocument doc = do
    let pid = lookup "_id" doc
    body <- lookup "body" doc
    from <- lookup "from" doc
    to <- lookup "to" doc
    time <- lookup "time" doc
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
