{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Messanger.Policy where

import Data.Maybe
import Hails.Policy
import Messanger.Models
import LIO.Data.Time

-- | Internal messanger policy. The type constructor should not be
-- exported to avoid leaking the privilege.
data MessangerPolicy = MessangerPolicy TCBPriv (Database DCLabel)
  deriving (Typeable)

instance DatabasePolicy MessangerPolicy where
  createDatabasePolicy conf p = do
    messageColl <- messagesCollection p
    userColl <- messagesCollection p
    db <- do
        labelDatabase conf collectionsLabel lpub
          >>= assocCollectionP p messageColl
        labelDatabase conf collectionsLabel lpub
          >>= assocCollectionP p userColl
    return $ MessangerPolicy p db
    where collectionsLabel = newDC (<>) (priv p)

  policyDB (MessangerPolicy _ db) = db

messagesCollection :: TCBPriv -> DC (Collection DCLabel)
messagesCollection p = collectionP p "messages" lpub colClearance $
  RawPolicy (labelForMessage . fromJust . fromDocument)
            [ ("_id",   SearchableField)
            , ("from", SearchableField)
            , ("to", SearchableField)
            ]
    where colClearance = newDC (priv p) (<>)
          labelForMessage message = 
            let r = messageTo message .\/. messageFrom message .\/. priv p
            in newDC r r

userCollection :: TCBPriv -> DC (Collection DCLabel)
userCollection p = collectionP p "users" lpub colClearance $
  RawPolicy (labelForUser . fromJust . fromDocument)
            [ ("_id",   SearchableField)
            ]
    where colClearance = newDC (priv p) (<>)
          labelForUser user = 
            let r = userId user .\/. priv p
            in newDC r r


-- | Policy handler
messangerDB :: DC MessangerPolicy
messangerDB = mkPolicy

fooToLabeled ldoc act = do
  (MessangerPolicy privs _) <- messangerDB
  gateToLabeled privs ldoc act

addDate :: DCLabeled (Document DCLabel) -> DC (DCLabeled (Document DCLabel))
addDate ldoc = do
  date <- getCurrentTime
  fooToLabeled ldoc $ \partial ->
    return $ ["time" =: show date] ++ partial
