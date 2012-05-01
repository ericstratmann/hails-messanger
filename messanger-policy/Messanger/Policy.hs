{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Messanger.Policy where

import Data.Maybe
import Hails.Policy
import Messanger.Models

-- | Internal messanger policy. The type constructor should not be
-- exported to avoid leaking the privilege.
data MessangerPolicy = MessangerPolicy TCBPriv (Database DCLabel)
  deriving (Typeable)

instance DatabasePolicy MessangerPolicy where
  createDatabasePolicy conf p = do
    messageColl <- messagesCollection p
    db <- labelDatabase conf collectionsLabel lpub
          >>= assocCollectionP p messageColl
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

-- | Policy handler
messangerDB :: DC MessangerPolicy
messangerDB = mkPolicy
