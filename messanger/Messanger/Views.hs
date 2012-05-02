{-# LANGUAGE OverloadedStrings #-}
module Messanger.Views where

import Prelude hiding (div, span, head, id)

import Messanger.Models

import Control.Monad
import Data.Maybe
import Data.IterIO.Http.Support
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)
import Text.Blaze.Renderer.Utf8

respondHtml content = render "text/html" $ renderHtml $ docTypeHtml $ do
  head $ do
      stylesheet "/static/bootstrap.css"
  body content


stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)


-- Message Views

newMessage :: Username -> Html
newMessage currentUser = do
  title $ toHtml currentUser
  h1 $ "Send a private message"
  form ! action "/messages" ! method "POST" $ do
    input ! type_ "hidden" ! name "from" ! value (toValue currentUser)
    div $ do
      label ! for "to" $ "To:"
      input ! type_ "text" ! name "to" ! id "to"
    div $ do
      label ! for "body" $ "Message:"
      textarea ! name "body" $ ""
    p $ input ! type_ "submit" ! value "Send message"

indexMessages :: [Message] -> Html
indexMessages messages = do
  h1 $ "List of messages"
  table $ do
    tr $ do
      th $ ""
      th $ "From"
      th $ "To"
      th $ "Time"
    forM_ messages $ \message -> do
      tr $ do
        td $
            a ! href (toValue $ "/messages/" ++ (show $ fromJust $ messageId message))
              $ "Read"
        td $ toHtml $ messageFrom message
        td $ toHtml $ messageTo message
        td $
          toHtml $ messageTime message

showMessage :: Message -> Html
showMessage message = do
  h1 $ "Message"
  table $ do
    tr $ do
      td $ "From"
      td $ toHtml $ messageFrom message
    tr $ do
      td $ "To"
      td $ toHtml $ messageTo message
    tr $ do
      td $ "Message"
      td $ toHtml $ messageBody message


-- 
