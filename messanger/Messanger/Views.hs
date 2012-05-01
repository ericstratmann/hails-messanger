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
    title "Messages!"
  body content

newMessage :: Username -> Html
newMessage currentUser = do
  h1 $ "Send a private message"
  form ! action "/messages" ! method "POST" $ do
    input ! type_ "hidden" ! name "from" ! value (toValue currentUser)
    input ! type_ "hidden" ! name "time" ! value "1234567890"
    div $ do
      label ! for "to" $ "To:"
      input ! type_ "text" ! name "to" ! id "to"
    div $ do
      textarea ! name "body" $ ""
    p $ input ! type_ "submit" ! value "Send message"

indexMessages :: [Message] -> Html
indexMessages messages = do
  h1 $ "List of messages"
  ol $ do
    forM_ messages $ \message -> do
      li $
        a ! href (toValue $ "/messages/" ++ (show $ fromJust $ messageId message))
          $ toHtml $ ("Placeholder" :: String)

showMessage :: Message -> Html
showMessage message = do
  h1 $ "Message"
  p $ do
    "From: "
    toHtml $ messageFrom message
  p $ do
    "To: "
    toHtml $ messageTo message
  p $ toHtml $ messageBody message
