module Handler.Home where

import Import
import Yesod
import Control.Concurrent.Chan (Chan, writeChan, dupChan)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Text.Julius


import Data.Monoid (mappend)
import Data.Text

getUserName :: Handler Text
getUserName = do
    mname <- lookupSession "name"
    case mname of
        Nothing -> redirect LoginR
        Just n  -> return n

getLoginR :: Handler Html
getLoginR = defaultLayout $ do
    setTitle "Login"
    $(widgetFile "login")


postLoginR :: Handler ()
postLoginR = do
    name <- runInputPost $ ireq textField "name"
    setSession "name" name
    redirect HomeR



getHomeR :: Handler Html
getHomeR = do
    name <- getUserName
    defaultLayout $ do
      setTitle "Chat Demo"
      getYesod >>= addScriptEither . urlJqueryJs
      addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
      $(widgetFile "home")



getReceiveR :: Handler ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    req <- waiRequest
    res <- liftResourceT $ eventSourceAppChan chan req
    sendWaiResponse res

postSendR :: Handler ()
postSendR = do
    from <- getUserName
    body <- runInputGet $ ireq textField "message"
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing 
           $ return $ fromText from `mappend` fromText ": " `mappend` fromText body



chatWidget :: Widget
chatWidget = do
    -- Get some unique identifiers to help in creating our HTML/CSS. Remember,
    -- we have no idea what the master site's HTML will look like, so we
    -- should not assume we can make up identifiers that won't be reused.
    -- Also, it's possible that multiple chatWidgets could be embedded in the
    -- same page.
    chat <- newIdent   -- the containing div
    output <- newIdent -- the box containing the messages
    input <- newIdent  -- input field from the user
    $(widgetFile "chat")
