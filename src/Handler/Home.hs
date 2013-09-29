module Handler.Home where

import Import
import Yesod
import Control.Concurrent.Chan (Chan, writeChan)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..))


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
    name <- runInputGet $ ireq textField "name"
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

getReceiveR :: Handler Html
getReceiveR = undefined

postSendR :: Handler ()
postSendR = do
    from <- getUserName
    body <- runInputGet $ ireq textField "message"
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing 
           $ return $ fromText from `mappend` fromText ": " `mappend` fromText body
  