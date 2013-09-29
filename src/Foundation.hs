module Foundation where

import Yesod
import Yesod.Default.Util
import Data.Default (def)
import Yesod.Form.Jquery

import Control.Concurrent.Chan (Chan)
import Network.Wai.EventSource (ServerEvent)

import Data.Text

data Chat = Chat (Chan ServerEvent)

instance Yesod Chat
instance YesodJquery Chat where
    urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
instance RenderMessage Chat FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "Chat" $(parseRoutesFile "config/routes")