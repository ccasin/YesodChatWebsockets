import Import
import Handler.Home

import Control.Concurrent.Chan (Chan, newChan)

mkYesodDispatch "Chat" resourcesChat
    
main :: IO ()
main = do
    chan <- newChan
    warpEnv (Chat chan)