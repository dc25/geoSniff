import Haste
import Haste.Prim

import GeoSniff

-- javascript functionality
foreign import ccall consoleLog_ffi :: JSString -> IO ()

foreign import ccall setStartClientCallback_ffi :: Ptr (JSString -> IO ()) -> IO ()

main :: IO ()
main = setStartClientCallback_ffi(toPtr startClient)

startClient :: JSString -> IO ()
startClient serverName = launchApp ("ws://" ++ fromJSStr serverName) 0
