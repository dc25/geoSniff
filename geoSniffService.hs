{-# LANGUAGE CPP #-}

import GeoSniff

-- | A utility for doing live display of tcp connections in the browser.
-- | Started out as a copy of the Haste.App chatbox demo program.

-- | Launch the application!
main :: IO ()
main = launchApp("ws://127.0.0.1:24601" )
