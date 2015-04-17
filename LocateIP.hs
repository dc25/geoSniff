module LocateIP (
    Location (..),
    IPLookupResults (..),
    getIPLocation
) where

import Data.List.Split
import qualified Data.Map as M
import Network.Info
import Network.HTTP

data Location = Location Double Double deriving (Show)

data IPLookupResults = IPLookupResults {
                           location :: Location,
                           lookupFunction :: IPv4 -> IO IPLookupResults
                       }

getIPLocationOverInternet :: IPv4 -> IO Location
getIPLocationOverInternet ip = do
    rsp <- simpleHTTP (getRequest ("http://freegeoip.net/csv/" ++ show ip ))
    s <- fmap (take 1000) (getResponseBody rsp)
    let sValues = splitOn "," s
    return $ Location (read (sValues !! 8)) (read (sValues !! 9))

getIPLocation:: IPv4 -> IO IPLookupResults
getIPLocation = getIPLocationMemoized M.empty where
    
    -- getIPLocationMemoized is memoized version of outer function.
    getIPLocationMemoized locationMap ip = 

        -- First, attempt lookup in previously saved results.
        case M.lookup ip locationMap of  

            -- If found, then return location and unchanged lookup function
            Just loc -> do
                -- Recreate this lookup function with same lookup map.
                let unchangedLookup = getIPLocationMemoized locationMap
                return $ IPLookupResults loc unchangedLookup

            -- If not found then do query over internet
            Nothing -> do 
                loc <- getIPLocationOverInternet ip

                -- add the newly found display data to the saved results.
                let newMemo = M.insert ip loc locationMap 

                -- Create new lookup function with modified lookup map
                let newLookup = getIPLocationMemoized newMemo
                return $ IPLookupResults loc newLookup


