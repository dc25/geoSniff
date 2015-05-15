{-# LANGUAGE OverloadedStrings #-}
module LocateIP (
    Location (..),
    IPLookupResults (..),
    getIPLocation
) where

import Data.Aeson
import Control.Applicative
import Network.Info
import Network.HTTP.Conduit

import qualified Data.Map as M

-- | Type of each JSON entry in record syntax.
data Location =
  Location { latitude  :: Double,
             longitude :: Double
            } deriving (Show)

instance FromJSON Location where
  parseJSON (Object p) = Location <$>
                          ((p .: "location") >>= (.: "latitude")) <*>
                          ((p .: "location") >>= (.: "longitude")) 

getIPLocationOverInternet :: IPv4 -> IO (Maybe Location)
getIPLocationOverInternet ip = 
    let jsonURL = "http://geoip.nekudo.com/api/" ++ show ip
    in (decode <$> simpleHttp jsonURL) :: IO (Maybe Location)

data IPLookupResults = IPLookupResults {
                           location :: Maybe Location,
                           lookupFunction :: IPv4 -> IO IPLookupResults
                       }

getIPLocationMemoized:: M.Map IPv4 (Maybe Location) -> IPv4 -> IO IPLookupResults
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


getIPLocation:: IPv4 -> IO IPLookupResults
getIPLocation = getIPLocationMemoized M.empty 
    
