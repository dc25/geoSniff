{-# LANGUAGE OverloadedStrings #-}
module LocateIP (
    Location (..),
    IPLookupResults (..),
    getIPLocation
) where

import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Network.Info
import Network.HTTP.Conduit

import qualified Data.Map as M

-- | Type of each JSON entry in record syntax.
data Location =
  Location { latitude  :: Double,
              longitude   :: Double
            } deriving (Show)

instance FromJSON Location where
  parseJSON (Object p) = Location <$>
                          ((p .: "location") >>= (.: "latitude")) <*>
                          ((p .: "location") >>= (.: "longitude")) 

data IPLookupResults = IPLookupResults {
                           location :: Maybe Location,
                           lookupFunction :: Network.Info.IPv4 -> IO IPLookupResults
                       }

getIPLocationOverInternet :: IPv4 -> IO (Maybe Location)
getIPLocationOverInternet ip = do
    let jsonURL = "http://geoip.nekudo.com/api" ++ show ip

    -- Get JSON data and decode it
    d <- (eitherDecode <$> simpleHttp jsonURL) :: IO (Either String Location)
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case d of
        Left err -> do 
                       putStrLn err
                       return Nothing
        Right ps -> return $ Just ps

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


