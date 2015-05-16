{-# LANGUAGE OverloadedStrings #-}
module LocateIP (
    Location (..),
    IPLookupResults (..),
    IPLocationMap,
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

type IPLocationMap = M.Map IPv4 (Maybe Location) 

data IPLookupResults = IPLookupResults {
                           location :: Maybe Location,
                           locationMap :: IPLocationMap
                       }

getIPLocation :: IPLocationMap -> IPv4 -> IO IPLookupResults
getIPLocation lm ip = 

    -- First, attempt lookup in previously saved results.
    case M.lookup ip lm of  

        Just loc -> return $ IPLookupResults loc lm

        -- If not found then do query over internet
        Nothing -> do 
            loc <- getIPLocationOverInternet ip
            return $ IPLookupResults loc $ M.insert ip loc lm 

    
