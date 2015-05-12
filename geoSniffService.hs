import System.Console.GetOpt
import System.Environment(getArgs)
import System.Exit
import Text.Read

import GeoSniff

--
-- See the haskell.org tutorial for explanation of 
-- pattern matching using field labels below.
-- https://www.haskell.org/tutorial/moretypes.html
--

-- | Launch the application!
main :: IO ()
main = do
    Options { optPort = bindPort } <- getOptions
    launchApp "" bindPort

-- Everything below here is for command line argument evaluation.

-- Thanks to Leif Frenzel and Tomasz Zielonka for outlining
-- the command line processing techniques used here.
-- Original material at:
-- http://leiffrenzel.de/papers/commandline-options-in-haskell.html
-- https://mail.haskell.org/pipermail/haskell/2004-January/013412.html


getOptions :: IO Options 
getOptions = do
    args <- getArgs
    let ( actions, _, _ ) = getOpt RequireOrder options args 
    foldl (>>=) (return defaultOptions) actions

data Options = Options {
    optPort :: Int
}

defaultOptions :: Options
defaultOptions = Options {
    optPort = 24601
}

options :: [OptDescr (Options->IO Options)]
options = [
    Option "p" ["port"]  (ReqArg setOptPort "NUMBER") "set the number of port to bind to"
    ]

-- See the language report for explanation of the use of field labels below.
-- https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15

-- Update options to contain the port to bind to.
setOptPort :: String -> Options -> IO Options
setOptPort cmdLineArg opt = 
    case readMaybe cmdLineArg  of
         Just x -> return opt { optPort = x}
         Nothing -> do
             putStrLn ("Invalid value for port:" ++ cmdLineArg)
             exitFailure

