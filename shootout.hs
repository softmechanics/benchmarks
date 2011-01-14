#!/usr/bin/env runghc

import Text.Printf
import System.Process
import System.Posix.Process (forkProcess, executeFile, getProcessStatus)
import System.Posix.Signals
import System.IO
import System.IO.Unsafe
import System.Directory
import Control.Applicative ((<$>))
import Text.Regex.Posix
import Data.List
import Data.Maybe (fromJust)
import Data.Either (rights)
import Control.Exception
import Control.Concurrent

data BenchmarkResults = BenchmarkResults
     { testName :: String
     , testVersion :: String
     , httperfResults :: HttperfResults
     , rtsResults :: RtsResults
     }
  deriving (Show)

data HttperfResults = HttperfResults
     { httperfReport :: String
     , minResponseRate :: Double
     , maxResponseRate :: Double
     , avgResponseRate :: Double
     , totalErrors :: Int
     }
  deriving (Show)

data RtsResults = RtsResults
     { rtsReport :: String
     , bytesAllocated :: Integer
     , mutCpuSeconds :: Double
     , gcCpuSeconds :: Double
     }
  deriving (Show)

ghcCmd name = 
  "ghc --make -O3 -threaded -rtsopts -fforce-recomp " ++ name

exec name version = do
  putStrLn $ unwords $ exe : args
  setCurrentDirectory name
  executeFile version False args Nothing
  where exe = "./" ++ name
        args = words $ printf "+RTS -N3 -A4m -t%s.stats --machine-readable" version

parseHttperfResults :: String -> HttperfResults
parseHttperfResults report = HttperfResults report minRR maxRR avgRR errors
  where lines' = lines report 
        replyLine = head $ filter (isPrefixOf "Reply rate") lines'
        minRR = parseRR "min"
        maxRR = parseRR "max"
        avgRR = parseRR "avg"
        parseRR rr = read $ head $ groups replyLine $ rr ++ " ([0-9.]*)"
        errors = read $ (!! 2) $ words $ head $ filter (isPrefixOf "Errors: total") lines'

        groups src pat = 
          let (_,_,_,gs) = src =~ pat :: (String,String,String,[String])
          in gs

parseRtsResults :: String -> RtsResults
parseRtsResults report = RtsResults report bytes mut cpu
  where bytes = get "bytes allocated"
        mut = get "mutator_cpu_seconds"
        cpu = get "GC_cpu_seconds"
        get :: Read a => String -> a
        get = read . fromJust . flip lookup stats
        stats = read $ dropWhile (/= '\n') report :: [(String, String)]

benchmark :: String -> String -> IO (Either SomeException BenchmarkResults)
benchmark name version = do
  result <- go
  case result of 
       Left e -> print e
       _ -> return ()
  return result
  where go = try $ do
              printf "------------------------ %s (%s) --------------------------\n" name version

              let exeFile = "./" ++ name ++ "/" ++ version
                  rtsFile = exeFile ++ ".stats"

              -- build
              print $ ghcCmd exeFile 
              _ <- system $ ghcCmd exeFile 

              -- run
              pid <- forkProcess $ exec name version
              httperfOut <- readProcess (name ++ "/bench") [] []

              threadDelay 2000000 -- 2s

              -- stop
              signalProcess sigINT pid
              _ <- getProcessStatus True True pid

              rts <- readFile rtsFile 

              return $ BenchmarkResults name version
                         (parseHttperfResults httperfOut)
                         (parseRtsResults rts)

benchmarkResultsCSV :: [BenchmarkResults] -> [String]
benchmarkResultsCSV results = header : map go results
  where 
    header = 
      "Test Name, Test Version, Min Response Rate, Max Response Rate, " ++
      "Avg Response Rate, Errors, Bytes Allocated, Mut CPU Seconds, GC CPU Seconds"

    go (BenchmarkResults n v (HttperfResults _ c1 c2 c3 c4) (RtsResults _ c5 c6 c7)) = 
      printf "%s,%s,%f,%f,%f,%d,%d,%f,%f" n v c1 c2 c3 c4 c5 c6 c7

benchmarkAll :: [(String,String)] -> IO [BenchmarkResults]
benchmarkAll tests = rights <$> mapM (uncurry benchmark) tests

versions = ["snap","warp","yesod"]

testTypes = [
    "pong"
  , "bigtable"
  , "static-file"
  ]

tests = [ (name, version)
        | name <- testTypes
        , version <- versions
        ]


printTests = sequence_ $ zipWith go [0..] tests
  where go n (name,version) = printf "%2d: %s (%s)\n" (n::Int) name version

selectTests ['a':_] = tests
selectTests is = go is
  where go [] = []
        go (i:is) = tests !! read i : go is

menu = do
  printTests
  putStrLn "Enter tests to run by number, separated by spaces (or [a]ll)"
  input <- words <$> getLine
  return $ selectTests input

main = do
  tests' <- menu
  rs <- benchmarkAll tests'
  mapM_ putStrLn $ benchmarkResultsCSV rs

