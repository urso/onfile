{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import Control.Monad (when)
import Data.Function (on)
import Data.List (nub, unionBy,foldl')
import Time (ClockTime)
import System (system)
import System.IO.Error
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (namesMatching)
import System.Directory (getDirectoryContents, getModificationTime, doesDirectoryExist)
import System.Posix (sleep)
import System.Posix.Signals (installHandler, sigINT, Handler(Default))

data FileInfo = FileInfo { filePath :: !FilePath
                         , fileClockTime :: !ClockTime }
  deriving(Show, Eq)

usage :: IO ()
usage = do 
  -- pn <- getProgName
  undefined

main :: IO ()
main = installHandler sigINT Default Nothing >> getArgs >>= runWithArgs

runWithArgs :: [[Char]] -> IO ()
runWithArgs [] = usage
runWithArgs [cmd] = runWithArgs [cmd, "."]
runWithArgs (cmd:files) = run True =<< listFiles
  where
    run runCmd fileInfos = do
      when runCmd exec
      wait
      newFileInfos <- try listFiles
      case newFileInfos of
        Left err -> if isDoesNotExistError err 
                      then run False fileInfos
                      else ioError err -- rethrow error
        Right lst -> run (lst /= fileInfos) lst

    listFiles = readAllFileInfos False files

    wait = sleep 2 >> return ()

    nl = putStrLn ""

    exec = do nl; putStrLn (replicate 80 '='); nl
              _ <- system cmd     -- just ignore exit code
              return ()

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo a = FileInfo a <$> getModificationTime a

readAllFileInfos :: Bool -> [String] -> IO [FileInfo]
readAllFileInfos showHidden = fmap (foldl' f []) . mapM (readFileInfos showHidden)
  where f = unionBy ((==) `on` filePath)

readFileInfos :: Bool -> String -> IO [FileInfo]
readFileInfos showHidden pat = namesMatching pat >>= collect >>= 
                               mapM mkFileInfo . nub
  where
    collect = fmap concat . mapM collectForPath
    collectForPath p = do exist <- doesDirectoryExist p
                          if exist then recurs p else return [p]

    recurs a = collect . map (a</>) . filterHidden =<< getDirectoryContents a 

    filterDot = filter (`notElem` [".",".."])
    filterHidden = if showHidden then filterDot else filter ((/='.') . head)

