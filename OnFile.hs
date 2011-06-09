{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import Control.Monad (when)
import Data.Function (on)
import Data.List (nub, unionBy,foldl')
import Time (ClockTime)
import System (system)
import System.IO.Error
import System.Environment (getArgs, getProgName)
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

    exec = putStrLn "" >> putStrLn (replicate 80 '=') >> putStrLn "" >>
           system cmd >> return ()

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo a = FileInfo a <$> getModificationTime a

readAllFileInfos :: Bool -> [String] -> IO [FileInfo]
readAllFileInfos showHidden = foldl' f [] <.> mapM (readFileInfos showHidden)
  where f = unionBy ((==) `on` filePath)

readFileInfos :: Bool -> String -> IO [FileInfo]
readFileInfos showHidden pat = namesMatching pat >>= collect >>= 
                               mapM mkFileInfo . nub
  where
    collect = concat <.> mapM collectForPath
    collectForPath p = (p:) <$> ifM (doesDirectoryExist p) 
                                    (recurs p) 
                                    (return [])

    recurs a = collect . map (a</>) . filterHidden =<< getDirectoryContents a 

    filterDot = filter (`notElem` [".",".."])
    filterHidden = if showHidden then filterDot else filter ((/='.') . head)

ifM :: (Monad m) => m Bool -> m b -> m b -> m b
ifM mb mt me = do b <- mb; if b then mt else me

infixl 8 <.> 
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(<.>) f m = fmap f . m

