module Main where

import Control.Applicative
import Control.Monad (when)
import Data.Function (on)
import Data.List (nub, unionBy,find,foldl')
import Data.Maybe (isJust)
import Time (ClockTime)
import System (system)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>))
import System.FilePath.Glob (namesMatching)
import System.Directory (getDirectoryContents, getModificationTime, doesFileExist, doesDirectoryExist)
import System.Posix (sleep)
import System.Posix.Signals (installHandler, sigINT, Handler(Default))

data FileInfo = FileInfo { filePath :: FilePath
                         , fileClockTime :: ClockTime }
  deriving(Show)

usage :: IO ()
usage = do 
  pn <- getProgName
  undefined

main :: IO ()
main = installHandler sigINT Default Nothing >> getArgs >>= runWithArgs

runWithArgs :: [String] -> IO ()
runWithArgs [] = usage
runWithArgs [cmd] = runWithArgs [cmd, "."]
runWithArgs (cmd:files) = run True =<< readFiles
  where 
    readFiles = readAllFileInfos False files

    wait = sleep 2 >> return ()

    exec = putStrLn "" >> putStrLn (replicate 80 '=') >> putStrLn "" >>
           system cmd >> return ()
  
    run runCmd fileInfos = do 
          when runCmd exec
          wait
          ifM (testModified fileInfos) (readFiles >>= run True) $
              ifM (testDeleted fileInfos) (readFiles >>= run True) $
                   do newInfos <- readFiles
                      run (hasNewFiles fileInfos newInfos) newInfos

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo a = FileInfo a <$> getModificationTime a

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

readAllFileInfos :: Bool -> [String] -> IO [FileInfo]
readAllFileInfos showHidden = foldl' f [] <.> mapM (readFileInfos showHidden)
  where f = unionBy ((==) `on` filePath)

testModified :: [FileInfo] -> IO Bool
testModified = any id <.> mapM t
  where t (FileInfo p time) = ifM (doesDirectoryExist p) 
                                  (return False)  -- don't test modification time 
                                                  -- if directory
                                  ((time<) <$> getModificationTime p)

testDeleted :: [FileInfo] -> IO Bool
testDeleted = any id <.> mapM f
  where f (FileInfo p _) = not . or <$> sequence [doesDirectoryExist p,
                                                  doesFileExist p]

hasNewFiles :: [FileInfo] -> [FileInfo] -> Bool
hasNewFiles oldFileInfos newFileInfos = not $ null$ filter f newFileInfos
  where f (FileInfo p1 _) = not.isJust $ find ((p1==) . filePath) oldFileInfos

ifM :: (Monad m) => m Bool -> m b -> m b -> m b
ifM mb mt me = do b <- mb; if b then mt else me

infixl 8 <.> 
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(<.>) f m = fmap f . m

