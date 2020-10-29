module CLI where

import           Compiler
import           Console.Display
import           Console.Options
import           Data.Maybe      (fromJust, fromMaybe)
import qualified Data.Version    as V
import           DirUtil         (copyNativeToArduinoLib, getExePath)
import           System.Process

main :: IO ()
main =
  defaultMain $ do
    programName "plang compiler"
    programVersion (V.makeVersion [0, 1])
    programDescription "This is a tool for compiling and managing p lang projects."
    command "new" newAction
    command "build" buildAction
    command "upload" uploadAction

newAction = do
  description "Create a new project from a default template."
  allPath <- remainingArguments "FILE"
  action $ \toParam -> do
    let file = head $ toParam allPath
    pwd <- getExePath
    res <- readProcess "/bin/bash" [pwd ++ "new.sh", file] ""
    putStrLn res

buildAction = do
  description "Build and compile program."
  flagTarget <-
    flagParam (FlagDescription "Target board type" <> FlagShort 't' <> FlagLong "target") (FlagRequired $ \s -> Right s)
  action $ \toParam -> do
    copyNativeToArduinoLib
    let target = fromMaybe "uno" $ toParam flagTarget
    putStrLn $ "Building for target: " ++ target
    putStrLn "-------------"
    res <- compile "src/" "Main"
    writeFile "build/out.h" $ concat $ snd res
    putStrLn "\nStatistics:"
    putStrLn "-------------"
    pwd <- getExePath
    putStrLn =<< readProcess "/bin/bash" [pwd ++ "verify.sh", target] ""

uploadAction = do
  description "Upload built program to Arduino board."
  flagTarget <-
    flagParam (FlagDescription "Target board type" <> FlagShort 't' <> FlagLong "target") (FlagRequired $ \s -> Right s)
  flagPort <-
    flagParam (FlagDescription "Target board type" <> FlagShort 'p' <> FlagLong "port") (FlagRequired $ \s -> Right s)
  action $ \toParam -> do
    let target = fromMaybe "uno" $ toParam flagTarget
    let port = fromMaybe "ACM0" $ toParam flagPort
    putStrLn $ "Uploading for target: " ++ target
    putStrLn $ "PORT: " ++ port
    pwd <- getExePath
    putStrLn =<< readProcess "/bin/bash" [pwd ++ "upload.sh", target, port] ""
