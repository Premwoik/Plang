module DirUtil where

import           System.Directory
import           System.Environment
import           System.Process

getExePath = do
  exePath <- getExecutablePath
  progName <- getProgName
  return $ take (length exePath - length progName - length "bin/") exePath

getNativeFiles = do
  path <- flip (++) "native" <$> getExePath
  map (cutNames path) <$> listDirectory path
  where
    cutNames nPath = drop (length nPath)

copyNativeToArduinoLib = do
  pathArd <- flip (++) "/Arduino/libraries/plang-natives" <$> getHomeDirectory
  nativePath <- flip (++) "native" <$> getExePath
  isExist <- doesDirectoryExist pathArd
  if isExist
    then  
--      putStrLn "Native libraries are copied"
      return ()
    else do
--      putStrLn "Copying native libraries!"
      putStrLn =<< readProcess "cp" ["-r", nativePath, pathArd] ""
      return ()
