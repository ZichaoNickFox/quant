{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Tests.Web.Fetcher.PythonSpec (tests) where

import           Control.Exception (bracket, finally)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.Text as Text
import           IHP.Log.Types (Logger, newLogger)
import           Prelude
import           System.Directory
  ( Permissions (..)
  , createDirectory
  , createDirectoryIfMissing
  , getCurrentDirectory
  , getPermissions
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  , setCurrentDirectory
  , setPermissions
  )
import           System.FilePath ((</>))
import           System.IO (hClose, openTempFile)
import           Test.Hspec
import           Web.Fetcher.Python
import           Web.Prelude (def)

tests :: Spec
tests = do
  describe "Web.Fetcher.Python.runPython" do
    it "returns Right on success" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      script <- writeScript dir "success.py" "import json\nprint(json.dumps({'ok': True}))\n"
      result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
      result `shouldBe` Right (A.object ["ok" .= True])

    it "returns JsonDecodeError on invalid JSON" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      script <- writeScript dir "invalid_json.py" "print('not json')\n"
      result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
      case result of
        Left (PythonJsonDecodeError _) -> pure ()
        other -> expectationFailure ("expected PythonJsonDecodeError, got " <> show other)

    it "returns JsonDecodeError on valid JSON with wrong shape" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      script <- writeScript dir "wrong_shape.py" "print('null')\n"
      result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError [Int])
      case result of
        Left (PythonJsonDecodeError _) -> pure ()
        other -> expectationFailure ("expected PythonJsonDecodeError, got " <> show other)

    it "returns ExitFailure on non-zero exit" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      script <- writeScript dir "exit_fail.py" "import sys\nsys.exit(2)\n"
      result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
      case result of
        Left (PythonExitFailure 2 _) -> pure ()
        other -> expectationFailure ("expected PythonExitFailure 2, got " <> show other)

    it "returns Timeout when execution exceeds 10s" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      script <- writeScript dir "timeout.py" "import time\ntime.sleep(2)\nprint('{\"ok\": true}')\n"
      result <- runPython 1000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
      case result of
        Left PythonTimeout -> pure ()
        other -> expectationFailure ("expected PythonTimeout, got " <> show other)

    it "returns SpawnError when venv python is not executable" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      setupFakeVenvPython dir False
      script <- writeScript dir "spawn_fail.py" "print('{\"ok\": true}')\n"
      withWorkingDir dir do
        result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
        case result of
          Left (PythonSpawnError _) -> pure ()
          other -> expectationFailure ("expected PythonSpawnError, got " <> show other)

    it "prefers .venv/bin/python when present" $ withLogger $ \logger -> withTempDir $ \dir -> do
      let ?context = logger
      setupFakeVenvPython dir True
      script <- writeScript dir "system.py" "import json\nprint(json.dumps({'source': 'system'}))\n"
      withWorkingDir dir do
        result <- runPython 10000 (Text.pack script) (A.object []) False :: IO (Either PythonError A.Value)
        result `shouldBe` Right (A.object ["source" .= ("venv" :: String)])

withLogger :: (Logger -> IO a) -> IO a
withLogger action = do
  logger <- newLogger def
  action logger

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = bracket createTempDir removeDirectoryRecursive action
  where
    createTempDir = do
      tmpRoot <- getTemporaryDirectory
      (path, handle) <- openTempFile tmpRoot "python-fetcher-"
      hClose handle
      removeFile path
      createDirectory path
      pure path

withWorkingDir :: FilePath -> IO a -> IO a
withWorkingDir dir action = do
  cwd <- getCurrentDirectory
  setCurrentDirectory dir
  finally action (setCurrentDirectory cwd)

writeScript :: FilePath -> FilePath -> String -> IO FilePath
writeScript dir name content = do
  let path = dir </> name
  writeFile path content
  pure path

setupFakeVenvPython :: FilePath -> Bool -> IO ()
setupFakeVenvPython dir isExecutable = do
  let venvBin = dir </> ".venv" </> "bin"
  createDirectoryIfMissing True venvBin
  let venvPython = venvBin </> "python"
  writeFile venvPython "#!/usr/bin/env bash\necho '{\"source\": \"venv\"}'\n"
  perms <- getPermissions venvPython
  setPermissions venvPython (perms { executable = isExecutable })
