{-# LANGUAGE ScopedTypeVariables #-}
module Web.Fetcher.Python
  ( PythonError (..)
  , renderPythonError
  , runPython
  ) where

import           Control.Exception             (IOException, displayException, try)
import           Data.Aeson
import           Data.Bool
import           Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import           System.Directory              (doesFileExist)
import           System.Exit
import           System.Process
import           System.Timeout                (timeout)
import           Web.Prelude                    hiding (error)

data PythonError
  = PythonTimeout
  | PythonJsonDecodeError Text.Text
  | PythonExitFailure Int Text.Text
  | PythonSpawnError Text.Text
  deriving (Eq, Show)

renderPythonError :: PythonError -> Text.Text
renderPythonError = \case
  PythonTimeout -> "timeout after 10s"
  PythonJsonDecodeError msg -> "json decode error: " <> msg
  PythonExitFailure code msg -> "exit code " <> tshow code <> " stderr: " <> msg
  PythonSpawnError msg -> "spawn error: " <> msg

runPython :: forall context result parameter.
             (?context :: context, LoggingProvider context, FromJSON result, ToJSON parameter)
          => Int -> Text.Text -> parameter -> Bool -> IO (Either PythonError result)
runPython timeoutMs scriptFullpath parameter printPythonLog = do
  let parameterJSON = LBS.unpack (encode parameter) :: String
  logInfo $ "[Python] Call python Parameter Json : " <> Text.pack parameterJSON

  pythonCmd <- pickPythonCmd
  let timeoutUs = max 1 timeoutMs * 1000
  result <- runWithTimeout timeoutUs pythonCmd scriptFullpath parameterJSON
  case result of
    Left err -> do
      logError $ "[Python] " <> renderPythonError err
      return (Left err)
    Right (exitCode, resultJSON, processErrorString) ->
      handleResult exitCode resultJSON processErrorString
  where
    pickPythonCmd :: IO FilePath
    pickPythonCmd = do
      let venvPython = ".venv/bin/python"
      useVenv <- doesFileExist venvPython
      return (if useVenv then venvPython else "python3")

    runWithTimeout
      :: Int
      -> FilePath
      -> Text.Text
      -> String
      -> IO (Either PythonError (ExitCode, String, String))
    runWithTimeout timeoutUs pythonCmd script parameterJson = do
      let args = [Text.unpack script, parameterJson]
      resultOrError <-
        (try $
          timeout timeoutUs $
            readProcessWithExitCode pythonCmd args "")
          :: IO (Either IOException (Maybe (ExitCode, String, String)))
      case resultOrError of
        Left ex ->
          return (Left (PythonSpawnError (Text.pack (displayException ex))))
        Right Nothing ->
          return (Left PythonTimeout)
        Right (Just result) ->
          return (Right result)

    handleResult
      :: ExitCode
      -> String
      -> String
      -> IO (Either PythonError result)
    handleResult ExitSuccess resultJSON _ = decodeResult resultJSON
    handleResult (ExitFailure code) _ processErrorString =
      return (Left (PythonExitFailure code (Text.pack processErrorString)))

    decodeResult :: String -> IO (Either PythonError result)
    decodeResult resultJSON = do
      let byteString = TextEncoding.encodeUtf8 (Text.pack resultJSON)
          lazyByteString = LBS.fromStrict byteString
      case eitherDecode lazyByteString of
        Right result -> do
          logInfo $ "[Python] Success" <> bool "" (" : " <> Text.pack resultJSON) printPythonLog
          return (Right result)
        Left jsonErrorString -> do
          let err = PythonJsonDecodeError (Text.pack jsonErrorString)
          logError $
            "[Python] Json parse error"
              <> bool "" (" : " <> Text.pack resultJSON) printPythonLog
              <> " Parser : "
              <> Text.pack jsonErrorString
          return (Left err)
