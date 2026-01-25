module Web.Fetcher.Python
  ( runPython
  ) where

import           Data.Aeson
import           Data.Bool
import           Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import           System.Exit
import           System.Process
import           Web.Prelude                    hiding (error)

runPython :: (?context :: context, LoggingProvider context, FromJSON result, ToJSON parameter)
          => Text.Text -> parameter -> Bool -> IO (Maybe result)
runPython scriptFullpath parameter printPythonLog = do
  let parameterJSON = LBS.unpack (encode parameter) :: String
  logInfo $ "[Python] Call python Parameter Json : " <> Text.pack parameterJSON

  (exitCode, resultJSON, processErrorString) <- readProcessWithExitCode "python3" [Text.unpack scriptFullpath, parameterJSON] ""

  let byteString = TextEncoding.encodeUtf8 (Text.pack resultJSON)
      lazyByteString = LBS.fromStrict byteString
  case exitCode of
    ExitSuccess -> do
      let eitherResult = eitherDecode lazyByteString
      case eitherResult of
        Right result -> do
          logInfo $ "[Python] Success" <> bool "" (" : " <> Text.pack resultJSON) printPythonLog
          return result 
        Left jsonErrorString -> do
          logError $ "[Python] Json parse error" <> bool "" (" : " <> Text.pack resultJSON) printPythonLog <> "Parser : " <> Text.pack jsonErrorString
          return Nothing
    ExitFailure code -> do
      logError $ "[Python] Python error : code-" <> tshow code <> " message-" <> Text.pack processErrorString
      return Nothing
