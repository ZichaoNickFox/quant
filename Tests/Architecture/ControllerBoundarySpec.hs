module Tests.Architecture.ControllerBoundarySpec (tests) where

import Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for controllers" do
    it "non-page response boundaries (NotifyController exception included)" do
      files <- listDirectory "Web/Controller"
      offenders <- findCombinedControllerOffenders files
      offenders `shouldBe` []

findCombinedControllerOffenders :: [FilePath] -> IO [String]
findCombinedControllerOffenders files = do
  missingRenderJson <- findNonPageMissingRenderJsonOffenders files
  notifyViolations <- findNotifyControllerForbiddenRenderOffenders
  pure (missingRenderJson <> notifyViolations)

findNonPageMissingRenderJsonOffenders :: [FilePath] -> IO [String]
findNonPageMissingRenderJsonOffenders files = do
  let controllerFiles =
        filter
          (\f -> f /= "PageController.hs" && f /= "NotifyController.hs" && isInfixOf "Controller.hs" f)
          files
  badPerFile <- mapM findMissingRenderJsonInFile controllerFiles
  pure (concat badPerFile)

findMissingRenderJsonInFile :: FilePath -> IO [String]
findMissingRenderJsonInFile file = do
  content <- readFile ("Web/Controller" </> file)
  if any isRenderJsonCall (lines content)
    then pure []
    else pure [file <> ": missing renderJson call"]

findNotifyControllerForbiddenRenderOffenders :: IO [String]
findNotifyControllerForbiddenRenderOffenders = do
  content <- readFile ("Web/Controller" </> "NotifyController.hs")
  if any isForbiddenNotifyActionCall (lines content)
    then pure ["NotifyController.hs: must not call renderJson/renderHtml/redirect*"]
    else pure []

isRenderJsonCall :: String -> Bool
isRenderJsonCall line =
  case words (dropWhile isSpace line) of
    "renderJson" : _ -> True
    _ -> False

isForbiddenNotifyActionCall :: String -> Bool
isForbiddenNotifyActionCall line =
  case words (dropWhile isSpace line) of
    f : _ ->
      f == "renderJson"
        || f == "renderHtml"
        || f == "renderHTML"
        || f == "redirectTo"
        || isPrefixOf "redirect" f
    _ -> False
