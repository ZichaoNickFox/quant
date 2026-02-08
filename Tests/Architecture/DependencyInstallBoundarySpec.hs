module Tests.Architecture.DependencyInstallBoundarySpec (tests) where

import Prelude

import Data.List (isInfixOf, isPrefixOf)
import System.Directory (doesFileExist)
import Test.Hspec

tests :: Spec
tests = do
  describe "Architecture constraints for dependency installation" do
    it "install steps must stay in Nix files, not runtime scripts" do
      offenders <- findInstallOffenders
      offenders `shouldBe` []

findInstallOffenders :: IO [String]
findInstallOffenders = do
  let scriptFiles =
        [ "run"
        , "test"
        , "e2e"
        , "smoke"
        , "push"
        ]
  fmap concat (mapM findInFile scriptFiles)

findInFile :: FilePath -> IO [String]
findInFile file = do
  exists <- doesFileExist file
  if not exists
    then pure []
    else do
      content <- readFile file
      pure
        [ file <> ":" <> show lineNo <> ": " <> lineTxt
        | (lineNo, rawLine) <- zip [1 :: Int ..] (lines content)
        , let lineTxt = trim rawLine
        , not (null lineTxt)
        , not ("#" `isPrefixOf` lineTxt)
        , isInstallLine lineTxt
        ]

isInstallLine :: String -> Bool
isInstallLine line =
  any (`isInfixOf` line)
    [ "npm install"
    , "pnpm install"
    , "yarn install"
    , "pip install"
    , "pip3 install"
    , "poetry install"
    , "spago install"
    , "playwright install"
    ]

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t'

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse
