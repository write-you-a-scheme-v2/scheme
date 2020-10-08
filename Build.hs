{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Main where

import Prelude
import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["output/scheme.html"]
    want ["wiki"]

    phony "wiki" $ do
      files <- fmap ("docs/" </>) <$> getDirectoryFiles "docs" ["*.md"]
      let targets = ["output" </> (f -<.> "wiki") | f <- files]
      need targets

    "output/scheme.html" %> \out -> do
        need ["resources/page.tmpl"]
        files <- fmap ("docs/" </>) <$> getDirectoryFiles "docs" ["*.md"]
        need files
        cmd ("pandoc" :: String) files ["-o" :: String] out (["--template", "resources/page.tmpl"] :: [String])

    "output//*.wiki" %> \out -> do
        let src = dropDirectory1 $ out -<.> "md"
        cmd ("pandoc" :: String) src ("-o" :: String) out (["-t", "mediawiki"] :: [String])
